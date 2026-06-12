/**
 * Wire-level smoke test against the LIVE task-server, run from node
 * (node >= 22 ships a browser-compatible global WebSocket, which is
 * all @bearcove/vox-ws needs).
 *
 *   pnpm smoke            # expects the server on ws://127.0.0.1:18080
 *   VITE_TASK_ORG=... VITE_TASK_SERVER=... pnpm smoke
 *
 * Exercises what the lab routes exercise: discover orgs (well-known),
 * connect per org, list projects, get one project, list its tasks +
 * milestones, and a real AuthService sign-in against the home org.
 */
import { channel } from "@bearcove/vox-core";

import type { TaskEvent } from "../src/generated/taskservicestream.generated";
import { fetchOrgs, homeSlug } from "../src/lib/orgs";
import {
  DEFAULT_ORG,
  authFor,
  milestonesFor,
  projectsFor,
  taskStreamFor,
  tasksFor,
  unwrap,
  voxUrlFor,
  workstreamsFor,
} from "../src/lib/vox";

async function main() {
  // Org discovery — the same well-known fetch the org switcher uses.
  const orgs = await fetchOrgs();
  console.log(
    `well-known -> ${orgs.length} org(s): ${orgs.map((o) => o.slug).join(", ")}`,
  );
  if (orgs.length === 0) throw new Error("well-known returned zero orgs");
  const home = homeSlug(orgs);

  const org = DEFAULT_ORG;
  console.log(`connecting to ${voxUrlFor(org)} ...`);

  const projectClient = await projectsFor(org);
  const all = unwrap(await projectClient.list());
  console.log(`ProjectServiceRpc.list -> ${all.length} project(s)`);
  for (const p of all.slice(0, 5)) {
    console.log(
      `  - ${p.title || p.path} [${p.status} / ${p.project_type || "project"}] ${String(p.id)}`,
    );
  }

  const taskClient = await tasksFor(org);
  const allTasks = unwrap(await taskClient.list());
  console.log(`TaskServiceRpc.list -> ${allTasks.length} task(s)`);

  const milestoneClient = await milestonesFor(org);
  const allMilestones = unwrap(await milestoneClient.list());
  console.log(`MilestoneServiceRpc.list -> ${allMilestones.length} milestone(s)`);

  // Find a project whose `get` round-trips. A vault page without a
  // persisted `id:` in its frontmatter gets a fresh backfilled UUID on
  // every scan, so `get(list()[i].id)` can legitimately be NotFound
  // for such pages — skip those rather than failing the smoke.
  let fetchedOne = false;
  for (const p of all) {
    const r = await projectClient.get(p.id);
    if (!r.ok) {
      console.log(
        `  (skip ${p.title}: get -> ${r.error.tag}; unpersisted frontmatter id)`,
      );
      continue;
    }
    const its = allTasks.filter(
      (t) => String(t.project_id ?? "") === String(p.id),
    );
    console.log(
      `ProjectServiceRpc.get(${String(p.id)}) -> "${r.value.title}", ${its.length} task(s) attached`,
    );
    fetchedOne = true;
    break;
  }
  if (all.length > 0 && !fetchedOne) {
    throw new Error("get() failed for every project returned by list()");
  }

  // Workstream round-trip: list + the server-side rollup verb. The
  // `test` org carries the synthetic workstream fixtures; fall back to
  // the default org when it isn't hosted. Read-only — smoke never
  // mutates.
  const wsOrg = orgs.some((o) => o.slug === "test") ? "test" : org;
  const wsClient = await workstreamsFor(wsOrg);
  const workstreams = unwrap(await wsClient.list(null));
  console.log(
    `WorkstreamServiceRpc.list(${wsOrg}) -> ${workstreams.length} workstream(s)`,
  );
  if (workstreams.length > 0) {
    const first = workstreams[0];
    const { workstream: w, rollup } = unwrap(await wsClient.rollup(first.id));
    if (String(w.id) !== String(first.id)) {
      throw new Error("rollup returned a different workstream");
    }
    if (rollup.done > rollup.total || rollup.in_progress > rollup.total) {
      throw new Error(`rollup arithmetic is off: ${JSON.stringify(rollup)}`);
    }
    console.log(
      `WorkstreamServiceRpc.rollup("${w.title}") -> ${rollup.done}/${rollup.total} done, ` +
        `${rollup.in_progress} in progress, ${rollup.blocked} blocked, ${rollup.estimate_points_sum} pts`,
    );
  }

  // `#[subscribe]` stream smoke: subscribe to TaskServiceStream
  // events, mutate, and assert the events arrive (fetch-once-then-
  // fold is the board's live path). Mutations are confined to the
  // `test` org; when it isn't hosted the stream check is skipped.
  if (orgs.some((o) => o.slug === "test")) {
    const stream = await taskStreamFor("test");
    const [tx, rx] = channel<TaskEvent>();
    // Subscribe BEFORE mutating — the awaited call returns once the
    // sink is attached to the backend hub, so nothing is missed.
    await stream.events(tx);

    const testTasks = await tasksFor("test");
    const created = unwrap(
      await testTasks.create({
        id: "00000000-0000-0000-0000-000000000000", // nil -> backend assigns
        path: "",
        title: `smoke stream probe ${Date.now()}`,
        status: "open",
        priority: "normal",
        due: null,
        scheduled: null,
        tags: ["smoke"],
        contexts: [],
        projects: [],
        project_id: null,
        milestone_id: null,
        time_estimate: null,
        time_entries: [],
        recurrence: null,
        recurrence_anchor: null,
        complete_instances: [],
        completed_date: null,
        agent_profile: "",
        dispatched_agent_tasks: [],
        date_created: null,
        date_modified: null,
        details: "",
        workflow: null,
      }),
    );
    const next = async (): Promise<TaskEvent> => {
      const timeout = new Promise<never>((_, reject) =>
        setTimeout(
          () => reject(new Error("no TaskEvent within 10s of a mutation")),
          10_000,
        ),
      );
      const ev = await Promise.race([rx.recv(), timeout]);
      if (ev === null) throw new Error("task event stream closed early");
      return ev;
    };

    const upserted = await next();
    if (
      upserted.tag !== "Upserted" ||
      String(upserted.value.id) !== String(created.id)
    ) {
      throw new Error(
        `expected Upserted(${String(created.id)}) after create, got ${JSON.stringify(upserted).slice(0, 200)}`,
      );
    }
    unwrap(await testTasks.delete(created.id));
    const deleted = await next();
    if (deleted.tag !== "Deleted" || String(deleted.value) !== String(created.id)) {
      throw new Error(
        `expected Deleted(${String(created.id)}) after delete, got ${JSON.stringify(deleted).slice(0, 200)}`,
      );
    }
    console.log(
      `TaskServiceStream.events(test) -> Upserted + Deleted observed for ${String(created.id)}`,
    );
  } else {
    console.log("(skip stream smoke: `test` org not hosted)");
  }

  // Real sign-in against the home org — the account switcher's path.
  const auth = await authFor(home);
  const bundle = unwrap(
    await auth.signInEmailPassword({
      email: "guest@fasttrackstudios.com",
      password: "dev-guest-2026",
      ip_address: null,
      user_agent: "ui-lab-smoke",
    }),
  );
  const who = unwrap(await auth.whoami(bundle.token));
  console.log(
    `AuthService.signInEmailPassword(guest) -> whoami ${who.email} (${who.name ?? "?"}) @ ${home}`,
  );
  unwrap(await auth.signOut(bundle.token));

  console.log("smoke OK");
  process.exit(0);
}

main().catch((e) => {
  console.error("smoke FAILED:", e);
  process.exit(1);
});
