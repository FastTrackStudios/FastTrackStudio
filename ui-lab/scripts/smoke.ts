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
import { fetchOrgs, homeSlug } from "../src/lib/orgs";
import {
  DEFAULT_ORG,
  authFor,
  milestonesFor,
  projectsFor,
  tasksFor,
  unwrap,
  voxUrlFor,
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
