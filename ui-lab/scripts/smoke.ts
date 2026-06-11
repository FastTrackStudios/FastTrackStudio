/**
 * Wire-level smoke test against the LIVE task-server, run from node
 * (node >= 22 ships a browser-compatible global WebSocket, which is
 * all @bearcove/vox-ws needs).
 *
 *   pnpm smoke            # expects the server on ws://127.0.0.1:18080
 *   VITE_TASK_ORG=... VITE_TASK_SERVER=... pnpm smoke
 *
 * Exercises exactly what the two lab routes exercise: connect, list
 * projects, get one project, list its tasks.
 */
import { projects, tasks, unwrap, VOX_URL } from "../src/lib/vox";

async function main() {
  console.log(`connecting to ${VOX_URL} ...`);

  const projectClient = await projects();
  const all = unwrap(await projectClient.list());
  console.log(`ProjectServiceRpc.list -> ${all.length} project(s)`);
  for (const p of all.slice(0, 5)) {
    console.log(
      `  - ${p.title || p.path} [${p.status} / ${p.project_type || "project"}] ${String(p.id)}`,
    );
  }

  const taskClient = await tasks();
  const allTasks = unwrap(await taskClient.list());
  console.log(`TaskServiceRpc.list -> ${allTasks.length} task(s)`);

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

  console.log("smoke OK");
  process.exit(0);
}

main().catch((e) => {
  console.error("smoke FAILED:", e);
  process.exit(1);
});
