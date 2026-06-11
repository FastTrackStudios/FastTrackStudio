/**
 * TanStack Query options for the vox-backed data this lab renders.
 * Fetch-and-render only — no mutations, no caching cleverness; the
 * prototype→port loop keeps this layer deliberately thin.
 */
import { queryOptions } from "@tanstack/react-query";
import { projects, tasks, unwrap } from "./vox";

export const projectListQuery = queryOptions({
  queryKey: ["projects"],
  queryFn: async () => {
    const client = await projects();
    return unwrap(await client.list());
  },
});

export const projectQuery = (id: string) =>
  queryOptions({
    queryKey: ["projects", id],
    queryFn: async () => {
      const client = await projects();
      // Uuid crosses the wire as a string; the generated signature is
      // `unknown` because codegen has no TS mapping for uuid::Uuid yet.
      return unwrap(await client.get(id));
    },
  });

/**
 * Tasks belonging to one project. `TaskService.list()` has no filters
 * by design ("clients filter client-side after fetching" — see
 * features/task/task/src/service.rs), so that's what we do.
 */
export const projectTasksQuery = (projectId: string) =>
  queryOptions({
    queryKey: ["tasks", { project: projectId }],
    queryFn: async () => {
      const client = await tasks();
      const all = unwrap(await client.list());
      return all.filter((t) => String(t.project_id ?? "") === projectId);
    },
  });
