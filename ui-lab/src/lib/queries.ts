/**
 * TanStack Query options for the vox-backed data this lab renders.
 * Every query is **org-scoped** (the org slug leads the key); pages
 * fan out across the org switcher's selection with `useQueries` and
 * tag rows with their org. Fetch-and-render only — no mutations; the
 * prototype→port loop keeps this layer deliberately thin.
 */
import { queryOptions } from "@tanstack/react-query";

import type { Milestone } from "@/generated/milestoneservicerpc.generated";
import type { ProjectInfo } from "@/generated/projectservicerpc.generated";
import type { TaskInfo } from "@/generated/taskservicerpc.generated";
import { milestonesFor, projectsFor, tasksFor, unwrap } from "./vox";

/** A row tagged with the org it came from (the "All orgs" fan-out). */
export interface OrgProject {
  org: string;
  project: ProjectInfo;
}

export const projectListQuery = (org: string) =>
  queryOptions({
    queryKey: [org, "projects"],
    queryFn: async (): Promise<OrgProject[]> => {
      const client = await projectsFor(org);
      return unwrap(await client.list()).map((project) => ({ org, project }));
    },
  });

export const projectQuery = (org: string, id: string) =>
  queryOptions({
    queryKey: [org, "projects", id],
    queryFn: async () => {
      const client = await projectsFor(org);
      // Uuid crosses the wire as a string; the generated signature is
      // `unknown` because codegen has no TS mapping for uuid::Uuid yet.
      return unwrap(await client.get(id));
    },
  });

/**
 * Tasks belonging to one project. `TaskService.list()` has no filters
 * by design ("clients filter client-side after fetching" — see
 * features/task/task/src/service.rs), so that's what we do. The
 * unfiltered list is cached once per org under `[org, "tasks"]`.
 */
export const orgTasksQuery = (org: string) =>
  queryOptions({
    queryKey: [org, "tasks"],
    queryFn: async (): Promise<TaskInfo[]> => {
      const client = await tasksFor(org);
      return unwrap(await client.list());
    },
  });

export const projectTasksQuery = (org: string, projectId: string) =>
  queryOptions({
    ...orgTasksQuery(org),
    select: (all: TaskInfo[]) =>
      all.filter((t) => String(t.project_id ?? "") === projectId),
  });

/** Milestones of one project — same client-side-filter convention. */
export const orgMilestonesQuery = (org: string) =>
  queryOptions({
    queryKey: [org, "milestones"],
    queryFn: async (): Promise<Milestone[]> => {
      const client = await milestonesFor(org);
      return unwrap(await client.list());
    },
  });

export const projectMilestonesQuery = (org: string, projectId: string) =>
  queryOptions({
    ...orgMilestonesQuery(org),
    select: (all: Milestone[]) =>
      all.filter((m) => String(m.project_id ?? "") === projectId),
  });
