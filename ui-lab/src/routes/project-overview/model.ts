/**
 * Pure derivation layer for the project overview — classification of
 * the agent/human task layers, swarm rollups, milestone progress,
 * estimate math, and project health. No React in here; everything is
 * computed once per query result and memoized by the page.
 *
 * THE core problem this feeds: agent-scale vs human-scale tasks. A
 * project like the test org's "Hermes Integration" carries a handful
 * of human tasks/epics and a ~56-task agent swarm; the human layer is
 * the default view, with each epic rolling its swarm up into a strip.
 */
import type {
  AgentRef,
  Estimate,
  TaskInfo,
} from "@/generated/taskservicerpc.generated";
import type { Milestone } from "@/generated/milestoneservicerpc.generated";
import type { ProjectInfo } from "@/generated/projectservicerpc.generated";

// ── agent / human classification ────────────────────────────────────

/**
 * Is this assignee an agent? The wire decodes `AgentRef::Agent` as
 * `{ tag: 'Agent', name, model_version }`; the on-disk frontmatter
 * (and possibly older peers) spells it `{ kind: 'agent', ... }` — we
 * accept both shapes.
 */
export function isAgentRef(a: AgentRef): boolean {
  const raw = a as unknown as { tag?: string; kind?: string };
  return (
    raw.tag === "Agent" || raw.kind === "agent" || raw.kind === "Agent"
  );
}

/** The claiming agent (`name` + optional model), if any. */
export function agentClaimant(
  t: TaskInfo,
): { name: string; model: string | null } | null {
  for (const a of t.workflow?.assignees ?? []) {
    if (isAgentRef(a)) {
      const raw = a as unknown as {
        name?: string;
        model_version?: string | null;
      };
      return { name: raw.name ?? "agent", model: raw.model_version ?? null };
    }
  }
  return null;
}

/**
 * Agent-layer task: carries an `AgentRef::Agent` assignee (the claim),
 * or — for unclaimed swarm members that have no assignee yet — the
 * `agent` tag the dispatcher stamps on dispatched subtasks.
 */
export function isAgentTask(t: TaskInfo): boolean {
  return agentClaimant(t) !== null || t.tags.includes("agent");
}

/** Epic: a human-layer task other tasks point at via `workflow.parent`. */
export function isEpicTag(t: TaskInfo): boolean {
  return t.tags.includes("epic");
}

// ── status buckets ──────────────────────────────────────────────────

export type StatusBucket =
  | "done"
  | "running"
  | "blocked"
  | "review"
  | "cancelled"
  | "open";

/**
 * Park-for-review detection. Convention: a parked task whose park
 * reason is prefixed `review-required:`. The reason rides whatever
 * field the parking flow wrote — we check tags (`review-required:…`),
 * the status itself, and the first line of details.
 */
export function reviewReason(t: TaskInfo): string | null {
  for (const tag of t.tags) {
    if (tag.startsWith("review-required")) {
      return tag.includes(":") ? tag.slice(tag.indexOf(":") + 1).trim() : "review required";
    }
  }
  const firstLine = t.details.trimStart().split("\n", 1)[0] ?? "";
  if (firstLine.toLowerCase().startsWith("review-required:")) {
    return firstLine.slice("review-required:".length).trim();
  }
  if (t.status === "parked" && firstLine.toLowerCase().includes("review")) {
    return firstLine;
  }
  return null;
}

/**
 * Bucket a task's status, deriving `blocked` from unresolved blockers
 * (`workflow.blockers` entries whose task isn't done yet) — the wire
 * has no first-class blocked status.
 */
export function statusBucket(
  t: TaskInfo,
  byId: Map<string, TaskInfo>,
): StatusBucket {
  if (reviewReason(t) !== null) return "review";
  switch (t.status) {
    case "done":
    case "completed":
      return "done";
    case "cancelled":
      return "cancelled";
    case "in-progress":
    case "active":
    case "running":
      return "running";
    case "blocked":
      return "blocked";
  }
  for (const blocker of t.workflow?.blockers ?? []) {
    const other = byId.get(String(blocker));
    // Unknown blocker id still blocks — absence of evidence isn't done.
    if (!other || statusOf(other) !== "done") return "blocked";
  }
  return "open";
}

function statusOf(t: TaskInfo): "done" | "other" {
  return t.status === "done" || t.status === "completed" ? "done" : "other";
}

export interface SwarmCounts {
  done: number;
  running: number;
  blocked: number;
  review: number;
  open: number;
  cancelled: number;
  total: number;
}

export function emptyCounts(): SwarmCounts {
  return {
    done: 0,
    running: 0,
    blocked: 0,
    review: 0,
    open: 0,
    cancelled: 0,
    total: 0,
  };
}

export function countTasks(
  tasks: TaskInfo[],
  byId: Map<string, TaskInfo>,
): SwarmCounts {
  const c = emptyCounts();
  for (const t of tasks) {
    c[statusBucket(t, byId)] += 1;
    c.total += 1;
  }
  return c;
}

/** Completion ratio that ignores cancelled work (Linear convention). */
export function completionPct(c: SwarmCounts): number {
  const denom = c.total - c.cancelled;
  return denom === 0 ? 0 : Math.round((c.done / denom) * 100);
}

// ── the two layers + epic rollups ───────────────────────────────────

export interface EpicRollup {
  epic: TaskInfo;
  /** The epic's agent-subtask swarm (children via `workflow.parent`). */
  swarm: TaskInfo[];
  counts: SwarmCounts;
}

export interface ProjectModel {
  byId: Map<string, TaskInfo>;
  /** Human-layer rows that are NOT epics. */
  humans: TaskInfo[];
  /** Epics with their swarm rollups (largest swarm first). */
  epics: EpicRollup[];
  /** Every agent-layer task in the project (the full swarm). */
  agents: TaskInfo[];
  /** Agent tasks parked for human review — the review lane. */
  review: TaskInfo[];
  all: SwarmCounts;
  agentCounts: SwarmCounts;
  humanCounts: SwarmCounts;
}

export function buildModel(tasks: TaskInfo[]): ProjectModel {
  const byId = new Map<string, TaskInfo>();
  for (const t of tasks) byId.set(String(t.id), t);

  const agents: TaskInfo[] = [];
  const humanLayer: TaskInfo[] = [];
  for (const t of tasks) (isAgentTask(t) ? agents : humanLayer).push(t);

  const swarmByParent = new Map<string, TaskInfo[]>();
  for (const t of agents) {
    const parent = t.workflow?.parent ? String(t.workflow.parent) : "";
    if (!parent) continue;
    const bucket = swarmByParent.get(parent);
    if (bucket) bucket.push(t);
    else swarmByParent.set(parent, [t]);
  }

  const epics: EpicRollup[] = [];
  const humans: TaskInfo[] = [];
  for (const t of humanLayer) {
    const swarm = swarmByParent.get(String(t.id)) ?? [];
    if (swarm.length > 0 || isEpicTag(t)) {
      epics.push({ epic: t, swarm, counts: countTasks(swarm, byId) });
    } else {
      humans.push(t);
    }
  }
  epics.sort((a, b) => b.swarm.length - a.swarm.length);
  humans.sort(
    (a, b) =>
      bucketRank(statusBucket(a, byId)) - bucketRank(statusBucket(b, byId)) ||
      priorityRank(a.priority) - priorityRank(b.priority) ||
      a.title.localeCompare(b.title),
  );

  const review = agents.filter((t) => reviewReason(t) !== null);

  return {
    byId,
    humans,
    epics,
    agents,
    review,
    all: countTasks(tasks, byId),
    agentCounts: countTasks(agents, byId),
    humanCounts: countTasks(humanLayer, byId),
  };
}

export function bucketRank(b: StatusBucket): number {
  switch (b) {
    case "review":
      return 0;
    case "blocked":
      return 1;
    case "running":
      return 2;
    case "open":
      return 3;
    case "done":
      return 4;
    case "cancelled":
      return 5;
  }
}

export function priorityRank(p: string): number {
  switch (p) {
    case "urgent":
      return 0;
    case "high":
      return 1;
    case "normal":
    case "medium":
      return 2;
    case "low":
      return 3;
    default:
      return 4;
  }
}

// ── estimates ───────────────────────────────────────────────────────

/** Fibonacci-ish point value per t-shirt size; `Points` passes through. */
export function estimatePoints(e: Estimate | null | undefined): number {
  switch (e?.tag) {
    case "XS":
      return 1;
    case "S":
      return 2;
    case "M":
      return 3;
    case "L":
      return 5;
    case "XL":
      return 8;
    case "Points":
      return e.value;
    default:
      return 0;
  }
}

export interface EstimateRollup {
  total: number;
  done: number;
  /** How many tasks actually carry an estimate. */
  estimated: number;
}

export function rollupEstimates(
  tasks: TaskInfo[],
  byId: Map<string, TaskInfo>,
): EstimateRollup {
  let total = 0;
  let done = 0;
  let estimated = 0;
  for (const t of tasks) {
    const pts = estimatePoints(t.workflow?.estimate);
    if (pts === 0) continue;
    estimated += 1;
    total += pts;
    if (statusBucket(t, byId) === "done") done += pts;
  }
  return { total, done, estimated };
}

// ── milestones ──────────────────────────────────────────────────────

export interface MilestoneProgress {
  milestone: Milestone;
  counts: SwarmCounts;
  pct: number;
  /** ISO date string when the milestone carries one. */
  due: string | null;
  overdue: boolean;
}

export function milestoneProgress(
  milestones: Milestone[],
  tasks: TaskInfo[],
  byId: Map<string, TaskInfo>,
): MilestoneProgress[] {
  return milestones.map((m) => {
    const mine = tasks.filter(
      (t) => String(t.milestone_id ?? "") === String(m.id),
    );
    const counts = countTasks(mine, byId);
    const due = m.due_date == null ? null : String(m.due_date);
    const closed = m.status === "done" || m.status === "closed";
    return {
      milestone: m,
      counts,
      pct: closed ? 100 : completionPct(counts),
      due,
      overdue: !closed && due !== null && due < todayIso(),
    };
  });
}

function todayIso(): string {
  return new Date().toISOString().slice(0, 10);
}

// ── health ──────────────────────────────────────────────────────────

export type Health = {
  label: "On track" | "At risk" | "Off track";
  tone: "ok" | "warn" | "bad";
};

/**
 * Derived health (the wire has no first-class health field yet):
 * overdue milestone or >15% of live work blocked → off track;
 * any blocked/review-parked work → at risk; else on track.
 */
export function deriveHealth(
  all: SwarmCounts,
  milestones: MilestoneProgress[],
): Health {
  const live = all.total - all.done - all.cancelled;
  if (
    milestones.some((m) => m.overdue) ||
    (live > 0 && all.blocked / live > 0.15)
  ) {
    return { label: "Off track", tone: "bad" };
  }
  if (all.blocked > 0 || all.review > 0) {
    return { label: "At risk", tone: "warn" };
  }
  return { label: "On track", tone: "ok" };
}

// ── misc formatting ─────────────────────────────────────────────────

/** Compact relative age ("3h", "2d") from an ISO timestamp. */
export function relativeAge(iso: unknown): string {
  if (iso == null) return "";
  const then = Date.parse(String(iso));
  if (Number.isNaN(then)) return "";
  const s = Math.max(0, (Date.now() - then) / 1000);
  if (s < 60) return `${Math.floor(s)}s`;
  if (s < 3600) return `${Math.floor(s / 60)}m`;
  if (s < 86400) return `${Math.floor(s / 3600)}h`;
  if (s < 86400 * 30) return `${Math.floor(s / 86400)}d`;
  return `${Math.floor(s / (86400 * 30))}mo`;
}

export function formatDate(iso: unknown): string {
  if (iso == null) return "";
  const d = new Date(String(iso));
  if (Number.isNaN(d.getTime())) return String(iso);
  return d.toLocaleDateString(undefined, {
    month: "short",
    day: "numeric",
    year: d.getFullYear() === new Date().getFullYear() ? undefined : "numeric",
  });
}

export function estimateLabel(e: Estimate | null | undefined): string | null {
  if (!e) return null;
  return e.tag === "Points" ? `${e.value}pt` : e.tag;
}
