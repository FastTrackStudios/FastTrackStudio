/**
 * Agent-swarm rendering: the compact per-epic progress strip, the
 * dense virtualized subtask list it expands into, the full kanban
 * lens, and the review lane for parked review-required work.
 *
 * The swarm is the agent-scale side of the page — dozens of dispatched
 * subtasks per epic. Rows are fixed-height and windowed by a tiny
 * hand-rolled virtualizer (no dep; 56 rows today,500 by design), so
 * expanding an epic or opening the Agents lens stays O(viewport).
 */
import {
  useCallback,
  useMemo,
  useRef,
  useState,
  type CSSProperties,
  type ReactNode,
} from "react";
import {
  AlertTriangle,
  Bot,
  CheckCircle2,
  ChevronRight,
  Circle,
  CircleDashed,
  CircleSlash,
  Eye,
  Loader2,
} from "lucide-react";

import { Badge } from "@/components/ui/badge";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "@/components/ui/tooltip";
import type { TaskInfo } from "@/generated/taskservicerpc.generated";
import { cn } from "@/lib/utils";
import {
  agentClaimant,
  bucketRank,
  relativeAge,
  reviewReason,
  statusBucket,
  type ProjectModel,
  type StatusBucket,
  type SwarmCounts,
} from "./model";

// ── status visual language ──────────────────────────────────────────

export const BUCKET_META: Record<
  StatusBucket,
  { label: string; bar: string; text: string; icon: ReactNode }
> = {
  done: {
    label: "done",
    bar: "bg-emerald-500",
    text: "text-emerald-500",
    icon: <CheckCircle2 className="size-3.5 text-emerald-500" />,
  },
  running: {
    label: "running",
    bar: "bg-blue-500",
    text: "text-blue-500",
    icon: <Loader2 className="size-3.5 text-blue-500" />,
  },
  blocked: {
    label: "blocked",
    bar: "bg-red-500",
    text: "text-red-500",
    icon: <CircleSlash className="size-3.5 text-red-500" />,
  },
  review: {
    label: "needs review",
    bar: "bg-purple-500",
    text: "text-purple-500",
    icon: <Eye className="size-3.5 text-purple-500" />,
  },
  open: {
    label: "open",
    bar: "bg-muted-foreground/30",
    text: "text-muted-foreground",
    icon: <Circle className="size-3.5 text-muted-foreground/70" />,
  },
  cancelled: {
    label: "cancelled",
    bar: "bg-muted-foreground/15",
    text: "text-muted-foreground/60",
    icon: <CircleDashed className="size-3.5 text-muted-foreground/50" />,
  },
};

const STRIP_ORDER: StatusBucket[] = [
  "done",
  "running",
  "review",
  "blocked",
  "open",
  "cancelled",
];

/**
 * The compact swarm rollup: a segmented bar + "23 done · 12 running ·
 * 3 blocked" counts. This is how a 50-task swarm reads at a glance on
 * its epic's row.
 */
export function SwarmStrip({
  counts,
  className,
}: {
  counts: SwarmCounts;
  className?: string;
}) {
  if (counts.total === 0) return null;
  const parts = STRIP_ORDER.filter((b) => counts[b] > 0);
  return (
    <div className={cn("flex min-w-0 items-center gap-2.5", className)}>
      <div className="flex h-1.5 w-28 shrink-0 overflow-hidden rounded-full bg-muted">
        {parts.map((b) => (
          <div
            key={b}
            className={BUCKET_META[b].bar}
            style={{ width: `${(counts[b] / counts.total) * 100}%` }}
          />
        ))}
      </div>
      <span className="text-muted-foreground truncate text-[11px] tabular-nums">
        {parts
          .map((b) => `${counts[b]} ${BUCKET_META[b].label}`)
          .join(" · ")}
      </span>
    </div>
  );
}

// ── tiny fixed-row virtualizer ──────────────────────────────────────

/**
 * Windowed list over fixed-height rows. Renders only what's visible
 * (+overscan) inside its own scroll container — the page never mounts
 * the whole swarm at once.
 */
export function VirtualList<T>({
  items,
  rowHeight,
  maxHeight,
  renderRow,
  className,
}: {
  items: T[];
  rowHeight: number;
  maxHeight: number;
  renderRow: (item: T, index: number, style: CSSProperties) => ReactNode;
  className?: string;
}) {
  const [scrollTop, setScrollTop] = useState(0);
  const ref = useRef<HTMLDivElement>(null);
  const onScroll = useCallback(() => {
    if (ref.current) setScrollTop(ref.current.scrollTop);
  }, []);

  const viewport = Math.min(maxHeight, items.length * rowHeight);
  const overscan = 6;
  const first = Math.max(0, Math.floor(scrollTop / rowHeight) - overscan);
  const last = Math.min(
    items.length,
    Math.ceil((scrollTop + viewport) / rowHeight) + overscan,
  );

  return (
    <div
      ref={ref}
      onScroll={onScroll}
      className={cn("overflow-y-auto overscroll-contain", className)}
      style={{ maxHeight }}
    >
      <div
        className="relative w-full"
        style={{ height: items.length * rowHeight }}
      >
        {items.slice(first, last).map((item, i) =>
          renderRow(item, first + i, {
            position: "absolute",
            top: (first + i) * rowHeight,
            left: 0,
            right: 0,
            height: rowHeight,
          }),
        )}
      </div>
    </div>
  );
}

// ── dense swarm rows (expanded epic) ────────────────────────────────

export const SWARM_ROW_HEIGHT = 34;

function ClaimantChip({ task }: { task: TaskInfo }) {
  const claimant = agentClaimant(task);
  if (!claimant) return null;
  return (
    <span className="text-muted-foreground flex shrink-0 items-center gap-1 font-mono text-[10px]">
      <Bot className="size-3" />
      {claimant.name}
      {claimant.model ? `·${claimant.model}` : ""}
    </span>
  );
}

export function SwarmRow({
  task,
  byId,
  style,
}: {
  task: TaskInfo;
  byId: Map<string, TaskInfo>;
  style: CSSProperties;
}) {
  const bucket = statusBucket(task, byId);
  return (
    <div
      style={style}
      className="hover:bg-accent/50 flex items-center gap-2 border-b border-border/50 px-3 text-xs last:border-0"
    >
      <span className="shrink-0">{BUCKET_META[bucket].icon}</span>
      <span
        className={cn(
          "min-w-0 flex-1 truncate",
          bucket === "done" && "text-muted-foreground",
          bucket === "cancelled" && "text-muted-foreground/60 line-through",
        )}
      >
        {task.title || task.path}
      </span>
      <ClaimantChip task={task} />
      <span className="text-muted-foreground/70 w-7 shrink-0 text-right font-mono text-[10px] tabular-nums">
        {relativeAge(task.date_created)}
      </span>
    </div>
  );
}

/**
 * Sorted swarm for the expanded list: attention first (review,
 * blocked, running), then open, done, cancelled.
 */
export function sortSwarm(
  swarm: TaskInfo[],
  byId: Map<string, TaskInfo>,
): TaskInfo[] {
  return [...swarm].sort(
    (a, b) =>
      bucketRank(statusBucket(a, byId)) - bucketRank(statusBucket(b, byId)) ||
      a.title.localeCompare(b.title, undefined, { numeric: true }),
  );
}

/** An epic's human-scale row, expandable into its dense swarm list. */
export function EpicRow({
  epic,
  swarm,
  counts,
  byId,
}: {
  epic: TaskInfo;
  swarm: TaskInfo[];
  counts: SwarmCounts;
  byId: Map<string, TaskInfo>;
}) {
  const [open, setOpen] = useState(false);
  const sorted = useMemo(
    () => (open ? sortSwarm(swarm, byId) : []),
    [open, swarm, byId],
  );
  const bucket = statusBucket(epic, byId);

  return (
    <div className="border-b last:border-0">
      <button
        type="button"
        onClick={() => setOpen((v) => !v)}
        className="hover:bg-accent/50 flex w-full items-center gap-2.5 px-3 py-2.5 text-left transition-colors"
        aria-expanded={open}
      >
        <ChevronRight
          className={cn(
            "text-muted-foreground size-3.5 shrink-0 transition-transform",
            open && "rotate-90",
          )}
        />
        {BUCKET_META[bucket].icon}
        <span className="min-w-0 flex-1">
          <span className="block truncate text-sm font-medium">
            {epic.title}
          </span>
        </span>
        <Badge
          variant="outline"
          className="text-muted-foreground shrink-0 gap-1 font-mono text-[10px]"
        >
          <Bot className="size-3" />
          {counts.total}
        </Badge>
        <SwarmStrip counts={counts} className="hidden w-72 justify-end sm:flex" />
      </button>
      {open &&
        (swarm.length === 0 ? (
          <p className="text-muted-foreground border-t bg-card/50 px-9 py-3 text-xs">
            No agent subtasks dispatched yet.
          </p>
        ) : (
          <VirtualList
            items={sorted}
            rowHeight={SWARM_ROW_HEIGHT}
            maxHeight={SWARM_ROW_HEIGHT * 10.5}
            className="border-t bg-card/50"
            renderRow={(t, _i, style) => (
              <SwarmRow key={String(t.id)} task={t} byId={byId} style={style} />
            )}
          />
        ))}
    </div>
  );
}

// ── review lane ─────────────────────────────────────────────────────

/**
 * Parked review-required work, surfaced above everything else — the
 * human's queue. Only renders when such items exist.
 */
export function ReviewLane({
  tasks,
  byId,
}: {
  tasks: TaskInfo[];
  byId: Map<string, TaskInfo>;
}) {
  if (tasks.length === 0) return null;
  return (
    <section className="rounded-lg border border-purple-500/40 bg-purple-500/5">
      <header className="flex items-center gap-2 border-b border-purple-500/20 px-3 py-2">
        <Eye className="size-3.5 text-purple-500" />
        <h3 className="text-sm font-medium">Needs your review</h3>
        <Badge className="bg-purple-500/15 text-purple-500" variant="secondary">
          {tasks.length}
        </Badge>
      </header>
      <div className="flex flex-col">
        {tasks.map((t) => (
          <div
            key={String(t.id)}
            className="flex items-center gap-2.5 border-b border-purple-500/10 px-3 py-2 text-xs last:border-0"
          >
            <AlertTriangle className="size-3.5 shrink-0 text-purple-500" />
            <span className="min-w-0 flex-1">
              <span className="block truncate font-medium">
                {t.title || t.path}
              </span>
              <span className="text-muted-foreground block truncate">
                {reviewReason(t)}
              </span>
            </span>
            <ClaimantChip task={t} />
            <span className="text-muted-foreground/70 shrink-0 font-mono text-[10px]">
              {relativeAge(t.date_modified ?? t.date_created)}
            </span>
          </div>
        ))}
      </div>
    </section>
  );
}

// ── the Agents lens (kanban) ────────────────────────────────────────

const BOARD_COLUMNS: StatusBucket[] = ["running", "blocked", "open", "done"];
const CARD_HEIGHT = 64;

function AgentCard({
  task,
  byId,
  style,
}: {
  task: TaskInfo;
  byId: Map<string, TaskInfo>;
  style: CSSProperties;
}) {
  const bucket = statusBucket(task, byId);
  return (
    <div style={style} className="px-1.5 py-[3px]">
      <div className="bg-card hover:border-ring/40 flex h-full flex-col justify-center gap-1 rounded-md border px-2.5 py-1.5 transition-colors">
        <div className="flex items-center gap-1.5">
          <span className="shrink-0">{BUCKET_META[bucket].icon}</span>
          <span className="min-w-0 flex-1 truncate text-xs font-medium">
            {task.title || task.path}
          </span>
        </div>
        <div className="flex items-center justify-between gap-2 pl-5">
          <ClaimantChip task={task} />
          <span className="text-muted-foreground/70 font-mono text-[10px] tabular-nums">
            {relativeAge(task.date_created)}
          </span>
        </div>
      </div>
    </div>
  );
}

/**
 * The full agent swarm, kanban-style: review lane on top, then one
 * column per status with virtualized compact cards (title, claimant
 * agent ref, age).
 */
export function AgentsBoard({ model }: { model: ProjectModel }) {
  const { agents, byId, review } = model;
  const columns = useMemo(() => {
    const map = new Map<StatusBucket, TaskInfo[]>(
      BOARD_COLUMNS.map((b) => [b, []]),
    );
    for (const t of agents) {
      const bucket = statusBucket(t, byId);
      if (bucket === "review") continue; // surfaced in the lane above
      const col = bucket === "cancelled" ? "done" : bucket;
      map.get(col)?.push(t);
    }
    for (const [, list] of map) {
      list.sort((a, b) =>
        a.title.localeCompare(b.title, undefined, { numeric: true }),
      );
    }
    return map;
  }, [agents, byId]);

  if (agents.length === 0) {
    return (
      <p className="text-muted-foreground rounded-lg border border-dashed px-4 py-12 text-center text-sm">
        No agent subtasks in this project — dispatch some with{" "}
        <span className="font-mono">task triage</span>.
      </p>
    );
  }

  return (
    <div className="flex flex-col gap-3">
      <ReviewLane tasks={review} byId={byId} />
      <div className="grid grid-cols-2 gap-3 lg:grid-cols-4">
        {BOARD_COLUMNS.map((bucket) => {
          const cards = columns.get(bucket) ?? [];
          return (
            <section
              key={bucket}
              className="bg-muted/40 flex min-w-0 flex-col rounded-lg border"
            >
              <header className="flex items-center gap-1.5 px-3 py-2">
                {BUCKET_META[bucket].icon}
                <h3 className="text-xs font-medium capitalize">
                  {BUCKET_META[bucket].label}
                </h3>
                <span className="text-muted-foreground ml-auto font-mono text-[10px] tabular-nums">
                  {cards.length}
                </span>
              </header>
              {cards.length === 0 ? (
                <p className="text-muted-foreground/60 px-3 pb-3 pt-1 text-center text-[11px]">
                  Empty
                </p>
              ) : (
                <VirtualList
                  items={cards}
                  rowHeight={CARD_HEIGHT}
                  maxHeight={CARD_HEIGHT * 8.5}
                  className="pb-1.5"
                  renderRow={(t, _i, style) => (
                    <AgentCard
                      key={String(t.id)}
                      task={t}
                      byId={byId}
                      style={style}
                    />
                  )}
                />
              )}
            </section>
          );
        })}
      </div>
    </div>
  );
}

/** Tooltipped legend chip used by the header progress visual. */
export function BucketChip({
  bucket,
  count,
}: {
  bucket: StatusBucket;
  count: number;
}) {
  if (count === 0) return null;
  return (
    <Tooltip>
      <TooltipTrigger asChild>
        <span
          className={cn(
            "flex cursor-default items-center gap-1 text-[11px] tabular-nums",
            BUCKET_META[bucket].text,
          )}
        >
          <span
            className={cn("size-1.5 rounded-full", BUCKET_META[bucket].bar)}
          />
          {count} {BUCKET_META[bucket].label}
        </span>
      </TooltipTrigger>
      <TooltipContent>
        {count} {BUCKET_META[bucket].label}
        {bucket === "blocked" && " (unresolved blockers)"}
      </TooltipContent>
    </Tooltip>
  );
}
