/**
 * Lazy singleton vox clients against the live task-server.
 *
 * Each generated `connect*` opens its own WebSocket session to the
 * per-org vox mount (`/org/<slug>/vox`); the server hosts every
 * feature service behind one LayerRouter, dispatching by method id.
 *
 * In the browser the socket is opened same-origin and vite's dev/
 * preview proxy forwards it to the task-server (see vite.config.ts —
 * Chrome's Local Network Access checks stall cross-origin ws:// to
 * loopback). Under node (scripts/smoke.ts) there is no proxy, so it
 * dials the server directly. Override either with `VITE_TASK_SERVER` /
 * `VITE_TASK_ORG`.
 */
import {
  connectProjectServiceRpc,
  type ProjectServiceRpcClient,
} from "@/generated/projectservicerpc.generated";
import {
  connectTaskServiceRpc,
  type TaskServiceRpcClient,
} from "@/generated/taskservicerpc.generated";

const SERVER: string =
  (import.meta.env.VITE_TASK_SERVER as string | undefined) ??
  (typeof location !== "undefined"
    ? location.origin.replace(/^http/, "ws") // browser: same-origin, proxied
    : "ws://127.0.0.1:18080"); // node smoke: direct
const ORG: string =
  (import.meta.env.VITE_TASK_ORG as string | undefined) ?? "codywright";

export const VOX_URL = `${SERVER}/org/${ORG}/vox`;

let projectClient: Promise<ProjectServiceRpcClient> | null = null;
let taskClient: Promise<TaskServiceRpcClient> | null = null;

export function projects(): Promise<ProjectServiceRpcClient> {
  projectClient ??= connectProjectServiceRpc(VOX_URL).catch((e) => {
    projectClient = null; // retry on next call instead of caching the failure
    throw e;
  });
  return projectClient;
}

export function tasks(): Promise<TaskServiceRpcClient> {
  taskClient ??= connectTaskServiceRpc(VOX_URL).catch((e) => {
    taskClient = null;
    throw e;
  });
  return taskClient;
}

/** The generated fallible methods return `{ok,value}|{ok,error}`. */
type VoxResult<T, E> = { ok: true; value: T } | { ok: false; error: E };

/** Unwrap a vox result, throwing a readable Error on the user-error arm. */
export function unwrap<T, E>(result: VoxResult<T, E>): T {
  if (result.ok) return result.value;
  throw new Error(formatVoxError(result.error));
}

function formatVoxError(error: unknown): string {
  if (error && typeof error === "object" && "tag" in error) {
    const e = error as { tag: string; value?: unknown };
    return e.value === undefined ? e.tag : `${e.tag}: ${String(e.value)}`;
  }
  return String(error);
}
