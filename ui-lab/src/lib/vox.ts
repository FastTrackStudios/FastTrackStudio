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
  MiddlewareCaller,
  session,
  voxServiceMetadata,
  type Caller,
} from "@bearcove/vox-core";
import { wsConnector } from "@bearcove/vox-ws";

import { ProjectServiceRpcClient } from "@/generated/projectservicerpc.generated";
import { TaskServiceRpcClient } from "@/generated/taskservicerpc.generated";
import {
  errorMessage,
  installTelemetry,
  span,
  telemetryMiddleware,
} from "./telemetry";

// Before any WebSocket exists (idempotent; main.tsx also installs for
// the browser, this covers node/smoke).
installTelemetry();

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

/**
 * Instrumented mirror of the generated `connect*` helpers (which are
 * two-liners over `session.initiator` + the client class). Mirrored
 * here because the generated signature accepts SessionTransportOptions
 * but NO client middleware and NO way to observe connect phases —
 * that's a vox-codegen gap (connect* should take `middleware?:
 * ClientMiddleware[]`); until then we build the caller ourselves so
 * telemetry sees connect spans and per-RPC outcomes.
 */
async function connectInstrumented<T>(
  service: string,
  make: (caller: Caller) => T,
): Promise<T> {
  const end = span(`vox.connect.${service}`, VOX_URL);
  try {
    const established = await session.initiator(wsConnector(VOX_URL), {
      metadata: voxServiceMetadata(service),
    });
    end("connected");
    const caller = new MiddlewareCaller(
      established.rootConnection().caller(),
      [telemetryMiddleware],
    );
    return make(caller);
  } catch (e) {
    end(`error: ${errorMessage(e)}`);
    throw e;
  }
}

export function projects(): Promise<ProjectServiceRpcClient> {
  projectClient ??= connectInstrumented(
    "ProjectServiceRpc",
    (caller) => new ProjectServiceRpcClient(caller),
  ).catch((e) => {
    projectClient = null; // retry on next call instead of caching the failure
    throw e;
  });
  return projectClient;
}

export function tasks(): Promise<TaskServiceRpcClient> {
  taskClient ??= connectInstrumented(
    "TaskServiceRpc",
    (caller) => new TaskServiceRpcClient(caller),
  ).catch((e) => {
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
