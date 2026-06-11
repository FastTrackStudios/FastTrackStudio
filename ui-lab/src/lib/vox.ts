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
  record,
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
 * Initial-connect policy. The vox runtime's ReconnectPolicy only
 * covers RESUMABLE sessions after a drop — the FIRST connect is a
 * single attempt with no timeout: a ws upgrade that stalls (dev-proxy
 * racing a page reload, dev-server restart mid-flight, Chrome LNA
 * stalling a socket) hangs `connect*` forever with zero signal, and a
 * fast first-attempt failure surfaces only through the query layer's
 * slow retry. So we bound each attempt and retry quickly with jitter:
 * a racing first attempt costs ~250ms, worst case ~1s — not 15s.
 */
const CONNECT_ATTEMPT_TIMEOUT_MS = 3_000;
const CONNECT_MAX_ATTEMPTS = 3;
const CONNECT_RETRY_BASE_MS = 250;

function attemptTimeout<T>(promise: Promise<T>, ms: number): Promise<T> {
  return new Promise<T>((resolve, reject) => {
    const timer = setTimeout(
      () => reject(new Error(`connect attempt timed out after ${ms}ms`)),
      ms,
    );
    promise.then(
      (v) => {
        clearTimeout(timer);
        resolve(v);
      },
      (e) => {
        clearTimeout(timer);
        reject(e);
      },
    );
  });
}

/**
 * Instrumented mirror of the generated `connect*` helpers (which are
 * two-liners over `session.initiator` + the client class). Mirrored
 * here because the generated signature accepts SessionTransportOptions
 * but NO client middleware, NO initial-connect retry/timeout config,
 * and NO way to observe connect phases — that's a vox-codegen gap
 * (connect* should take `middleware?: ClientMiddleware[]` and an
 * initial-connect policy); until then we build the caller ourselves so
 * telemetry sees connect spans, per-attempt errors, and RPC outcomes.
 */
async function connectInstrumented<T>(
  service: string,
  make: (caller: Caller) => T,
): Promise<T> {
  const end = span(`vox.connect.${service}`, VOX_URL);
  let lastError: unknown;
  for (let attempt = 1; attempt <= CONNECT_MAX_ATTEMPTS; attempt++) {
    try {
      // NOTE: on timeout the underlying socket attempt is orphaned
      // (session.initiator exposes no abort); it is dropped unused if
      // it ever completes.
      const established = await attemptTimeout(
        session.initiator(wsConnector(VOX_URL), {
          metadata: voxServiceMetadata(service),
        }),
        CONNECT_ATTEMPT_TIMEOUT_MS,
      );
      end(attempt === 1 ? "connected" : `connected (attempt ${attempt})`);
      const caller = new MiddlewareCaller(
        established.rootConnection().caller(),
        [telemetryMiddleware],
      );
      return make(caller);
    } catch (e) {
      lastError = e;
      // THE key signal: why this attempt failed (pre-telemetry this
      // error was swallowed into a generic failure 15s later).
      record(
        "vox.connect.retry",
        `${service} attempt ${attempt}/${CONNECT_MAX_ATTEMPTS} failed: ${errorMessage(e)}`,
      );
      if (attempt < CONNECT_MAX_ATTEMPTS) {
        // 125-250ms, 250-500ms, ... (full jitter on a doubling base).
        const ceiling = CONNECT_RETRY_BASE_MS * 2 ** (attempt - 1);
        const delay = ceiling / 2 + Math.random() * (ceiling / 2);
        await new Promise((r) => setTimeout(r, delay));
      }
    }
  }
  end(`error: ${errorMessage(lastError)}`);
  throw lastError;
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
