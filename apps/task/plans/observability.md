# Observability: OTLP traces, logs, and metrics

**Status:** in progress

Full traceability for the Task backend — one OTLP pipe out of the
server into the cluster's observability stack, so errors and latency
are queryable over time instead of scrolling past in `kubectl logs`.

## Shape

```
task-server ──OTLP/http──▶ otel-collector ──▶ Tempo       (traces)
  (tracing spans,          (observability     ├─▶ Loki    (logs)
   log records,             namespace)        └─▶ Prometheus (metrics)
   RED metrics)                                     │
                                            Grafana ┘ (grafana.starcommand.live)
```

The cluster already runs Prometheus, Grafana (Authelia SSO), Loki, and
Promtail via `FTS.cluster.services.observability` in the starcommand
repo. This effort adds **Tempo** and an **OTel collector** next to
them, and makes the app emit.

## Opt-in, always

Task's product promise is local-first and telemetry-free. Both the
Sentry DSN and the OTLP endpoint follow the same rule: **unset means
nothing is emitted**. The `otel` cargo feature gates the dependency
weight (only `task-server` enables it); `OTEL_EXPORTER_OTLP_ENDPOINT`
gates the runtime. A self-hosted instance that sets neither ships no
data anywhere, ever.

## Done

- `task-telemetry` grew an `otel` module (feature `otel`): builds OTLP
  span / log / metric exporters from the standard `OTEL_EXPORTER_OTLP_*`
  env vars, returns tracing layers to compose into the registry plus a
  guard that flushes on drop. `enabled()` reports whether the endpoint
  is configured.
- `task-server` composes those layers into its existing registry
  (fmt + Sentry + OTel), so every `tracing` span becomes a trace and
  every event becomes an OTel log record.
- HTTP layer: one span per request (method + **matched route**, never
  the raw URI — org slugs and note paths must not become labels) and
  RED metrics (`http.server.requests`, `http.server.request.duration`).
- Chart: `server.env` documents the OTLP knobs;
  `values-fasttrackstudio.yaml` points at the in-cluster collector.

## Remaining

- **Cluster** (starcommand repo, `modules/services/observability/`):
  add the Tempo helm release + the OTel collector deployment, wire a
  Tempo datasource into Grafana next to Loki, and register a
  `starcommand.cluster.routes` entry so Grafana itself has a real
  Caddy/Authelia route (today it only has a vestigial traefik Ingress).
- **ServiceMonitor** for task-server so Prometheus scrapes it directly
  as well (the collector path covers push; a scrape target is better
  for uptime). Needs a metrics port on the deployment.
- **Dashboards**: request rate / error rate / p95 by route, vox RPC
  volume, sync + collab health. Provision as ConfigMaps so they are
  code, not clicked.
- **Alerts**: extend the existing `PrometheusRule` pattern — 5xx rate,
  p95 latency regression, restart loops.
- **The engine** (`apps/fasttrackstudio`): `architect::host::init_tracing`
  builds a non-layered subscriber, so it needs restructuring before the
  live rig can export. Low priority — and the rig should probably stay
  local-only anyway.
- **Client-side**: the wasm app has no exporter. Sentry already covers
  crashes; browser OTLP is a bigger call (CORS, PII, volume).
