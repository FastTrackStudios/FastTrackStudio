//! Native-only Sentry error/crash telemetry for the Task apps.
//!
//! The DSN is **never** committed. It is resolved at runtime from the
//! build-time `TASK_SENTRY_DSN` (baked into shipped clients via a GH
//! Actions secret) or, failing that, the process env (the server).
//!
//! Callers must hold the returned [`sentry::ClientInitGuard`] for the
//! whole process lifetime — dropping it flushes and disables reporting.

#[cfg(not(target_arch = "wasm32"))]
mod native {
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;
    use tracing_subscriber::EnvFilter;

    /// Initialise Sentry and return the guard. `service` is attached as a
    /// tag so events can be filtered per app (server / mobile / desktop).
    ///
    /// Returns `None` when no DSN is configured (dev / local builds) — the
    /// apps then run without telemetry.
    pub fn init(service: &str) -> Option<sentry::ClientInitGuard> {
        let dsn = option_env!("TASK_SENTRY_DSN")
            .map(str::to_owned)
            .or_else(|| std::env::var("TASK_SENTRY_DSN").ok())
            .filter(|s| !s.trim().is_empty())?;

        let guard = sentry::init((
            dsn,
            sentry::ClientOptions {
                release: sentry::release_name!(),
                send_default_pii: false,
                ..Default::default()
            },
        ));

        let svc = service.to_owned();
        sentry::configure_scope(|s| s.set_tag("service", svc));

        Some(guard)
    }

    /// The tracing layer that forwards `error!`/`warn!` events (and spans
    /// as breadcrumbs) to Sentry. Compose into a
    /// [`fn@tracing_subscriber::registry`].
    pub fn tracing_layer<S>() -> sentry_tracing::SentryLayer<S>
    where
        S: tracing::Subscriber + for<'a> tracing_subscriber::registry::LookupSpan<'a>,
    {
        sentry_tracing::layer()
    }

    /// Convenience: initialise Sentry **and** install a global tracing
    /// subscriber (env-filter + fmt + the Sentry layer) in one call.
    ///
    /// Uses `.try_init()` so a later subscriber init (e.g. dioxus) failing
    /// to become the global default is a no-op rather than a panic.
    /// Returns the Sentry guard — hold it for the process lifetime.
    pub fn init_tracing(service: &str, env_filter_default: &str) -> Option<sentry::ClientInitGuard> {
        let guard = init(service);

        let filter = EnvFilter::try_from_default_env()
            .unwrap_or_else(|_| EnvFilter::new(env_filter_default));

        let _ = tracing_subscriber::registry()
            .with(filter)
            .with(tracing_subscriber::fmt::layer())
            .with(tracing_layer())
            .try_init();

        guard
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use native::{init, init_tracing, tracing_layer};

// ── OpenTelemetry (feature `otel`, native only) ──────────────────────
//
// OTLP export of traces, logs, and metrics to a collector. Doubly
// opt-in: the cargo feature gates the dependency weight, and at
// runtime nothing initializes unless `OTEL_EXPORTER_OTLP_ENDPOINT` is
// set — the local-first "no telemetry" promise holds by default.
#[cfg(all(not(target_arch = "wasm32"), feature = "otel"))]
pub mod otel {
    use opentelemetry::global;
    use opentelemetry::trace::TracerProvider as _;
    use opentelemetry_sdk::logs::SdkLoggerProvider;
    use opentelemetry_sdk::metrics::SdkMeterProvider;
    use opentelemetry_sdk::trace::SdkTracerProvider;
    use opentelemetry_sdk::Resource;
    use tracing_subscriber::Layer;

    /// Re-export for callers that record custom metrics (the server's
    /// HTTP middleware) without adding their own opentelemetry dep.
    pub use opentelemetry;

    /// Owns the three providers; dropping it flushes and shuts down
    /// the exporters. Hold for the process lifetime (like the Sentry
    /// guard).
    pub struct OtelGuard {
        tracer: SdkTracerProvider,
        logger: SdkLoggerProvider,
        meter: SdkMeterProvider,
    }

    impl Drop for OtelGuard {
        fn drop(&mut self) {
            if let Err(e) = self.tracer.shutdown() {
                eprintln!("otel: tracer shutdown: {e}");
            }
            if let Err(e) = self.logger.shutdown() {
                eprintln!("otel: logger shutdown: {e}");
            }
            if let Err(e) = self.meter.shutdown() {
                eprintln!("otel: meter shutdown: {e}");
            }
        }
    }

    /// Whether OTLP export is configured for this process.
    #[must_use]
    pub fn enabled() -> bool {
        std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
            .map(|v| !v.trim().is_empty())
            .unwrap_or(false)
    }

    /// Initialise the OTLP pipelines (http/protobuf; endpoint and
    /// headers come from the standard `OTEL_EXPORTER_OTLP_*` env vars)
    /// and return the guard plus the tracing layers to compose into
    /// the subscriber registry:
    ///
    /// - a `tracing-opentelemetry` layer — spans become OTel traces;
    /// - the appender bridge — `tracing` events become OTel log
    ///   records (queryable in Loki alongside pod stdout).
    ///
    /// The meter provider is installed globally
    /// (`opentelemetry::global::meter`), so instruments work from
    /// anywhere. Returns `None` when [`enabled`] is false or an
    /// exporter fails to build.
    pub fn init<S>(
        service: &'static str,
    ) -> Option<(OtelGuard, Vec<Box<dyn Layer<S> + Send + Sync>>)>
    where
        S: tracing::Subscriber
            + for<'a> tracing_subscriber::registry::LookupSpan<'a>
            + Send
            + Sync,
    {
        if !enabled() {
            return None;
        }
        let resource = Resource::builder().with_service_name(service).build();

        let span_exporter = opentelemetry_otlp::SpanExporter::builder()
            .with_http()
            .build()
            .map_err(|e| eprintln!("otel: span exporter: {e}"))
            .ok()?;
        let tracer_provider = SdkTracerProvider::builder()
            .with_batch_exporter(span_exporter)
            .with_resource(resource.clone())
            .build();
        let tracer = tracer_provider.tracer(service);
        global::set_tracer_provider(tracer_provider.clone());

        let log_exporter = opentelemetry_otlp::LogExporter::builder()
            .with_http()
            .build()
            .map_err(|e| eprintln!("otel: log exporter: {e}"))
            .ok()?;
        let logger_provider = SdkLoggerProvider::builder()
            .with_batch_exporter(log_exporter)
            .with_resource(resource.clone())
            .build();

        let metric_exporter = opentelemetry_otlp::MetricExporter::builder()
            .with_http()
            .build()
            .map_err(|e| eprintln!("otel: metric exporter: {e}"))
            .ok()?;
        let meter_provider = SdkMeterProvider::builder()
            .with_periodic_exporter(metric_exporter)
            .with_resource(resource)
            .build();
        global::set_meter_provider(meter_provider.clone());

        // The log bridge MUST NOT ingest the OTel SDK's own diagnostics.
        // The SDK reports export problems through `tracing`; if the
        // bridge turns those into log records it emits more diagnostics,
        // which the bridge ingests again — an unbounded feedback loop
        // that overflows the stack and aborts the process (observed
        // with an unreachable collector). Drop anything originating in
        // the OTel crates before it reaches the bridge.
        let no_otel_feedback = tracing_subscriber::filter::FilterFn::new(|meta| {
            !meta.target().starts_with("opentelemetry")
        });
        let layers: Vec<Box<dyn Layer<S> + Send + Sync>> = vec![
            Box::new(tracing_opentelemetry::layer().with_tracer(tracer)),
            Box::new(
                opentelemetry_appender_tracing::layer::OpenTelemetryTracingBridge::new(
                    &logger_provider,
                )
                .with_filter(no_otel_feedback),
            ),
        ];
        Some((
            OtelGuard {
                tracer: tracer_provider,
                logger: logger_provider,
                meter: meter_provider,
            },
            layers,
        ))
    }
}

// ── wasm: no-op stubs so callers compile cross-target ────────────────
#[cfg(target_arch = "wasm32")]
pub fn init(_service: &str) -> Option<()> {
    None
}

#[cfg(target_arch = "wasm32")]
pub fn init_tracing(_service: &str, _env_filter_default: &str) -> Option<()> {
    None
}
