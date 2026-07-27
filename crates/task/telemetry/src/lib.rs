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

// ── wasm: no-op stubs so callers compile cross-target ────────────────
#[cfg(target_arch = "wasm32")]
pub fn init(_service: &str) -> Option<()> {
    None
}

#[cfg(target_arch = "wasm32")]
pub fn init_tracing(_service: &str, _env_filter_default: &str) -> Option<()> {
    None
}
