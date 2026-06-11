//! [`Connection`] — generic connection state for a typed client bundle.
//!
//! Every feature used to hand-roll its own `enum ConnState { Connecting,
//! Ready(Clients), Failed(String) }` + `use_conn()` pair. `Connection<C>`
//! is that pattern once, generic over the client type: the shell
//! establishes the clients (with whatever transport + retry policy) and
//! resolves the connection; feature hooks read it from context.
//!
//! ```ignore
//! // app root — one line per client bundle:
//! use_connect(|| async { transport::connect(&transport).await });
//!
//! // a feature data hook:
//! let conn = use_connection::<ExampleRepoClient>();
//! let Some(client) = conn.ready() else { return /* still connecting */ };
//! ```

use dioxus::prelude::*;

/// The lifecycle of one typed client bundle.
#[derive(Clone, PartialEq, Debug)]
pub enum ConnectionState<C> {
    /// The connect (+ retry policy) hasn't resolved yet.
    Connecting,
    /// The clients are established and ready to call.
    Ready(C),
    /// Connecting failed after the configured retries.
    Failed(String),
}

/// A `Copy` handle to a [`ConnectionState`] provided at the app root.
pub struct Connection<C: 'static> {
    state: Signal<ConnectionState<C>>,
}

impl<C: 'static> Clone for Connection<C> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<C: 'static> Copy for Connection<C> {}

impl<C: Clone + 'static> Connection<C> {
    /// The current state (clones the bundle — client handles are cheap).
    pub fn state(&self) -> ConnectionState<C> {
        self.state.read().clone()
    }

    /// The clients if the connection is up, `None` while connecting or
    /// after a failure. The reactive read: a hook that calls this re-runs
    /// when the connection resolves.
    pub fn ready(&self) -> Option<C> {
        match &*self.state.read() {
            ConnectionState::Ready(c) => Some(c.clone()),
            _ => None,
        }
    }

    /// The connect failure, if there is one.
    pub fn error(&self) -> Option<String> {
        match &*self.state.read() {
            ConnectionState::Failed(e) => Some(e.clone()),
            _ => None,
        }
    }

    /// True until the connect future settles.
    pub fn is_connecting(&self) -> bool {
        matches!(&*self.state.read(), ConnectionState::Connecting)
    }
}

/// Pull the connection for client bundle `C` that the shell provided with
/// [`use_connect`].
pub fn use_connection<C: 'static>() -> Connection<C> {
    use_context::<Connection<C>>()
}

/// Reactive sibling of [`use_connect`]: re-runs `connect` whenever a
/// signal it reads **synchronously** (before its first await) changes —
/// an org/workspace switcher, an editable server URL — resetting the
/// connection to `Connecting` and re-establishing.
///
/// ```ignore
/// // app root: reconnects when the active org changes
/// use_connect_reactive(move || {
///     let slug = active_org_slug();          // signal read → dependency
///     async move { connect_org(&slug).await }
/// });
/// ```
///
/// Dependency tracking is dioxus-resource semantics: only signals read
/// before the closure returns its future re-trigger the connect. Reads
/// inside the future are invisible.
pub fn use_connect_reactive<C, F, Fut>(connect: F) -> Connection<C>
where
    C: Clone + 'static,
    F: Fn() -> Fut + 'static,
    Fut: std::future::Future<Output = Result<C, String>> + 'static,
{
    let mut state = use_signal(|| ConnectionState::Connecting);
    let conn = use_context_provider(|| Connection { state });
    let _resource = use_resource(move || {
        // Synchronous part: signal reads here register as dependencies.
        let fut = connect();
        async move {
            state.set(ConnectionState::Connecting);
            match fut.await {
                Ok(c) => state.set(ConnectionState::Ready(c)),
                Err(e) => state.set(ConnectionState::Failed(e)),
            }
        }
    });
    conn
}

/// Establish a client bundle once at the app root and provide it as
/// context: starts `Connecting`, runs `connect` on mount, resolves to
/// `Ready`/`Failed`. Call once per bundle type `C`; features pull it with
/// [`use_connection::<C>()`](use_connection).
pub fn use_connect<C, F, Fut>(connect: F) -> Connection<C>
where
    C: Clone + 'static,
    F: FnOnce() -> Fut + 'static,
    Fut: std::future::Future<Output = Result<C, String>> + 'static,
{
    let state = use_signal(|| ConnectionState::Connecting);
    let conn = use_context_provider(|| Connection { state });
    // One-shot connect on mount. `use_future` (not use_resource): the
    // connect closure is FnOnce and shouldn't restart reactively — the
    // retry policy lives inside it.
    let mut connect = use_hook(|| CopyValue::new(Some(connect)));
    use_future(move || async move {
        let Some(connect) = connect.write().take() else {
            return;
        };
        let mut state = state;
        match connect().await {
            Ok(c) => state.set(ConnectionState::Ready(c)),
            Err(e) => state.set(ConnectionState::Failed(e)),
        }
    });
    conn
}
