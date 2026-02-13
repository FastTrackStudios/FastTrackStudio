//! # Architecture Playground — Typestate + better-auth Edition
//!
//! Demonstrates the full vertical slice with **typestate** pattern and
//! **better-auth-rs** for real authentication:
//!
//! ```text
//!   better-auth (MemoryDatabaseAdapter — users, sessions, accounts)
//!       ↓
//!   Auth Service (#[roam::service] — wraps better-auth for RPC)
//!       ↓
//!   Item Repository (rusqlite — per-user item storage)
//!       ↓
//!   Item Service (#[roam::service] — CRUD with session validation)
//!       ↓
//!   Loopback Connection (in-memory duplex, full roam protocol)
//!       ↓
//!   Control Facade (typestate generics — compile-time state enforcement)
//!       ↓
//!   Dioxus UI (Tailwind CSS, login/signup ↔ authenticated item list)
//! ```
//!
//! Run with: `cargo run -p playground`

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 0: Database (item storage only — auth uses MemoryDatabaseAdapter)
// ═══════════════════════════════════════════════════════════════════════════════

mod database {
    use rusqlite::Connection;
    use std::sync::{Arc, Mutex};

    pub type DbConn = Arc<Mutex<Connection>>;

    pub fn open_memory() -> DbConn {
        let conn = Connection::open_in_memory().expect("failed to open SQLite");
        conn.execute_batch(
            "CREATE TABLE items (
                id       TEXT PRIMARY KEY,
                owner_id TEXT NOT NULL,
                name     TEXT NOT NULL,
                done     INTEGER NOT NULL DEFAULT 0
            );",
        )
        .expect("failed to create schema");
        Arc::new(Mutex::new(conn))
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 1: Repository (pure DB access for items)
// ═══════════════════════════════════════════════════════════════════════════════

mod repository {
    use super::database::DbConn;

    #[derive(Debug, Clone)]
    pub struct ItemRow {
        pub id: String,
        pub name: String,
        pub done: bool,
    }

    pub fn list_items(db: &DbConn, owner_id: &str) -> Vec<ItemRow> {
        let conn = db.lock().unwrap();
        let mut stmt = conn
            .prepare("SELECT id, name, done FROM items WHERE owner_id = ?1 ORDER BY name")
            .expect("prepare failed");
        stmt.query_map(rusqlite::params![owner_id], |row| {
            Ok(ItemRow {
                id: row.get(0)?,
                name: row.get(1)?,
                done: row.get::<_, i32>(2)? != 0,
            })
        })
        .expect("query failed")
        .filter_map(|r| r.ok())
        .collect()
    }

    pub fn insert_item(db: &DbConn, id: &str, owner_id: &str, name: &str) {
        let conn = db.lock().unwrap();
        conn.execute(
            "INSERT INTO items (id, owner_id, name, done) VALUES (?1, ?2, ?3, 0)",
            rusqlite::params![id, owner_id, name],
        )
        .expect("insert failed");
    }

    pub fn toggle_item(db: &DbConn, id: &str, owner_id: &str) -> bool {
        let conn = db.lock().unwrap();
        conn.execute(
            "UPDATE items SET done = 1 - done WHERE id = ?1 AND owner_id = ?2",
            rusqlite::params![id, owner_id],
        )
        .expect("update failed")
            > 0
    }

    pub fn delete_item(db: &DbConn, id: &str, owner_id: &str) -> bool {
        let conn = db.lock().unwrap();
        conn.execute(
            "DELETE FROM items WHERE id = ?1 AND owner_id = ?2",
            rusqlite::params![id, owner_id],
        )
        .expect("delete failed")
            > 0
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 2: Proto (domain types + service traits — the RPC contract)
//
// Two services:
//   AuthService — wraps better-auth sign-up/sign-in/get-session
//   ItemService — CRUD with session token validation
// ═══════════════════════════════════════════════════════════════════════════════

mod proto {
    use facet::Facet;
    use roam::Tx;

    // ── Auth types ──────────────────────────────────────────────────

    /// Opaque session token issued by better-auth on successful login.
    /// This is a Facet type — full type identity preserved across RPC.
    #[derive(Debug, Clone, PartialEq, Facet)]
    pub struct SessionToken {
        pub token: String,
        pub user_id: String,
        pub user_email: String,
        pub user_name: String,
    }

    #[repr(u8)]
    #[derive(Debug, Clone, PartialEq, Facet)]
    pub enum AuthResult {
        Success { session: SessionToken } = 0,
        Failed { message: String } = 1,
    }

    // ── Item types ──────────────────────────────────────────────────

    #[derive(Debug, Clone, PartialEq, Facet)]
    pub struct ItemInfo {
        pub id: String,
        pub name: String,
        pub done: bool,
    }

    #[repr(u8)]
    #[derive(Debug, Clone, PartialEq, Facet)]
    pub enum ItemEvent {
        ListChanged { items: Vec<ItemInfo> },
    }

    // ── Service traits ──────────────────────────────────────────────

    #[roam::service]
    pub trait AuthService {
        async fn sign_up(&self, email: String, password: String, name: String) -> AuthResult;
        async fn sign_in(&self, email: String, password: String) -> AuthResult;
        async fn validate_session(&self, token: String) -> Option<SessionToken>;
        async fn sign_out(&self, token: String);
    }

    #[roam::service]
    pub trait ItemService {
        async fn list_items(&self, token: SessionToken) -> Vec<ItemInfo>;
        async fn create_item(&self, token: SessionToken, name: String) -> String;
        async fn toggle_item(&self, token: SessionToken, id: String);
        async fn delete_item(&self, token: SessionToken, id: String);
        async fn subscribe(&self, token: SessionToken, tx: Tx<ItemEvent>);
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 3: Standalone Implementation
//
// AuthService wraps better-auth's handle_request() pattern.
// ItemService validates sessions through AuthService, then calls repos.
// ═══════════════════════════════════════════════════════════════════════════════

mod standalone {
    use super::database::DbConn;
    use super::proto::*;
    use super::repository;
    use better_auth::adapters::MemoryDatabaseAdapter;
    use better_auth::plugins::EmailPasswordPlugin;
    use better_auth::{AuthBuilder, AuthConfig, BetterAuth};
    use better_auth_core::types::{AuthRequest, HttpMethod};
    use roam::{Context, Tx};
    use std::collections::HashMap;
    use std::sync::{Arc, RwLock};

    type Auth = BetterAuth<MemoryDatabaseAdapter>;

    // ── AuthService impl ────────────────────────────────────────────

    #[derive(Clone)]
    pub struct StandaloneAuthService {
        auth: Arc<Auth>,
    }

    impl StandaloneAuthService {
        pub async fn new() -> eyre::Result<Self> {
            let config = AuthConfig::new("playground-secret-key-must-be-at-least-32-chars-long!")
                .base_url("http://localhost")
                .password_min_length(6);

            let adapter = MemoryDatabaseAdapter::new();

            let auth = AuthBuilder::new(config)
                .database(adapter)
                .plugin(EmailPasswordPlugin::new().enable_signup(true))
                .build()
                .await
                .map_err(|e| eyre::eyre!("Failed to build auth: {e:?}"))?;

            Ok(Self {
                auth: Arc::new(auth),
            })
        }

        /// Shared reference for the item service to validate sessions.
        pub fn auth_ref(&self) -> Arc<Auth> {
            self.auth.clone()
        }
    }

    impl AuthService for StandaloneAuthService {
        async fn sign_up(
            &self,
            _cx: &Context,
            email: String,
            password: String,
            name: String,
        ) -> AuthResult {
            let body = serde_json::json!({
                "email": email,
                "password": password,
                "name": name,
            });

            let req = AuthRequest {
                method: HttpMethod::Post,
                path: "/sign-up/email".to_string(),
                headers: HashMap::from([(
                    "content-type".to_string(),
                    "application/json".to_string(),
                )]),
                body: Some(serde_json::to_vec(&body).unwrap()),
                query: HashMap::new(),
            };

            match self.auth.handle_request(req).await {
                Ok(resp) if resp.status == 200 => {
                    // Parse the response to extract session token
                    if let Ok(value) = serde_json::from_slice::<serde_json::Value>(&resp.body) {
                        let token = value["session"]["token"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();
                        let user_id = value["user"]["id"].as_str().unwrap_or_default().to_string();
                        let user_email = value["user"]["email"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();
                        let user_name = value["user"]["name"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();

                        AuthResult::Success {
                            session: SessionToken {
                                token,
                                user_id,
                                user_email,
                                user_name,
                            },
                        }
                    } else {
                        AuthResult::Failed {
                            message: "Failed to parse auth response".into(),
                        }
                    }
                }
                Ok(resp) => {
                    let msg = serde_json::from_slice::<serde_json::Value>(&resp.body)
                        .ok()
                        .and_then(|v| v["message"].as_str().map(String::from))
                        .unwrap_or_else(|| format!("Sign up failed (status {})", resp.status));
                    AuthResult::Failed { message: msg }
                }
                Err(e) => AuthResult::Failed {
                    message: format!("{e:?}"),
                },
            }
        }

        async fn sign_in(&self, _cx: &Context, email: String, password: String) -> AuthResult {
            let body = serde_json::json!({
                "email": email,
                "password": password,
            });

            let req = AuthRequest {
                method: HttpMethod::Post,
                path: "/sign-in/email".to_string(),
                headers: HashMap::from([(
                    "content-type".to_string(),
                    "application/json".to_string(),
                )]),
                body: Some(serde_json::to_vec(&body).unwrap()),
                query: HashMap::new(),
            };

            match self.auth.handle_request(req).await {
                Ok(resp) if resp.status == 200 => {
                    if let Ok(value) = serde_json::from_slice::<serde_json::Value>(&resp.body) {
                        let token = value["session"]["token"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();
                        let user_id = value["user"]["id"].as_str().unwrap_or_default().to_string();
                        let user_email = value["user"]["email"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();
                        let user_name = value["user"]["name"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string();

                        AuthResult::Success {
                            session: SessionToken {
                                token,
                                user_id,
                                user_email,
                                user_name,
                            },
                        }
                    } else {
                        AuthResult::Failed {
                            message: "Failed to parse auth response".into(),
                        }
                    }
                }
                Ok(resp) => {
                    let msg = serde_json::from_slice::<serde_json::Value>(&resp.body)
                        .ok()
                        .and_then(|v| v["message"].as_str().map(String::from))
                        .unwrap_or_else(|| format!("Sign in failed (status {})", resp.status));
                    AuthResult::Failed { message: msg }
                }
                Err(e) => AuthResult::Failed {
                    message: format!("{e:?}"),
                },
            }
        }

        async fn validate_session(&self, _cx: &Context, token: String) -> Option<SessionToken> {
            let req = AuthRequest {
                method: HttpMethod::Get,
                path: "/get-session".to_string(),
                headers: HashMap::from([("authorization".to_string(), format!("Bearer {token}"))]),
                body: None,
                query: HashMap::new(),
            };

            match self.auth.handle_request(req).await {
                Ok(resp) if resp.status == 200 => {
                    let value = serde_json::from_slice::<serde_json::Value>(&resp.body).ok()?;
                    Some(SessionToken {
                        token,
                        user_id: value["user"]["id"].as_str()?.to_string(),
                        user_email: value["user"]["email"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string(),
                        user_name: value["user"]["name"]
                            .as_str()
                            .unwrap_or_default()
                            .to_string(),
                    })
                }
                _ => None,
            }
        }

        async fn sign_out(&self, _cx: &Context, token: String) {
            let req = AuthRequest {
                method: HttpMethod::Post,
                path: "/sign-out".to_string(),
                headers: HashMap::from([("authorization".to_string(), format!("Bearer {token}"))]),
                body: None,
                query: HashMap::new(),
            };
            let _ = self.auth.handle_request(req).await;
        }
    }

    // ── ItemService impl ────────────────────────────────────────────

    #[derive(Clone)]
    pub struct StandaloneItemService {
        db: DbConn,
        auth: Arc<Auth>,
        subscribers: Arc<RwLock<Vec<(String, Arc<Tx<ItemEvent>>)>>>,
    }

    impl StandaloneItemService {
        pub fn new(db: DbConn, auth: Arc<Auth>) -> Self {
            Self {
                db,
                auth,
                subscribers: Arc::new(RwLock::new(Vec::new())),
            }
        }

        /// Validate a session token via better-auth.
        async fn validate(&self, token: &SessionToken) -> Option<String> {
            let req = AuthRequest {
                method: HttpMethod::Get,
                path: "/get-session".to_string(),
                headers: HashMap::from([(
                    "authorization".to_string(),
                    format!("Bearer {}", token.token),
                )]),
                body: None,
                query: HashMap::new(),
            };

            match self.auth.handle_request(req).await {
                Ok(resp) if resp.status == 200 => {
                    let value = serde_json::from_slice::<serde_json::Value>(&resp.body).ok()?;
                    value["user"]["id"].as_str().map(String::from)
                }
                _ => None,
            }
        }

        fn read_items(&self, owner_id: &str) -> Vec<ItemInfo> {
            repository::list_items(&self.db, owner_id)
                .into_iter()
                .map(|row| ItemInfo {
                    id: row.id,
                    name: row.name,
                    done: row.done,
                })
                .collect()
        }

        fn broadcast_change(&self, owner_id: &str) {
            let items = self.read_items(owner_id);
            let event = ItemEvent::ListChanged { items };
            let subs = self.subscribers.read().unwrap().clone();
            for (uid, tx) in subs {
                if uid == owner_id {
                    let event = event.clone();
                    tokio::spawn(async move {
                        let _ = tx.send(&event).await;
                    });
                }
            }
        }
    }

    impl ItemService for StandaloneItemService {
        async fn list_items(&self, _cx: &Context, token: SessionToken) -> Vec<ItemInfo> {
            match self.validate(&token).await {
                Some(user_id) => self.read_items(&user_id),
                None => Vec::new(),
            }
        }

        async fn create_item(&self, _cx: &Context, token: SessionToken, name: String) -> String {
            let Some(user_id) = self.validate(&token).await else {
                return String::new();
            };
            let id = uuid::Uuid::new_v4().to_string();
            repository::insert_item(&self.db, &id, &user_id, &name);
            self.broadcast_change(&user_id);
            id
        }

        async fn toggle_item(&self, _cx: &Context, token: SessionToken, id: String) {
            if let Some(user_id) = self.validate(&token).await {
                repository::toggle_item(&self.db, &id, &user_id);
                self.broadcast_change(&user_id);
            }
        }

        async fn delete_item(&self, _cx: &Context, token: SessionToken, id: String) {
            if let Some(user_id) = self.validate(&token).await {
                repository::delete_item(&self.db, &id, &user_id);
                self.broadcast_change(&user_id);
            }
        }

        async fn subscribe(&self, _cx: &Context, token: SessionToken, tx: Tx<ItemEvent>) {
            if let Some(user_id) = self.validate(&token).await {
                let items = self.read_items(&user_id);
                let _ = tx.send(&ItemEvent::ListChanged { items }).await;
                self.subscribers
                    .write()
                    .unwrap()
                    .push((user_id, Arc::new(tx)));
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 3.5: Loopback Connection (in-memory roam RPC)
//
// Two dispatchers chained: AuthService + ItemService via RoutedDispatcher.
// ═══════════════════════════════════════════════════════════════════════════════

mod loopback {
    use super::proto::{AuthServiceDispatcher, ItemServiceDispatcher};
    use super::standalone::{StandaloneAuthService, StandaloneItemService};
    use roam::session::{ConnectionHandle, HandshakeConfig, RoutedDispatcher};
    use roam_stream::LengthPrefixedFramed;

    pub async fn connect(
        auth_svc: StandaloneAuthService,
        item_svc: StandaloneItemService,
    ) -> eyre::Result<ConnectionHandle> {
        let auth_dispatcher = AuthServiceDispatcher::new(auth_svc);
        let item_dispatcher = ItemServiceDispatcher::new(item_svc);
        let dispatcher = RoutedDispatcher::new(auth_dispatcher, item_dispatcher);

        let (client_stream, server_stream) = tokio::io::duplex(64 * 1024);
        let client_framed = LengthPrefixedFramed::new(client_stream);
        let server_framed = LengthPrefixedFramed::new(server_stream);

        let config = HandshakeConfig {
            max_payload_size: 1024 * 1024,
            initial_channel_credit: 4 * 1024 * 1024,
            max_concurrent_requests: 32,
        };

        let server_config = config.clone();
        tokio::spawn(async move {
            match roam_session::accept_framed(server_framed, server_config, dispatcher).await {
                Ok((_handle, _incoming, driver)) => {
                    if let Err(e) = driver.run().await {
                        tracing::warn!("loopback server error: {e}");
                    }
                }
                Err(e) => tracing::error!("loopback accept failed: {e}"),
            }
        });

        let (handle, _incoming, driver) =
            roam_session::initiate_framed(client_framed, config, roam_session::NoDispatcher)
                .await?;

        tokio::spawn(async move {
            if let Err(e) = driver.run().await {
                tracing::warn!("loopback client error: {e}");
            }
        });

        Ok(handle)
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 4: Control Facade — TYPESTATE PATTERN
//
// Two states:
//   AppControl<Anonymous>     — can sign_up() and sign_in()
//   AppControl<Authenticated> — can list/create/toggle/delete + sign_out()
//
// Transitions consume self → return new type. The compiler rejects invalid
// operation sequences at compile time.
// ═══════════════════════════════════════════════════════════════════════════════

mod control {
    use super::proto::{
        AuthResult as ProtoAuthResult, AuthServiceClient, ItemEvent, ItemInfo, ItemServiceClient,
        SessionToken,
    };
    use roam::session::ConnectionHandle;
    use std::marker::PhantomData;
    use std::sync::Arc;

    // ── State Markers ───────────────────────────────────────────────

    pub struct Anonymous;
    pub struct Authenticated;

    // ── Shared client handle ────────────────────────────────────────

    struct Clients {
        auth: AuthServiceClient,
        items: ItemServiceClient,
    }

    // ── The Typestate Facade ────────────────────────────────────────

    pub struct AppControl<State = Anonymous> {
        clients: Arc<Clients>,
        session: Option<SessionToken>,
        _state: PhantomData<State>,
    }

    impl Clone for AppControl<Authenticated> {
        fn clone(&self) -> Self {
            Self {
                clients: self.clients.clone(),
                session: self.session.clone(),
                _state: PhantomData,
            }
        }
    }

    impl PartialEq for AppControl<Authenticated> {
        fn eq(&self, other: &Self) -> bool {
            Arc::ptr_eq(&self.clients, &other.clients)
        }
    }

    // ── Anonymous state ─────────────────────────────────────────────

    impl AppControl<Anonymous> {
        pub fn new(handle: ConnectionHandle) -> Self {
            Self {
                clients: Arc::new(Clients {
                    auth: AuthServiceClient::new(handle.clone()),
                    items: ItemServiceClient::new(handle),
                }),
                session: None,
                _state: PhantomData,
            }
        }

        pub async fn sign_up(
            self,
            email: &str,
            password: &str,
            name: &str,
        ) -> Result<AppControl<Authenticated>, (Self, String)> {
            match self
                .clients
                .auth
                .sign_up(email.to_string(), password.to_string(), name.to_string())
                .await
            {
                Ok(ProtoAuthResult::Success { session }) => Ok(AppControl {
                    clients: self.clients,
                    session: Some(session),
                    _state: PhantomData,
                }),
                Ok(ProtoAuthResult::Failed { message }) => Err((self, message)),
                Err(e) => Err((self, format!("RPC error: {e}"))),
            }
        }

        pub async fn sign_in(
            self,
            email: &str,
            password: &str,
        ) -> Result<AppControl<Authenticated>, (Self, String)> {
            match self
                .clients
                .auth
                .sign_in(email.to_string(), password.to_string())
                .await
            {
                Ok(ProtoAuthResult::Success { session }) => Ok(AppControl {
                    clients: self.clients,
                    session: Some(session),
                    _state: PhantomData,
                }),
                Ok(ProtoAuthResult::Failed { message }) => Err((self, message)),
                Err(e) => Err((self, format!("RPC error: {e}"))),
            }
        }
    }

    // ── Authenticated state ─────────────────────────────────────────

    impl AppControl<Authenticated> {
        fn token(&self) -> SessionToken {
            self.session.clone().unwrap()
        }

        pub fn user_name(&self) -> &str {
            &self.session.as_ref().unwrap().user_name
        }

        pub fn user_email(&self) -> &str {
            &self.session.as_ref().unwrap().user_email
        }

        pub async fn sign_out(self) {
            let token = self.session.as_ref().unwrap().token.clone();
            let _ = self.clients.auth.sign_out(token).await;
            // self is consumed — caller can't use it anymore
        }

        pub async fn list(&self) -> Vec<ItemInfo> {
            self.clients
                .items
                .list_items(self.token())
                .await
                .unwrap_or_default()
        }

        pub async fn create(&self, name: &str) -> Option<String> {
            self.clients
                .items
                .create_item(self.token(), name.to_string())
                .await
                .ok()
        }

        pub async fn toggle(&self, id: &str) {
            let _ = self
                .clients
                .items
                .toggle_item(self.token(), id.to_string())
                .await;
        }

        pub async fn delete(&self, id: &str) {
            let _ = self
                .clients
                .items
                .delete_item(self.token(), id.to_string())
                .await;
        }

        pub async fn subscribe(&self) -> eyre::Result<roam::Rx<ItemEvent>> {
            let (tx, rx) = roam::channel::<ItemEvent>();
            self.clients.items.subscribe(self.token(), tx).await?;
            Ok(rx)
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Layer 5: Dioxus UI (Tailwind CSS, typestate-driven auth flow)
// ═══════════════════════════════════════════════════════════════════════════════

mod ui {
    use super::control::{AppControl, Authenticated};
    use super::proto::{ItemEvent, ItemInfo};
    use dioxus::prelude::*;
    use roam::session::ConnectionHandle;
    use std::sync::OnceLock;

    // ── Bootstrap ───────────────────────────────────────────────────

    static BOOTSTRAP: OnceLock<ConnectionHandle> = OnceLock::new();

    pub fn set_handle(handle: ConnectionHandle) {
        let _ = BOOTSTRAP.set(handle);
    }

    // ── Signals ─────────────────────────────────────────────────────

    static AUTH_CONTROL: GlobalSignal<Option<AppControl<Authenticated>>> = Signal::global(|| None);
    static ITEMS: GlobalSignal<Vec<ItemInfo>> = Signal::global(Vec::new);
    static LOGIN_ERROR: GlobalSignal<Option<String>> = Signal::global(|| None);

    // ── CSS ─────────────────────────────────────────────────────────

    const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");
    const MAIN_CSS: Asset = asset!("/assets/main.css");

    // ── Root ────────────────────────────────────────────────────────

    #[component]
    pub fn App() -> Element {
        let is_authenticated = AUTH_CONTROL.read().is_some();

        rsx! {
            document::Link { rel: "stylesheet", href: TAILWIND_CSS }
            document::Link { rel: "stylesheet", href: MAIN_CSS }

            div { class: "min-h-screen bg-zinc-950 text-zinc-100",
                if is_authenticated {
                    AuthenticatedApp {}
                } else {
                    AuthScreen {}
                }
            }
        }
    }

    // ── Auth Screen (Anonymous state — sign in / sign up) ───────────

    #[component]
    fn AuthScreen() -> Element {
        let mut is_signup = use_signal(|| false);
        let mut email = use_signal(String::new);
        let mut password = use_signal(String::new);
        let mut name = use_signal(String::new);
        let mut loading = use_signal(|| false);
        let error = LOGIN_ERROR.read();

        let on_submit = move |_| {
            if *loading.read() {
                return;
            }
            loading.set(true);
            *LOGIN_ERROR.write() = None;

            let email_val = email.read().clone();
            let password_val = password.read().clone();
            let name_val = name.read().clone();
            let signup = *is_signup.read();

            let Some(handle) = BOOTSTRAP.get().cloned() else {
                *LOGIN_ERROR.write() = Some("Service not initialized".into());
                loading.set(false);
                return;
            };
            let anon = AppControl::new(handle);

            spawn(async move {
                let result = if signup {
                    anon.sign_up(&email_val, &password_val, &name_val).await
                } else {
                    anon.sign_in(&email_val, &password_val).await
                };

                match result {
                    Ok(authed) => {
                        if let Ok(mut rx) = authed.subscribe().await {
                            let authed_clone = authed.clone();
                            *AUTH_CONTROL.write() = Some(authed);

                            spawn(async move {
                                loop {
                                    match rx.recv().await {
                                        Ok(Some(event)) => handle_event(event),
                                        Ok(None) => break,
                                        Err(_) => continue,
                                    }
                                }
                                *AUTH_CONTROL.write() = None;
                                let _ = authed_clone;
                            });
                        }
                    }
                    Err((_anon_back, msg)) => {
                        *LOGIN_ERROR.write() = Some(msg);
                    }
                }
                loading.set(false);
            });
        };

        rsx! {
            div { class: "flex items-center justify-center min-h-screen",
                div { class: "w-full max-w-sm mx-auto",
                    // Header
                    div { class: "text-center mb-8",
                        h1 { class: "text-3xl font-bold text-zinc-100 mb-2",
                            "Architecture Playground"
                        }
                        p { class: "text-zinc-500 text-sm",
                            "better-auth → Service → RPC → Control (typestate) → UI"
                        }
                    }

                    // Auth card
                    div { class: "bg-zinc-900 border border-zinc-800 rounded-lg p-6 shadow-xl",
                        // Tab toggle
                        div { class: "flex mb-6 bg-zinc-800 rounded-md p-1",
                            button {
                                class: if !*is_signup.read() {
                                    "flex-1 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                                } else {
                                    "flex-1 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-300"
                                },
                                onclick: move |_| is_signup.set(false),
                                "Sign In"
                            }
                            button {
                                class: if *is_signup.read() {
                                    "flex-1 py-1.5 text-sm font-medium rounded bg-zinc-700 text-zinc-100"
                                } else {
                                    "flex-1 py-1.5 text-sm font-medium rounded text-zinc-400 hover:text-zinc-300"
                                },
                                onclick: move |_| is_signup.set(true),
                                "Sign Up"
                            }
                        }

                        form { onsubmit: on_submit,
                            // Name (sign up only)
                            if *is_signup.read() {
                                div { class: "mb-4",
                                    label { class: "block text-sm font-medium text-zinc-400 mb-1", "Name" }
                                    input {
                                        class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                        r#type: "text",
                                        placeholder: "Your name",
                                        value: "{name}",
                                        oninput: move |e| name.set(e.value()),
                                    }
                                }
                            }

                            // Email
                            div { class: "mb-4",
                                label { class: "block text-sm font-medium text-zinc-400 mb-1", "Email" }
                                input {
                                    class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                    r#type: "email",
                                    placeholder: "you@example.com",
                                    value: "{email}",
                                    oninput: move |e| email.set(e.value()),
                                }
                            }

                            // Password
                            div { class: "mb-4",
                                label { class: "block text-sm font-medium text-zinc-400 mb-1", "Password" }
                                input {
                                    class: "w-full px-3 py-2 bg-zinc-800 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                                    r#type: "password",
                                    placeholder: "Min 6 characters",
                                    value: "{password}",
                                    oninput: move |e| password.set(e.value()),
                                }
                            }

                            // Error
                            if let Some(err) = error.as_ref() {
                                div { class: "mb-4 px-3 py-2 bg-red-900/30 border border-red-800 rounded-md text-red-400 text-sm",
                                    "{err}"
                                }
                            }

                            // Submit
                            button {
                                class: "w-full py-2 px-4 bg-blue-600 hover:bg-blue-500 disabled:bg-zinc-700 disabled:text-zinc-500 text-white font-medium rounded-md transition-colors",
                                r#type: "submit",
                                disabled: *loading.read(),
                                if *loading.read() {
                                    "Working..."
                                } else if *is_signup.read() {
                                    "Create Account"
                                } else {
                                    "Sign In"
                                }
                            }
                        }
                    }

                    // Architecture note
                    div { class: "mt-6 p-4 bg-zinc-900/50 border border-zinc-800/50 rounded-lg",
                        p { class: "text-xs text-zinc-500 font-mono leading-relaxed",
                            "AppControl<Anonymous> → sign_in() → AppControl<Authenticated>"
                        }
                        p { class: "text-xs text-zinc-600 mt-1",
                            "Compile-time enforcement: list/create/toggle/delete unavailable before auth."
                        }
                    }
                }
            }
        }
    }

    // ── Authenticated App ───────────────────────────────────────────

    #[component]
    fn AuthenticatedApp() -> Element {
        let auth = AUTH_CONTROL.read();
        let Some(ctl) = auth.as_ref() else {
            return rsx! {};
        };
        let user_name = ctl.user_name().to_string();
        let user_email = ctl.user_email().to_string();

        rsx! {
            div { class: "max-w-2xl mx-auto px-4 py-8",
                // Top bar
                div { class: "flex items-center justify-between mb-6",
                    div {
                        h1 { class: "text-2xl font-bold text-zinc-100", "Items" }
                        p { class: "text-sm text-zinc-500",
                            "Signed in as "
                            span { class: "text-zinc-300 font-medium", "{user_name}" }
                            span { class: "text-zinc-600 ml-1", "({user_email})" }
                        }
                    }
                    SignOutButton {}
                }

                CreateForm {}
                ItemList {}

                div { class: "mt-8 pt-4 border-t border-zinc-800",
                    p { class: "text-xs text-zinc-600 font-mono",
                        "AppControl<Authenticated> — all methods available"
                    }
                    p { class: "text-xs text-zinc-700 mt-1",
                        "Auth: better-auth-rs (MemoryDatabaseAdapter) | Items: rusqlite | RPC: roam"
                    }
                }
            }
        }
    }

    // ── Sign Out ────────────────────────────────────────────────────

    #[component]
    fn SignOutButton() -> Element {
        let on_signout = move |_| {
            let ctl = AUTH_CONTROL.write().take();
            *ITEMS.write() = Vec::new();
            if let Some(ctl) = ctl {
                spawn(async move {
                    ctl.sign_out().await;
                });
            }
        };

        rsx! {
            button {
                class: "px-3 py-1.5 text-sm text-zinc-400 hover:text-zinc-200 border border-zinc-700 hover:border-zinc-600 rounded-md transition-colors",
                onclick: on_signout,
                "Sign Out"
            }
        }
    }

    // ── Create Form ─────────────────────────────────────────────────

    #[component]
    fn CreateForm() -> Element {
        let mut input = use_signal(String::new);

        let on_submit = move |_| {
            let name = input.read().trim().to_string();
            if name.is_empty() {
                return;
            }
            input.set(String::new());

            let Some(ctl) = AUTH_CONTROL.read().clone() else {
                return;
            };
            spawn(async move {
                ctl.create(&name).await;
            });
        };

        rsx! {
            form { onsubmit: on_submit, class: "flex gap-2 mb-6",
                input {
                    class: "flex-1 px-3 py-2 bg-zinc-900 border border-zinc-700 rounded-md text-zinc-100 placeholder-zinc-500 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent",
                    r#type: "text",
                    placeholder: "Add an item...",
                    value: "{input}",
                    oninput: move |e| input.set(e.value()),
                }
                button {
                    class: "px-4 py-2 bg-blue-600 hover:bg-blue-500 text-white font-medium rounded-md transition-colors",
                    r#type: "submit",
                    "Add"
                }
            }
        }
    }

    // ── Item List ───────────────────────────────────────────────────

    #[component]
    fn ItemList() -> Element {
        let items = ITEMS.read();

        if items.is_empty() {
            return rsx! {
                div { class: "flex flex-col items-center justify-center py-16 text-zinc-600",
                    p { class: "text-lg mb-1", "No items yet" }
                    p { class: "text-sm", "Add one above to get started." }
                }
            };
        }

        rsx! {
            div { class: "space-y-1",
                for item in items.iter() {
                    ItemRow { key: "{item.id}", item: item.clone() }
                }
            }
            div { class: "mt-4 text-right text-xs text-zinc-600",
                "{items.len()} items · {items.iter().filter(|i| i.done).count()} done"
            }
        }
    }

    // ── Item Row ────────────────────────────────────────────────────

    #[component]
    fn ItemRow(item: ItemInfo) -> Element {
        let id = item.id.clone();
        let id_del = item.id.clone();

        let on_toggle = move |_| {
            let id = id.clone();
            let Some(ctl) = AUTH_CONTROL.read().clone() else {
                return;
            };
            spawn(async move { ctl.toggle(&id).await });
        };

        let on_delete = move |_| {
            let id = id_del.clone();
            let Some(ctl) = AUTH_CONTROL.read().clone() else {
                return;
            };
            spawn(async move { ctl.delete(&id).await });
        };

        rsx! {
            div { class: "group flex items-center gap-3 px-3 py-2.5 rounded-md hover:bg-zinc-900 transition-colors",
                input {
                    r#type: "checkbox",
                    checked: item.done,
                    onchange: on_toggle,
                    class: "w-4 h-4 rounded border-zinc-600 bg-zinc-800 text-blue-500 focus:ring-blue-500 focus:ring-offset-0 cursor-pointer",
                }
                span {
                    class: if item.done {
                        "flex-1 text-zinc-500 line-through"
                    } else {
                        "flex-1 text-zinc-200"
                    },
                    "{item.name}"
                }
                button {
                    onclick: on_delete,
                    class: "opacity-0 group-hover:opacity-100 px-2 py-1 text-zinc-600 hover:text-red-400 transition-all text-sm",
                    "delete"
                }
            }
        }
    }

    // ── Event Handler ───────────────────────────────────────────────

    fn handle_event(event: ItemEvent) {
        match event {
            ItemEvent::ListChanged { items } => {
                *ITEMS.write() = items;
            }
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════════════
// Main — wire everything together
// ═══════════════════════════════════════════════════════════════════════════════

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter("playground=debug,roam=warn")
        .init();

    // 1. Item database (rusqlite in-memory)
    let db = database::open_memory();

    // 2. Tokio runtime
    let rt = tokio::runtime::Runtime::new().expect("failed to create runtime");

    // 3. Auth service (better-auth with MemoryDatabaseAdapter)
    let auth_svc = rt
        .block_on(standalone::StandaloneAuthService::new())
        .expect("failed to create auth service");

    // 4. Item service (rusqlite + validates sessions via better-auth)
    let item_svc = standalone::StandaloneItemService::new(db, auth_svc.auth_ref());

    // 5. Loopback RPC (both services chained via RoutedDispatcher)
    let handle = rt
        .block_on(loopback::connect(auth_svc, item_svc))
        .expect("failed to create loopback connection");

    // 6. Store handle for UI — typestate starts at Anonymous
    ui::set_handle(handle);

    // 7. Launch Dioxus
    dioxus::launch(ui::App);
}
