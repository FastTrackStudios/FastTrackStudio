//! Web auth — the active account context + the bottom-left switcher.
//!
//! The server mounts architect-auth's `AuthService` per org
//! (`AuthServerMiddleware` wraps the dispatcher), so the UI signs in
//! over the same per-org vox socket every other service rides
//! ([`crate::vox_clients::establish_for`] against the home org).
//!
//! Flow: [`provide_auth`] runs at the app root and provides
//! `Signal<Option<ActiveAccount>>` + [`AuthCtx`]. On boot it restores
//! the persisted account (localStorage `task.auth.active`), defaulting
//! to the Guest account — Guest is the account we use for anonymous
//! sessions. [`AuthCtx::switch_account`] first tries the cached token
//! (`task.auth.token.<email>` → `whoami` validates), and only on a
//! miss performs a real `sign_in_email_password`. Switching never
//! signs the previous account out — its token stays cached so
//! switching back is instant; [`AuthCtx::sign_out`] is the only
//! explicit revocation.
//!
//! The [`AccountSwitcher`] (sidebar footer) replaces the old
//! free-text presence name input: identity now comes from the
//! account, and the presence status picker folded into the same
//! popover as a "Status" section.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use auth_proto::{AuthServiceClient, AuthUser, SignInEmailPassword, SignUpEmailPassword};

use crate::orgs::{OrgMeta, home_slug};
use crate::presence::{ManualStatus, PresenceLocal, PresenceStatus};
use crate::vox_clients::establish_for;

// ── dev accounts (DEV-ONLY SECTION) ─────────────────────────────────
//
// The switcher performs the REAL sign-in flow — real session tokens
// issued by the org's auth engine — it just has these dev credentials
// pre-filled so switching is one click. Production replaces the
// account picker with a login form; everything else (token cache,
// whoami validation, context, presence/claims integration) stays.

/// One pre-seeded dev account in the home org's auth DB.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DevAccount {
    pub email: &'static str,
    pub password: &'static str,
    pub name: &'static str,
    pub username: &'static str,
}

/// The four dev accounts seeded into the home org's `auth.sqlite`.
pub const DEV_ACCOUNTS: [DevAccount; 4] = [
    DevAccount {
        email: "cody@fasttrackstudios.com",
        password: "dev-cody-2026",
        name: "Cody Wright",
        username: "cody",
    },
    DevAccount {
        email: "carter@fasttrackstudios.com",
        password: "dev-carter-2026",
        name: "Carter Whitlock",
        username: "carter",
    },
    DevAccount {
        email: "tom@fasttrackstudios.com",
        password: "dev-tom-2026",
        name: "Tom Brooks",
        username: "tom",
    },
    DevAccount {
        email: "guest@fasttrackstudios.com",
        password: "dev-guest-2026",
        name: "Guest",
        username: "guest",
    },
];

/// The account a fresh browser lands on — anonymous-ish shared
/// identity ("Guest is the account we use for stuff like that").
pub const GUEST_EMAIL: &str = "guest@fasttrackstudios.com";

// ── active account context ──────────────────────────────────────────

/// The signed-in identity, derived from the server's session bundle.
#[derive(Clone, Debug, PartialEq)]
pub struct ActiveAccount {
    /// `AuthUser::id` — the auth system's user uuid (claims key).
    pub user_id: Uuid,
    pub email: String,
    pub name: String,
    /// The raw session token (also cached in localStorage under
    /// `task.auth.token.<email>` for instant switch-back).
    pub token: String,
}

/// Messages for the root auth service coroutine — the ONLY way auth
/// state changes. Every surface (desktop dropdown, mobile sheet, boot
/// restore) sends these; the single sequential consumer makes
/// concurrent-switch races and unmount-cancelled tasks structurally
/// impossible.
pub enum AuthAction {
    Switch(String),
    /// Real credential sign-in from the login form.
    SignIn { email: String, password: String },
    /// Self-serve account creation (architect-auth `sign_up_email_password`).
    SignUp {
        email: String,
        password: String,
        name: String,
    },
    SignOut,
}

/// Copyable auth handle — provided at the app root next to the plain
/// `Signal<Option<ActiveAccount>>` context (consumers that only read
/// identity take the signal; the switcher takes this).
#[derive(Clone, Copy)]
pub struct AuthCtx {
    pub active: Signal<Option<ActiveAccount>>,
    /// Last auth error, surfaced as a small text line under the
    /// switcher (never panics, never blocks the app).
    pub error: Signal<Option<String>>,
    /// True while a switch/sign-in is in flight.
    pub busy: Signal<bool>,
    /// The root auth service (see [`provide_auth`]).
    actions: Coroutine<AuthAction>,
}

impl AuthCtx {
    /// Request a switch to `email`. Sync fire-and-forget — safe from
    /// UI that closes/unmounts on selection (sheets, dropdowns): the
    /// work runs in the root coroutine, not the caller's scope.
    pub fn switch_account(self, email: impl Into<String>) {
        self.actions.send(AuthAction::Switch(email.into()));
    }

    /// Sign in with real credentials (the login form). Fire-and-forget.
    pub fn sign_in(self, email: impl Into<String>, password: impl Into<String>) {
        self.actions.send(AuthAction::SignIn {
            email: email.into(),
            password: password.into(),
        });
    }

    /// Create a new account, then sign into it (the sign-up form).
    pub fn sign_up(
        self,
        email: impl Into<String>,
        password: impl Into<String>,
        name: impl Into<String>,
    ) {
        self.actions.send(AuthAction::SignUp {
            email: email.into(),
            password: password.into(),
            name: name.into(),
        });
    }

    /// Request sign-out (revoke server-side, drop cached token, fall
    /// back to Guest). Same fire-and-forget contract as
    /// [`Self::switch_account`].
    pub fn sign_out(self) {
        self.actions.send(AuthAction::SignOut);
    }
}

/// The signals the auth service mutates — plumbed as one bundle so
/// the coroutine closure and the async workers stay readable.
#[derive(Clone, Copy)]
struct AuthState {
    active: Signal<Option<ActiveAccount>>,
    error: Signal<Option<String>>,
    busy: Signal<bool>,
    orgs: Signal<Vec<OrgMeta>>,
}

/// Switch to `email`: cached token → `whoami` validates; on miss or
/// invalid → real `sign_in_email_password` → cache the fresh token.
/// Sets the context + persists `task.auth.active`. The previous
/// account is NOT signed out — its token stays cached so switching
/// back is instant.
async fn run_switch(mut st: AuthState, email: &str) {
    let slug = home_slug(&st.orgs.peek());
    if slug.is_empty() {
        st.error
            .set(Some("org discovery hasn't resolved yet".to_owned()));
        return;
    }
    st.busy.set(true);
    st.error.set(None);
    match resolve_session(&slug, email).await {
        Ok(account) => {
            save_active_email(&account.email);
            st.active.set(Some(account));
        }
        Err(e) => st.error.set(Some(e)),
    }
    st.busy.set(false);
}

/// Real credential sign-in (login form). When `name` is `Some`, first
/// creates the account via `sign_up_email_password`, then the returned
/// bundle IS the session (sign-up logs you in). Caches the token under
/// the email so later `switch_account`/boot restores it via `whoami`.
async fn run_credential_sign_in(
    mut st: AuthState,
    email: &str,
    password: &str,
    name: Option<&str>,
) {
    let slug = home_slug(&st.orgs.peek());
    if slug.is_empty() {
        st.error
            .set(Some("org discovery hasn't resolved yet".to_owned()));
        return;
    }
    st.busy.set(true);
    st.error.set(None);
    let result = async {
        let client = establish_for::<AuthServiceClient>(&slug).await?;
        let bundle = if let Some(name) = name {
            client
                .sign_up_email_password(SignUpEmailPassword {
                    email: email.to_owned(),
                    password: password.to_owned(),
                    name: Some(name.to_owned()),
                    username: None,
                    image: None,
                    metadata_json: None,
                    ip_address: None,
                    user_agent: Some("task-web".to_owned()),
                })
                .await
                .map_err(|e| format!("create account: {e}"))?
        } else {
            client
                .sign_in_email_password(SignInEmailPassword {
                    email: email.to_owned(),
                    password: password.to_owned(),
                    ip_address: None,
                    user_agent: Some("task-web".to_owned()),
                })
                .await
                .map_err(|e| format!("sign in: {e}"))?
        };
        save_cached_token(email, &bundle.token);
        Ok::<ActiveAccount, String>(account_from(bundle.user, email, bundle.token))
    }
    .await;
    match result {
        Ok(account) => {
            save_active_email(&account.email);
            st.active.set(Some(account));
        }
        Err(e) => st.error.set(Some(e)),
    }
    st.busy.set(false);
}

/// Explicit sign-out: revoke the session server-side, drop the cached
/// token + active marker, then fall back to Guest (the anonymous
/// default — auto sign-in).
async fn run_sign_out(mut st: AuthState) {
    let Some(account) = st.active.peek().clone() else {
        return;
    };
    clear_cached_token(&account.email);
    clear_active_email();
    st.active.set(None);
    let slug = home_slug(&st.orgs.peek());
    if !slug.is_empty() {
        if let Ok(client) = establish_for::<AuthServiceClient>(&slug).await {
            // Best-effort revocation — sign_out is idempotent.
            let _ = client.sign_out(account.token.clone()).await;
        }
    }
    run_switch(st, GUEST_EMAIL).await;
}

/// Provide the auth contexts and kick off boot restore. Call once at
/// the app root, after the org-list provider (`fetch_orgs` discovery)
/// and before the router.
pub fn provide_auth() -> AuthCtx {
    use futures_util::StreamExt as _;

    let orgs = use_context::<Signal<Vec<OrgMeta>>>();
    let active = use_signal(|| None::<ActiveAccount>);
    let error = use_signal(|| None::<String>);
    let busy = use_signal(|| false);
    let st = AuthState {
        active,
        error,
        busy,
        orgs,
    };

    // The auth service: one sequential consumer for every auth action
    // in the app. Root-owned (unmount-safe — a sheet closing can't
    // cancel it) and ordered (no concurrent switches to race). A
    // queued burst coalesces to the newest action, so clicking an
    // account while the boot auto-sign-in is still resolving skips
    // straight to the click — no Guest flash, no generation counter.
    let actions = use_coroutine(move |mut rx: UnboundedReceiver<AuthAction>| async move {
        while let Some(mut msg) = rx.next().await {
            while let Ok(newer) = rx.try_recv() {
                msg = newer;
            }
            match msg {
                AuthAction::Switch(email) => run_switch(st, &email).await,
                AuthAction::SignIn { email, password } => {
                    run_credential_sign_in(st, &email, &password, None).await;
                }
                AuthAction::SignUp {
                    email,
                    password,
                    name,
                } => {
                    run_credential_sign_in(st, &email, &password, Some(&name)).await;
                }
                AuthAction::SignOut => run_sign_out(st).await,
            }
        }
    });

    let ctx = AuthCtx {
        active,
        error,
        busy,
        actions,
    };
    use_context_provider(|| active);
    use_context_provider(|| ctx);

    // Boot restore: wait for org discovery (home slug resolves), then
    // validate the persisted account — or auto sign-in as Guest when
    // nothing is stored. Runs exactly once.
    let mut booted = use_signal(|| false);
    use_effect(move || {
        let slug = home_slug(&orgs.read());
        if slug.is_empty() || *booted.peek() {
            return;
        }
        booted.set(true);
        ctx.switch_account(load_active_email().unwrap_or_else(|| GUEST_EMAIL.to_owned()));
    });
    ctx
}

/// Token-cache-first session resolution against the home org.
async fn resolve_session(slug: &str, email: &str) -> Result<ActiveAccount, String> {
    let client = establish_for::<AuthServiceClient>(slug).await?;

    // 1. Cached token → whoami validates it without a fresh sign-in.
    if let Some(token) = load_cached_token(email) {
        match client.whoami(token.clone()).await {
            Ok(user) => return Ok(account_from(user, email, token)),
            Err(_) => clear_cached_token(email), // expired/revoked — fall through
        }
    }

    // 2. Real sign-in with the pre-filled dev credentials.
    let dev = DEV_ACCOUNTS
        .iter()
        .find(|a| a.email == email)
        .ok_or_else(|| format!("no credentials on file for {email}"))?;
    let bundle = client
        .sign_in_email_password(SignInEmailPassword {
            email: email.to_owned(),
            password: dev.password.to_owned(),
            ip_address: None,
            user_agent: Some("task-web".to_owned()),
        })
        .await
        .map_err(|e| format!("sign in {email}: {e}"))?;
    save_cached_token(email, &bundle.token);
    Ok(account_from(bundle.user, email, bundle.token))
}

/// Build the context value from an `AuthUser`, with dev-roster
/// fallbacks for the optional fields.
fn account_from(user: AuthUser, email: &str, token: String) -> ActiveAccount {
    let dev_name = DEV_ACCOUNTS
        .iter()
        .find(|a| a.email == email)
        .map(|a| a.name.to_owned());
    ActiveAccount {
        user_id: user.id,
        email: user.email.unwrap_or_else(|| email.to_owned()),
        name: user
            .name
            .filter(|n| !n.trim().is_empty())
            .or(dev_name)
            .unwrap_or_else(|| email.to_owned()),
        token,
    }
}

// ── avatars ─────────────────────────────────────────────────────────

/// Tasteful gradient palette — `(from, to)` CSS colors. Six entries;
/// [`gradient_index`] picks one deterministically per account.
const AVATAR_GRADIENTS: [(&str, &str); 6] = [
    ("#f59e0b", "#ef4444"), // amber → red
    ("#8b5cf6", "#6366f1"), // violet → indigo
    ("#06b6d4", "#3b82f6"), // cyan → blue
    ("#10b981", "#14b8a6"), // emerald → teal
    ("#ec4899", "#f43f5e"), // pink → rose
    ("#84cc16", "#22c55e"), // lime → green
];

/// FNV-1a over the key, mod the palette size. Deterministic across
/// targets and sessions — the same account always gets the same
/// gradient, with no external requests or asset files.
#[must_use]
pub fn gradient_index(key: &str) -> usize {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    for byte in key.as_bytes() {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
    }
    (hash % AVATAR_GRADIENTS.len() as u64) as usize
}

/// Two-letter initials: first letters of the first two words, or the
/// first two characters of a single word. Uppercased; `?` when empty.
#[must_use]
pub fn initials(name: &str) -> String {
    let mut words = name.split_whitespace();
    match (words.next(), words.next()) {
        (Some(a), Some(b)) => {
            let mut s = String::new();
            s.extend(a.chars().next().map(|c| c.to_ascii_uppercase()));
            s.extend(b.chars().next().map(|c| c.to_ascii_uppercase()));
            s
        }
        (Some(a), None) => a.chars().take(2).map(|c| c.to_ascii_uppercase()).collect(),
        _ => "?".to_owned(),
    }
}

/// Round initials avatar with a deterministic per-account gradient.
/// `email` keys the gradient; when it's empty the name keys it
/// (presence rows from peers that predate account identity).
#[component]
pub fn Avatar(name: String, email: String, #[props(default = 28)] size: u32) -> Element {
    let key = if email.is_empty() { &name } else { &email };
    let (from, to) = AVATAR_GRADIENTS[gradient_index(key)];
    let letters = initials(&name);
    // ~0.38em type within the disc, floored so 16px stays legible.
    let font = (size * 2 / 5).max(7);
    rsx! {
        span {
            class: "flex shrink-0 select-none items-center justify-center rounded-full font-semibold leading-none text-white",
            style: "width:{size}px;height:{size}px;font-size:{font}px;background:linear-gradient(135deg,{from},{to});",
            title: "{name}",
            "{letters}"
        }
    }
}

// ── account & status, shared content ────────────────────────────────

/// The status-picker rows both presentations render: the manual
/// override value, its label, and the status whose dot previews it.
pub const STATUS_OPTIONS: [(ManualStatus, &str, PresenceStatus); 3] = [
    (ManualStatus::Auto, "Active (auto)", PresenceStatus::Active),
    (
        ManualStatus::Available,
        "Available",
        PresenceStatus::Available,
    ),
    (ManualStatus::Dnd, "Do not disturb", PresenceStatus::Dnd),
];

/// Email/password sign-in (and self-serve sign-up) form. Rides
/// [`AuthCtx`] — `sign_in`/`sign_up` are fire-and-forget; `busy`/`error`
/// come back on the context. Real architect-auth accounts; this is what
/// production shows in place of the dev-account picker.
#[component]
pub fn LoginForm() -> Element {
    let ctx = use_context::<AuthCtx>();
    let busy = ctx.busy;
    let mut error = ctx.error;
    let mut email = use_signal(String::new);
    let mut password = use_signal(String::new);
    let mut name = use_signal(String::new);
    let mut creating = use_signal(|| false);

    let submit = move |_| {
        let e = email.peek().trim().to_owned();
        let p = password.peek().clone();
        if e.is_empty() || p.is_empty() {
            return;
        }
        if *creating.peek() {
            ctx.sign_up(e, p, name.peek().trim().to_owned());
        } else {
            ctx.sign_in(e, p);
        }
    };

    rsx! {
        div { class: "flex flex-col gap-2",
            if creating() {
                Input {
                    value: name,
                    placeholder: "Name",
                    on_change: move |_| error.set(None),
                }
            }
            Input {
                value: email,
                input_type: "email".to_string(),
                placeholder: "Email",
                on_change: move |_| error.set(None),
            }
            Input {
                value: password,
                input_type: "password".to_string(),
                placeholder: "Password",
                on_change: move |_| error.set(None),
            }
            Button {
                variant: ButtonVariant::Primary,
                size: ButtonSize::Medium,
                loading: busy(),
                on_click: submit,
                class: "w-full",
                if creating() { "Create account" } else { "Sign in" }
            }
            button {
                r#type: "button",
                class: "text-xs text-muted-foreground hover:text-foreground",
                onclick: move |_| {
                    error.set(None);
                    let now = *creating.peek();
                    creating.set(!now);
                },
                if creating() { "Have an account? Sign in" } else { "Create an account" }
            }
            if let Some(msg) = error.read().as_ref() {
                div { class: "px-1 text-xs text-destructive", "{msg}" }
            }
        }
    }
}

/// Account & status content for the mobile bottom sheet — the same
/// roster / status / sign-out actions as the desktop [`AccountSwitcher`]
/// popover (both ride [`AuthCtx`] + [`PresenceLocal`]), restyled as
/// touch-sized rows (≥44px). `on_done` fires after any action so the
/// hosting sheet can close.
#[component]
pub fn AccountSheetBody(on_done: EventHandler<()>) -> Element {
    let ctx = use_context::<AuthCtx>();
    let active = ctx.active;
    let error = ctx.error;
    let local = use_context::<PresenceLocal>();
    let mut manual = local.manual;

    let account = active.read().clone();
    let (name, email) = account.as_ref().map_or_else(
        || ("Signing in…".to_owned(), String::new()),
        |a| (a.name.clone(), a.email.clone()),
    );
    let active_email = account.as_ref().map(|a| a.email.clone());
    let effective = local.effective_status();
    let dot = effective.dot_class();
    let current_status = *manual.read();

    rsx! {
        div { class: "flex flex-col gap-4 pb-2",
            // Signed-in identity card.
            div { class: "flex items-center gap-3 rounded-xl border border-border bg-card px-3 py-3",
                span { class: "relative shrink-0",
                    Avatar { name: name.clone(), email: email.clone(), size: 40 }
                    span { class: "absolute -bottom-0.5 -right-0.5 h-3 w-3 rounded-full border-2 border-card {dot}",
                        title: "{effective.label()}",
                    }
                }
                span { class: "flex min-w-0 flex-col",
                    span { class: "truncate text-sm font-semibold text-foreground", "{name}" }
                    if !email.is_empty() {
                        span { class: "truncate text-xs text-muted-foreground", "{email}" }
                    }
                }
            }
            if let Some(msg) = error.read().as_ref() {
                div { class: "px-1 text-xs text-destructive", "{msg}" }
            }

            section {
                h3 { class: "px-1 pb-1 text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                    "Sign in"
                }
                LoginForm {}
            }

            // Dev-account quick picker — one-click switch, DEBUG builds only.
            if cfg!(debug_assertions) {
                section {
                    h3 { class: "px-1 pb-1 text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                        "Dev accounts"
                    }
                    div { class: "flex flex-col",
                        for dev in DEV_ACCOUNTS {
                            button {
                                key: "{dev.email}",
                                r#type: "button",
                                class: "flex min-h-[44px] w-full items-center gap-3 rounded-lg px-2 py-2 text-left active:bg-accent",
                                onclick: move |_| {
                                    on_done.call(());
                                    ctx.switch_account(dev.email);
                                },
                                Avatar { name: dev.name.to_string(), email: dev.email.to_string(), size: 28 }
                                span { class: "flex min-w-0 flex-col",
                                    span { class: "truncate text-sm text-foreground", "{dev.name}" }
                                    span { class: "truncate text-xs text-muted-foreground", "{dev.email}" }
                                }
                                if active_email.as_deref() == Some(dev.email) {
                                    span { class: "ml-auto text-sm text-primary", "●" }
                                }
                            }
                        }
                    }
                }
            }

            section {
                h3 { class: "px-1 pb-1 text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                    "Status"
                }
                div { class: "flex flex-col",
                    for (value , label , status) in STATUS_OPTIONS {
                        button {
                            key: "{label}",
                            r#type: "button",
                            class: "flex min-h-[44px] w-full items-center gap-3 rounded-lg px-2 py-2 text-left active:bg-accent",
                            onclick: move |_| {
                                manual.set(value);
                                on_done.call(());
                            },
                            span { class: "h-2.5 w-2.5 rounded-full {status.dot_class()}" }
                            span { class: "text-sm text-foreground", "{label}" }
                            if current_status == value {
                                span { class: "ml-auto text-sm text-primary", "●" }
                            }
                        }
                    }
                }
            }

            button {
                r#type: "button",
                class: "flex min-h-[44px] w-full items-center justify-center rounded-lg border border-destructive/40 px-3 py-2 text-sm font-medium text-destructive active:bg-destructive/10",
                onclick: move |_| {
                    on_done.call(());
                    ctx.sign_out();
                },
                "Sign out"
            }
        }
    }
}

// ── bottom-left account switcher ────────────────────────────────────

/// Account switcher: avatar + presence dot opening a popover with the
/// account roster (instant switch), the presence status section
/// (Auto/Available/DND), and sign-out. Two skins: the full identity
/// card (default), and `rail` — an icon-sized avatar button for the
/// icon rail's foot, where name/email live inside the popover instead.
#[component]
pub fn AccountSwitcher(#[props(default = false)] rail: bool) -> Element {
    let ctx = use_context::<AuthCtx>();
    let active = ctx.active;
    let error = ctx.error;
    let local = use_context::<PresenceLocal>();
    let mut manual = local.manual;
    let mut open = use_signal(|| false);

    let account = active.read().clone();
    let (name, email) = account.as_ref().map_or_else(
        || ("Signing in…".to_owned(), String::new()),
        |a| (a.name.clone(), a.email.clone()),
    );
    let active_email = account.as_ref().map(|a| a.email.clone());

    let effective = local.effective_status();
    let dot = effective.dot_class();
    let current_status = *manual.read();
    let status_options = STATUS_OPTIONS;

    rsx! {
        div { class: if rail { "flex flex-col" } else { "flex w-full flex-col gap-1" },
            Dropdown {
                open: open(),
                on_open_change: move |o| open.set(o),
                class: if rail { "" } else { "w-full" },
                DropdownTrigger { class: if rail { "" } else { "w-full" },
                    if rail {
                        button {
                            r#type: "button",
                            class: "flex h-8 w-8 items-center justify-center rounded-lg hover:bg-accent/50",
                            title: "Account & status — {name}",
                            span { class: "relative shrink-0",
                                Avatar { name: name.clone(), email: email.clone(), size: 26 }
                                span { class: "absolute -bottom-0.5 -right-0.5 h-2.5 w-2.5 rounded-full border-2 border-card {dot}",
                                    title: "{effective.label()}",
                                }
                            }
                        }
                    } else {
                        button {
                            r#type: "button",
                            class: "flex w-full items-center gap-2 rounded-xl border border-border bg-card px-2 py-1.5 text-left hover:bg-accent",
                            title: "Account & status",
                            span { class: "relative shrink-0",
                                Avatar { name: name.clone(), email: email.clone(), size: 32 }
                                // Presence status dot, Discord-style on the
                                // avatar's corner.
                                span { class: "absolute -bottom-0.5 -right-0.5 h-2.5 w-2.5 rounded-full border-2 border-card {dot}",
                                    title: "{effective.label()}",
                                }
                            }
                            span { class: "flex min-w-0 flex-col",
                                span { class: "truncate text-xs font-semibold text-foreground", "{name}" }
                                if !email.is_empty() {
                                    span { class: "truncate text-[11px] text-muted-foreground", "{email}" }
                                }
                            }
                        }
                    }
                }
                DropdownContent { side: "top", align: "start", width: "w-64",
                    // Rail skin: the trigger is a bare avatar, so the
                    // identity (name/email) heads the popover instead.
                    if rail {
                        div { class: "flex items-center gap-2 px-2 pb-1 pt-0.5",
                            Avatar { name: name.clone(), email: email.clone(), size: 28 }
                            span { class: "flex min-w-0 flex-col",
                                span { class: "truncate text-xs font-semibold text-foreground", "{name}" }
                                if !email.is_empty() {
                                    span { class: "truncate text-[11px] text-muted-foreground", "{email}" }
                                }
                            }
                        }
                        DropdownSeparator {}
                    }
                    DropdownLabel { "Account" }
                    for (idx, dev) in DEV_ACCOUNTS.into_iter().enumerate() {
                        DropdownItem {
                            key: "{dev.email}",
                            value: dev.email.to_string(),
                            index: idx,
                            on_select: move |_| {
                                open.set(false);
                                ctx.switch_account(dev.email);
                            },
                            div { class: "flex w-full items-center justify-between gap-2",
                                span { class: "flex min-w-0 items-center gap-2",
                                    Avatar { name: dev.name.to_string(), email: dev.email.to_string(), size: 22 }
                                    span { class: "truncate", "{dev.name}" }
                                }
                                if active_email.as_deref() == Some(dev.email) {
                                    span { class: "text-xs text-primary", "●" }
                                }
                            }
                        }
                    }
                    DropdownSeparator {}
                    DropdownLabel { "Status" }
                    for (idx, (value, label, status)) in status_options.into_iter().enumerate() {
                        DropdownItem {
                            key: "{label}",
                            value: label.to_string(),
                            index: DEV_ACCOUNTS.len() + idx,
                            on_select: move |_| {
                                manual.set(value);
                                open.set(false);
                            },
                            div { class: "flex w-full items-center justify-between gap-2",
                                span { class: "flex items-center gap-2",
                                    span { class: "h-2 w-2 rounded-full {status.dot_class()}" }
                                    span { "{label}" }
                                }
                                if current_status == value {
                                    span { class: "text-xs text-primary", "●" }
                                }
                            }
                        }
                    }
                    DropdownSeparator {}
                    DropdownItem {
                        value: "__sign_out".to_string(),
                        index: DEV_ACCOUNTS.len() + status_options.len(),
                        destructive: true,
                        on_select: move |_| {
                            open.set(false);
                            ctx.sign_out();
                        },
                        "Sign out"
                    }
                }
            }
            if let Some(msg) = error.read().as_ref() {
                div { class: "px-1 text-[11px] text-destructive", "{msg}" }
            }
        }
    }
}

// ── localStorage persistence (web only) ─────────────────────────────

#[cfg(target_arch = "wasm32")]
const ACTIVE_KEY: &str = "task.auth.active";

// Used by the wasm storage fns + the key-shape test; native builds
// have no token cache yet, hence the cfg.
#[cfg(any(target_arch = "wasm32", test))]
fn token_key(email: &str) -> String {
    format!("task.auth.token.{email}")
}

#[cfg(target_arch = "wasm32")]
fn storage() -> Option<web_sys::Storage> {
    web_sys::window().and_then(|w| w.local_storage().ok().flatten())
}

#[cfg(target_arch = "wasm32")]
fn load_cached_token(email: &str) -> Option<String> {
    storage()
        .and_then(|s| s.get_item(&token_key(email)).ok().flatten())
        .filter(|t| !t.is_empty())
}

#[cfg(target_arch = "wasm32")]
fn save_cached_token(email: &str, token: &str) {
    if let Some(s) = storage() {
        let _ = s.set_item(&token_key(email), token);
    }
}

#[cfg(target_arch = "wasm32")]
fn clear_cached_token(email: &str) {
    if let Some(s) = storage() {
        let _ = s.remove_item(&token_key(email));
    }
}

#[cfg(target_arch = "wasm32")]
fn load_active_email() -> Option<String> {
    storage()
        .and_then(|s| s.get_item(ACTIVE_KEY).ok().flatten())
        .filter(|e| !e.is_empty())
}

#[cfg(target_arch = "wasm32")]
fn save_active_email(email: &str) {
    if let Some(s) = storage() {
        let _ = s.set_item(ACTIVE_KEY, email);
    }
}

#[cfg(target_arch = "wasm32")]
fn clear_active_email() {
    if let Some(s) = storage() {
        let _ = s.remove_item(ACTIVE_KEY);
    }
}

// Native: no persistent token cache yet (rides the future settings
// store) — every launch performs a fresh sign-in, which is correct.
#[cfg(not(target_arch = "wasm32"))]
fn load_cached_token(_email: &str) -> Option<String> {
    None
}

#[cfg(not(target_arch = "wasm32"))]
fn save_cached_token(_email: &str, _token: &str) {}

#[cfg(not(target_arch = "wasm32"))]
fn clear_cached_token(_email: &str) {}

#[cfg(not(target_arch = "wasm32"))]
fn load_active_email() -> Option<String> {
    None
}

#[cfg(not(target_arch = "wasm32"))]
fn save_active_email(_email: &str) {}

#[cfg(not(target_arch = "wasm32"))]
fn clear_active_email() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn gradient_index_is_deterministic_and_in_range() {
        for dev in DEV_ACCOUNTS {
            let idx = gradient_index(dev.email);
            assert!(idx < AVATAR_GRADIENTS.len());
            assert_eq!(idx, gradient_index(dev.email), "stable across calls");
        }
    }

    #[test]
    fn gradient_index_spreads_the_dev_roster() {
        // Not a uniformity proof — just pin that the four dev accounts
        // don't all collapse onto one gradient (a regression guard for
        // hash fn edits).
        let mut seen: Vec<usize> = DEV_ACCOUNTS
            .iter()
            .map(|a| gradient_index(a.email))
            .collect();
        seen.sort_unstable();
        seen.dedup();
        assert!(seen.len() >= 2, "dev accounts share one gradient: {seen:?}");
    }

    #[test]
    fn initials_take_first_letters_of_two_words() {
        assert_eq!(initials("Cody Wright"), "CW");
        assert_eq!(initials("Carter Whitlock"), "CW");
        assert_eq!(initials("tom brooks"), "TB");
        assert_eq!(initials("Guest"), "GU");
        assert_eq!(initials("  spaced   out  "), "SO");
        assert_eq!(initials(""), "?");
    }

    #[test]
    fn guest_is_a_dev_account() {
        assert!(DEV_ACCOUNTS.iter().any(|a| a.email == GUEST_EMAIL));
    }

    #[test]
    fn token_keys_are_per_email() {
        assert_eq!(
            token_key("cody@fasttrackstudios.com"),
            "task.auth.token.cody@fasttrackstudios.com"
        );
        assert_ne!(token_key("a@x"), token_key("b@x"));
    }
}
