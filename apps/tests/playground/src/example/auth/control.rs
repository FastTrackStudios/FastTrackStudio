//! Auth control — typestate facade
//!
//! AppControl<Anonymous>     → sign_up/sign_in → AppControl<Authenticated>
//! AppControl<Authenticated> → sign_out (consumes self)

use super::live::AuthServiceLive;
use super::proto::*;
use crate::example::context::{DefaultContextFactory, SharedContextFactory};
use std::sync::Arc;

// ── State Markers ───────────────────────────────────────────────

pub struct Anonymous;
pub struct Authenticated {
    session: SessionToken,
}

// ── The Typestate Facade ────────────────────────────────────────

pub struct AuthControl<State = Anonymous, S = AuthServiceLive>
where
    S: AuthService,
{
    service: Arc<S>,
    context_factory: SharedContextFactory,
    state: State,
}

impl<S> Clone for AuthControl<Authenticated, S>
where
    S: AuthService,
{
    fn clone(&self) -> Self {
        Self {
            service: self.service.clone(),
            context_factory: self.context_factory.clone(),
            state: Authenticated {
                session: self.state.session.clone(),
            },
        }
    }
}

impl<S> PartialEq for AuthControl<Authenticated, S>
where
    S: AuthService,
{
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

// ── Anonymous state ─────────────────────────────────────────────

impl<S> AuthControl<Anonymous, S>
where
    S: AuthService,
{
    pub fn new(service: Arc<S>) -> Self {
        Self::new_with_context(service, Arc::new(DefaultContextFactory))
    }

    pub fn new_with_context(service: Arc<S>, context_factory: SharedContextFactory) -> Self {
        Self {
            service,
            context_factory,
            state: Anonymous,
        }
    }

    pub async fn sign_up(
        self,
        email: &str,
        password: &str,
        name: &str,
    ) -> Result<AuthControl<Authenticated, S>, (Self, String)> {
        let cx = self.context_factory.make_context();
        match self
            .service
            .sign_up(email.into(), password.into(), name.into())
            .await
        {
            AuthResult::Success { session } => Ok(AuthControl {
                service: self.service,
                context_factory: self.context_factory,
                state: Authenticated { session },
            }),
            AuthResult::Failed { message } => Err((self, message)),
        }
    }

    pub async fn sign_in(
        self,
        email: &str,
        password: &str,
    ) -> Result<AuthControl<Authenticated, S>, (Self, String)> {
        let cx = self.context_factory.make_context();
        match self
            .service
            .sign_in(email.into(), password.into())
            .await
        {
            AuthResult::Success { session } => Ok(AuthControl {
                service: self.service,
                context_factory: self.context_factory,
                state: Authenticated { session },
            }),
            AuthResult::Failed { message } => Err((self, message)),
        }
    }
}

// ── Authenticated state ─────────────────────────────────────────

impl<S> AuthControl<Authenticated, S>
where
    S: AuthService,
{
    pub fn session(&self) -> &SessionToken {
        &self.state.session
    }

    pub fn user_name(&self) -> &str {
        &self.session().user_name
    }

    pub fn user_email(&self) -> &str {
        &self.session().user_email
    }

    pub async fn sign_out(self) {
        let token = self.session().token.clone();
        let cx = self.context_factory.make_context();
        self.service.sign_out(token).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::example::context::ContextFactory;
    use roam::Context;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    struct CountingContextFactory {
        calls: AtomicUsize,
    }

    impl CountingContextFactory {
        fn new() -> Self {
            Self {
                calls: AtomicUsize::new(0),
            }
        }

        fn call_count(&self) -> usize {
            self.calls.load(Ordering::SeqCst)
        }
    }

    impl ContextFactory for CountingContextFactory {
        fn make_context(&self) -> roam::Context {
            self.calls.fetch_add(1, Ordering::SeqCst);
            Context::new(
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                vec![],
            )
        }
    }

    #[derive(Default)]
    struct FakeAuthService {
        next_sign_in: Mutex<Option<AuthResult>>,
        next_sign_up: Mutex<Option<AuthResult>>,
        signed_out_tokens: Mutex<Vec<String>>,
    }

    impl FakeAuthService {
        fn with_sign_in(result: AuthResult) -> Self {
            Self {
                next_sign_in: Mutex::new(Some(result)),
                next_sign_up: Mutex::new(None),
                signed_out_tokens: Mutex::new(Vec::new()),
            }
        }

        fn with_sign_up(result: AuthResult) -> Self {
            Self {
                next_sign_in: Mutex::new(None),
                next_sign_up: Mutex::new(Some(result)),
                signed_out_tokens: Mutex::new(Vec::new()),
            }
        }
    }

    impl AuthService for FakeAuthService {
        async fn sign_up(
            &self,
            ,
            _email: String,
            _password: String,
            _name: String,
        ) -> AuthResult {
            self.next_sign_up
                .lock()
                .unwrap()
                .take()
                .unwrap_or(AuthResult::Failed {
                    message: "missing sign_up result".into(),
                })
        }

        async fn sign_in(&self, __email: String, _password: String) -> AuthResult {
            self.next_sign_in
                .lock()
                .unwrap()
                .take()
                .unwrap_or(AuthResult::Failed {
                    message: "missing sign_in result".into(),
                })
        }

        async fn validate_session(&self, __token: String) -> Option<SessionToken> {
            None
        }

        async fn sign_out(&self, _token: String) {
            self.signed_out_tokens.lock().unwrap().push(token);
        }
    }

    fn demo_session() -> SessionToken {
        SessionToken {
            token: "t-123".into(),
            user_id: "u-1".into(),
            user_email: "demo@example.com".into(),
            user_name: "Demo".into(),
        }
    }

    #[tokio::test]
    async fn sign_in_success_uses_factory_once() {
        let service = Arc::new(FakeAuthService::with_sign_in(AuthResult::Success {
            session: demo_session(),
        }));
        let factory = Arc::new(CountingContextFactory::new());
        let control = AuthControl::new_with_context(service, factory.clone());

        let authed = match control.sign_in("demo@example.com", "demo123").await {
            Ok(authed) => authed,
            Err(_) => panic!("expected sign_in success"),
        };
        assert_eq!(authed.user_name(), "Demo");
        assert_eq!(factory.call_count(), 1);
    }

    #[tokio::test]
    async fn sign_in_failure_preserves_message() {
        let service = Arc::new(FakeAuthService::with_sign_in(AuthResult::Failed {
            message: "invalid credentials".into(),
        }));
        let control = AuthControl::new(service);

        let result = control.sign_in("demo@example.com", "bad").await;
        let Err((_control, msg)) = result else {
            panic!("expected sign_in failure");
        };
        assert_eq!(msg, "invalid credentials");
    }

    #[tokio::test]
    async fn sign_up_and_sign_out_use_factory() {
        let service = Arc::new(FakeAuthService::with_sign_up(AuthResult::Success {
            session: demo_session(),
        }));
        let factory = Arc::new(CountingContextFactory::new());
        let control = AuthControl::new_with_context(service.clone(), factory.clone());

        let authed = match control.sign_up("demo@example.com", "demo123", "Demo").await {
            Ok(authed) => authed,
            Err(_) => panic!("expected sign_up success"),
        };
        authed.sign_out().await;

        assert_eq!(factory.call_count(), 2);
        assert_eq!(service.signed_out_tokens.lock().unwrap().as_slice(), &["t-123"]);
    }
}
