//! Phase 2 integration test — architect-auth wired over vox.
//!
//! Plan (`plans/decentralized-foundation.md` §13 Phase 2):
//! create a user, sign in over vox, get a session token, call
//! `current_session` with that token, assert identity round-trips.
//!
//! Sign-up is NOT exposed over vox in upstream architect-auth's
//! `AuthService` trait — only sign-in / current-session / sign-out
//! are. So the user is created server-side via direct
//! `ArchitectAuth::create_email_password_user`, then the vox path
//! exercises sign-in + current-session.

use architect_auth::{CreateEmailPasswordUser, SignInEmailPassword};
use task_server::{AppState, AuthState, capability::ServerKeypair, router};

async fn boot_server() -> eyre::Result<(String, AuthState)> {
    // In-memory auth DB so tests don't touch the user's XDG dir.
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let keypair = ServerKeypair::generate_ephemeral();
    let state = AppState::new_with_auth(auth.clone(), keypair).await?;

    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok((format!("ws://127.0.0.1:{port}/vox"), auth))
}

#[tokio::test(flavor = "multi_thread")]
async fn signup_signin_current_session_roundtrip() -> eyre::Result<()> {
    let (url, auth_state) = boot_server().await?;

    // Seed the user server-side. Open signup over vox is a later
    // phase — Phase 2 just proves the auth pipeline is live.
    let bundle = auth_state
        .auth
        .create_email_password_user(CreateEmailPasswordUser {
            email: "alice@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            name: Some("Alice".into()),
            username: None,
            image: None,
            metadata_json: None,
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("seed user: {e:?}"))?;
    let alice_user_id = bundle.user.id;

    // Sign in over vox.
    let client: architect_auth::proto::AuthServiceClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let sign_in = client
        .sign_in_email_password(SignInEmailPassword {
            email: "alice@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("sign_in_email_password: {e:?}"))?;
    assert_eq!(
        sign_in.user.id, alice_user_id,
        "sign-in should return the same user we seeded"
    );
    assert!(
        !sign_in.token.is_empty(),
        "sign-in must yield a non-empty token"
    );

    // Use the token to fetch the current session over vox.
    let current = client
        .current_session(sign_in.token.clone())
        .await
        .map_err(|e| eyre::eyre!("current_session: {e:?}"))?;
    assert_eq!(current.user.id, alice_user_id);
    assert_eq!(current.token, sign_in.token);

    // Wrong token must fail.
    let bad = client.current_session("not-a-real-token".into()).await;
    assert!(bad.is_err(), "bogus token must not resolve to a session");

    Ok(())
}
