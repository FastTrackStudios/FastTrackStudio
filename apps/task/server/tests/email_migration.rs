#![allow(clippy::large_futures)]
//! Email migration against a REAL sqlite auth store.
//!
//! The `auth` crate's own tests cover this flow with in-memory storage,
//! which never touches sea-orm or the migrator — the gap that let a
//! duplicate migration name through until the server's boot tests caught
//! it. So this exercises the parts only a real database can fail at: that
//! the `auth_user_email_history` table exists after migrations run, that
//! the sea-orm storage impl reads and writes it, and that a live
//! `AuthState` (the same one the server opens) behaves.

use architect_auth::{CreateEmailPasswordUser, MigrateUserEmail};
use task_server::AuthState;

/// A real on-disk auth store, migrated exactly as the server does it.
async fn store() -> eyre::Result<(AuthState, tempfile::TempDir)> {
    let tmp = tempfile::tempdir()?;
    let db = tmp.path().join("auth.sqlite");
    let auth = AuthState::open(
        &format!("sqlite://{}?mode=rwc", db.display()),
        "test-secret-at-least-32-bytes!!!",
    )
    .await?;
    Ok((auth, tmp))
}

async fn seed(auth: &AuthState, email: &str) -> eyre::Result<uuid::Uuid> {
    let bundle = auth
        .auth
        .create_email_password_user(CreateEmailPasswordUser {
            email: email.into(),
            password: "correct-horse-battery-staple".into(),
            name: Some("Seed".into()),
            username: None,
            image: None,
            metadata_json: None,
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("seed `{email}`: {e:?}"))?;
    Ok(bundle.user.id)
}

#[tokio::test(flavor = "multi_thread")]
async fn migration_persists_and_records_against_real_sqlite() -> eyre::Result<()> {
    let (auth, _tmp) = store().await?;
    let id = seed(&auth, "old@example.test").await?;

    let moved = auth
        .auth
        .migrate_user_email(MigrateUserEmail {
            user_id: id,
            new_email: "new@example.test".into(),
            changed_by: None,
            reason: Some("operator migration".into()),
        })
        .await
        .map_err(|e| eyre::eyre!("migrate: {e:?}"))?;

    // THE property: the id is what everything else is keyed on.
    assert_eq!(moved.id, id, "migration must not change the user id");
    assert_eq!(moved.email.as_deref(), Some("new@example.test"));
    assert!(!moved.email_verified, "a new address starts unverified");

    // The row actually landed in sqlite — not just in a Vec somewhere.
    let history = auth
        .auth
        .list_email_history(id)
        .await
        .map_err(|e| eyre::eyre!("history: {e:?}"))?;
    assert_eq!(history.len(), 1, "expected one row, got {history:?}");
    assert_eq!(history[0].previous_email.as_deref(), Some("old@example.test"));
    assert_eq!(history[0].new_email, "new@example.test");
    assert_eq!(
        history[0].changed_by, None,
        "an operator migration records no signed-in user"
    );

    // Signing in with the NEW address works…
    auth.auth
        .sign_in_email_password(architect_auth::SignInEmailPassword {
            email: "new@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("sign in with the migrated address: {e:?}"))?;

    // …and the old one no longer authenticates, but is still ATTRIBUTABLE.
    // Both halves matter: the address is released, the record is not.
    let old_login = auth
        .auth
        .sign_in_email_password(architect_auth::SignInEmailPassword {
            email: "old@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            ip_address: None,
            user_agent: None,
        })
        .await;
    assert!(old_login.is_err(), "the old address must stop authenticating");

    let who = auth
        .auth
        .find_user_by_previous_email("old@example.test")
        .await
        .map_err(|e| eyre::eyre!("reverse lookup: {e:?}"))?
        .expect("the old address should still resolve to its account");
    assert_eq!(who.id, id);

    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn migration_is_idempotent_and_refuses_collisions_on_real_sqlite() -> eyre::Result<()> {
    let (auth, _tmp) = store().await?;
    let id = seed(&auth, "mine@example.test").await?;
    seed(&auth, "theirs@example.test").await?;

    // Onto an address someone else holds: refused, and no row written.
    let clash = auth
        .auth
        .migrate_user_email(MigrateUserEmail {
            user_id: id,
            new_email: "theirs@example.test".into(),
            changed_by: None,
            reason: None,
        })
        .await;
    assert!(clash.is_err(), "must not merge two accounts onto one address");
    assert!(
        auth.auth.list_email_history(id).await.unwrap().is_empty(),
        "a refused migration must leave no trace"
    );

    // Onto the address already held: a no-op, so re-running a bulk
    // migration after a partial failure is safe.
    auth.auth
        .migrate_user_email(MigrateUserEmail {
            user_id: id,
            new_email: "mine@example.test".into(),
            changed_by: None,
            reason: None,
        })
        .await
        .map_err(|e| eyre::eyre!("same-address migrate should succeed: {e:?}"))?;
    assert!(
        auth.auth.list_email_history(id).await.unwrap().is_empty(),
        "a no-op must not append a row claiming a change"
    );

    Ok(())
}
