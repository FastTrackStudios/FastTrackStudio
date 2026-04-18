//! IMAP IDLE push client for ProtonMail Bridge (or any IMAP server).
//!
//! Runs as a long-lived process that connects to Bridge, logs in,
//! selects a mailbox, and subscribes via RFC 2177 IDLE. Emits one
//! event per server-pushed update (typically an `EXISTS` response
//! indicating a new message).
//!
//! The caller is responsible for reacting to events — this module
//! is transport-only. A typical downstream: emit a JSON line,
//! fire `task email sweep` once the main task loop picks it up.
//!
//! # TLS
//!
//! Bridge presents a per-install self-signed cert. Pass `ca_bundle`
//! pointing at the merged bundle (on starcommand:
//! `/var/lib/nc-mail-trust/ca-bundle.crt`) so rustls can verify it.
//! `insecure = true` disables verification entirely — acceptable only
//! on loopback connections.

use crate::service::VaultError;
use std::net::IpAddr;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::net::TcpStream;
use tokio_util::compat::{TokioAsyncReadCompatExt, Compat};

/// Runtime config for a single IDLE subscription.
#[derive(Debug, Clone)]
pub struct ImapWatchConfig {
    /// IMAP host (e.g. `127.0.0.1`).
    pub host: String,
    /// IMAP port (e.g. `1143`).
    pub port: u16,
    /// Login username. For Bridge, this is the Proton address.
    pub user: String,
    pub password: String,
    /// Mailbox to select (e.g. `INBOX`).
    pub mailbox: String,
    /// Optional path to a PEM-encoded CA bundle. When set, rustls
    /// verifies the server cert against just this bundle. When None
    /// AND `insecure = false`, falls back to webpki-roots (public CAs).
    pub ca_bundle: Option<PathBuf>,
    /// Disable cert verification entirely. Only safe on loopback.
    pub insecure: bool,
    /// How long to stay in a single IDLE round before re-issuing.
    /// RFC recommends <30 min to avoid server-side timeouts.
    pub idle_timeout: Duration,
}

impl Default for ImapWatchConfig {
    fn default() -> Self {
        Self {
            host: "127.0.0.1".into(),
            port: 1143,
            user: String::new(),
            password: String::new(),
            mailbox: "INBOX".into(),
            ca_bundle: None,
            insecure: false,
            idle_timeout: Duration::from_secs(28 * 60),
        }
    }
}

/// Per-event payload emitted on new activity in the watched mailbox.
#[derive(Debug, Clone)]
pub struct WatchEvent {
    pub mailbox: String,
    /// New total message count reported by IMAP (from `* N EXISTS`).
    pub exists: Option<u64>,
    /// Raw IMAP untagged response text, for diagnostics.
    pub raw: String,
}

/// Run the watcher forever. Each server-pushed update invokes `on_event`.
/// Returns only on fatal error (caller should retry with backoff).
pub async fn watch_idle<F>(
    config: ImapWatchConfig,
    mut on_event: F,
) -> Result<(), VaultError>
where
    F: FnMut(WatchEvent) + Send,
{
    // ── Connect + STARTTLS ──────────────────────────────────────────────
    let tcp = TcpStream::connect((config.host.as_str(), config.port))
        .await
        .map_err(|e| VaultError::IoError(format!("imap connect: {e}")))?;
    let compat = tcp.compat();

    let mut client = async_imap::Client::new(compat);
    // Read greeting.
    client
        .read_response()
        .await
        .map_err(|e| VaultError::IoError(format!("imap greeting: {e}")))?;

    // STARTTLS: send, receive OK, unwrap stream, TLS-upgrade, rewrap.
    client
        .run_command_and_check_ok("STARTTLS", None)
        .await
        .map_err(|e| VaultError::IoError(format!("STARTTLS: {e}")))?;
    let raw: Compat<TcpStream> = client.into_inner();
    let tcp = raw.into_inner();

    let tls_config = build_tls_config(&config)?;
    let server_name = server_name_for(&config.host)?;
    let connector = tokio_rustls::TlsConnector::from(Arc::new(tls_config));
    let tls_stream = connector
        .connect(server_name, tcp)
        .await
        .map_err(|e| VaultError::IoError(format!("tls handshake: {e}")))?;

    let client = async_imap::Client::new(tls_stream.compat());

    // ── Login + select mailbox ──────────────────────────────────────────
    let mut session = client
        .login(&config.user, &config.password)
        .await
        .map_err(|(e, _)| VaultError::IoError(format!("imap login: {e}")))?;
    session
        .select(&config.mailbox)
        .await
        .map_err(|e| VaultError::IoError(format!("imap select {}: {e}", config.mailbox)))?;

    // ── IDLE loop ───────────────────────────────────────────────────────
    loop {
        let mut idle = session.idle();
        idle.init()
            .await
            .map_err(|e| VaultError::IoError(format!("IDLE init: {e}")))?;
        let (fut, _stop) = idle.wait_with_timeout(config.idle_timeout);
        let outcome = fut.await;

        // Drain any unsolicited responses buffered during the wait.
        session = idle
            .done()
            .await
            .map_err(|e| VaultError::IoError(format!("IDLE done: {e}")))?;

        match outcome {
            Ok(async_imap::extensions::idle::IdleResponse::NewData(data)) => {
                // ResponseData in async-imap 0.11 only exposes `.parsed()`.
                // We render the parsed response for diagnostics, and pick
                // `EXISTS` out directly when present.
                let parsed = data.parsed();
                let (exists, raw) = match parsed {
                    async_imap::imap_proto::Response::MailboxData(
                        async_imap::imap_proto::MailboxDatum::Exists(n),
                    ) => (Some(*n as u64), format!("* {n} EXISTS")),
                    other => (None, format!("{other:?}")),
                };
                on_event(WatchEvent {
                    mailbox: config.mailbox.clone(),
                    exists,
                    raw,
                });
            }
            Ok(async_imap::extensions::idle::IdleResponse::Timeout) => {
                // Natural timeout; drain any unsolicited and loop.
                drain_unsolicited(&mut session, &config.mailbox, &mut on_event);
            }
            Ok(async_imap::extensions::idle::IdleResponse::ManualInterrupt) => {
                // External cancel requested; fall through to drain and loop.
                drain_unsolicited(&mut session, &config.mailbox, &mut on_event);
            }
            Err(e) => {
                return Err(VaultError::IoError(format!("IDLE wait: {e}")));
            }
        }
    }
}

fn drain_unsolicited<F: FnMut(WatchEvent)>(
    session: &mut async_imap::Session<Compat<tokio_rustls::client::TlsStream<TcpStream>>>,
    mailbox: &str,
    on_event: &mut F,
) {
    while let Ok(Some(resp)) = session.unsolicited_responses.try_recv().map(Some) {
        // `resp` here is already the parsed UnsolicitedResponse variant.
        // Translate the common ones into WatchEvents for the caller.
        match resp {
            async_imap::types::UnsolicitedResponse::Exists(n) => {
                on_event(WatchEvent {
                    mailbox: mailbox.to_string(),
                    exists: Some(n as u64),
                    raw: format!("* {n} EXISTS"),
                });
            }
            async_imap::types::UnsolicitedResponse::Recent(_n) => {}
            other => {
                let _ = other;
            }
        }
    }
}

fn build_tls_config(config: &ImapWatchConfig) -> Result<rustls::ClientConfig, VaultError> {
    use rustls::ClientConfig;

    if config.insecure {
        let verifier = Arc::new(DangerousAcceptAny);
        let cfg = ClientConfig::builder()
            .dangerous()
            .with_custom_certificate_verifier(verifier)
            .with_no_client_auth();
        return Ok(cfg);
    }

    let mut roots = rustls::RootCertStore::empty();
    if let Some(path) = &config.ca_bundle {
        let bytes = std::fs::read(path)
            .map_err(|e| VaultError::IoError(format!("read ca bundle {path:?}: {e}")))?;
        let mut cursor = std::io::Cursor::new(bytes);
        for cert in rustls_pemfile::certs(&mut cursor) {
            let cert =
                cert.map_err(|e| VaultError::ParseError(format!("ca bundle PEM: {e}")))?;
            roots
                .add(cert)
                .map_err(|e| VaultError::ParseError(format!("add cert: {e}")))?;
        }
    } else {
        // Fall back to system/webpki roots via rustls-native-certs equivalent.
        // We keep this cheap — if the caller has a self-signed bridge cert,
        // they MUST pass a ca_bundle.
        roots.extend(webpki_roots_fallback());
    }
    let cfg = ClientConfig::builder()
        .with_root_certificates(roots)
        .with_no_client_auth();
    Ok(cfg)
}

fn webpki_roots_fallback() -> Vec<rustls_pki_types::TrustAnchor<'static>> {
    // Empty fallback — if a caller doesn't supply a ca_bundle and the
    // target isn't loopback, they'll hit a trust error, which is the
    // correct failure mode. We don't pull in webpki-roots as a dep
    // just for this edge case.
    Vec::new()
}

fn server_name_for(host: &str) -> Result<rustls_pki_types::ServerName<'static>, VaultError> {
    if let Ok(ip) = host.parse::<IpAddr>() {
        return Ok(rustls_pki_types::ServerName::IpAddress(ip.into()));
    }
    rustls_pki_types::ServerName::try_from(host.to_string())
        .map_err(|e| VaultError::ParseError(format!("server name {host}: {e}")))
}

/// rustls verifier that accepts any server cert. Only safe on loopback.
#[derive(Debug)]
struct DangerousAcceptAny;

impl rustls::client::danger::ServerCertVerifier for DangerousAcceptAny {
    fn verify_server_cert(
        &self,
        _end_entity: &rustls_pki_types::CertificateDer<'_>,
        _intermediates: &[rustls_pki_types::CertificateDer<'_>],
        _server_name: &rustls_pki_types::ServerName<'_>,
        _ocsp_response: &[u8],
        _now: rustls_pki_types::UnixTime,
    ) -> Result<rustls::client::danger::ServerCertVerified, rustls::Error> {
        Ok(rustls::client::danger::ServerCertVerified::assertion())
    }
    fn verify_tls12_signature(
        &self,
        _message: &[u8],
        _cert: &rustls_pki_types::CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }
    fn verify_tls13_signature(
        &self,
        _message: &[u8],
        _cert: &rustls_pki_types::CertificateDer<'_>,
        _dss: &rustls::DigitallySignedStruct,
    ) -> Result<rustls::client::danger::HandshakeSignatureValid, rustls::Error> {
        Ok(rustls::client::danger::HandshakeSignatureValid::assertion())
    }
    fn supported_verify_schemes(&self) -> Vec<rustls::SignatureScheme> {
        vec![
            rustls::SignatureScheme::RSA_PKCS1_SHA256,
            rustls::SignatureScheme::RSA_PKCS1_SHA384,
            rustls::SignatureScheme::RSA_PKCS1_SHA512,
            rustls::SignatureScheme::ECDSA_NISTP256_SHA256,
            rustls::SignatureScheme::ECDSA_NISTP384_SHA384,
            rustls::SignatureScheme::ED25519,
            rustls::SignatureScheme::RSA_PSS_SHA256,
            rustls::SignatureScheme::RSA_PSS_SHA384,
            rustls::SignatureScheme::RSA_PSS_SHA512,
        ]
    }
}
