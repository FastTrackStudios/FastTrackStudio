//! Connection bootstrap. TLS-on-connect, STARTTLS, and plain
//! TCP variants are kept behind one entry point so the rest of
//! the backend doesn't branch on `TlsMode`.

use async_imap::{Client, Session};
use email_config::TlsMode;
use email_secret::SecretValue;
use thiserror::Error;
use tokio::net::TcpStream;

/// The stream type the session wraps. In production we always
/// use a TLS-on-TCP socket. The `test-plaintext` feature swaps
/// in raw `TcpStream` so an in-process mock IMAP server on
/// `127.0.0.1` can drive the backend end-to-end without needing
/// to ship a fake cert.
#[cfg(not(feature = "test-plaintext"))]
pub type ImapStream = async_native_tls::TlsStream<TcpStream>;
#[cfg(feature = "test-plaintext")]
pub type ImapStream = TcpStream;

/// Live, authenticated IMAP session. Stream type flips between
/// TLS + plaintext via the `test-plaintext` feature.
pub type ImapSession = Session<ImapStream>;

#[derive(Debug, Error)]
pub enum ConnectError {
    #[error("tcp connect: {0}")]
    Tcp(String),
    #[error("tls handshake: {0}")]
    Tls(String),
    #[error("imap greeting: {0}")]
    Greeting(String),
    #[error("login: {0}")]
    Login(String),
    #[error("starttls is not yet implemented in this backend")]
    StarttlsUnsupported,
    #[error("plaintext IMAP is refused (tests/loopback only)")]
    PlaintextRefused,
}

pub async fn connect_and_login(
    host: &str,
    port: u16,
    tls: TlsMode,
    username: &str,
    password: &SecretValue,
) -> Result<ImapSession, ConnectError> {
    let tcp = TcpStream::connect((host, port))
        .await
        .map_err(|e| ConnectError::Tcp(e.to_string()))?;

    let stream = build_stream(host, tcp, tls).await?;
    let client = Client::new(stream);
    let session = client
        .login(username, password.as_str())
        .await
        .map_err(|(e, _)| ConnectError::Login(e.to_string()))?;
    Ok(session)
}

#[cfg(not(feature = "test-plaintext"))]
async fn build_stream(
    host: &str,
    tcp: TcpStream,
    tls: TlsMode,
) -> Result<ImapStream, ConnectError> {
    match tls {
        TlsMode::Implicit => {
            let connector = async_native_tls::TlsConnector::new();
            connector
                .connect(host, tcp)
                .await
                .map_err(|e| ConnectError::Tls(e.to_string()))
        }
        TlsMode::Starttls => Err(ConnectError::StarttlsUnsupported),
        TlsMode::None => Err(ConnectError::PlaintextRefused),
    }
}

#[cfg(feature = "test-plaintext")]
async fn build_stream(
    _host: &str,
    tcp: TcpStream,
    tls: TlsMode,
) -> Result<ImapStream, ConnectError> {
    // Test build: accept TlsMode::None on plain TCP. Other
    // modes still fail — tests shouldn't accidentally hit a
    // real TLS endpoint with this feature on.
    match tls {
        TlsMode::None => Ok(tcp),
        TlsMode::Implicit => Err(ConnectError::Tls(
            "test-plaintext build: implicit TLS not available".into(),
        )),
        TlsMode::Starttls => Err(ConnectError::StarttlsUnsupported),
    }
}
