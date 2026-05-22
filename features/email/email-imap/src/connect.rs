//! Connection bootstrap. TLS-on-connect, STARTTLS, and plain
//! TCP variants are kept behind one entry point so the rest of
//! the backend doesn't branch on `TlsMode`.

use async_imap::{Client, Session};
use async_native_tls::TlsStream;
use email_config::TlsMode;
use email_secret::SecretValue;
use thiserror::Error;
use tokio::net::TcpStream;

/// Live, authenticated IMAP session. We hold the TLS-on-tcp
/// variant; STARTTLS upgrades the same socket in place via
/// `async-imap`'s session helpers.
pub type ImapSession = Session<TlsStream<TcpStream>>;

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

    let tls_stream = match tls {
        TlsMode::Implicit => {
            let connector = async_native_tls::TlsConnector::new();
            connector
                .connect(host, tcp)
                .await
                .map_err(|e| ConnectError::Tls(e.to_string()))?
        }
        TlsMode::Starttls => {
            // STARTTLS needs a different stream-type plumbing
            // through async-imap. Out of scope for the first
            // cut — implicit-TLS port 993 works for every
            // major provider (Gmail / Fastmail / Outlook /
            // self-hosted). STARTTLS lands in the next pass.
            return Err(ConnectError::StarttlsUnsupported);
        }
        TlsMode::None => return Err(ConnectError::PlaintextRefused),
    };

    let client = Client::new(tls_stream);
    let session = client
        .login(username, password.as_str())
        .await
        .map_err(|(e, _)| ConnectError::Login(e.to_string()))?;
    Ok(session)
}
