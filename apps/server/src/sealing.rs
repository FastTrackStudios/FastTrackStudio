//! AES-256-GCM "sealing" for at-rest webhook secrets.
//!
//! `GitRepoConnection.webhook_secret_hash` (per `agent_proto`) stores
//! the raw webhook secret encrypted with a server-only key — the
//! field is named "hash" historically, but it's actually
//! reversible-by-the-server ciphertext so inbound HMAC verification
//! can recover the plaintext secret on every request.
//!
//! Storage format (single-line string, prefix-versioned for future
//! algorithm bumps without DB migration):
//!
//! ```text
//! v1:<nonce_hex>:<ciphertext_hex>
//! ```
//!
//! Bootstrap: the cipher key is read from `TASK_SERVER_SEALING_KEY`
//! (32 bytes encoded as 64 hex chars). When that env var is unset
//! we derive a stable dev key from `sha256("task-sealing-dev::" +
//! hostname)` so dev restarts on the same machine don't lose
//! existing sealed values. **Never** use the dev derivation in
//! production — set `TASK_SERVER_SEALING_KEY` to a real 32-byte
//! secret.

use aes_gcm::{
    Aes256Gcm, KeyInit, Nonce,
    aead::{Aead, generic_array::GenericArray},
};
use rand::RngCore;
use sha2::{Digest, Sha256};

#[derive(Clone)]
pub struct Sealing {
    cipher: Aes256Gcm,
}

#[derive(thiserror::Error, Debug)]
pub enum SealingError {
    #[error("invalid format")]
    Format,
    #[error("decryption failed")]
    Decrypt,
    #[error("not enabled — no key configured")]
    NotConfigured,
}

impl Sealing {
    /// Load from `TASK_SERVER_SEALING_KEY` env var (32 raw bytes
    /// encoded as 64 hex chars). If unset, derive a stable dev key
    /// from `sha256("task-sealing-dev::" + hostname)` — surviving
    /// process restarts on the same dev box but obviously NOT a
    /// production secret.
    pub fn from_env() -> Self {
        let key_bytes: [u8; 32] = match std::env::var("TASK_SERVER_SEALING_KEY") {
            Ok(hex_str) => {
                let mut out = [0u8; 32];
                match hex::decode(hex_str.trim()) {
                    Ok(decoded) if decoded.len() == 32 => {
                        out.copy_from_slice(&decoded);
                        out
                    }
                    _ => {
                        tracing::warn!(
                            "TASK_SERVER_SEALING_KEY set but not valid 64-char hex; \
                             falling back to dev-derived key"
                        );
                        derive_dev_key()
                    }
                }
            }
            Err(_) => {
                tracing::warn!(
                    "TASK_SERVER_SEALING_KEY unset — using machine-stable dev key \
                     (NOT for production). Set the env var to 64 hex chars in prod."
                );
                derive_dev_key()
            }
        };
        let key = GenericArray::from_slice(&key_bytes);
        Self {
            cipher: Aes256Gcm::new(key),
        }
    }

    /// Encrypt `plaintext` → `"v1:<nonce_hex>:<ciphertext_hex>"`.
    pub fn seal(&self, plaintext: &[u8]) -> String {
        let mut nonce_bytes = [0u8; 12];
        rand::thread_rng().fill_bytes(&mut nonce_bytes);
        let nonce = Nonce::from_slice(&nonce_bytes);
        let ct = self
            .cipher
            .encrypt(nonce, plaintext)
            .expect("aes-gcm encrypt cannot fail for valid key");
        format!("v1:{}:{}", hex::encode(nonce_bytes), hex::encode(&ct))
    }

    /// Decrypt a `"v1:..."` sealed string.
    pub fn unseal(&self, sealed: &str) -> Result<Vec<u8>, SealingError> {
        let mut parts = sealed.splitn(3, ':');
        let version = parts.next().ok_or(SealingError::Format)?;
        if version != "v1" {
            return Err(SealingError::Format);
        }
        let nonce_hex = parts.next().ok_or(SealingError::Format)?;
        let ct_hex = parts.next().ok_or(SealingError::Format)?;
        let nonce_bytes = hex::decode(nonce_hex).map_err(|_| SealingError::Format)?;
        if nonce_bytes.len() != 12 {
            return Err(SealingError::Format);
        }
        let ct = hex::decode(ct_hex).map_err(|_| SealingError::Format)?;
        let nonce = Nonce::from_slice(&nonce_bytes);
        self.cipher
            .decrypt(nonce, ct.as_ref())
            .map_err(|_| SealingError::Decrypt)
    }
}

fn derive_dev_key() -> [u8; 32] {
    let host = hostname_string();
    let mut h = Sha256::new();
    h.update(b"task-sealing-dev::");
    h.update(host.as_bytes());
    let digest = h.finalize();
    let mut out = [0u8; 32];
    out.copy_from_slice(&digest);
    out
}

fn hostname_string() -> String {
    // Avoid pulling the `hostname` crate — uname / env fallback is
    // good enough for dev-only key derivation.
    std::env::var("HOSTNAME")
        .or_else(|_| std::env::var("COMPUTERNAME"))
        .unwrap_or_else(|_| "localhost".to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        // Force a deterministic key for the test so order-of-tests
        // doesn't bleed env-var state.
        let key = [42u8; 32];
        let cipher = Aes256Gcm::new(GenericArray::from_slice(&key));
        let sealing = Sealing { cipher };

        let plaintext = b"super-secret-webhook-key";
        let sealed = sealing.seal(plaintext);
        assert!(sealed.starts_with("v1:"));
        let recovered = sealing.unseal(&sealed).unwrap();
        assert_eq!(recovered, plaintext);
    }

    #[test]
    fn bad_format_rejected() {
        let key = [7u8; 32];
        let cipher = Aes256Gcm::new(GenericArray::from_slice(&key));
        let sealing = Sealing { cipher };
        assert!(matches!(
            sealing.unseal("v2:abcd:ef01").unwrap_err(),
            SealingError::Format
        ));
        assert!(matches!(
            sealing.unseal("not-a-sealed-value").unwrap_err(),
            SealingError::Format
        ));
    }
}
