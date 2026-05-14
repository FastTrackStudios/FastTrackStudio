//! Phase 3 — capability tokens. Phase 4 extends the canonical
//! message with optional `token_id` + `peer_id` fields used by
//! the share-link service.
//!
//! A `CapabilityToken` is `base64url(signature || canonical_message)`,
//! where `canonical_message` is a deterministic UTF-8 form:
//!
//! ```text
//! v1|<expires_unix>|<RW|RO>|<doc_id_1>,<doc_id_2>,...|<token_id?>|<peer_id?>
//! ```
//!
//! - Trailing `token_id` + `peer_id` may be empty. A token with no
//!   `token_id` is never revocable through `ShareService::revoke`.
//! - `peer_id` is currently a logged-only attribution stub. Phase 8
//!   wires it into the anonymous-claim flow.
//!
//! Ed25519 signs the message; the server's keypair is generated on
//! first boot at `$XDG_DATA_HOME/task-server/server-key.ed25519` (raw
//! 32-byte secret seed). Verification re-derives the public key from
//! the seed and checks the signature.
//!
//! The wire format is intentionally hand-rolled rather than going
//! through `facet` so the canonical bytes are unambiguous: a single
//! tweak to the serializer would silently invalidate every previously
//! issued token.

use std::path::{Path, PathBuf};

use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};
use ed25519_dalek::{
    SECRET_KEY_LENGTH, SIGNATURE_LENGTH, Signature, Signer, SigningKey, Verifier, VerifyingKey,
};
use project_proto::DocId;
use uuid::Uuid;

/// What a token grants.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CapabilityScope {
    /// Unix seconds. Tokens past this point fail verification.
    pub expires_unix: i64,
    /// Whether the holder can call `apply_update`. `false` = read-only
    /// (`subscribe` only).
    pub can_write: bool,
    /// Doc ids the token grants access to. Empty = no access.
    pub doc_ids: Vec<DocId>,
    /// Token id — set by the share-link service for revocable tokens.
    /// `None` = the token is not registered for revocation
    /// (e.g. server-internal capability).
    pub token_id: Option<Uuid>,
    /// Anonymous peer attribution. Phase 4 logs this; Phase 8 uses
    /// it for the anonymous-claim flow.
    pub peer_id: Option<String>,
    /// Phase 7 — when `true`, the connection is allowed to obtain
    /// + redeem attachment URLs for `doc_ids` but is NOT allowed
    /// to `subscribe` or `apply_update` on those docs. Default
    /// `false` (sync allowed).
    pub attachments_only: bool,
}

impl CapabilityScope {
    pub fn allows_doc(&self, doc_id: &DocId) -> bool {
        self.doc_ids.iter().any(|d| d == doc_id)
    }

    /// True if `now_unix` is past expiry.
    pub fn is_expired(&self, now_unix: i64) -> bool {
        now_unix >= self.expires_unix
    }

    fn canonical_message(&self) -> String {
        let mut joined = String::new();
        for (i, d) in self.doc_ids.iter().enumerate() {
            if i > 0 {
                joined.push(',');
            }
            joined.push_str(d.as_str());
        }
        let token_id_str = self.token_id.map(|u| u.to_string()).unwrap_or_default();
        let peer_id_str = self.peer_id.clone().unwrap_or_default();
        let attachments_only_str = if self.attachments_only { "1" } else { "0" };
        format!(
            "v1|{}|{}|{}|{}|{}|{}",
            self.expires_unix,
            if self.can_write { "RW" } else { "RO" },
            joined,
            token_id_str,
            peer_id_str,
            attachments_only_str,
        )
    }

    fn parse_message(s: &str) -> Result<Self, CapabilityError> {
        let mut parts = s.splitn(7, '|');
        let v = parts.next().ok_or(CapabilityError::Malformed)?;
        if v != "v1" {
            return Err(CapabilityError::UnsupportedVersion);
        }
        let expires_unix: i64 = parts
            .next()
            .ok_or(CapabilityError::Malformed)?
            .parse()
            .map_err(|_| CapabilityError::Malformed)?;
        let mode = parts.next().ok_or(CapabilityError::Malformed)?;
        let can_write = match mode {
            "RW" => true,
            "RO" => false,
            _ => return Err(CapabilityError::Malformed),
        };
        let docs = parts.next().ok_or(CapabilityError::Malformed)?;
        let doc_ids: Vec<DocId> = if docs.is_empty() {
            Vec::new()
        } else {
            docs.split(',').map(|s| DocId::new(s.to_string())).collect()
        };
        // Trailing fields are all optional — pre-Phase-7 tokens
        // have 5 parts, Phase 4 has 7, Phase 7 has 8.
        let token_id_part = parts.next().unwrap_or("");
        let token_id = if token_id_part.is_empty() {
            None
        } else {
            Some(Uuid::parse_str(token_id_part).map_err(|_| CapabilityError::Malformed)?)
        };
        let peer_id_part = parts.next().unwrap_or("");
        let peer_id = if peer_id_part.is_empty() {
            None
        } else {
            Some(peer_id_part.to_string())
        };
        let attachments_only_part = parts.next().unwrap_or("0");
        let attachments_only = matches!(attachments_only_part, "1");
        Ok(Self {
            expires_unix,
            can_write,
            doc_ids,
            token_id,
            peer_id,
            attachments_only,
        })
    }
}

/// Errors from token parse / verify.
#[derive(Debug, thiserror::Error)]
pub enum CapabilityError {
    #[error("token base64 invalid")]
    BadBase64,
    #[error("token signature invalid")]
    BadSignature,
    #[error("token expired")]
    Expired,
    #[error("token malformed")]
    Malformed,
    #[error("unsupported token version")]
    UnsupportedVersion,
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("keypair: {0}")]
    Keypair(String),
}

/// Server's signing keypair. Wraps `SigningKey`; serialized on disk
/// as the raw 32-byte seed (no envelope — file mode 0600 is the
/// trust boundary).
#[derive(Clone)]
pub struct ServerKeypair {
    signing: SigningKey,
}

impl ServerKeypair {
    pub fn load_or_generate(path: &Path) -> Result<Self, CapabilityError> {
        if path.exists() {
            let bytes = std::fs::read(path)?;
            if bytes.len() != SECRET_KEY_LENGTH {
                return Err(CapabilityError::Keypair(format!(
                    "expected {SECRET_KEY_LENGTH} bytes, got {} at {}",
                    bytes.len(),
                    path.display()
                )));
            }
            let arr: [u8; SECRET_KEY_LENGTH] = bytes
                .try_into()
                .map_err(|_| CapabilityError::Keypair("len".into()))?;
            Ok(Self {
                signing: SigningKey::from_bytes(&arr),
            })
        } else {
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            let signing = SigningKey::generate(&mut rand::thread_rng());
            std::fs::write(path, signing.to_bytes())?;
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let perms = std::fs::Permissions::from_mode(0o600);
                let _ = std::fs::set_permissions(path, perms);
            }
            tracing::info!(
                path = %path.display(),
                public = %URL_SAFE_NO_PAD.encode(signing.verifying_key().to_bytes()),
                "generated new server keypair",
            );
            Ok(Self { signing })
        }
    }

    /// In-memory keypair, never written to disk. Tests only.
    pub fn generate_ephemeral() -> Self {
        Self {
            signing: SigningKey::generate(&mut rand::thread_rng()),
        }
    }

    pub fn verifying_key(&self) -> VerifyingKey {
        self.signing.verifying_key()
    }

    /// Crate-internal accessor for the signing key. Used by
    /// sibling modules (attachments/signed_url) that need to sign
    /// non-capability messages with the same identity.
    pub(crate) fn signing_key(&self) -> &SigningKey {
        &self.signing
    }

    pub fn issue(&self, scope: &CapabilityScope) -> String {
        let msg = scope.canonical_message();
        let sig: Signature = self.signing.sign(msg.as_bytes());
        let mut out = Vec::with_capacity(SIGNATURE_LENGTH + msg.len());
        out.extend_from_slice(&sig.to_bytes());
        out.extend_from_slice(msg.as_bytes());
        URL_SAFE_NO_PAD.encode(out)
    }

    pub fn verify(&self, token: &str, now_unix: i64) -> Result<CapabilityScope, CapabilityError> {
        let bytes = URL_SAFE_NO_PAD
            .decode(token)
            .map_err(|_| CapabilityError::BadBase64)?;
        if bytes.len() < SIGNATURE_LENGTH {
            return Err(CapabilityError::Malformed);
        }
        let (sig_bytes, msg_bytes) = bytes.split_at(SIGNATURE_LENGTH);
        let sig_arr: [u8; SIGNATURE_LENGTH] = sig_bytes
            .try_into()
            .map_err(|_| CapabilityError::Malformed)?;
        let sig = Signature::from_bytes(&sig_arr);
        self.verifying_key()
            .verify(msg_bytes, &sig)
            .map_err(|_| CapabilityError::BadSignature)?;
        let msg = std::str::from_utf8(msg_bytes).map_err(|_| CapabilityError::Malformed)?;
        let scope = CapabilityScope::parse_message(msg)?;
        if scope.is_expired(now_unix) {
            return Err(CapabilityError::Expired);
        }
        Ok(scope)
    }
}

/// Resolve `$XDG_DATA_HOME/task-server/server-key.ed25519`.
pub fn default_keypair_path() -> eyre::Result<PathBuf> {
    let base = match std::env::var("XDG_DATA_HOME") {
        Ok(v) if !v.is_empty() => PathBuf::from(v),
        _ => {
            let home = std::env::var("HOME")
                .map_err(|_| eyre::eyre!("neither XDG_DATA_HOME nor HOME is set"))?;
            PathBuf::from(home).join(".local").join("share")
        }
    };
    Ok(base.join("task-server").join("server-key.ed25519"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    fn future() -> i64 {
        i64::MAX / 2
    }

    #[test]
    fn issue_then_verify_round_trips() {
        let kp = ServerKeypair::generate_ephemeral();
        let scope = CapabilityScope {
            expires_unix: future(),
            can_write: true,
            doc_ids: vec![
                DocId::project(Uuid::nil()),
                DocId::comms_thread(Uuid::from_u128(1)),
            ],
            token_id: Some(Uuid::from_u128(42)),
            peer_id: Some("share-link-42".into()),
            attachments_only: false,
        };
        let token = kp.issue(&scope);
        let got = kp.verify(&token, 0).expect("verify");
        assert_eq!(got, scope);
    }

    #[test]
    fn tampered_signature_fails() {
        let kp = ServerKeypair::generate_ephemeral();
        let scope = CapabilityScope {
            expires_unix: future(),
            can_write: false,
            doc_ids: vec![DocId::new("project/x")],
            ..Default::default()
        };
        let token = kp.issue(&scope);
        let mut bytes = URL_SAFE_NO_PAD.decode(&token).unwrap();
        bytes[0] ^= 0x01;
        let bad = URL_SAFE_NO_PAD.encode(bytes);
        match kp.verify(&bad, 0) {
            Err(CapabilityError::BadSignature) => {}
            other => panic!("expected BadSignature, got {other:?}"),
        }
    }

    #[test]
    fn expired_token_rejected() {
        let kp = ServerKeypair::generate_ephemeral();
        let scope = CapabilityScope {
            expires_unix: 100,
            can_write: true,
            doc_ids: vec![DocId::new("x")],
            ..Default::default()
        };
        let token = kp.issue(&scope);
        match kp.verify(&token, 200) {
            Err(CapabilityError::Expired) => {}
            other => panic!("expected Expired, got {other:?}"),
        }
    }

    #[test]
    fn wrong_keypair_fails() {
        let kp_a = ServerKeypair::generate_ephemeral();
        let kp_b = ServerKeypair::generate_ephemeral();
        let scope = CapabilityScope {
            expires_unix: future(),
            can_write: false,
            doc_ids: vec![],
            ..Default::default()
        };
        let token = kp_a.issue(&scope);
        assert!(matches!(
            kp_b.verify(&token, 0),
            Err(CapabilityError::BadSignature)
        ));
    }

    #[test]
    fn allows_doc_matches_by_exact_string() {
        let scope = CapabilityScope {
            expires_unix: future(),
            can_write: true,
            doc_ids: vec![DocId::new("project/a"), DocId::new("project/b")],
            ..Default::default()
        };
        assert!(scope.allows_doc(&DocId::new("project/a")));
        assert!(scope.allows_doc(&DocId::new("project/b")));
        assert!(!scope.allows_doc(&DocId::new("project/c")));
        assert!(!scope.allows_doc(&DocId::new("workspace")));
    }
}
