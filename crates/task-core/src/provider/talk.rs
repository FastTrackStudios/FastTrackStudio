//! Nextcloud Talk client — rooms + chat messages via OCS spreed API.
//!
//! Covers the minimum surface Hermes needs to participate in Talk rooms:
//! list rooms, post messages, read recent history.
//!
//! # Scope
//!
//! Long-poll chat receive (push-driven bot loop) and rich-object embedding
//! ("task card" in chat) are deliberately out of scope for this first cut —
//! they require a long-lived daemon and a specific Spreed rich-object shape.
//!
//! # API surface
//!
//! Nextcloud Talk's OCS API lives at:
//!
//! - `GET /ocs/v2.php/apps/spreed/api/v4/room` — list rooms the user is in
//! - `POST /ocs/v2.php/apps/spreed/api/v1/chat/{token}` — post a message
//! - `GET /ocs/v2.php/apps/spreed/api/v1/chat/{token}?lookIntoFuture=0` —
//!   recent history (without long-poll)
//!
//! All responses are OCS-wrapped JSON:
//! `{"ocs":{"meta":{...},"data":...}}`.

use crate::service::VaultError;

/// Connection settings for a Nextcloud Talk instance.
#[derive(Debug, Clone)]
pub struct TalkConfig {
    /// Base URL of the Nextcloud instance, e.g. `https://cloud.example.com`.
    pub url: String,
    pub username: String,
    /// App password (preferred) or account password.
    pub password: String,
}

impl TalkConfig {
    fn ocs_base(&self) -> String {
        format!("{}/ocs/v2.php/apps/spreed", self.url.trim_end_matches('/'))
    }
}

/// A Talk conversation (room).
#[derive(Debug, Clone)]
pub struct Room {
    /// Stable token — use for all subsequent calls. NOT the numeric id.
    pub token: String,
    /// Display name.
    pub name: String,
    /// Room type: 1=one-to-one, 2=group, 3=public, 4=changelog, 5=one-to-one-former,
    /// 6=note-to-self (Spreed's `conversationType`). Kept as raw int so we
    /// don't force callers to match exhaustively.
    pub room_type: u32,
    pub participant_count: u32,
    /// Unix timestamp of the most recent activity.
    pub last_activity: i64,
    /// Last message body preview, if the API returned one.
    pub last_message: Option<String>,
}

/// A chat message inside a room.
#[derive(Debug, Clone)]
pub struct Message {
    pub id: u64,
    pub token: String,
    pub actor_id: String,
    /// "users" for normal accounts, "bots" for bot actors, "guests" for guests.
    pub actor_type: String,
    pub actor_display_name: String,
    pub timestamp: i64,
    pub message: String,
    /// Parent message id if this is a reply.
    pub reply_to: Option<u64>,
}

/// Talk OCS client.
pub struct TalkClient {
    config: TalkConfig,
    http: reqwest::Client,
}

impl TalkClient {
    pub fn new(config: TalkConfig) -> Self {
        Self {
            config,
            http: reqwest::Client::new(),
        }
    }

    /// List rooms the authenticated user is a member of.
    pub async fn list_rooms(&self) -> Result<Vec<Room>, VaultError> {
        let url = format!("{}/api/v4/room", self.config.ocs_base());
        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .header("Accept", "application/json")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("talk list_rooms: {e}")))?;

        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "talk list_rooms {status}: {body}"
            )));
        }

        let v: serde_json::Value = serde_json::from_str(&body)
            .map_err(|e| VaultError::ParseError(format!("talk JSON: {e}")))?;
        let data = v
            .get("ocs")
            .and_then(|o| o.get("data"))
            .and_then(|d| d.as_array())
            .ok_or_else(|| VaultError::ParseError("talk rooms: no ocs.data[]".into()))?;

        Ok(data.iter().filter_map(room_from_json).collect())
    }

    /// Post a message in `token`. `reply_to` nests the message under a
    /// parent id when provided.
    pub async fn send_message(
        &self,
        token: &str,
        message: &str,
        reply_to: Option<u64>,
    ) -> Result<u64, VaultError> {
        let url = format!(
            "{}/api/v1/chat/{}",
            self.config.ocs_base(),
            urlencode(token)
        );

        let mut form = vec![("message", message.to_string())];
        if let Some(id) = reply_to {
            form.push(("replyTo", id.to_string()));
        }

        let resp = self
            .http
            .post(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .header("Accept", "application/json")
            .form(&form)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("talk send: {e}")))?;

        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "talk send {status}: {body}"
            )));
        }

        let v: serde_json::Value = serde_json::from_str(&body)
            .map_err(|e| VaultError::ParseError(format!("talk send JSON: {e}")))?;
        let id = v
            .get("ocs")
            .and_then(|o| o.get("data"))
            .and_then(|d| d.get("id"))
            .and_then(|n| n.as_u64())
            .ok_or_else(|| VaultError::ParseError("talk send: no ocs.data.id".into()))?;
        Ok(id)
    }

    /// Fetch recent messages in a room. Uses `lookIntoFuture=0` so it returns
    /// immediately instead of long-polling.
    pub async fn recent_messages(
        &self,
        token: &str,
        limit: u32,
    ) -> Result<Vec<Message>, VaultError> {
        let url = format!(
            "{}/api/v1/chat/{}?lookIntoFuture=0&limit={}",
            self.config.ocs_base(),
            urlencode(token),
            limit
        );
        let resp = self
            .http
            .get(&url)
            .basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .header("Accept", "application/json")
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("talk history: {e}")))?;

        let status = resp.status();
        let body = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "talk history {status}: {body}"
            )));
        }

        let v: serde_json::Value = serde_json::from_str(&body)
            .map_err(|e| VaultError::ParseError(format!("talk history JSON: {e}")))?;
        let data = v
            .get("ocs")
            .and_then(|o| o.get("data"))
            .and_then(|d| d.as_array())
            .ok_or_else(|| VaultError::ParseError("talk history: no ocs.data[]".into()))?;

        Ok(data
            .iter()
            .filter_map(|m| message_from_json(m, token))
            .collect())
    }
}

fn room_from_json(v: &serde_json::Value) -> Option<Room> {
    Some(Room {
        token: v.get("token")?.as_str()?.to_string(),
        name: v
            .get("displayName")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        room_type: v.get("type").and_then(|n| n.as_u64()).unwrap_or(0) as u32,
        participant_count: v
            .get("participantCount")
            .and_then(|n| n.as_u64())
            .unwrap_or(0) as u32,
        last_activity: v.get("lastActivity").and_then(|n| n.as_i64()).unwrap_or(0),
        last_message: v
            .get("lastMessage")
            .and_then(|m| m.get("message"))
            .and_then(|s| s.as_str())
            .map(|s| s.to_string()),
    })
}

fn message_from_json(v: &serde_json::Value, token: &str) -> Option<Message> {
    Some(Message {
        id: v.get("id")?.as_u64()?,
        token: token.to_string(),
        actor_id: v.get("actorId")?.as_str()?.to_string(),
        actor_type: v
            .get("actorType")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        actor_display_name: v
            .get("actorDisplayName")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        timestamp: v.get("timestamp").and_then(|n| n.as_i64()).unwrap_or(0),
        message: v
            .get("message")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        reply_to: v
            .get("parent")
            .and_then(|p| p.get("id"))
            .and_then(|n| n.as_u64()),
    })
}

fn urlencode(s: &str) -> String {
    // Minimal percent-encoding for Talk tokens (they're path-safe already in practice).
    s.bytes()
        .map(|b| match b {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                (b as char).to_string()
            }
            _ => format!("%{b:02X}"),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_room_json() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{
                "token": "abc123",
                "displayName": "Mixing session",
                "type": 2,
                "participantCount": 4,
                "lastActivity": 1744903200,
                "lastMessage": {"message": "New mix uploaded"}
            }"#,
        )
        .unwrap();
        let r = room_from_json(&v).unwrap();
        assert_eq!(r.token, "abc123");
        assert_eq!(r.name, "Mixing session");
        assert_eq!(r.room_type, 2);
        assert_eq!(r.participant_count, 4);
        assert_eq!(r.last_activity, 1744903200);
        assert_eq!(r.last_message.as_deref(), Some("New mix uploaded"));
    }

    #[test]
    fn parses_message_json_with_reply() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{
                "id": 42,
                "actorId": "hermes",
                "actorType": "bots",
                "actorDisplayName": "Hermes Bot",
                "timestamp": 1744903300,
                "message": "Running latency check now",
                "parent": {"id": 40}
            }"#,
        )
        .unwrap();
        let m = message_from_json(&v, "abc123").unwrap();
        assert_eq!(m.id, 42);
        assert_eq!(m.actor_id, "hermes");
        assert_eq!(m.actor_type, "bots");
        assert_eq!(m.reply_to, Some(40));
        assert_eq!(m.token, "abc123");
    }

    #[test]
    fn message_json_without_reply() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{"id":7,"actorId":"cody","timestamp":0,"message":"hi"}"#,
        )
        .unwrap();
        let m = message_from_json(&v, "t").unwrap();
        assert!(m.reply_to.is_none());
    }
}
