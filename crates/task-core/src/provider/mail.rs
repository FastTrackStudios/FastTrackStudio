//! Nextcloud Mail client.
//!
//! Reference-only read access for bot-driven email linking. We don't
//! download bodies or attachments to disk — the bot fetches what it
//! needs live and stores a pointer (`EmailRef`) on the task/project.
//!
//! # API shape
//!
//! Nextcloud Mail has two parallel API surfaces:
//!
//! - **OCS** at `/ocs/v2.php/apps/mail/api/*` — typed, third-party-ready.
//!   Covers accounts, mailboxes, and message lists.
//! - **App routes** at `/index.php/apps/mail/api/messages/{id}*` — covers
//!   single-message body, thread, source. Used by the Vue frontend.
//!   Stable in practice but not a public contract.
//!
//! We use both: OCS where it's available, app routes for get-message and
//! body (no OCS endpoint yet).
//!
//! # Auth
//!
//! Basic auth with a Nextcloud app password — same as Talk and the rest
//! of our NC integration. Add `OCS-APIRequest: true` on OCS routes.

use crate::service::VaultError;

/// Connection config for Nextcloud Mail.
#[derive(Debug, Clone)]
pub struct MailConfig {
    /// Base URL (e.g. `https://cloud.starcommand.live`).
    pub url: String,
    pub username: String,
    /// App password — not the account password.
    pub password: String,
}

impl MailConfig {
    /// Base for the Nextcloud Mail app routes. As of Mail 5.x there's no
    /// third-party-stable OCS surface for most operations — the app
    /// routes under `/index.php/apps/mail/api/*` are the live endpoint.
    /// We rely on `OCS-APIRequest: true` to bypass CSRF.
    fn app_base(&self) -> String {
        format!(
            "{}/index.php/apps/mail/api",
            self.url.trim_end_matches('/')
        )
    }
}

/// A configured Mail account.
#[derive(Debug, Clone)]
pub struct MailAccount {
    pub id: i64,
    pub email: String,
    pub name: Option<String>,
}

/// A mailbox (folder) within an account.
#[derive(Debug, Clone)]
pub struct Mailbox {
    /// Stable id used in list-messages calls.
    pub id: i64,
    /// Folder name, e.g. "INBOX", "Sent", "Archive".
    pub name: String,
    pub account_id: i64,
    /// Unread count, when reported.
    pub unread: Option<u32>,
}

/// A message returned from the list endpoint. Lightweight — no body.
#[derive(Debug, Clone)]
pub struct MailMessage {
    /// Nextcloud Mail DB id. Stable until the message moves folders.
    pub id: i64,
    /// RFC 2822 Message-Id. The durable identifier — use this on the
    /// task/project side.
    pub message_id: Option<String>,
    pub subject: String,
    pub from: String,
    pub to: Vec<String>,
    /// Unix timestamp of the sent date.
    pub date: i64,
    /// Short body preview.
    pub preview: Option<String>,
    pub mailbox_id: i64,
    pub account_id: Option<i64>,
    pub imap_uid: Option<u32>,
    pub has_attachments: bool,
    pub attachment_count: u32,
    pub flags: Vec<String>,
}

/// Full message fetched via the app route, including a body and
/// attachment metadata.
#[derive(Debug, Clone, Default)]
pub struct MailMessageDetail {
    pub id: i64,
    pub message_id: Option<String>,
    pub subject: String,
    pub from: String,
    pub to: Vec<String>,
    pub cc: Vec<String>,
    pub date: i64,
    pub body_plain: Option<String>,
    pub body_html: Option<String>,
    pub attachments: Vec<MailAttachment>,
    pub in_reply_to: Option<String>,
    pub references: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct MailAttachment {
    pub id: i64,
    pub file_name: String,
    pub mime: String,
    pub size: u64,
}

/// NC Mail tag (IMAP keyword). NC-local — not propagated to Proton.
#[derive(Debug, Clone)]
pub struct MailTag {
    pub id: i64,
    pub display_name: String,
    /// The IMAP keyword string used in `setTag` / `removeTag` calls.
    pub imap_label: String,
    /// 7-char hex colour code.
    pub color: Option<String>,
}

/// Nextcloud Mail HTTP client.
pub struct MailClient {
    config: MailConfig,
    http: reqwest::Client,
}

impl MailClient {
    pub fn new(config: MailConfig) -> Self {
        Self {
            config,
            http: reqwest::Client::new(),
        }
    }

    fn auth(&self, req: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        req.basic_auth(&self.config.username, Some(&self.config.password))
            .header("OCS-APIRequest", "true")
            .header("Accept", "application/json")
            .header("User-Agent", "task-cli")
    }

    /// List mail accounts the authenticated user has configured.
    pub async fn list_accounts(&self) -> Result<Vec<MailAccount>, VaultError> {
        let url = format!("{}/accounts", self.config.app_base());
        let data = self.app_get_json(&url).await?;
        let arr = data
            .as_array()
            .ok_or_else(|| VaultError::ParseError("mail accounts: not an array".into()))?;
        Ok(arr.iter().filter_map(parse_account).collect())
    }

    /// List mailboxes in an account.
    pub async fn list_mailboxes(&self, account_id: i64) -> Result<Vec<Mailbox>, VaultError> {
        let url = format!(
            "{}/mailboxes?accountId={account_id}",
            self.config.app_base()
        );
        let data = self.app_get_json(&url).await?;
        let arr = data
            .as_array()
            .or_else(|| data.get("mailboxes").and_then(|v| v.as_array()))
            .ok_or_else(|| VaultError::ParseError("mailboxes: no array".into()))?;
        Ok(arr.iter().filter_map(parse_mailbox).collect())
    }

    /// List messages in a mailbox. `filter` is NC Mail's free-text filter
    /// — supports `from:`, `to:`, `subject:`, `cc:`, `bcc:` prefixed
    /// tokens plus plain text. `limit` is clamped to 100 server-side.
    pub async fn list_messages(
        &self,
        mailbox_id: i64,
        filter: Option<&str>,
        limit: u32,
        cursor: Option<&str>,
    ) -> Result<Vec<MailMessage>, VaultError> {
        let mut url = format!(
            "{}/messages?mailboxId={mailbox_id}&limit={}",
            self.config.app_base(),
            limit.min(100)
        );
        if let Some(f) = filter {
            url.push_str(&format!("&filter={}", urlencode(f)));
        }
        if let Some(c) = cursor {
            url.push_str(&format!("&cursor={}", urlencode(c)));
        }
        let data = self.app_get_json(&url).await?;
        let arr = data
            .as_array()
            .ok_or_else(|| VaultError::ParseError("messages: not an array".into()))?;
        Ok(arr
            .iter()
            .filter_map(|m| parse_message(m, mailbox_id))
            .collect())
    }

    /// Fetch a single message with body + attachment metadata. Uses the
    /// app route (no OCS equivalent yet).
    pub async fn get_message(&self, id: i64) -> Result<MailMessageDetail, VaultError> {
        let url = format!("{}/messages/{id}", self.config.app_base());
        let resp = self
            .auth(self.http.get(&url))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("mail get_message: {e}")))?;
        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "mail get_message {status}: {text}"
            )));
        }
        let v: serde_json::Value = serde_json::from_str(&text)
            .map_err(|e| VaultError::ParseError(format!("mail get_message JSON: {e}")))?;
        parse_message_detail(&v).ok_or_else(|| {
            VaultError::ParseError("mail get_message: missing required fields".into())
        })
    }

    /// Fetch the message body (plain text preferred). Internally hits
    /// `/messages/{id}/body`.
    pub async fn get_body(&self, id: i64) -> Result<String, VaultError> {
        let url = format!("{}/messages/{id}/body", self.config.app_base());
        let resp = self
            .auth(self.http.get(&url))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("mail get_body: {e}")))?;
        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "mail get_body {status}: {text}"
            )));
        }
        Ok(text)
    }

    /// GET an app route and return the parsed JSON body. App routes return
    /// naked JSON (no `ocs.data` wrapper).
    async fn app_get_json(&self, url: &str) -> Result<serde_json::Value, VaultError> {
        let resp = self
            .auth(self.http.get(url))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("mail GET {url}: {e}")))?;
        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "mail GET {url} {status}: {text}"
            )));
        }
        serde_json::from_str(&text)
            .map_err(|e| VaultError::ParseError(format!("mail JSON: {e}")))
    }

    async fn app_send_json(
        &self,
        method: reqwest::Method,
        url: &str,
        body: Option<&serde_json::Value>,
    ) -> Result<serde_json::Value, VaultError> {
        let mut req = self.auth(self.http.request(method.clone(), url));
        if let Some(b) = body {
            req = req.json(b);
        }
        let resp = req
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("mail {method} {url}: {e}")))?;
        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "mail {method} {url} {status}: {text}"
            )));
        }
        // Some endpoints return 200 with empty body (e.g. mailbox delete).
        if text.trim().is_empty() {
            return Ok(serde_json::Value::Null);
        }
        serde_json::from_str(&text)
            .map_err(|e| VaultError::ParseError(format!("mail JSON: {e}")))
    }

    /// Create a new mailbox (folder) on the account. To create a Proton
    /// label, pass `name = "Labels/<label>"`; Bridge translates that into
    /// a native Proton label.
    pub async fn create_mailbox(
        &self,
        account_id: i64,
        name: &str,
    ) -> Result<Mailbox, VaultError> {
        let url = format!("{}/mailboxes", self.config.app_base());
        let body = serde_json::json!({
            "accountId": account_id,
            "name": name,
        });
        let v = self
            .app_send_json(reqwest::Method::POST, &url, Some(&body))
            .await?;
        parse_mailbox(&v).ok_or_else(|| VaultError::ParseError("mailbox response".into()))
    }

    /// Delete a mailbox (folder). Deleting a mailbox under `Labels/`
    /// removes the corresponding Proton label.
    pub async fn delete_mailbox(&self, mailbox_id: i64) -> Result<(), VaultError> {
        let url = format!("{}/mailboxes/{mailbox_id}", self.config.app_base());
        self.app_send_json(reqwest::Method::DELETE, &url, None)
            .await?;
        Ok(())
    }

    /// Move a message to another mailbox. Note: this is **move**, not copy
    /// — the source mailbox loses the message. For Proton-style labels
    /// that keep a message in INBOX while also tagging it, use NC Mail's
    /// tag API (`set_tag`) or direct IMAP COPY instead.
    pub async fn move_message(
        &self,
        message_id: i64,
        dest_folder_id: i64,
    ) -> Result<(), VaultError> {
        let url = format!("{}/messages/{message_id}/move", self.config.app_base());
        let body = serde_json::json!({ "destFolderId": dest_folder_id });
        self.app_send_json(reqwest::Method::POST, &url, Some(&body))
            .await?;
        Ok(())
    }

    /// List NC Mail tags (IMAP keywords) available on this server. These
    /// are Nextcloud-local — they don't propagate to Proton's label list.
    ///
    /// NC Mail doesn't expose a `GET /api/tags` endpoint; the tag list
    /// instead ships base64-JSON-encoded inside the Mail app's page
    /// bootstrap (`<input id="initial-state-mail-tags" value="...">`).
    /// We fetch the page and pull the tags out of that input.
    pub async fn list_tags(&self) -> Result<Vec<MailTag>, VaultError> {
        use base64::prelude::*;
        let url = format!(
            "{}/index.php/apps/mail/",
            self.config.url.trim_end_matches('/')
        );
        let resp = self
            .auth(self.http.get(&url))
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("mail tags page {url}: {e}")))?;
        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "mail tags page {url}: {}",
                resp.status()
            )));
        }
        let html = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        let marker = r#"id="initial-state-mail-tags" value=""#;
        let start = html
            .find(marker)
            .ok_or_else(|| VaultError::ParseError("mail tags: initial-state not found".into()))?
            + marker.len();
        let rest = &html[start..];
        let end = rest
            .find('"')
            .ok_or_else(|| VaultError::ParseError("mail tags: closing quote not found".into()))?;
        let b64 = &rest[..end];

        let decoded = BASE64_STANDARD
            .decode(b64)
            .map_err(|e| VaultError::ParseError(format!("mail tags base64: {e}")))?;
        let v: serde_json::Value = serde_json::from_slice(&decoded)
            .map_err(|e| VaultError::ParseError(format!("mail tags JSON: {e}")))?;
        let arr = v
            .as_array()
            .ok_or_else(|| VaultError::ParseError("mail tags: not an array".into()))?;
        Ok(arr.iter().filter_map(parse_tag).collect())
    }

    /// Create an NC Mail tag. Color is a 7-char hex string (e.g. `#8b5cf6`).
    pub async fn create_tag(
        &self,
        display_name: &str,
        color: &str,
    ) -> Result<MailTag, VaultError> {
        let url = format!("{}/tags", self.config.app_base());
        let body = serde_json::json!({
            "displayName": display_name,
            "color": color,
        });
        let v = self
            .app_send_json(reqwest::Method::POST, &url, Some(&body))
            .await?;
        parse_tag(&v).ok_or_else(|| VaultError::ParseError("tag response".into()))
    }

    /// Delete a tag. NC Mail scopes deletion by account.
    pub async fn delete_tag(&self, account_id: i64, tag_id: i64) -> Result<(), VaultError> {
        let url = format!(
            "{}/tags/{account_id}/delete/{tag_id}",
            self.config.app_base()
        );
        self.app_send_json(reqwest::Method::DELETE, &url, None)
            .await?;
        Ok(())
    }

    /// Attach an existing NC Mail tag to a message.
    pub async fn set_tag(&self, message_id: i64, imap_label: &str) -> Result<(), VaultError> {
        let label = urlencode(imap_label);
        let url = format!(
            "{}/messages/{message_id}/tags/{label}",
            self.config.app_base()
        );
        self.app_send_json(reqwest::Method::PUT, &url, None).await?;
        Ok(())
    }

    /// Remove an NC Mail tag from a message.
    pub async fn remove_tag(&self, message_id: i64, imap_label: &str) -> Result<(), VaultError> {
        let label = urlencode(imap_label);
        let url = format!(
            "{}/messages/{message_id}/tags/{label}",
            self.config.app_base()
        );
        self.app_send_json(reqwest::Method::DELETE, &url, None)
            .await?;
        Ok(())
    }
}

// ── parsers ─────────────────────────────────────────────────────────────────

fn parse_account(v: &serde_json::Value) -> Option<MailAccount> {
    // The Nextcloud Mail HTTP API returns `emailAddress`; the fallback to
    // `email` keeps test fixtures and older deployments working.
    let email = v
        .get("emailAddress")
        .or_else(|| v.get("email"))
        .and_then(|s| s.as_str())
        .unwrap_or("")
        .to_string();
    Some(MailAccount {
        id: v.get("id")?.as_i64()?,
        email,
        name: v
            .get("name")
            .and_then(|s| s.as_str())
            .map(|s| s.to_string()),
    })
}

fn parse_tag(v: &serde_json::Value) -> Option<MailTag> {
    let id = v.get("id").and_then(|n| n.as_i64())?;
    let display_name = v
        .get("displayName")
        .and_then(|s| s.as_str())
        .unwrap_or("")
        .to_string();
    let imap_label = v
        .get("imapLabel")
        .and_then(|s| s.as_str())
        .unwrap_or("")
        .to_string();
    let color = v
        .get("color")
        .and_then(|s| s.as_str())
        .map(String::from);
    Some(MailTag {
        id,
        display_name,
        imap_label,
        color,
    })
}

fn parse_mailbox(v: &serde_json::Value) -> Option<Mailbox> {
    // NC Mail returns `databaseId` as the stable integer id used by other
    // endpoints; the `id` field is a base64 IMAP path string and not
    // directly useful for us. Keep `id` as a fallback for older fixtures.
    let id = v
        .get("databaseId")
        .and_then(|n| n.as_i64())
        .or_else(|| v.get("id").and_then(|n| n.as_i64()))?;
    Some(Mailbox {
        id,
        name: v
            .get("name")
            .or_else(|| v.get("displayName"))
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        account_id: v.get("accountId").and_then(|n| n.as_i64()).unwrap_or(0),
        unread: v
            .get("unread")
            .and_then(|n| n.as_u64())
            .map(|n| n as u32),
    })
}

fn parse_message(v: &serde_json::Value, mailbox_id: i64) -> Option<MailMessage> {
    // NC Mail returns the numeric row id as `databaseId`.
    let id = v
        .get("databaseId")
        .and_then(|n| n.as_i64())
        .or_else(|| v.get("id").and_then(|n| n.as_i64()))?;
    let subject = v
        .get("subject")
        .and_then(|s| s.as_str())
        .unwrap_or("")
        .to_string();
    let from = v
        .get("from")
        .and_then(|arr| arr.as_array())
        .and_then(|arr| arr.first())
        .and_then(|a| format_address(a))
        .or_else(|| v.get("from").and_then(|s| s.as_str()).map(String::from))
        .unwrap_or_default();
    let to = v
        .get("to")
        .and_then(|arr| arr.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(format_address)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();
    let date = v
        .get("sentAt")
        .and_then(|n| n.as_i64())
        .or_else(|| v.get("dateInt").and_then(|n| n.as_i64()))
        .unwrap_or(0);
    let message_id = v
        .get("messageId")
        .and_then(|s| s.as_str())
        .map(String::from);
    let preview = v
        .get("previewText")
        .and_then(|s| s.as_str())
        .map(String::from);
    let imap_uid = v
        .get("uid")
        .and_then(|n| n.as_u64())
        .map(|n| n as u32);
    let attachments = v
        .get("attachments")
        .and_then(|a| a.as_array())
        .map(|arr| arr.len())
        .unwrap_or(0);
    let flags = v
        .get("flags")
        .and_then(|o| o.as_object())
        .map(|m| {
            m.iter()
                .filter_map(|(k, v)| v.as_bool().filter(|&b| b).map(|_| k.clone()))
                .collect()
        })
        .unwrap_or_default();
    Some(MailMessage {
        id,
        message_id,
        subject,
        from,
        to,
        date,
        preview,
        mailbox_id,
        account_id: v.get("accountId").and_then(|n| n.as_i64()),
        imap_uid,
        has_attachments: attachments > 0,
        attachment_count: attachments as u32,
        flags,
    })
}

fn parse_message_detail(v: &serde_json::Value) -> Option<MailMessageDetail> {
    let id = v.get("id")?.as_i64()?;
    Some(MailMessageDetail {
        id,
        message_id: v
            .get("messageId")
            .and_then(|s| s.as_str())
            .map(String::from),
        subject: v
            .get("subject")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        from: v
            .get("from")
            .and_then(|arr| arr.as_array())
            .and_then(|arr| arr.first())
            .and_then(format_address)
            .unwrap_or_default(),
        to: v
            .get("to")
            .and_then(|arr| arr.as_array())
            .map(|arr| arr.iter().filter_map(format_address).collect())
            .unwrap_or_default(),
        cc: v
            .get("cc")
            .and_then(|arr| arr.as_array())
            .map(|arr| arr.iter().filter_map(format_address).collect())
            .unwrap_or_default(),
        date: v.get("sentAt").and_then(|n| n.as_i64()).unwrap_or(0),
        body_plain: v
            .get("body")
            .and_then(|s| s.as_str())
            .map(String::from),
        body_html: v
            .get("hasHtmlBody")
            .and_then(|b| b.as_bool())
            .filter(|&b| b)
            .and(None), // bodyHtml is fetched separately; left None here
        attachments: v
            .get("attachments")
            .and_then(|a| a.as_array())
            .map(|arr| arr.iter().filter_map(parse_attachment).collect())
            .unwrap_or_default(),
        in_reply_to: v
            .get("inReplyTo")
            .and_then(|s| s.as_str())
            .map(String::from),
        references: v
            .get("references")
            .and_then(|a| a.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|r| r.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default(),
    })
}

fn parse_attachment(v: &serde_json::Value) -> Option<MailAttachment> {
    Some(MailAttachment {
        id: v.get("id")?.as_i64()?,
        file_name: v
            .get("fileName")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        mime: v
            .get("mime")
            .and_then(|s| s.as_str())
            .unwrap_or("")
            .to_string(),
        size: v.get("size").and_then(|n| n.as_u64()).unwrap_or(0),
    })
}

/// Turn NC's `{ email, label }` shape into a `"Label <email>"` string,
/// or just the email when no label is set.
fn format_address(v: &serde_json::Value) -> Option<String> {
    let email = v.get("email").and_then(|s| s.as_str())?;
    let label = v
        .get("label")
        .and_then(|s| s.as_str())
        .filter(|s| !s.is_empty() && *s != email);
    Some(match label {
        Some(l) => format!("{l} <{email}>"),
        None => email.to_string(),
    })
}

fn urlencode(s: &str) -> String {
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
    fn parses_account() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{"id":1,"email":"cody@example.com","name":"Cody"}"#,
        )
        .unwrap();
        let a = parse_account(&v).unwrap();
        assert_eq!(a.id, 1);
        assert_eq!(a.email, "cody@example.com");
        assert_eq!(a.name.as_deref(), Some("Cody"));
    }

    #[test]
    fn parses_mailbox() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{"id":42,"name":"INBOX","accountId":1,"unread":5}"#,
        )
        .unwrap();
        let m = parse_mailbox(&v).unwrap();
        assert_eq!(m.id, 42);
        assert_eq!(m.name, "INBOX");
        assert_eq!(m.account_id, 1);
        assert_eq!(m.unread, Some(5));
    }

    #[test]
    fn parses_message_with_attachments() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{
                "id": 100,
                "messageId": "<abc@example.com>",
                "subject": "Re: mix",
                "from": [{"email":"client@acme.com","label":"Acme"}],
                "to": [{"email":"cody@ftstudios.com"}],
                "sentAt": 1744903200,
                "uid": 77,
                "previewText": "Hey, listened to the mix",
                "accountId": 1,
                "attachments": [{"id":1,"fileName":"notes.txt","mime":"text/plain","size":42}],
                "flags": {"seen":true,"flagged":false}
            }"#,
        )
        .unwrap();
        let m = parse_message(&v, 42).unwrap();
        assert_eq!(m.id, 100);
        assert_eq!(m.message_id.as_deref(), Some("<abc@example.com>"));
        assert_eq!(m.subject, "Re: mix");
        assert_eq!(m.from, "Acme <client@acme.com>");
        assert_eq!(m.to, vec!["cody@ftstudios.com"]);
        assert_eq!(m.imap_uid, Some(77));
        assert!(m.has_attachments);
        assert_eq!(m.attachment_count, 1);
        assert_eq!(m.flags, vec!["seen"]);
        assert_eq!(m.mailbox_id, 42);
    }

    #[test]
    fn format_address_falls_back_to_email() {
        let v: serde_json::Value =
            serde_json::from_str(r#"{"email":"a@b.com"}"#).unwrap();
        assert_eq!(format_address(&v), Some("a@b.com".to_string()));
    }
}
