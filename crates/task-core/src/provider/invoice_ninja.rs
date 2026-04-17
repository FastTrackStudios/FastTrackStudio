//! Invoice Ninja v5 client — push invoices, pull clients.
//!
//! Scope: the minimum surface needed to turn local TimeEntries into an IN
//! invoice. We do NOT mirror our data model into IN — IN is just the
//! billing backend. Our markdown stays the source of truth.
//!
//! # Endpoints used
//! - `GET  /api/v1/clients` — paginated client list
//! - `POST /api/v1/clients` — create a client
//! - `POST /api/v1/invoices?send_email={bool}` — create (and optionally
//!   email) an invoice with pre-computed line items
//!
//! # Auth
//! Header `X-API-TOKEN: <token>` plus `X-Requested-With: XMLHttpRequest`.
//! Every token is scoped to a single IN company.

use serde_json::json;

use crate::service::VaultError;

/// Connection config for an Invoice Ninja instance.
#[derive(Debug, Clone)]
pub struct InvoiceNinjaConfig {
    /// Base URL (e.g. `https://invoice.fasttrackstudios.com` or
    /// `https://invoicing.co` for cloud).
    pub url: String,
    /// Personal API token — found under Settings → Account Management → API Tokens.
    pub api_token: String,
}

impl InvoiceNinjaConfig {
    fn base(&self) -> String {
        format!("{}/api/v1", self.url.trim_end_matches('/'))
    }
}

/// A minimal projection of an IN client — enough to show in a picker.
#[derive(Debug, Clone)]
pub struct InvoiceNinjaClient {
    pub id: String,
    pub name: String,
    pub currency_id: Option<u32>,
    pub email: Option<String>,
    pub balance_cents: Option<i64>,
}

/// A line item we're about to send to IN.
#[derive(Debug, Clone)]
pub struct InvoiceLine {
    /// Product key — usually "Billable Hours" or the task name.
    pub product_key: String,
    /// Notes shown on the line (description + optional timelog context).
    pub notes: String,
    /// Hours billed. IN stores as `quantity`.
    pub hours: f64,
    /// Hourly rate in cents. IN stores as `cost` (decimal, major units).
    pub rate_cents: u32,
    /// Percentage discount (0..100). 0 disables.
    pub discount_percent: f64,
    /// Optional per-line tax rate (name, rate_percent).
    pub tax: Option<(String, f64)>,
}

impl InvoiceLine {
    /// Pre-tax amount in cents.
    pub fn amount_cents(&self) -> u64 {
        ((self.hours * self.rate_cents as f64).round() as i64).max(0) as u64
    }

    fn to_json(&self) -> serde_json::Value {
        let cost_dollars = self.rate_cents as f64 / 100.0;
        let mut obj = json!({
            "product_key": self.product_key,
            "notes": self.notes,
            "quantity": self.hours,
            "cost": cost_dollars,
            "type_id": "2", // IN: "2" = service
            "discount": self.discount_percent,
            "is_amount_discount": false,
        });
        if let Some((name, rate)) = &self.tax {
            obj["tax_name1"] = json!(name);
            obj["tax_rate1"] = json!(rate);
        }
        obj
    }
}

/// A draft invoice: client + date range + line items. Not yet sent.
#[derive(Debug, Clone)]
pub struct InvoiceDraft {
    /// IN hashed client id. Must be resolved BEFORE building the draft.
    pub client_invoice_ninja_id: String,
    /// Optional PO number to show on the invoice.
    pub po_number: Option<String>,
    /// Invoice date (YYYY-MM-DD). None = IN uses today.
    pub date: Option<String>,
    /// Due date (YYYY-MM-DD). None = IN computes from client terms.
    pub due_date: Option<String>,
    /// Public notes (shown on the invoice).
    pub public_notes: Option<String>,
    /// Private notes (internal only).
    pub private_notes: Option<String>,
    /// Line items.
    pub lines: Vec<InvoiceLine>,
}

impl InvoiceDraft {
    /// Total cents across all lines (pre-discount, pre-tax).
    pub fn subtotal_cents(&self) -> u64 {
        self.lines.iter().map(|l| l.amount_cents()).sum()
    }
}

/// Response shape we care about from `POST /invoices`.
#[derive(Debug, Clone)]
pub struct CreatedInvoice {
    pub id: String,
    pub number: String,
    pub amount_dollars: f64,
    pub balance_dollars: f64,
}

/// Invoice Ninja HTTP client.
pub struct InvoiceNinjaClientApi {
    config: InvoiceNinjaConfig,
    http: reqwest::Client,
}

impl InvoiceNinjaClientApi {
    pub fn new(config: InvoiceNinjaConfig) -> Self {
        Self {
            config,
            http: reqwest::Client::new(),
        }
    }

    fn auth(&self, req: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        req.header("X-API-TOKEN", &self.config.api_token)
            .header("X-Requested-With", "XMLHttpRequest")
            .header("Accept", "application/json")
    }

    /// Pull all clients. Paginates via `per_page=100`; follows until data
    /// is exhausted or we hit 5000 (arbitrary cap for sanity).
    pub async fn list_clients(&self) -> Result<Vec<InvoiceNinjaClient>, VaultError> {
        let mut out = Vec::new();
        let mut page = 1u32;
        loop {
            let url = format!(
                "{}/clients?per_page=100&page={page}",
                self.config.base()
            );
            let resp = self
                .auth(self.http.get(&url))
                .send()
                .await
                .map_err(|e| VaultError::IoError(format!("IN list_clients: {e}")))?;

            let status = resp.status();
            let body = resp
                .text()
                .await
                .map_err(|e| VaultError::IoError(e.to_string()))?;
            if !status.is_success() {
                return Err(VaultError::IoError(format!(
                    "IN list_clients {status}: {body}"
                )));
            }

            let v: serde_json::Value = serde_json::from_str(&body)
                .map_err(|e| VaultError::ParseError(format!("IN clients: {e}")))?;
            let data = v
                .get("data")
                .and_then(|d| d.as_array())
                .ok_or_else(|| VaultError::ParseError("IN clients: no data[]".into()))?;

            if data.is_empty() {
                break;
            }
            for c in data {
                if let Some(parsed) = parse_client(c) {
                    out.push(parsed);
                }
            }
            // IN response has `meta.pagination` — cheapest: stop when we
            // got fewer than the page size.
            if data.len() < 100 || out.len() >= 5000 {
                break;
            }
            page += 1;
        }
        Ok(out)
    }

    /// Create a client in IN. `contact_email` is required — IN 422s if
    /// the contacts array is empty.
    pub async fn create_client(
        &self,
        name: &str,
        contact_email: &str,
        contact_name: Option<&str>,
        currency_id: Option<u32>,
    ) -> Result<InvoiceNinjaClient, VaultError> {
        let mut contact = json!({
            "email": contact_email,
            "is_primary": true,
            "send_email": true,
        });
        if let Some(n) = contact_name {
            contact["first_name"] = json!(n);
        }

        let mut body = json!({
            "name": name,
            "contacts": [contact],
        });
        if let Some(cid) = currency_id {
            body["settings"] = json!({ "currency_id": cid.to_string() });
        }

        let url = format!("{}/clients", self.config.base());
        let resp = self
            .auth(self.http.post(&url))
            .json(&body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("IN create_client: {e}")))?;

        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "IN create_client {status}: {text}"
            )));
        }
        let v: serde_json::Value = serde_json::from_str(&text)
            .map_err(|e| VaultError::ParseError(format!("IN create_client JSON: {e}")))?;
        let data = v
            .get("data")
            .ok_or_else(|| VaultError::ParseError("IN create_client: no data".into()))?;
        parse_client(data).ok_or_else(|| {
            VaultError::ParseError("IN create_client: missing id/name".into())
        })
    }

    /// Create (and optionally email) an invoice. Returns the created row.
    pub async fn create_invoice(
        &self,
        draft: &InvoiceDraft,
        send_email: bool,
    ) -> Result<CreatedInvoice, VaultError> {
        let line_items: Vec<serde_json::Value> =
            draft.lines.iter().map(|l| l.to_json()).collect();

        let mut body = json!({
            "client_id": draft.client_invoice_ninja_id,
            "line_items": line_items,
        });
        if let Some(ref p) = draft.po_number {
            body["po_number"] = json!(p);
        }
        if let Some(ref d) = draft.date {
            body["date"] = json!(d);
        }
        if let Some(ref d) = draft.due_date {
            body["due_date"] = json!(d);
        }
        if let Some(ref n) = draft.public_notes {
            body["public_notes"] = json!(n);
        }
        if let Some(ref n) = draft.private_notes {
            body["private_notes"] = json!(n);
        }

        let url = format!(
            "{}/invoices?send_email={}",
            self.config.base(),
            if send_email { "true" } else { "false" }
        );
        let resp = self
            .auth(self.http.post(&url))
            .json(&body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("IN create_invoice: {e}")))?;

        let status = resp.status();
        let text = resp
            .text()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        if !status.is_success() {
            return Err(VaultError::IoError(format!(
                "IN create_invoice {status}: {text}"
            )));
        }
        let v: serde_json::Value = serde_json::from_str(&text)
            .map_err(|e| VaultError::ParseError(format!("IN create_invoice JSON: {e}")))?;
        let data = v
            .get("data")
            .ok_or_else(|| VaultError::ParseError("IN create_invoice: no data".into()))?;
        Ok(CreatedInvoice {
            id: data
                .get("id")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            number: data
                .get("number")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string(),
            amount_dollars: data.get("amount").and_then(|v| v.as_f64()).unwrap_or(0.0),
            balance_dollars: data.get("balance").and_then(|v| v.as_f64()).unwrap_or(0.0),
        })
    }
}

fn parse_client(v: &serde_json::Value) -> Option<InvoiceNinjaClient> {
    let id = v.get("id")?.as_str()?.to_string();
    let name = v
        .get("name")
        .and_then(|s| s.as_str())
        .unwrap_or("")
        .to_string();
    let currency_id = v
        .get("settings")
        .and_then(|s| s.get("currency_id"))
        .and_then(|c| {
            // IN sometimes returns currency_id as string, sometimes as number.
            c.as_str()
                .and_then(|s| s.parse::<u32>().ok())
                .or_else(|| c.as_u64().map(|n| n as u32))
        });
    let email = v
        .get("contacts")
        .and_then(|c| c.as_array())
        .and_then(|arr| arr.first())
        .and_then(|c| c.get("email"))
        .and_then(|e| e.as_str())
        .map(String::from);
    let balance_cents = v
        .get("balance")
        .and_then(|b| b.as_f64())
        .map(|d| (d * 100.0).round() as i64);
    Some(InvoiceNinjaClient {
        id,
        name,
        currency_id,
        email,
        balance_cents,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn line_json_includes_type_service() {
        let l = InvoiceLine {
            product_key: "Billable Hours".into(),
            notes: "April mixing".into(),
            hours: 1.5,
            rate_cents: 12_000,
            discount_percent: 0.0,
            tax: None,
        };
        let v = l.to_json();
        assert_eq!(v["type_id"], "2");
        assert_eq!(v["quantity"], 1.5);
        assert_eq!(v["cost"], 120.0);
        assert_eq!(l.amount_cents(), 18_000);
    }

    #[test]
    fn line_json_includes_tax_when_set() {
        let l = InvoiceLine {
            product_key: "hours".into(),
            notes: "".into(),
            hours: 1.0,
            rate_cents: 10_000,
            discount_percent: 0.0,
            tax: Some(("GST".into(), 5.0)),
        };
        let v = l.to_json();
        assert_eq!(v["tax_name1"], "GST");
        assert_eq!(v["tax_rate1"], 5.0);
    }

    #[test]
    fn parses_client_with_string_currency_id() {
        let v: serde_json::Value = serde_json::from_str(
            r#"{"id":"Opnel5","name":"Acme","balance":125.50,
                 "settings":{"currency_id":"1"},
                 "contacts":[{"email":"a@b.com"}]}"#,
        )
        .unwrap();
        let c = parse_client(&v).unwrap();
        assert_eq!(c.id, "Opnel5");
        assert_eq!(c.currency_id, Some(1));
        assert_eq!(c.email.as_deref(), Some("a@b.com"));
        assert_eq!(c.balance_cents, Some(12_550));
    }

    #[test]
    fn draft_subtotal_sums_lines() {
        let draft = InvoiceDraft {
            client_invoice_ninja_id: "X".into(),
            po_number: None,
            date: None,
            due_date: None,
            public_notes: None,
            private_notes: None,
            lines: vec![
                InvoiceLine {
                    product_key: "a".into(),
                    notes: "".into(),
                    hours: 2.0,
                    rate_cents: 10_000,
                    discount_percent: 0.0,
                    tax: None,
                },
                InvoiceLine {
                    product_key: "b".into(),
                    notes: "".into(),
                    hours: 0.5,
                    rate_cents: 20_000,
                    discount_percent: 0.0,
                    tax: None,
                },
            ],
        };
        // 2h * $100 = $200 + 0.5h * $200 = $100 → $300
        assert_eq!(draft.subtotal_cents(), 30_000);
    }
}
