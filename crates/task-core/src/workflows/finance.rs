//! Integration with Firefly III for budget tracking and Invoice Ninja for invoicing.
//!
//! Both services use Bearer token authentication and JSON APIs.
//! References to external resources are stored via [`FireflyRef`] and
//! [`InvoiceNinjaRef`] in task/project frontmatter; this module provides
//! the HTTP clients that talk to the live services.

use crate::service::VaultError;
// Integration types available: FireflyRef, InvoiceNinjaRef

// ── Firefly III ─────────────────────────────────────────────────────────────

/// Firefly III API client for budget/expense tracking.
pub struct FireflyClient {
    base_url: String,
    token: String,
    http: reqwest::Client,
}

impl FireflyClient {
    pub fn new(base_url: &str, token: &str) -> Self {
        Self {
            base_url: base_url.trim_end_matches('/').to_string(),
            token: token.to_string(),
            http: reqwest::Client::new(),
        }
    }

    /// Build an authenticated GET request.
    fn get(&self, path: &str) -> reqwest::RequestBuilder {
        self.http
            .get(format!("{}{}", self.base_url, path))
            .bearer_auth(&self.token)
    }

    /// Build an authenticated POST request.
    fn post(&self, path: &str) -> reqwest::RequestBuilder {
        self.http
            .post(format!("{}{}", self.base_url, path))
            .bearer_auth(&self.token)
            .header("Content-Type", "application/json")
    }

    /// Get a transaction by ID.
    pub async fn get_transaction(&self, id: &str) -> Result<FireflyTransaction, VaultError> {
        let resp = self
            .get(&format!("/api/v1/transactions/{id}"))
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::NotFound(format!(
                "Firefly transaction {id}: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        parse_firefly_transaction(&body)
    }

    /// Create a transaction (expense linked to a project).
    /// Returns the new transaction ID.
    pub async fn create_transaction(&self, txn: &FireflyTransaction) -> Result<String, VaultError> {
        let payload = serde_json::json!({
            "transactions": [{
                "type": "withdrawal",
                "description": txn.description,
                "amount": txn.amount.to_string(),
                "currency_code": txn.currency,
                "date": txn.date,
                "category_name": txn.category,
                "budget_name": txn.budget,
                "source_name": txn.source_account,
                "destination_name": txn.destination_account,
                "tags": txn.tags,
                "notes": txn.notes,
            }]
        });

        let resp = self
            .post("/api/v1/transactions")
            .json(&payload)
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Firefly create transaction: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        body["data"]["id"]
            .as_str()
            .map(|s| s.to_string())
            .ok_or_else(|| VaultError::ParseError("missing transaction id in response".into()))
    }

    /// Get budget by name.
    pub async fn get_budget(&self, name: &str) -> Result<Option<FireflyBudget>, VaultError> {
        let resp = self
            .get("/api/v1/budgets")
            .query(&[("name", name)])
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Firefly get budget: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        let data = body["data"].as_array();
        let Some(items) = data else {
            return Ok(None);
        };

        for item in items {
            let attrs = &item["attributes"];
            if attrs["name"].as_str() == Some(name) {
                return Ok(Some(FireflyBudget {
                    id: item["id"].as_str().unwrap_or_default().to_string(),
                    name: name.to_string(),
                    amount: attrs["amount"]
                        .as_str()
                        .and_then(|s| s.parse::<f64>().ok())
                        .unwrap_or(0.0),
                    spent: attrs["spent"]
                        .as_str()
                        .and_then(|s| s.parse::<f64>().ok())
                        .unwrap_or(0.0),
                }));
            }
        }

        Ok(None)
    }

    /// Get total spent for a budget in the current period.
    pub async fn budget_spent(&self, budget_id: &str) -> Result<f64, VaultError> {
        let resp = self
            .get(&format!("/api/v1/budgets/{budget_id}/limits"))
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Firefly budget limits: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        // Sum the `spent` field across all budget limits in the response.
        let total = body["data"]
            .as_array()
            .map(|limits| {
                limits
                    .iter()
                    .filter_map(|l| {
                        l["attributes"]["spent"]
                            .as_str()
                            .and_then(|s| s.parse::<f64>().ok())
                    })
                    .sum()
            })
            .unwrap_or(0.0);

        Ok(total)
    }
}

/// Parse a Firefly III transaction from the standard API envelope.
fn parse_firefly_transaction(body: &serde_json::Value) -> Result<FireflyTransaction, VaultError> {
    let txn = &body["data"]["attributes"]["transactions"][0];
    if txn.is_null() {
        return Err(VaultError::ParseError(
            "no transaction data in Firefly response".into(),
        ));
    }
    Ok(FireflyTransaction {
        id: body["data"]["id"].as_str().map(|s| s.to_string()),
        description: txn["description"].as_str().unwrap_or_default().to_string(),
        amount: txn["amount"]
            .as_str()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0.0),
        currency: txn["currency_code"].as_str().unwrap_or("USD").to_string(),
        date: txn["date"].as_str().unwrap_or_default().to_string(),
        category: txn["category_name"].as_str().map(|s| s.to_string()),
        budget: txn["budget_name"].as_str().map(|s| s.to_string()),
        source_account: txn["source_name"].as_str().map(|s| s.to_string()),
        destination_account: txn["destination_name"].as_str().map(|s| s.to_string()),
        tags: txn["tags"]
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default(),
        notes: txn["notes"].as_str().map(|s| s.to_string()),
    })
}

#[derive(Debug, Clone)]
pub struct FireflyTransaction {
    pub id: Option<String>,
    pub description: String,
    pub amount: f64,
    pub currency: String,
    pub date: String,
    pub category: Option<String>,
    pub budget: Option<String>,
    pub source_account: Option<String>,
    pub destination_account: Option<String>,
    pub tags: Vec<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FireflyBudget {
    pub id: String,
    pub name: String,
    pub amount: f64,
    pub spent: f64,
}

// ── Invoice Ninja ───────────────────────────────────────────────────────────

/// Invoice Ninja API client.
pub struct InvoiceNinjaClient {
    base_url: String,
    token: String,
    http: reqwest::Client,
}

impl InvoiceNinjaClient {
    pub fn new(base_url: &str, token: &str) -> Self {
        Self {
            base_url: base_url.trim_end_matches('/').to_string(),
            token: token.to_string(),
            http: reqwest::Client::new(),
        }
    }

    /// Build an authenticated GET request.
    /// Invoice Ninja uses the `X-Api-Token` header for v5, but also supports
    /// Bearer auth. We send both for compatibility.
    fn get(&self, path: &str) -> reqwest::RequestBuilder {
        self.http
            .get(format!("{}{}", self.base_url, path))
            .bearer_auth(&self.token)
            .header("X-Api-Token", &self.token)
    }

    /// Build an authenticated POST request.
    fn post(&self, path: &str) -> reqwest::RequestBuilder {
        self.http
            .post(format!("{}{}", self.base_url, path))
            .bearer_auth(&self.token)
            .header("X-Api-Token", &self.token)
            .header("Content-Type", "application/json")
    }

    /// Get an invoice by ID.
    pub async fn get_invoice(&self, id: &str) -> Result<Invoice, VaultError> {
        let resp = self
            .get(&format!("/api/v1/invoices/{id}"))
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::NotFound(format!(
                "Invoice Ninja invoice {id}: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        parse_invoice(&body["data"])
    }

    /// Create an invoice. Returns the new invoice ID.
    pub async fn create_invoice(&self, invoice: &Invoice) -> Result<String, VaultError> {
        let line_items: Vec<serde_json::Value> = invoice
            .items
            .iter()
            .map(|item| {
                serde_json::json!({
                    "product_key": item.description,
                    "notes": item.description,
                    "quantity": item.quantity,
                    "cost": item.unit_cost,
                })
            })
            .collect();

        let payload = serde_json::json!({
            "client_id": invoice.client_name,
            "date": invoice.date,
            "due_date": invoice.due_date,
            "line_items": line_items,
            "public_notes": invoice.notes,
        });

        let resp = self
            .post("/api/v1/invoices")
            .json(&payload)
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Invoice Ninja create invoice: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        body["data"]["id"]
            .as_str()
            .map(|s| s.to_string())
            .ok_or_else(|| VaultError::ParseError("missing invoice id in response".into()))
    }

    /// Get invoice status as a human-readable string.
    pub async fn invoice_status(&self, id: &str) -> Result<String, VaultError> {
        let invoice = self.get_invoice(id).await?;
        Ok(invoice.status)
    }

    /// List invoices for a client.
    pub async fn list_invoices(&self, client_id: &str) -> Result<Vec<Invoice>, VaultError> {
        let resp = self
            .get("/api/v1/invoices")
            .query(&[("client_id", client_id)])
            .send()
            .await
            .map_err(|e| VaultError::IoError(e.to_string()))?;

        if !resp.status().is_success() {
            return Err(VaultError::IoError(format!(
                "Invoice Ninja list invoices: HTTP {}",
                resp.status()
            )));
        }

        let body: serde_json::Value = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(e.to_string()))?;

        let items = body["data"]
            .as_array()
            .ok_or_else(|| VaultError::ParseError("expected data array".into()))?;

        items.iter().map(parse_invoice).collect()
    }
}

/// Map Invoice Ninja numeric status to a human-readable string.
fn status_label(status_id: i64) -> &'static str {
    match status_id {
        1 => "draft",
        2 => "sent",
        3 => "viewed",
        4 => "partial",
        5 => "paid",
        6 => "cancelled",
        _ => "unknown",
    }
}

/// Parse a single invoice from the Invoice Ninja API JSON.
fn parse_invoice(data: &serde_json::Value) -> Result<Invoice, VaultError> {
    if data.is_null() {
        return Err(VaultError::ParseError(
            "null invoice data in response".into(),
        ));
    }

    let status_id = data["status_id"].as_i64().unwrap_or(0);

    let items = data["line_items"]
        .as_array()
        .map(|arr| {
            arr.iter()
                .map(|li| InvoiceItem {
                    description: li["notes"]
                        .as_str()
                        .or_else(|| li["product_key"].as_str())
                        .unwrap_or_default()
                        .to_string(),
                    quantity: li["quantity"].as_f64().unwrap_or(1.0),
                    unit_cost: li["cost"].as_f64().unwrap_or(0.0),
                })
                .collect()
        })
        .unwrap_or_default();

    Ok(Invoice {
        id: data["id"].as_str().map(|s| s.to_string()),
        client_name: data["client_id"].as_str().unwrap_or_default().to_string(),
        number: data["number"].as_str().map(|s| s.to_string()),
        date: data["date"].as_str().unwrap_or_default().to_string(),
        due_date: data["due_date"].as_str().map(|s| s.to_string()),
        amount: data["amount"].as_f64().unwrap_or(0.0),
        currency: data["currency_id"].as_str().unwrap_or("USD").to_string(),
        status: status_label(status_id).to_string(),
        items,
        notes: data["public_notes"].as_str().map(|s| s.to_string()),
    })
}

#[derive(Debug, Clone)]
pub struct Invoice {
    pub id: Option<String>,
    pub client_name: String,
    pub number: Option<String>,
    pub date: String,
    pub due_date: Option<String>,
    pub amount: f64,
    pub currency: String,
    pub status: String,
    pub items: Vec<InvoiceItem>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone)]
pub struct InvoiceItem {
    pub description: String,
    pub quantity: f64,
    pub unit_cost: f64,
}
