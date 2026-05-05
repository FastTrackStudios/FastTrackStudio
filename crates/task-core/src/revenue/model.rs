//! Revenue attribution — markdown-backed realized income ledger.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

use crate::invoice::Invoice;
use crate::task::WikiLink;

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Revenue {
    pub id: String,
    pub number: u32,
    pub date: NaiveDate,
    pub amount_cents: u64,
    #[facet(default)]
    pub currency_code: String,
    pub project: Option<WikiLink>,
    pub client: Option<WikiLink>,
    pub deliverable: Option<String>,
    pub invoice_id: Option<String>,
    pub invoice_line_id: Option<String>,
    pub category: Option<String>,
    pub payment_method: Option<String>,
    pub payment_reference: Option<String>,
    pub description: String,
    pub notes: Option<String>,
    pub created_by: Option<String>,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct RevenueCreateRequest {
    pub description: String,
    pub amount_cents: u64,
    pub date: Option<NaiveDate>,
    pub currency_code: Option<String>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub deliverable: Option<String>,
    pub invoice_id: Option<String>,
    pub invoice_line_id: Option<String>,
    pub category: Option<String>,
    pub payment_method: Option<String>,
    pub payment_reference: Option<String>,
    pub notes: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct RevenueFilter {
    pub from: Option<NaiveDate>,
    pub to: Option<NaiveDate>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub deliverable: Option<String>,
    pub invoice_id: Option<String>,
    pub category: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct RevenueBucket {
    pub name: String,
    pub revenue_count: u32,
    pub recognized_cents: u64,
}

#[derive(Debug, Clone, Facet)]
pub struct RevenueReport {
    pub generated_at: DateTime<Utc>,
    pub today: String,
    pub revenue_count: u32,
    pub recognized_cents: u64,
    pub invoice_paid_cents: u64,
    pub unattributed_invoice_paid_cents: u64,
    #[facet(default)]
    pub by_project: Vec<RevenueBucket>,
    #[facet(default)]
    pub by_client: Vec<RevenueBucket>,
    #[facet(default)]
    pub by_deliverable: Vec<RevenueBucket>,
    #[facet(default)]
    pub by_category: Vec<RevenueBucket>,
    #[facet(default)]
    pub revenues: Vec<Revenue>,
}

impl Default for RevenueReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            revenue_count: 0,
            recognized_cents: 0,
            invoice_paid_cents: 0,
            unattributed_invoice_paid_cents: 0,
            by_project: Vec::new(),
            by_client: Vec::new(),
            by_deliverable: Vec::new(),
            by_category: Vec::new(),
            revenues: Vec::new(),
        }
    }
}

pub fn format_revenue_id(year: i32, number: u32) -> String {
    format!("REV-{year:04}-{number:04}")
}

pub fn render_revenue_body(revenue: &Revenue) -> String {
    use std::fmt::Write;

    let mut out = String::new();
    let _ = writeln!(out, "# Revenue {}", revenue.id);
    let _ = writeln!(out);
    let _ = writeln!(out, "**Date:** {}", revenue.date);
    let _ = writeln!(
        out,
        "**Amount:** ${:.2}",
        revenue.amount_cents as f64 / 100.0
    );
    if !revenue.currency_code.is_empty() {
        let _ = writeln!(out, "**Currency:** {}", revenue.currency_code);
    }
    if let Some(project) = &revenue.project {
        let _ = writeln!(out, "**Project:** {}", project.0);
    }
    if let Some(client) = &revenue.client {
        let _ = writeln!(out, "**Client:** {}", client.0);
    }
    if let Some(deliverable) = &revenue.deliverable {
        let _ = writeln!(out, "**Deliverable:** {}", deliverable);
    }
    if let Some(invoice_id) = &revenue.invoice_id {
        let _ = writeln!(out, "**Invoice:** {}", invoice_id);
    }
    if let Some(category) = &revenue.category {
        let _ = writeln!(out, "**Category:** {}", category);
    }
    if let Some(reference) = &revenue.payment_reference {
        let _ = writeln!(out, "**Payment reference:** {}", reference);
    }
    let _ = writeln!(out);
    let _ = writeln!(out, "## Description");
    let _ = writeln!(out);
    let _ = writeln!(out, "{}", revenue.description);
    if let Some(notes) = &revenue.notes {
        let _ = writeln!(out);
        let _ = writeln!(out, "## Notes");
        let _ = writeln!(out);
        let _ = writeln!(out, "{}", notes);
    }
    out
}

pub fn matches_revenue_filter(revenue: &Revenue, filter: &RevenueFilter) -> bool {
    if filter.from.is_some_and(|from| revenue.date < from) {
        return false;
    }
    if filter.to.is_some_and(|to| revenue.date > to) {
        return false;
    }
    if filter
        .project
        .as_deref()
        .is_some_and(|wanted| revenue.project.as_ref().map(|p| p.0.as_str()) != Some(wanted))
    {
        return false;
    }
    if filter
        .client
        .as_deref()
        .is_some_and(|wanted| revenue.client.as_ref().map(|c| c.0.as_str()) != Some(wanted))
    {
        return false;
    }
    if filter
        .deliverable
        .as_deref()
        .is_some_and(|wanted| revenue.deliverable.as_deref() != Some(wanted))
    {
        return false;
    }
    if filter
        .invoice_id
        .as_deref()
        .is_some_and(|wanted| revenue.invoice_id.as_deref() != Some(wanted))
    {
        return false;
    }
    if filter
        .category
        .as_deref()
        .is_some_and(|wanted| revenue.category.as_deref() != Some(wanted))
    {
        return false;
    }
    true
}

pub fn build_revenue_report(
    revenues: &[Revenue],
    invoices: &[Invoice],
    today: NaiveDate,
) -> RevenueReport {
    use std::collections::BTreeMap;

    let mut report = RevenueReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        revenue_count: revenues.len() as u32,
        recognized_cents: revenues.iter().map(|r| r.amount_cents).sum(),
        invoice_paid_cents: invoices.iter().map(|i| i.paid_cents()).sum(),
        ..RevenueReport::default()
    };
    report.unattributed_invoice_paid_cents = report
        .invoice_paid_cents
        .saturating_sub(report.recognized_cents);
    report.revenues = revenues.to_vec();

    let mut by_project = BTreeMap::new();
    let mut by_client = BTreeMap::new();
    let mut by_deliverable = BTreeMap::new();
    let mut by_category = BTreeMap::new();
    for revenue in revenues {
        add_bucket(
            &mut by_project,
            revenue
                .project
                .as_ref()
                .map(|p| p.0.as_str())
                .unwrap_or("Unassigned"),
            revenue.amount_cents,
        );
        add_bucket(
            &mut by_client,
            revenue
                .client
                .as_ref()
                .map(|c| c.0.as_str())
                .unwrap_or("Unassigned"),
            revenue.amount_cents,
        );
        add_bucket(
            &mut by_deliverable,
            revenue.deliverable.as_deref().unwrap_or("Unassigned"),
            revenue.amount_cents,
        );
        add_bucket(
            &mut by_category,
            revenue.category.as_deref().unwrap_or("Uncategorized"),
            revenue.amount_cents,
        );
    }
    report.by_project = buckets(by_project);
    report.by_client = buckets(by_client);
    report.by_deliverable = buckets(by_deliverable);
    report.by_category = buckets(by_category);
    report
}

fn add_bucket(map: &mut std::collections::BTreeMap<String, (u32, u64)>, name: &str, amount: u64) {
    let entry = map.entry(name.to_string()).or_default();
    entry.0 += 1;
    entry.1 += amount;
}

fn buckets(map: std::collections::BTreeMap<String, (u32, u64)>) -> Vec<RevenueBucket> {
    map.into_iter()
        .map(|(name, (revenue_count, recognized_cents))| RevenueBucket {
            name,
            revenue_count,
            recognized_cents,
        })
        .collect()
}

pub fn render_revenue_report(report: &RevenueReport) -> String {
    format!(
        "Revenue: ${:.2}\nInvoice payments: ${:.2}\nUnattributed invoice payments: ${:.2}\nEntries: {}",
        report.recognized_cents as f64 / 100.0,
        report.invoice_paid_cents as f64 / 100.0,
        report.unattributed_invoice_paid_cents as f64 / 100.0,
        report.revenue_count
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn filters_and_reports_revenue() {
        let revenues = vec![Revenue {
            id: "REV-2026-0001".into(),
            number: 1,
            date: NaiveDate::from_ymd_opt(2026, 5, 4).unwrap(),
            amount_cents: 250_00,
            currency_code: "USD".into(),
            project: Some(WikiLink("Album".into())),
            client: Some(WikiLink("Client".into())),
            deliverable: Some("Mix".into()),
            category: Some("music".into()),
            description: "Mix payment".into(),
            ..Default::default()
        }];
        assert!(matches_revenue_filter(
            &revenues[0],
            &RevenueFilter {
                project: Some("Album".into()),
                deliverable: Some("Mix".into()),
                ..Default::default()
            }
        ));
        let report = build_revenue_report(&revenues, &[], revenues[0].date);
        assert_eq!(report.recognized_cents, 250_00);
        assert_eq!(report.by_project[0].name, "Album");
    }
}
