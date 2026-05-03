//! Expense tracking — markdown-backed project spend ledger.
//!
//! Expenses are stored as `expenses/<id>.md` files in the vault with YAML
//! frontmatter and a human-readable markdown body. This keeps the data
//! portable and headless-friendly while still being easy to inspect in an
//! editor or via CLI output.
//!
//! The initial model focuses on CLI-first bookkeeping:
//! - project/client/category/vendor attribution
//! - reimbursable flag
//! - list/show/update/delete operations
//! - grouped reporting by project, client, and category

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

use crate::task::WikiLink;

/// A recorded expense backed by `expenses/<id>.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Expense {
    /// Human-readable id, e.g. `EXP-2026-0001`.
    pub id: String,

    /// Monotonic yearly sequence used to build the id.
    pub number: u32,

    /// Lifecycle state for the expense.
    pub status: ExpenseStatus,

    /// Expense date.
    pub date: NaiveDate,

    /// Amount in cents.
    pub amount_cents: u64,

    /// ISO 4217 currency code, e.g. `USD`.
    #[facet(default)]
    pub currency_code: String,

    /// Owning project, if known.
    pub project: Option<WikiLink>,

    /// Owning client, if known.
    pub client: Option<WikiLink>,

    /// Human category label, e.g. `travel`, `gear`, `software`.
    pub category: Option<String>,

    /// Vendor / merchant name.
    pub vendor: Option<String>,

    /// Short description of the spend.
    pub description: String,

    /// Optional receipt URL, file reference, or note.
    pub receipt: Option<String>,

    /// Whether this expense is expected to be reimbursed or passed through.
    #[facet(default)]
    pub reimbursable: bool,

    /// Free-form notes.
    pub notes: Option<String>,

    /// Who created the expense entry.
    pub created_by: Option<String>,

    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,

    /// Optional generated markdown body. We regenerate it on save.
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum ExpenseStatus {
    /// Created locally but not yet reviewed.
    #[default]
    Draft,
    /// Ready for reimbursement / allocation.
    Open,
    /// Recorded as reimbursed or paid.
    Paid,
    /// Cancelled / voided.
    Cancelled,
}

impl Expense {
    pub fn total_cents(&self) -> u64 {
        self.amount_cents
    }
}

#[derive(Debug, Clone, Default, Facet)]
pub struct ExpenseCreateRequest {
    pub description: String,
    pub amount_cents: u64,
    pub date: Option<NaiveDate>,
    pub currency_code: Option<String>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub category: Option<String>,
    pub vendor: Option<String>,
    pub receipt: Option<String>,
    pub reimbursable: bool,
    pub status: Option<String>,
    pub notes: Option<String>,
    pub actor: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct ExpensePatch {
    pub status: Option<String>,
    pub date: Option<String>,
    pub amount_cents: Option<u64>,
    pub currency_code: Option<String>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub category: Option<String>,
    pub vendor: Option<String>,
    pub description: Option<String>,
    pub receipt: Option<String>,
    pub reimbursable: Option<bool>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct ExpenseFilter {
    pub from: Option<NaiveDate>,
    pub to: Option<NaiveDate>,
    pub project: Option<String>,
    pub client: Option<String>,
    pub category: Option<String>,
    pub vendor: Option<String>,
    pub status: Option<String>,
    pub reimbursable_only: bool,
}

#[derive(Debug, Clone, Default, Facet)]
pub struct ExpenseBucket {
    pub name: String,
    pub expense_count: u32,
    pub amount_cents: u64,
    pub reimbursable_cents: u64,
}

#[derive(Debug, Clone, Facet)]
pub struct ExpenseReport {
    pub generated_at: DateTime<Utc>,
    pub today: String,
    pub expense_count: u32,
    pub total_cents: u64,
    pub reimbursable_cents: u64,
    pub paid_cents: u64,
    pub open_cents: u64,
    pub draft_cents: u64,
    pub cancelled_cents: u64,
    #[facet(default)]
    pub by_project: Vec<ExpenseBucket>,
    #[facet(default)]
    pub by_client: Vec<ExpenseBucket>,
    #[facet(default)]
    pub by_category: Vec<ExpenseBucket>,
    #[facet(default)]
    pub expenses: Vec<Expense>,
}

impl Default for ExpenseReport {
    fn default() -> Self {
        Self {
            generated_at: Utc::now(),
            today: String::new(),
            expense_count: 0,
            total_cents: 0,
            reimbursable_cents: 0,
            paid_cents: 0,
            open_cents: 0,
            draft_cents: 0,
            cancelled_cents: 0,
            by_project: Vec::new(),
            by_client: Vec::new(),
            by_category: Vec::new(),
            expenses: Vec::new(),
        }
    }
}

/// Format the stable id: `EXP-{year}-{number:04}`.
pub fn format_expense_id(year: i32, number: u32) -> String {
    format!("EXP-{year:04}-{number:04}")
}

pub fn parse_expense_status(status: &str) -> Option<ExpenseStatus> {
    match status.to_ascii_lowercase().as_str() {
        "draft" => Some(ExpenseStatus::Draft),
        "open" => Some(ExpenseStatus::Open),
        "paid" => Some(ExpenseStatus::Paid),
        "cancelled" | "canceled" => Some(ExpenseStatus::Cancelled),
        _ => None,
    }
}

/// Render the human-readable body of an expense file.
pub fn render_expense_body(expense: &Expense) -> String {
    use std::fmt::Write;

    let mut out = String::new();
    let _ = writeln!(out, "# Expense {}", expense.id);
    let _ = writeln!(out);
    let _ = writeln!(out, "**Date:** {}", expense.date);
    let _ = writeln!(out, "**Status:** {:?}", expense.status);
    let _ = writeln!(out, "**Amount:** ${:.2}", expense.amount_cents as f64 / 100.0);
    if !expense.currency_code.is_empty() {
        let _ = writeln!(out, "**Currency:** {}", expense.currency_code);
    }
    if let Some(project) = &expense.project {
        let _ = writeln!(out, "**Project:** {}", project.0);
    }
    if let Some(client) = &expense.client {
        let _ = writeln!(out, "**Client:** {}", client.0);
    }
    if let Some(category) = &expense.category {
        let _ = writeln!(out, "**Category:** {}", category);
    }
    if let Some(vendor) = &expense.vendor {
        let _ = writeln!(out, "**Vendor:** {}", vendor);
    }
    if expense.reimbursable {
        let _ = writeln!(out, "**Reimbursable:** yes");
    }
    if let Some(receipt) = &expense.receipt {
        let _ = writeln!(out, "**Receipt:** {}", receipt);
    }
    let _ = writeln!(out);
    let _ = writeln!(out, "## Description");
    let _ = writeln!(out);
    let _ = writeln!(out, "{}", expense.description);
    let _ = writeln!(out);

    if let Some(notes) = &expense.notes {
        let _ = writeln!(out, "## Notes");
        let _ = writeln!(out);
        let _ = writeln!(out, "{}", notes);
        let _ = writeln!(out);
    }

    out
}

pub fn matches_expense_filter(expense: &Expense, filter: &ExpenseFilter) -> bool {
    if filter.from.is_some_and(|from| expense.date < from) {
        return false;
    }
    if filter.to.is_some_and(|to| expense.date > to) {
        return false;
    }
    if filter.reimbursable_only && !expense.reimbursable {
        return false;
    }
    if let Some(project) = filter.project.as_deref() {
        if expense
            .project
            .as_ref()
            .map(|p| !p.0.eq_ignore_ascii_case(project))
            .unwrap_or(true)
        {
            return false;
        }
    }
    if let Some(client) = filter.client.as_deref() {
        if expense
            .client
            .as_ref()
            .map(|c| !c.0.eq_ignore_ascii_case(client))
            .unwrap_or(true)
        {
            return false;
        }
    }
    if let Some(category) = filter.category.as_deref() {
        if expense
            .category
            .as_deref()
            .map(|c| !c.eq_ignore_ascii_case(category))
            .unwrap_or(true)
        {
            return false;
        }
    }
    if let Some(vendor) = filter.vendor.as_deref() {
        if expense
            .vendor
            .as_deref()
            .map(|v| !v.eq_ignore_ascii_case(vendor))
            .unwrap_or(true)
        {
            return false;
        }
    }
    if let Some(status) = filter.status.as_deref() {
        match parse_expense_status(status) {
            Some(want) if want == expense.status => {}
            _ => return false,
        }
    }
    true
}

pub fn build_expense_report(expenses: &[Expense], today: NaiveDate) -> ExpenseReport {
    use std::collections::BTreeMap;

    let mut report = ExpenseReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        ..ExpenseReport::default()
    };

    let mut project_totals: BTreeMap<String, ExpenseBucket> = BTreeMap::new();
    let mut client_totals: BTreeMap<String, ExpenseBucket> = BTreeMap::new();
    let mut category_totals: BTreeMap<String, ExpenseBucket> = BTreeMap::new();

    let mut items: Vec<Expense> = expenses.to_vec();
    items.sort_by(|a, b| b.date.cmp(&a.date).then_with(|| b.number.cmp(&a.number)));

    for expense in items {
        let amount = expense.total_cents();
        report.expense_count += 1;
        report.total_cents += amount;
        if expense.reimbursable {
            report.reimbursable_cents += amount;
        }
        match expense.status {
            ExpenseStatus::Draft => report.draft_cents += amount,
            ExpenseStatus::Open => report.open_cents += amount,
            ExpenseStatus::Paid => report.paid_cents += amount,
            ExpenseStatus::Cancelled => report.cancelled_cents += amount,
        }

        let add_bucket = |map: &mut BTreeMap<String, ExpenseBucket>, name: String, expense: &Expense| {
            let bucket = map.entry(name.clone()).or_insert_with(|| ExpenseBucket {
                name,
                ..ExpenseBucket::default()
            });
            bucket.expense_count += 1;
            bucket.amount_cents += expense.total_cents();
            if expense.reimbursable {
                bucket.reimbursable_cents += expense.total_cents();
            }
        };

        add_bucket(
            &mut project_totals,
            expense
                .project
                .as_ref()
                .map(|w| w.0.clone())
                .unwrap_or_else(|| "Unassigned".into()),
            &expense,
        );
        add_bucket(
            &mut client_totals,
            expense
                .client
                .as_ref()
                .map(|w| w.0.clone())
                .unwrap_or_else(|| "Unassigned".into()),
            &expense,
        );
        add_bucket(
            &mut category_totals,
            expense
                .category
                .as_ref()
                .cloned()
                .unwrap_or_else(|| "Uncategorized".into()),
            &expense,
        );

        report.expenses.push(expense);
    }

    let collect_sorted = |map: BTreeMap<String, ExpenseBucket>| {
        let mut values: Vec<_> = map.into_values().collect();
        values.sort_by(|a, b| b.amount_cents.cmp(&a.amount_cents).then_with(|| a.name.cmp(&b.name)));
        values
    };

    report.by_project = collect_sorted(project_totals);
    report.by_client = collect_sorted(client_totals);
    report.by_category = collect_sorted(category_totals);
    report
}

pub fn render_expense_report(report: &ExpenseReport) -> String {
    use std::fmt::Write;

    let mut out = String::new();
    let _ = writeln!(out, "Expense report for {}", report.today);
    let _ = writeln!(out);
    let _ = writeln!(out, "- Expense count: {}", report.expense_count);
    let _ = writeln!(out, "- Total: ${:.2}", report.total_cents as f64 / 100.0);
    let _ = writeln!(out, "- Reimbursable: ${:.2}", report.reimbursable_cents as f64 / 100.0);
    let _ = writeln!(out, "- Paid: ${:.2}", report.paid_cents as f64 / 100.0);
    let _ = writeln!(out, "- Open: ${:.2}", report.open_cents as f64 / 100.0);
    let _ = writeln!(out, "- Draft: ${:.2}", report.draft_cents as f64 / 100.0);
    let _ = writeln!(out, "- Cancelled: ${:.2}", report.cancelled_cents as f64 / 100.0);
    let _ = writeln!(out);

    fn render_bucket(title: &str, buckets: &[ExpenseBucket], out: &mut String) {
        use std::fmt::Write;
        if buckets.is_empty() {
            return;
        }
        let _ = writeln!(out, "## {}", title);
        let _ = writeln!(out);
        let _ = writeln!(out, "| Name | Count | Amount | Reimbursable |");
        let _ = writeln!(out, "|------|------:|-------:|--------------:|");
        for b in buckets {
            let _ = writeln!(out, "| {} | {} | ${:.2} | ${:.2} |", b.name, b.expense_count, b.amount_cents as f64 / 100.0, b.reimbursable_cents as f64 / 100.0);
        }
        let _ = writeln!(out);
    }

    render_bucket("By project", &report.by_project, &mut out);
    render_bucket("By client", &report.by_client, &mut out);
    render_bucket("By category", &report.by_category, &mut out);

    if !report.expenses.is_empty() {
        let _ = writeln!(out, "## Expenses");
        let _ = writeln!(out);
        let _ = writeln!(out, "| Date | Description | Project | Client | Amount | Status |");
        let _ = writeln!(out, "|------|-------------|---------|--------|-------:|--------|");
        for e in &report.expenses {
            let project = e.project.as_ref().map(|w| w.0.as_str()).unwrap_or("—");
            let client = e.client.as_ref().map(|w| w.0.as_str()).unwrap_or("—");
            let _ = writeln!(out, "| {} | {} | {} | {} | ${:.2} | {:?} |", e.date, e.description, project, client, e.amount_cents as f64 / 100.0, e.status);
        }
        let _ = writeln!(out);
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_expense_id() {
        assert_eq!(format_expense_id(2026, 3), "EXP-2026-0003");
    }

    #[test]
    fn parses_expense_statuses() {
        assert_eq!(parse_expense_status("draft"), Some(ExpenseStatus::Draft));
        assert_eq!(parse_expense_status("paid"), Some(ExpenseStatus::Paid));
        assert_eq!(parse_expense_status("cancelled"), Some(ExpenseStatus::Cancelled));
        assert_eq!(parse_expense_status("nope"), None);
    }

    #[test]
    fn renders_body_with_key_fields() {
        let expense = Expense {
            id: "EXP-2026-0001".into(),
            number: 1,
            status: ExpenseStatus::Open,
            date: NaiveDate::from_ymd_opt(2026, 5, 2).unwrap(),
            amount_cents: 12_345,
            currency_code: "USD".into(),
            project: Some(WikiLink("Project Alpha".into())),
            client: Some(WikiLink("Client Co".into())),
            category: Some("travel".into()),
            vendor: Some("Rail".into()),
            description: "Train fare".into(),
            receipt: Some("receipt://abc".into()),
            reimbursable: true,
            notes: Some("Booked for site visit".into()),
            created_by: Some("cody".into()),
            date_created: None,
            date_modified: None,
            body: String::new(),
        };
        let body = render_expense_body(&expense);
        assert!(body.contains("# Expense EXP-2026-0001"));
        assert!(body.contains("**Amount:** $123.45"));
        assert!(body.contains("**Project:** Project Alpha"));
        assert!(body.contains("## Notes"));
    }

    #[test]
    fn builds_a_rollup_report_with_buckets() {
        let expenses = vec![
            Expense {
                id: "EXP-2026-0001".into(),
                number: 1,
                status: ExpenseStatus::Open,
                date: NaiveDate::from_ymd_opt(2026, 5, 1).unwrap(),
                amount_cents: 10_000,
                currency_code: "USD".into(),
                project: Some(WikiLink("Project Alpha".into())),
                client: Some(WikiLink("Client One".into())),
                category: Some("travel".into()),
                vendor: Some("Rail".into()),
                description: "Train fare".into(),
                receipt: None,
                reimbursable: true,
                notes: None,
                created_by: None,
                date_created: None,
                date_modified: None,
                body: String::new(),
            },
            Expense {
                id: "EXP-2026-0002".into(),
                number: 2,
                status: ExpenseStatus::Paid,
                date: NaiveDate::from_ymd_opt(2026, 5, 2).unwrap(),
                amount_cents: 25_000,
                currency_code: "USD".into(),
                project: Some(WikiLink("Project Alpha".into())),
                client: Some(WikiLink("Client One".into())),
                category: Some("software".into()),
                vendor: Some("GitHub".into()),
                description: "Subscription".into(),
                receipt: None,
                reimbursable: false,
                notes: None,
                created_by: None,
                date_created: None,
                date_modified: None,
                body: String::new(),
            },
            Expense {
                id: "EXP-2026-0003".into(),
                number: 3,
                status: ExpenseStatus::Draft,
                date: NaiveDate::from_ymd_opt(2026, 5, 3).unwrap(),
                amount_cents: 5_000,
                currency_code: "USD".into(),
                project: None,
                client: None,
                category: Some("travel".into()),
                vendor: Some("Taxi".into()),
                description: "Airport ride".into(),
                receipt: None,
                reimbursable: true,
                notes: None,
                created_by: None,
                date_created: None,
                date_modified: None,
                body: String::new(),
            },
        ];

        let report = build_expense_report(&expenses, NaiveDate::from_ymd_opt(2026, 5, 4).unwrap());

        assert_eq!(report.expense_count, 3);
        assert_eq!(report.total_cents, 40_000);
        assert_eq!(report.reimbursable_cents, 15_000);
        assert_eq!(report.paid_cents, 25_000);
        assert_eq!(report.open_cents, 10_000);
        assert_eq!(report.draft_cents, 5_000);
        assert_eq!(report.by_project.len(), 2);
        assert_eq!(report.by_project[0].name, "Project Alpha");
        assert_eq!(report.by_project[0].expense_count, 2);
        assert_eq!(report.by_category.len(), 2);
    }
}
