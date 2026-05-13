//! Pure helpers — formatting, parsing, totals math. Zero Dioxus.

use fts_ui::prelude::StatusBadgeVariant;
use invoice_proto::InvoiceLine;

/// `$1,234.56`, `€340.00`. Maps ISO 4217 → symbol for major currencies;
/// otherwise falls back to a three-letter code prefix.
pub fn format_money(cents: i64, currency: &str) -> String {
    let symbol = match currency.to_ascii_uppercase().as_str() {
        "USD" | "CAD" | "AUD" => "$",
        "EUR" => "€",
        "GBP" => "£",
        "JPY" => "¥",
        _ => "",
    };
    let neg = cents < 0;
    let abs = cents.unsigned_abs() as i128;
    let whole = abs / 100;
    let cents_part = abs % 100;
    let mut w = whole.to_string();
    let chars: Vec<char> = w.chars().collect();
    w.clear();
    for (i, c) in chars.iter().rev().enumerate() {
        if i > 0 && i % 3 == 0 {
            w.insert(0, ',');
        }
        w.insert(0, *c);
    }
    let sign = if neg { "-" } else { "" };
    if symbol.is_empty() {
        format!("{sign}{} {}.{:02}", currency.to_uppercase(), w, cents_part)
    } else {
        format!("{sign}{symbol}{}.{:02}", w, cents_part)
    }
}

/// `1500` (thousandths) → `"1.5"`, `2400` → `"2.4"`, `1000` → `"1"`.
pub fn format_qty_thousandths(thou: i64) -> String {
    let neg = thou < 0;
    let abs = thou.unsigned_abs() as i128;
    let whole = abs / 1000;
    let frac = abs % 1000;
    let sign = if neg { "-" } else { "" };
    if frac == 0 {
        format!("{sign}{whole}")
    } else {
        // Strip trailing zeros from frac.
        let mut s = format!("{:03}", frac);
        while s.ends_with('0') {
            s.pop();
        }
        format!("{sign}{whole}.{s}")
    }
}

/// Parses `"1234.56"`, `"$1,234.56"`, `"1234"`, `"€ 1.50"` → cents.
pub fn parse_money_input(s: &str) -> Option<i64> {
    let mut cleaned = String::new();
    let mut saw_dot = false;
    let mut neg = false;
    for c in s.trim().chars() {
        match c {
            '-' if cleaned.is_empty() => neg = true,
            '0'..='9' => cleaned.push(c),
            '.' if !saw_dot => {
                cleaned.push('.');
                saw_dot = true;
            }
            ',' | ' ' | '$' | '€' | '£' | '¥' => {}
            _ => {}
        }
    }
    if cleaned.is_empty() || cleaned == "." {
        return None;
    }
    let parsed: f64 = cleaned.parse().ok()?;
    let v = (parsed * 100.0).round() as i64;
    Some(if neg { -v } else { v })
}

/// Maps `Invoice.status` → fts-ui `StatusBadgeVariant`. Paid is success
/// (caller can render strike-through label for visual emphasis).
pub fn invoice_status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "draft" => StatusBadgeVariant::Neutral,
        "sent" => StatusBadgeVariant::Success,
        "viewed" => StatusBadgeVariant::Success,
        "partial" => StatusBadgeVariant::Warning,
        "paid" => StatusBadgeVariant::Success,
        "overdue" => StatusBadgeVariant::Danger,
        "cancelled" => StatusBadgeVariant::Neutral,
        _ => StatusBadgeVariant::Neutral,
    }
}

/// Totals breakdown for an invoice. Matches the wire contract in the
/// `invoice-proto` crate docs.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct InvoiceTotals {
    pub subtotal: i64,
    pub discounted: i64,
    pub tax: i64,
    pub total: i64,
    pub balance: i64,
}

/// Implements the tax/total contract from `invoice-proto`:
///
/// - `subtotal   = Σ line.amount_cents`
/// - `discounted = max(0, subtotal - discount_cents)`
/// - exclusive: `tax = discounted * bps / 10_000`, `total = discounted + tax`
/// - inclusive: `tax = subtotal * bps / (10_000 + bps)`, `total = discounted`
/// - `balance   = total - payments_sum`
pub fn compute_totals(
    lines: &[InvoiceLine],
    discount_cents: i64,
    tax_rate_bps: i32,
    tax_inclusive: bool,
    payments_sum: i64,
) -> InvoiceTotals {
    let subtotal: i64 = lines.iter().map(|l| l.amount_cents).sum();
    let discounted = (subtotal - discount_cents).max(0);
    let bps = tax_rate_bps.max(0) as i64;
    let (tax, total) = if tax_inclusive {
        let denom = 10_000 + bps;
        let tax = if denom > 0 {
            subtotal.saturating_mul(bps) / denom
        } else {
            0
        };
        (tax, discounted)
    } else {
        let tax = discounted.saturating_mul(bps) / 10_000;
        (tax, discounted + tax)
    };
    InvoiceTotals {
        subtotal,
        discounted,
        tax,
        total,
        balance: total - payments_sum,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use invoice_proto::InvoiceLine;
    use uuid::Uuid;

    fn line(amount: i64) -> InvoiceLine {
        InvoiceLine {
            id: Uuid::new_v4(),
            invoice_id: Uuid::new_v4(),
            project_id: None,
            time_entry_id: None,
            description: String::new(),
            quantity_thousandths: 1000,
            unit_price_cents: amount,
            amount_cents: amount,
            tax_rate_bps: None,
            sort_index: 0,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    #[test]
    fn money_format() {
        assert_eq!(format_money(123456, "USD"), "$1,234.56");
        assert_eq!(format_money(34000, "EUR"), "€340.00");
        assert_eq!(format_money(100, "SEK"), "SEK 1.00");
        assert_eq!(format_money(-5000, "USD"), "-$50.00");
    }

    #[test]
    fn qty_format() {
        assert_eq!(format_qty_thousandths(1500), "1.5");
        assert_eq!(format_qty_thousandths(2400), "2.4");
        assert_eq!(format_qty_thousandths(1000), "1");
        assert_eq!(format_qty_thousandths(123), "0.123");
    }

    #[test]
    fn parse_money() {
        assert_eq!(parse_money_input("1234.56"), Some(123456));
        assert_eq!(parse_money_input("$1,234.56"), Some(123456));
        assert_eq!(parse_money_input("1234"), Some(123400));
        assert_eq!(parse_money_input(""), None);
    }

    #[test]
    fn exclusive_totals() {
        let lines = vec![line(10_000), line(5_000)];
        let t = compute_totals(&lines, 0, 2_500, false, 0);
        assert_eq!(t.subtotal, 15_000);
        assert_eq!(t.tax, 3_750);
        assert_eq!(t.total, 18_750);
    }

    #[test]
    fn inclusive_totals() {
        let lines = vec![line(12_500)];
        let t = compute_totals(&lines, 0, 2_500, true, 0);
        assert_eq!(t.subtotal, 12_500);
        assert_eq!(t.tax, 2_500); // 12500 * 2500 / 12500
        assert_eq!(t.total, 12_500);
    }

    #[test]
    fn discount_clamps_at_zero() {
        let lines = vec![line(1_000)];
        let t = compute_totals(&lines, 5_000, 0, false, 0);
        assert_eq!(t.discounted, 0);
        assert_eq!(t.total, 0);
    }

    #[test]
    fn balance_after_payments() {
        let lines = vec![line(10_000)];
        let t = compute_totals(&lines, 0, 0, false, 4_000);
        assert_eq!(t.total, 10_000);
        assert_eq!(t.balance, 6_000);
    }
}
