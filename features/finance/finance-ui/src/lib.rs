//! Finance feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 covers `Revenue` and `FinancialAsset`; `Expense` UI lands later.
//!
//! Revenue:
//! - [`RevenueList`]              — full collection view, dispatches `on_delete`
//! - [`RevenueRow`]               — single-row presentation
//! - [`RevenueCreateForm`]        — minimal new-revenue form
//! - [`RevenueDashboard`]         — page-level dashboard
//!
//! FinancialAsset (capital holdings — real estate, equities, crypto, gear):
//! - [`FinancialAssetList`]       — flat row list for table mode
//! - [`FinancialAssetRow`]        — single row
//! - [`FinancialAssetCard`]       — card-grid tile with delta badges
//! - [`FinancialAssetCreateForm`] — Card-based form with all fields
//! - [`FinancialAssetDashboard`]  — page-level dashboard with kind tabs + search

use chrono::{Duration, Utc};
use dioxus::prelude::*;
use finance_proto::{
    FINANCIAL_ASSET_KINDS, FinancialAsset, FinancialAssetCreate, Revenue, RevenueCreate,
};
use fts_ui::lucide_dioxus::{
    ArrowDown, ArrowUp, Asterisk, Bitcoin, Boxes, Building, CalendarDays, Car, DollarSign, House,
    Plus, Search, Trash2, TrendingUp, Wallet,
};
use fts_ui::prelude::*;
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn RevenueList(items: Vec<Revenue>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No revenue yet. Add one above.",
                icon: rsx! { DollarSign { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for rev in items.iter().cloned() {
                RevenueRow {
                    key: "{rev.id}",
                    rev: rev.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn RevenueRow(rev: Revenue, on_delete: EventHandler<Uuid>) -> Element {
    let id = rev.id;
    let meta = format!(
        "{} {} · {}",
        format_cents(rev.amount_cents),
        rev.currency,
        rev.received_at.format("%Y-%m-%d")
    );
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{rev.source}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn RevenueCreateForm(on_submit: EventHandler<RevenueCreate>) -> Element {
    let mut source = use_signal(String::new);
    let mut amount = use_signal(String::new);
    let mut currency = use_signal(|| "USD".to_string());

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add revenue" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: source,
                        placeholder: "Source (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: amount,
                        placeholder: "Amount (cents)",
                        class: "w-32",
                    }
                    Input {
                        value: currency,
                        placeholder: "USD",
                        class: "w-24",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let s = source.read().clone();
                            let a = amount.read().clone();
                            let c = currency.read().clone();
                            if s.trim().is_empty() || c.trim().is_empty() {
                                return;
                            }
                            let amount_cents = match a.trim().parse::<i64>() {
                                Ok(v) => v,
                                Err(_) => return,
                            };
                            let payload = RevenueCreate {
                                source: s,
                                client_id: None,
                                invoice_id: None,
                                amount_cents,
                                currency: c,
                                received_at: Utc::now(),
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            source.set(String::new());
                            amount.set(String::new());
                            currency.set("USD".into());
                        },
                        Plus { size: 14 }
                        " Add revenue"
                    }
                }
            }
        }
    }
}

fn format_cents(c: i64) -> String {
    format!("{:.2}", c as f64 / 100.0)
}

/// Purpose-built revenue dashboard. Composes summary stats, a currency tab
/// filter, the create form, and the revenue list into a feature page.
#[component]
pub fn RevenueDashboard(
    items: Vec<Revenue>,
    status: String,
    on_create: EventHandler<RevenueCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut currency_filter = use_signal(|| "all".to_string());

    let total_cents: i64 = items.iter().map(|r| r.amount_cents).sum();
    let count = items.len();

    let now = Utc::now();
    let cutoff_30 = now - Duration::days(30);
    let last_30_cents: i64 = items
        .iter()
        .filter(|r| r.received_at >= cutoff_30)
        .map(|r| r.amount_cents)
        .sum();

    // Currency breakdown (sorted)
    let mut by_currency: BTreeMap<String, i64> = BTreeMap::new();
    for r in &items {
        *by_currency.entry(r.currency.clone()).or_insert(0) += r.amount_cents;
    }
    let primary_currency = by_currency
        .iter()
        .max_by_key(|(_, v)| **v)
        .map(|(k, _)| k.clone())
        .unwrap_or_else(|| "USD".into());

    let currencies: Vec<String> = by_currency.keys().cloned().collect();
    let current_filter = currency_filter.read().clone();
    let filtered: Vec<Revenue> = if current_filter == "all" {
        items.clone()
    } else {
        items
            .iter()
            .filter(|r| r.currency == current_filter)
            .cloned()
            .collect()
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Revenue",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-emerald-500/10 p-2 text-emerald-500",
                    DollarSign { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Revenue dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Track money in. Add new revenue records and watch totals roll up live."
                    }
                }
            }

            // Stat cards row
            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total revenue" }
                            Wallet { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{primary_currency} {format_cents(total_cents)}"
                        }
                        Text { variant: TextVariant::Muted, "{count} record(s)" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Last 30 days" }
                            CalendarDays { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2,
                            "{primary_currency} {format_cents(last_30_cents)}"
                        }
                        Text { variant: TextVariant::Muted, "rolling window" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Currencies" }
                            TrendingUp { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{by_currency.len()}" }
                        Text { variant: TextVariant::Muted, "distinct" }
                    }
                }
            }

            // Currency filter tabs (only when more than one)
            if currencies.len() > 1 {
                HStack { class: "gap-2 flex-wrap items-center",
                    Text { variant: TextVariant::Muted, "Filter:" }
                    Button {
                        variant: if current_filter == "all" {
                            ButtonVariant::Primary
                        } else {
                            ButtonVariant::Outline
                        },
                        size: ButtonSize::Small,
                        on_click: move |_| currency_filter.set("all".into()),
                        "All"
                    }
                    for c in currencies.iter().cloned() {
                        Button {
                            key: "{c}",
                            variant: if current_filter == c {
                                ButtonVariant::Primary
                            } else {
                                ButtonVariant::Outline
                            },
                            size: ButtonSize::Small,
                            on_click: {
                                let c = c.clone();
                                move |_| currency_filter.set(c.clone())
                            },
                            "{c}"
                        }
                    }
                }
            }

            RevenueCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Entries",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            RevenueList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

// ═════════════════════════════════════════════════════════════════════════
// FinancialAsset — capital holdings UI
// ═════════════════════════════════════════════════════════════════════════

/// Pretty-format cents as "$X.YYk / $X.YYM" (or plain dollars when small).
fn format_money_compact(cents: i64) -> String {
    let dollars = cents as f64 / 100.0;
    let abs = dollars.abs();
    let sign = if dollars < 0.0 { "-" } else { "" };
    if abs >= 1_000_000.0 {
        format!("{sign}${:.2}M", abs / 1_000_000.0)
    } else if abs >= 1_000.0 {
        format!("{sign}${:.1}k", abs / 1_000.0)
    } else {
        format!("{sign}${:.2}", abs)
    }
}

/// Render the kind-glyph for an asset card. Uses lucide for everything.
#[component]
fn AssetKindIcon(kind: String, #[props(default = 18usize)] size: usize) -> Element {
    match kind.as_str() {
        "real-estate" => rsx! { House { size: size } },
        "stock" | "bond" => rsx! { TrendingUp { size: size } },
        "crypto" => rsx! { Bitcoin { size: size } },
        "vehicle" => rsx! { Car { size: size } },
        "equipment" => rsx! { Boxes { size: size } },
        "cash" => rsx! { Wallet { size: size } },
        _ => rsx! { Asterisk { size: size } },
    }
}

#[component]
pub fn FinancialAssetRow(asset: FinancialAsset, on_delete: EventHandler<Uuid>) -> Element {
    let id = asset.id;
    let sold = asset.sold_date.is_some();
    let value_label = asset
        .current_value_cents
        .map(format_money_compact)
        .unwrap_or_else(|| "—".into());
    let kind_label = asset.kind.clone();
    let symbol = asset.symbol.clone().unwrap_or_default();
    let meta = if symbol.is_empty() {
        format!("{kind_label} · {} {value_label}", asset.currency)
    } else {
        format!("{kind_label} · {symbol} · {} {value_label}", asset.currency)
    };
    let title_class = if sold {
        "line-through text-muted-foreground"
    } else {
        ""
    };
    rsx! {
        Item {
            ItemMedia {
                AssetKindIcon { kind: asset.kind.clone(), size: 20usize }
            }
            ItemContent {
                ItemTitle { class: title_class, "{asset.name}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn FinancialAssetList(items: Vec<FinancialAsset>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No financial assets yet. Add one above.",
                icon: rsx! { Building { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for a in items.iter().cloned() {
                FinancialAssetRow {
                    key: "{a.id}",
                    asset: a.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn FinancialAssetCard(asset: FinancialAsset, on_delete: EventHandler<Uuid>) -> Element {
    let id = asset.id;
    let sold = asset.sold_date.is_some();
    let value_str = asset
        .current_value_cents
        .map(format_money_compact)
        .unwrap_or_else(|| "—".into());
    let purchase = asset.purchase_price_cents;
    let current = asset.current_value_cents;
    let delta_cents: Option<i64> = match (purchase, current) {
        (Some(p), Some(c)) => Some(c - p),
        _ => None,
    };
    let delta_pct: Option<f64> = match (purchase, current) {
        (Some(p), Some(c)) if p != 0 => Some(((c - p) as f64 / p as f64) * 100.0),
        _ => None,
    };
    let monthly = asset.monthly_income_cents.filter(|v| *v > 0);
    let symbol = asset.symbol.clone();
    let account = asset.account.clone();
    let title_class = if sold {
        "line-through text-muted-foreground"
    } else {
        ""
    };

    let muted_line = match (symbol.as_deref(), account.as_deref()) {
        (Some(s), Some(a)) => format!("{s} · {a}"),
        (Some(s), None) => s.to_string(),
        (None, Some(a)) => a.to_string(),
        (None, None) => String::new(),
    };

    rsx! {
        Card {
            CardHeader {
                HStack { class: "items-center justify-between gap-2",
                    HStack { class: "items-center gap-2",
                        div { class: "rounded-md bg-muted p-1.5",
                            AssetKindIcon { kind: asset.kind.clone(), size: 18usize }
                        }
                        VStack { class: "gap-0",
                            CardTitle { class: title_class, "{asset.name}" }
                            if !muted_line.is_empty() {
                                Text { variant: TextVariant::Muted, "{muted_line}" }
                            }
                        }
                    }
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| on_delete.call(id),
                        Trash2 { size: 14 }
                    }
                }
            }
            CardContent { class: "flex flex-col gap-2",
                HStack { class: "items-baseline gap-2",
                    Heading { level: HeadingLevel::H2, "{value_str}" }
                    Text { variant: TextVariant::Muted, "{asset.currency}" }
                }
                HStack { class: "items-center gap-2 flex-wrap",
                    if let Some(d) = delta_cents {
                        if d >= 0 {
                            Badge {
                                variant: BadgeVariant::Default,
                                ArrowUp { size: 12 }
                                {
                                    let pct = delta_pct.map(|p| format!(" {:+.1}%", p)).unwrap_or_default();
                                    format!(" {}{}", format_money_compact(d), pct)
                                }
                            }
                        } else {
                            Badge {
                                variant: BadgeVariant::Destructive,
                                ArrowDown { size: 12 }
                                {
                                    let pct = delta_pct.map(|p| format!(" {:+.1}%", p)).unwrap_or_default();
                                    format!(" {}{}", format_money_compact(d), pct)
                                }
                            }
                        }
                    }
                    if let Some(m) = monthly {
                        Badge { variant: BadgeVariant::Secondary,
                            Wallet { size: 12 }
                            {format!(" {}/mo", format_money_compact(m))}
                        }
                    }
                    if let Some(sold_at) = asset.sold_date {
                        StatusBadge {
                            variant: StatusBadgeVariant::Neutral,
                            label: format!("Sold {}", sold_at.format("%Y-%m-%d")),
                        }
                    }
                }
                if !asset.tags.is_empty() {
                    HStack { class: "items-center gap-1 flex-wrap",
                        for t in asset.tags.iter().cloned() {
                            Badge { key: "{t}", variant: BadgeVariant::Outline, "{t}" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
pub fn FinancialAssetCreateForm(on_submit: EventHandler<FinancialAssetCreate>) -> Element {
    let mut name = use_signal(String::new);
    let kind = use_signal(|| "stock".to_string());
    let mut symbol = use_signal(String::new);
    let mut purchase = use_signal(String::new);
    let mut current = use_signal(String::new);
    let currency = use_signal(|| "USD".to_string());
    let mut account = use_signal(String::new);
    let mut owner = use_signal(String::new);
    let mut notes = use_signal(String::new);
    let mut purchase_date = use_signal(String::new); // YYYY-MM-DD free entry; v1 keeps it simple

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add financial asset" }
                CardDescription {
                    "Real estate, public equities, crypto, gear, or any capital holding."
                }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (e.g. Maple Ave Property)",
                        class: "flex-1 min-w-48",
                    }
                    div { class: "w-40",
                        Combobox {
                            value: kind,
                            placeholder: "Kind".to_string(),
                            ComboboxTrigger { placeholder: "Kind".to_string() }
                            ComboboxContent {
                                for k in FINANCIAL_ASSET_KINDS.iter() {
                                    ComboboxItem { key: "{k}", value: k.to_string(),
                                        "{k}"
                                    }
                                }
                                ComboboxEmpty { "No match" }
                            }
                        }
                    }
                    Input {
                        value: symbol,
                        placeholder: "Symbol / ticker (optional)",
                        class: "w-44",
                    }
                }
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: purchase,
                        placeholder: "Purchase $ (cents)",
                        class: "w-40",
                    }
                    Input {
                        value: current,
                        placeholder: "Current value $ (cents)",
                        class: "w-44",
                    }
                    Input {
                        value: currency,
                        placeholder: "USD",
                        class: "w-24",
                    }
                }
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: account,
                        placeholder: "Account (Schwab, Coinbase, …)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: owner,
                        placeholder: "Owner (self / LLC / trust)",
                        class: "w-40",
                    }
                    Input {
                        value: purchase_date,
                        placeholder: "Purchase date YYYY-MM-DD",
                        class: "w-44",
                    }
                }
                Textarea {
                    value: notes,
                    placeholder: "Notes (optional)",
                    rows: 2u32,
                }
                HStack { class: "items-center justify-end",
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            let k = kind.read().clone();
                            let cur = currency.read().clone();
                            if n.trim().is_empty() || k.trim().is_empty() {
                                return;
                            }
                            let parse_cents = |s: &str| -> Option<i64> {
                                let s = s.trim();
                                if s.is_empty() { None } else { s.parse::<i64>().ok() }
                            };
                            let p_date = {
                                let s = purchase_date.read().trim().to_string();
                                if s.is_empty() {
                                    None
                                } else {
                                    chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                                        .ok()
                                        .and_then(|d| d.and_hms_opt(0, 0, 0))
                                        .map(|ndt| chrono::DateTime::<Utc>::from_naive_utc_and_offset(ndt, Utc))
                                }
                            };
                            let symbol_val = {
                                let s = symbol.read().trim().to_string();
                                if s.is_empty() { None } else { Some(s) }
                            };
                            let account_val = {
                                let s = account.read().trim().to_string();
                                if s.is_empty() { None } else { Some(s) }
                            };
                            let owner_val = {
                                let s = owner.read().trim().to_string();
                                if s.is_empty() { None } else { Some(s) }
                            };
                            let notes_val = {
                                let s = notes.read().trim().to_string();
                                if s.is_empty() { None } else { Some(s) }
                            };
                            let payload = FinancialAssetCreate {
                                name: n,
                                kind: k,
                                symbol: symbol_val,
                                purchase_price_cents: parse_cents(&purchase.read()),
                                current_value_cents: parse_cents(&current.read()),
                                quantity_thousandths: None,
                                currency: if cur.trim().is_empty() { "USD".into() } else { cur },
                                purchase_date: p_date,
                                sold_date: None,
                                monthly_income_cents: None,
                                account: account_val,
                                owner: owner_val,
                                notes: notes_val,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            symbol.set(String::new());
                            purchase.set(String::new());
                            current.set(String::new());
                            account.set(String::new());
                            owner.set(String::new());
                            notes.set(String::new());
                            purchase_date.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add asset"
                    }
                }
            }
        }
    }
}

/// Page-level dashboard. Composes stats, kind tabs, search toolbar, create
/// form, and the card grid.
#[component]
pub fn FinancialAssetDashboard(
    items: Vec<FinancialAsset>,
    status: String,
    on_create: EventHandler<FinancialAssetCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut tab = use_signal(|| "all".to_string());
    let query = use_signal(String::new);

    // Summary stats
    let total_value: i64 = items
        .iter()
        .filter(|a| a.sold_date.is_none())
        .map(|a| a.current_value_cents.unwrap_or(0))
        .sum();
    let monthly_income: i64 = items
        .iter()
        .filter(|a| a.sold_date.is_none())
        .map(|a| a.monthly_income_cents.unwrap_or(0))
        .sum();
    let held = items.iter().filter(|a| a.sold_date.is_none()).count();
    let sold = items.len() - held;

    // Kind histogram (stable kind order from FINANCIAL_ASSET_KINDS)
    let mut by_kind: BTreeMap<&str, usize> = BTreeMap::new();
    for a in &items {
        let key = FINANCIAL_ASSET_KINDS
            .iter()
            .copied()
            .find(|k| *k == a.kind.as_str())
            .unwrap_or("other");
        *by_kind.entry(key).or_insert(0) += 1;
    }
    let kind_total: usize = by_kind.values().sum();

    let current_tab = tab.read().clone();
    let q = query.read().to_lowercase();
    let filtered: Vec<FinancialAsset> = items
        .iter()
        .filter(|a| match current_tab.as_str() {
            "all" => true,
            "sold" => a.sold_date.is_some(),
            kind => a.kind == kind && a.sold_date.is_none(),
        })
        .filter(|a| {
            if q.is_empty() {
                return true;
            }
            let hay = format!(
                "{} {} {}",
                a.name.to_lowercase(),
                a.symbol.clone().unwrap_or_default().to_lowercase(),
                a.account.clone().unwrap_or_default().to_lowercase()
            );
            hay.contains(&q)
        })
        .cloned()
        .collect();

    // Tab buttons (kind shortcuts).
    let tabs: &[(&str, &str)] = &[
        ("all", "All"),
        ("real-estate", "Real estate"),
        ("stock", "Stocks"),
        ("crypto", "Crypto"),
        ("equipment", "Equipment"),
        ("cash", "Cash"),
        ("sold", "Sold"),
    ];

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Financial assets",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-blue-500/10 p-2 text-blue-500",
                    Building { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Holdings dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Real estate, equities, crypto, and gear — synced live across devices."
                    }
                }
            }

            // Stat cards
            div { class: "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total holdings" }
                            Wallet { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{format_money_compact(total_value)}" }
                        Text { variant: TextVariant::Muted, "{held} held" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Monthly income" }
                            CalendarDays { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{format_money_compact(monthly_income)}" }
                        Text { variant: TextVariant::Muted, "rent + dividends" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "By kind" }
                            TrendingUp { size: 16 }
                        }
                    }
                    CardContent { class: "flex flex-col gap-2",
                        // Inline stacked horizontal bar
                        div { class: "flex h-2 w-full overflow-hidden rounded bg-muted",
                            for (idx, (k, n)) in by_kind.iter().enumerate() {
                                {
                                    let pct = if kind_total > 0 {
                                        (*n as f64 / kind_total as f64) * 100.0
                                    } else { 0.0 };
                                    let palette = [
                                        "bg-blue-500", "bg-emerald-500", "bg-amber-500",
                                        "bg-violet-500", "bg-pink-500", "bg-cyan-500",
                                        "bg-rose-500", "bg-lime-500",
                                    ];
                                    let cls = palette[idx % palette.len()];
                                    rsx! {
                                        div {
                                            key: "{k}",
                                            class: "{cls}",
                                            style: "width: {pct}%;",
                                            title: "{k}: {n}",
                                        }
                                    }
                                }
                            }
                        }
                        Text { variant: TextVariant::Muted, "{by_kind.len()} kind(s)" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Held / Sold" }
                            DollarSign { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{held} / {sold}" }
                        Text { variant: TextVariant::Muted, "{items.len()} total" }
                    }
                }
            }

            // Filter tabs
            HStack { class: "gap-2 flex-wrap items-center",
                for (val, label) in tabs.iter().copied() {
                    Button {
                        key: "{val}",
                        variant: if current_tab == val {
                            ButtonVariant::Primary
                        } else {
                            ButtonVariant::Outline
                        },
                        size: ButtonSize::Small,
                        on_click: {
                            let v = val.to_string();
                            move |_| tab.set(v.clone())
                        },
                        "{label}"
                    }
                }
            }

            // Search toolbar
            HStack { class: "gap-2 items-center",
                div { class: "flex items-center gap-1 text-muted-foreground",
                    Search { size: 14 }
                }
                Input {
                    value: query,
                    placeholder: "Search name, symbol, account…",
                    class: "max-w-sm",
                }
            }

            FinancialAssetCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Holdings",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }

            if filtered.is_empty() {
                EmptyState {
                    message: "Nothing matches that filter.",
                    icon: rsx! { Building { size: 32 } },
                }
            } else {
                div { class: "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-3",
                    for a in filtered.iter().cloned() {
                        FinancialAssetCard {
                            key: "{a.id}",
                            asset: a.clone(),
                            on_delete: move |id| on_delete.call(id),
                        }
                    }
                }
            }
        }
    }
}
