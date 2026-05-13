//! Finance feature route. Tabs over a shared `CrdtDoc`:
//!   * Revenue   → `RevenueDashboard`     (money in)
//!   * Assets    → `FinancialAssetDashboard` (capital holdings)
//! Both repos hang off the same ephemeral document so they share one
//! WebSocket session.

use std::rc::Rc;

use architect::Page;
use dioxus::prelude::*;
use finance_crdt::{CrdtDoc, FinancialAssetRepoLoro, RevenueRepoLoro};
use finance_proto::{
    FinancialAsset, FinancialAssetCreate, FinancialAssetRepo, Revenue, RevenueCreate, RevenueRepo,
};
use finance_ui::{FinancialAssetDashboard, RevenueDashboard};
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn RevenueView() -> Element {
    // Share a single ephemeral CrdtDoc across both repos so they
    // sync against the same WebSocket session.
    let revenue_repo: Rc<RevenueRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(RevenueRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(revenue_repo.doc().clone())));
    let asset_repo: Rc<FinancialAssetRepoLoro> =
        use_hook(|| Rc::new(FinancialAssetRepoLoro::new(&doc)));

    let mut items = use_signal::<Vec<Revenue>>(Vec::new);
    let mut assets = use_signal::<Vec<FinancialAsset>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());
    let mut tab = use_signal(|| "revenue".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let revenue = revenue_repo.clone();
        let asset = asset_repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                let mk = || Page {
                    index: 0,
                    size: 200,
                };
                if let Ok(list) = revenue.list(mk(), None, None).await {
                    items.set(list.items);
                }
                if let Ok(list) = asset.list(mk(), None, None).await {
                    assets.set(list.items);
                }
            }
        });
        tx
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    // Seed a handful of demo financial assets the first time the view
    // mounts, mirroring how the project route seeds tasks. Idempotent
    // enough for a dev tool — we only seed if the list is currently
    // empty after the first refresh round-trips.
    {
        let asset_repo = asset_repo.clone();
        let tx = refresh_tx.clone();
        use_effect(move || {
            let asset_repo = asset_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                // Give the WS hydration a beat to land.
                let already = asset_repo
                    .list(Page { index: 0, size: 1 }, None, None)
                    .await
                    .map(|l| l.total > 0)
                    .unwrap_or(false);
                if already {
                    return;
                }
                for sample in sample_assets() {
                    let _ = asset_repo.create(sample).await;
                }
                let _ = tx.unbounded_send(());
            });
        });
    }

    let on_revenue_create = {
        let repo = revenue_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: RevenueCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_revenue_delete = {
        let repo = revenue_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_asset_create = {
        let repo = asset_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: FinancialAssetCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_asset_delete = {
        let repo = asset_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    rsx! {
        div { class: "mx-auto flex max-w-6xl flex-col gap-4 p-6 lg:p-10",
            Tabs {
                value: Some(tab.read().clone()),
                on_change: move |v: String| tab.set(v),
                TabList {
                    TabTrigger { value: "revenue", index: 0usize, "Revenue" }
                    TabTrigger { value: "assets", index: 1usize, "Assets" }
                }
                TabContent { value: "revenue", index: 0usize,
                    RevenueDashboard {
                        items: items(),
                        status: status_msg(),
                        on_create: on_revenue_create,
                        on_delete: on_revenue_delete,
                    }
                }
                TabContent { value: "assets", index: 1usize,
                    FinancialAssetDashboard {
                        items: assets(),
                        status: status_msg(),
                        on_create: on_asset_create,
                        on_delete: on_asset_delete,
                    }
                }
            }
        }
    }
}

/// ~10 hand-curated demo assets across kinds so the dashboard has
/// something to render at zero-friction. Picked to exercise the
/// delta-badge, monthly-income chip, and sold-line styling paths.
fn sample_assets() -> Vec<FinancialAssetCreate> {
    use chrono::{Duration, Utc};
    let now = Utc::now();
    let years_ago = |y: i64| Some(now - Duration::days(365 * y));
    let mk = |name: &str,
              kind: &str,
              symbol: Option<&str>,
              purchase: Option<i64>,
              current: Option<i64>,
              monthly: Option<i64>,
              account: &str,
              purchase_date: Option<chrono::DateTime<Utc>>,
              sold_date: Option<chrono::DateTime<Utc>>,
              tags: &[&str]| FinancialAssetCreate {
        name: name.to_string(),
        kind: kind.to_string(),
        symbol: symbol.map(String::from),
        purchase_price_cents: purchase,
        current_value_cents: current,
        quantity_thousandths: None,
        currency: "USD".into(),
        purchase_date,
        sold_date,
        monthly_income_cents: monthly,
        account: Some(account.into()),
        owner: Some("self".into()),
        notes: None,
        tags: tags.iter().map(|s| s.to_string()).collect(),
    };

    vec![
        mk(
            "Maple Ave Property",
            "real-estate",
            None,
            Some(28_500_000_00),
            Some(41_200_000_00),
            Some(2_400_00),
            "Property Holdings LLC",
            years_ago(7),
            None,
            &["long-term", "income"],
        ),
        mk(
            "Lakeside Cabin",
            "real-estate",
            None,
            Some(18_000_000_00),
            Some(22_000_000_00),
            Some(1_200_00),
            "Property Holdings LLC",
            years_ago(4),
            None,
            &["long-term", "illiquid"],
        ),
        mk(
            "VTI Holdings",
            "stock",
            Some("VTI"),
            Some(95_000_00),
            Some(143_500_00),
            None,
            "Schwab Brokerage",
            years_ago(5),
            None,
            &["core", "growth"],
        ),
        mk(
            "NVIDIA Inc",
            "stock",
            Some("NVDA"),
            Some(40_000_00),
            Some(155_000_00),
            None,
            "Schwab Brokerage",
            years_ago(3),
            None,
            &["speculative", "growth"],
        ),
        mk(
            "Series I Bonds",
            "bond",
            Some("SIBOND"),
            Some(10_000_00),
            Some(11_300_00),
            Some(45_00),
            "Treasury Direct",
            years_ago(2),
            None,
            &["income", "tax-advantaged"],
        ),
        mk(
            "BTC Cold Storage",
            "crypto",
            Some("BTC"),
            Some(80_000_00),
            Some(225_000_00),
            None,
            "Ledger Cold",
            years_ago(6),
            None,
            &["speculative", "long-term"],
        ),
        mk(
            "ETH Staking Wallet",
            "crypto",
            Some("ETH"),
            Some(35_000_00),
            Some(28_500_00),
            Some(95_00),
            "Coinbase",
            years_ago(2),
            None,
            &["speculative", "income"],
        ),
        mk(
            "Tesla Model Y",
            "vehicle",
            None,
            Some(52_000_00),
            Some(38_000_00),
            None,
            "Personal",
            years_ago(2),
            None,
            &["short-term"],
        ),
        mk(
            "Studio A Mixer",
            "equipment",
            None,
            Some(18_500_00),
            Some(14_000_00),
            None,
            "Studio LLC",
            years_ago(3),
            None,
            &["illiquid"],
        ),
        mk(
            "Emergency Fund",
            "cash",
            None,
            Some(50_000_00),
            Some(50_000_00),
            Some(200_00),
            "Personal Checking",
            years_ago(1),
            None,
            &["core"],
        ),
        mk(
            "Old AAPL Position",
            "stock",
            Some("AAPL"),
            Some(12_000_00),
            Some(18_500_00),
            None,
            "Schwab Brokerage",
            years_ago(6),
            Some(now - Duration::days(120)),
            &["short-term"],
        ),
    ]
}
