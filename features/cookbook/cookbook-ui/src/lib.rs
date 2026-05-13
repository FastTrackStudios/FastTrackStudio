//! Cookbook feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 scope: `Recipe` + `PantryItem` (pantry/food-product/shopping-list demos).

use chrono::{Duration, Utc};
use cookbook_proto::{
    FoodProduct, FoodProductCreate, PantryItem, PantryItemCreate, Recipe, RecipeCreate,
    ShoppingListItem, ShoppingListItemCreate,
};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Boxes, ChefHat, Clock, CookingPot, MapPin, Package, Plus, ShoppingCart, Soup, Trash2,
    TriangleAlert, UtensilsCrossed,
};
use fts_ui::prelude::*;
use std::collections::BTreeSet;
use uuid::Uuid;

#[component]
pub fn RecipeList(items: Vec<Recipe>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No recipes yet. Add one above.",
                icon: rsx! { ChefHat { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for recipe in items.iter().cloned() {
                RecipeRow {
                    key: "{recipe.id}",
                    recipe: recipe.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn RecipeRow(recipe: Recipe, on_delete: EventHandler<Uuid>) -> Element {
    let id = recipe.id;
    let cuisine = recipe.cuisine.clone().unwrap_or_else(|| "recipe".into());
    let total = recipe
        .total_time_minutes
        .map(|m| format!("{}m", m))
        .unwrap_or_else(|| "—".into());
    let meta = format!("{} · {}", cuisine, total);
    let servings = recipe.servings;
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{recipe.name}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                if let Some(s) = servings {
                    Badge { variant: BadgeVariant::Secondary, "{s} servings" }
                }
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
pub fn RecipeDashboard(
    items: Vec<Recipe>,
    status: String,
    on_create: EventHandler<RecipeCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let cuisines: BTreeSet<String> = items.iter().filter_map(|r| r.cuisine.clone()).collect();
    let total_servings: u32 = items.iter().filter_map(|r| r.servings).sum();
    let quick = items
        .iter()
        .filter(|r| r.total_time_minutes.map(|m| m <= 30).unwrap_or(false))
        .count();
    let avg_time: u32 = {
        let timed: Vec<u32> = items.iter().filter_map(|r| r.total_time_minutes).collect();
        if timed.is_empty() {
            0
        } else {
            timed.iter().sum::<u32>() / timed.len() as u32
        }
    };
    let mut tab = use_signal(|| "all".to_string());

    let filtered: Vec<Recipe> = match tab.read().as_str() {
        "quick" => items
            .iter()
            .filter(|r| r.total_time_minutes.map(|m| m <= 30).unwrap_or(false))
            .cloned()
            .collect(),
        "long" => items
            .iter()
            .filter(|r| r.total_time_minutes.map(|m| m > 60).unwrap_or(false))
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-6 p-6 lg:p-10",
            SectionHeader {
                label: "Cookbook".to_string(),
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            HStack { class: "items-center gap-3",
                div { class: "rounded-md bg-amber-500/10 p-2 text-amber-600",
                    ChefHat { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Kitchen" }
                    Text { variant: TextVariant::Muted,
                        "Your shared cookbook — recipes, cuisines, and cook times at a glance."
                    }
                }
            }
            div { class: "grid gap-3 sm:grid-cols-2 lg:grid-cols-4",
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Recipes" }
                        UtensilsCrossed { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{total}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "{total_servings} total servings" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Cuisines" }
                        Soup { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{cuisines.len()}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "distinct cuisines" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Quick (≤30m)" }
                        Clock { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{quick}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "weeknight-friendly" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Avg time" }
                        CookingPot { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{avg_time}m" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "across timed recipes" }
                    }
                }
            }

            RecipeCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Recipes" }
                SegmentedControl {
                    value: tab.read().clone(),
                    on_change: move |v: String| tab.set(v),
                    options: vec![
                        ("all".to_string(), "All".to_string()),
                        ("quick".to_string(), "Quick".to_string()),
                        ("long".to_string(), "Long cooks".to_string()),
                    ],
                }
            }
            RecipeList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn RecipeCreateForm(on_submit: EventHandler<RecipeCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut summary = use_signal(String::new);
    let mut servings = use_signal(String::new);
    let mut cuisine = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add a recipe" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: servings,
                        placeholder: "Servings",
                        class: "w-28",
                    }
                    Input {
                        value: cuisine,
                        placeholder: "Cuisine",
                        class: "flex-1 min-w-40",
                    }
                }
                Textarea {
                    value: summary,
                    placeholder: "Summary",
                    rows: 2u32,
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let servings_val: Option<u32> = {
                                let s = servings.read().clone();
                                let s = s.trim();
                                if s.is_empty() {
                                    None
                                } else {
                                    match s.parse() {
                                        Ok(v) => Some(v),
                                        Err(_) => return,
                                    }
                                }
                            };
                            let payload = RecipeCreate {
                                cookbook_id: None,
                                name: n,
                                summary: trim_to_option(summary.read().clone()),
                                servings: servings_val,
                                prep_time_minutes: None,
                                cook_time_minutes: None,
                                total_time_minutes: None,
                                cuisine: trim_to_option(cuisine.read().clone()),
                                source_url: None,
                                image_url: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            summary.set(String::new());
                            servings.set(String::new());
                            cuisine.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add recipe"
                    }
                }
            }
        }
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

// ── PantryItem ────────────────────────────────────────────────────────

#[component]
pub fn PantryItemList(items: Vec<PantryItem>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No pantry items yet. Add one above.",
                icon: rsx! { Package { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for item in items.iter().cloned() {
                PantryItemRow {
                    key: "{item.id}",
                    item: item.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn PantryItemRow(item: PantryItem, on_delete: EventHandler<Uuid>) -> Element {
    let id = item.id;
    let qty = format_qty(item.qty_thousandths, &item.unit);
    let location = item.location.clone().unwrap_or_else(|| "pantry".into());
    let meta = format!("{} · {}", qty, location);
    let name = item.name.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{name}" }
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
pub fn PantryItemCreateForm(on_submit: EventHandler<PantryItemCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut qty = use_signal(String::new);
    let mut unit = use_signal(|| "g".to_string());

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Add pantry item" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: qty,
                        placeholder: "Qty",
                        class: "w-28",
                    }
                    Input {
                        value: unit,
                        placeholder: "Unit",
                        class: "w-24",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let q_str = qty.read().clone();
                            let q_f: f64 = match q_str.trim().parse() {
                                Ok(v) => v,
                                Err(_) => return,
                            };
                            let u = unit.read().clone();
                            if u.trim().is_empty() {
                                return;
                            }
                            let payload = PantryItemCreate {
                                product_id: None,
                                name: n,
                                qty_thousandths: (q_f * 1000.0) as i64,
                                unit: u,
                                location: Some("pantry".into()),
                                expires_at: None,
                                opened_at: None,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            qty.set(String::new());
                            unit.set("g".into());
                        },
                        Plus { size: 14 }
                        " Add item"
                    }
                }
            }
        }
    }
}

#[component]
pub fn PantryItemDashboard(
    items: Vec<PantryItem>,
    status: String,
    on_create: EventHandler<PantryItemCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut tab = use_signal(|| "all".to_string());

    let total = items.len();
    let now = Utc::now();
    let soon = now + Duration::days(7);

    let expiring: Vec<&PantryItem> = items
        .iter()
        .filter(|i| matches!(i.expires_at, Some(e) if e <= soon))
        .collect();
    let expiring_count = expiring.len();

    let locations: BTreeSet<String> = items.iter().filter_map(|i| i.location.clone()).collect();

    let opened_count = items.iter().filter(|i| i.opened_at.is_some()).count();

    let current = tab.read().clone();
    let filtered: Vec<PantryItem> = match current.as_str() {
        "expiring" => items
            .iter()
            .filter(|i| matches!(i.expires_at, Some(e) if e <= soon))
            .cloned()
            .collect(),
        "open" => items
            .iter()
            .filter(|i| i.opened_at.is_some())
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Pantry",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: if expiring_count > 0 { StatusDotColor::Warning } else { StatusDotColor::Success },
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-amber-500/10 p-2 text-amber-500",
                    Package { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Pantry & inventory" }
                    Text { variant: TextVariant::Muted,
                        "What's on hand, what's running low, and what's about to expire."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total items" }
                            Boxes { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{total}" }
                        Text { variant: TextVariant::Muted, "in pantry" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Expiring ≤7d" }
                            TriangleAlert { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{expiring_count}" }
                        Text { variant: TextVariant::Muted, "needs attention" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Open" }
                            Clock { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{opened_count}" }
                        Text { variant: TextVariant::Muted, "in use" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Locations" }
                            MapPin { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{locations.len()}" }
                        Text { variant: TextVariant::Muted, "distinct" }
                    }
                }
            }

            HStack { class: "gap-2 items-center",
                Text { variant: TextVariant::Muted, "Filter:" }
                Button {
                    variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("all".into()),
                    "All"
                }
                Button {
                    variant: if current == "expiring" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("expiring".into()),
                    "Expiring soon"
                }
                Button {
                    variant: if current == "open" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("open".into()),
                    "Open"
                }
            }

            PantryItemCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Items",
                trailing: rsx! { Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" } },
            }
            PantryItemList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

// ── FoodProduct ───────────────────────────────────────────────────────

#[component]
pub fn FoodProductList(items: Vec<FoodProduct>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No food products yet.",
                icon: rsx! { Boxes { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for item in items.iter().cloned() {
                FoodProductRow {
                    key: "{item.id}",
                    item: item.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn FoodProductRow(item: FoodProduct, on_delete: EventHandler<Uuid>) -> Element {
    let id = item.id;
    let brand = item.brand.clone().unwrap_or_else(|| "—".into());
    let category = item
        .category
        .clone()
        .unwrap_or_else(|| "uncategorized".into());
    let meta = format!("{} · {}", brand, category);
    let name = item.name.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{name}" }
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
pub fn FoodProductCreateForm(on_submit: EventHandler<FoodProductCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut brand = use_signal(String::new);
    let mut category = use_signal(String::new);

    rsx! {
        Card {
            CardHeader { CardTitle { "Add food product" } }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input { value: name, placeholder: "Name (required)", class: "flex-1 min-w-40" }
                    Input { value: brand, placeholder: "Brand", class: "flex-1 min-w-40" }
                    Input { value: category, placeholder: "Category", class: "w-32" }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() { return; }
                            let payload = FoodProductCreate {
                                name: n,
                                brand: trim_to_option(brand.read().clone()),
                                category: trim_to_option(category.read().clone()),
                                barcode: None,
                                default_unit: None,
                                default_qty_thousandths: None,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            brand.set(String::new());
                            category.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add product"
                    }
                }
            }
        }
    }
}

#[component]
pub fn FoodProductDashboard(
    items: Vec<FoodProduct>,
    status: String,
    on_create: EventHandler<FoodProductCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let categories: BTreeSet<String> = items.iter().filter_map(|p| p.category.clone()).collect();
    let brands: BTreeSet<String> = items.iter().filter_map(|p| p.brand.clone()).collect();

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Food products",
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Products" }
                            Boxes { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{total}" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Categories" }
                            Soup { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{categories.len()}" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Brands" }
                            ChefHat { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{brands.len()}" }
                    }
                }
            }
            FoodProductCreateForm { on_submit: move |p| on_create.call(p) }
            Divider {}
            FoodProductList { items: items.clone(), on_delete: move |id| on_delete.call(id) }
        }
    }
}

// ── ShoppingListItem ──────────────────────────────────────────────────

#[component]
pub fn ShoppingListItemList(
    items: Vec<ShoppingListItem>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "Shopping list is empty.",
                icon: rsx! { ShoppingCart { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for item in items.iter().cloned() {
                ShoppingListItemRow {
                    key: "{item.id}",
                    item: item.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn ShoppingListItemRow(item: ShoppingListItem, on_delete: EventHandler<Uuid>) -> Element {
    let id = item.id;
    let qty = format_qty(item.qty_thousandths, &item.unit);
    let status_label = if item.purchased {
        "purchased"
    } else {
        "to buy"
    };
    let meta = format!("{} · {}", qty, status_label);
    let name = item.name.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{name}" }
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
pub fn ShoppingListItemCreateForm(on_submit: EventHandler<ShoppingListItemCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut qty = use_signal(|| "1".to_string());
    let mut unit = use_signal(|| "ea".to_string());

    rsx! {
        Card {
            CardHeader { CardTitle { "Add to shopping list" } }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input { value: name, placeholder: "Name (required)", class: "flex-1 min-w-40" }
                    Input { value: qty, placeholder: "Qty", class: "w-24" }
                    Input { value: unit, placeholder: "Unit", class: "w-24" }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() { return; }
                            let q_f: f64 = match qty.read().trim().parse() {
                                Ok(v) => v,
                                Err(_) => return,
                            };
                            let u = unit.read().clone();
                            if u.trim().is_empty() { return; }
                            let payload = ShoppingListItemCreate {
                                product_id: None,
                                name: n,
                                qty_thousandths: (q_f * 1000.0) as i64,
                                unit: u,
                                purchased: false,
                                purchased_at: None,
                                sort_index: 0,
                                notes: None,
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            qty.set("1".into());
                            unit.set("ea".into());
                        },
                        Plus { size: 14 }
                        " Add item"
                    }
                }
            }
        }
    }
}

#[component]
pub fn ShoppingListItemDashboard(
    items: Vec<ShoppingListItem>,
    status: String,
    on_create: EventHandler<ShoppingListItemCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let purchased = items.iter().filter(|i| i.purchased).count();
    let pending = total - purchased;

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Shopping list",
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Total" }
                            ShoppingCart { size: 16 }
                        }
                    }
                    CardContent { Heading { level: HeadingLevel::H2, "{total}" } }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "To buy" }
                            Boxes { size: 16 }
                        }
                    }
                    CardContent { Heading { level: HeadingLevel::H2, "{pending}" } }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Purchased" }
                            CookingPot { size: 16 }
                        }
                    }
                    CardContent { Heading { level: HeadingLevel::H2, "{purchased}" } }
                }
            }
            ShoppingListItemCreateForm { on_submit: move |p| on_create.call(p) }
            Divider {}
            ShoppingListItemList { items: items.clone(), on_delete: move |id| on_delete.call(id) }
        }
    }
}

fn format_qty(q: i64, unit: &str) -> String {
    let raw = format!("{:.3}", q as f64 / 1000.0);
    let trimmed = if raw.contains('.') {
        let t = raw.trim_end_matches('0');
        let t = t.trim_end_matches('.');
        t.to_string()
    } else {
        raw
    };
    format!("{} {}", trimmed, unit)
}
