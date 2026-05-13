//! Cookbook feature route. Holds a local `CrdtDoc` shared across
//! Recipe / PantryItem / ShoppingListItem / FoodProduct repos, syncs
//! over WebSocket, drives `cookbook-ui` dumb components via a tabbed
//! shell (Recipes | Pantry | Shopping List | Products).

use std::rc::Rc;

use architect::Page;
use cookbook_crdt::{
    CrdtDoc, FoodProductRepoLoro, PantryItemRepoLoro, RecipeRepoLoro, ShoppingListItemRepoLoro,
};
use cookbook_proto::{
    FoodProduct, FoodProductCreate, FoodProductRepo, PantryItem, PantryItemCreate, PantryItemRepo,
    Recipe, RecipeCreate, RecipeRepo, ShoppingListItem, ShoppingListItemCreate,
    ShoppingListItemRepo,
};
use cookbook_ui::{
    FoodProductDashboard, PantryItemDashboard, RecipeDashboard, ShoppingListItemDashboard,
};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn RecipeView() -> Element {
    // Share a single ephemeral CrdtDoc across every cookbook repo so
    // they all sync against the same WebSocket session.
    let recipe_repo: Rc<RecipeRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(RecipeRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(recipe_repo.doc().clone())));
    let pantry_repo: Rc<PantryItemRepoLoro> = use_hook(|| Rc::new(PantryItemRepoLoro::new(&doc)));
    let shopping_repo: Rc<ShoppingListItemRepoLoro> =
        use_hook(|| Rc::new(ShoppingListItemRepoLoro::new(&doc)));
    let product_repo: Rc<FoodProductRepoLoro> =
        use_hook(|| Rc::new(FoodProductRepoLoro::new(&doc)));

    let mut recipes = use_signal::<Vec<Recipe>>(Vec::new);
    let mut pantry_items = use_signal::<Vec<PantryItem>>(Vec::new);
    let mut shopping_items = use_signal::<Vec<ShoppingListItem>>(Vec::new);
    let mut products = use_signal::<Vec<FoodProduct>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());
    let mut tab = use_signal(|| "recipes".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let r = recipe_repo.clone();
        let p = pantry_repo.clone();
        let s = shopping_repo.clone();
        let f = product_repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                let mk = || Page {
                    index: 0,
                    size: 200,
                };
                if let Ok(list) = r.list(mk(), None, None).await {
                    recipes.set(list.items);
                }
                if let Ok(list) = p.list(mk(), None, None).await {
                    pantry_items.set(list.items);
                }
                if let Ok(list) = s.list(mk(), None, None).await {
                    shopping_items.set(list.items);
                }
                if let Ok(list) = f.list(mk(), None, None).await {
                    products.set(list.items);
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

    // ── Recipe handlers ──
    let on_recipe_create = {
        let repo = recipe_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: RecipeCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_recipe_delete = {
        let repo = recipe_repo.clone();
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

    // ── Pantry handlers ──
    let on_pantry_create = {
        let repo = pantry_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: PantryItemCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_pantry_delete = {
        let repo = pantry_repo.clone();
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

    // ── Shopping handlers ──
    let on_shopping_create = {
        let repo = shopping_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: ShoppingListItemCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_shopping_delete = {
        let repo = shopping_repo.clone();
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

    // ── Product handlers ──
    let on_product_create = {
        let repo = product_repo.clone();
        let tx = refresh_tx.clone();
        move |payload: FoodProductCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_product_delete = {
        let repo = product_repo.clone();
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
        div { class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10",
            Tabs {
                value: Some(tab.read().clone()),
                on_change: move |v: String| tab.set(v),
                TabList {
                    TabTrigger { value: "recipes", index: 0usize, "Recipes" }
                    TabTrigger { value: "pantry", index: 1usize, "Pantry" }
                    TabTrigger { value: "shopping", index: 2usize, "Shopping List" }
                    TabTrigger { value: "products", index: 3usize, "Products" }
                }
                TabContent { value: "recipes", index: 0usize,
                    RecipeDashboard {
                        items: recipes(),
                        status: status_msg(),
                        on_create: on_recipe_create,
                        on_delete: on_recipe_delete,
                    }
                }
                TabContent { value: "pantry", index: 1usize,
                    PantryItemDashboard {
                        items: pantry_items(),
                        status: status_msg(),
                        on_create: on_pantry_create,
                        on_delete: on_pantry_delete,
                    }
                }
                TabContent { value: "shopping", index: 2usize,
                    ShoppingListItemDashboard {
                        items: shopping_items(),
                        status: status_msg(),
                        on_create: on_shopping_create,
                        on_delete: on_shopping_delete,
                    }
                }
                TabContent { value: "products", index: 3usize,
                    FoodProductDashboard {
                        items: products(),
                        status: status_msg(),
                        on_create: on_product_create,
                        on_delete: on_product_delete,
                    }
                }
            }
        }
    }
}
