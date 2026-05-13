//! Native integration tests for the `inventory` feature.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use inventory_crdt::{CrdtDoc, FoodProductRepoLoro, PantryItemRepoLoro, ShoppingListItemRepoLoro};
use inventory_proto::{
    FoodProductCreate, FoodProductRepo, PantryItemCreate, PantryItemRepo, ShoppingListItemCreate,
    ShoppingListItemRepo,
};
use loro::ExportMode;

#[tokio::test]
async fn food_product_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = FoodProductRepoLoro::new(&doc);

    let p = repo
        .create(FoodProductCreate {
            name: "Whole Milk".into(),
            brand: Some("Organic Valley".into()),
            category: Some("dairy".into()),
            barcode: Some("0123456789012".into()),
            default_unit: Some("ml".into()),
            default_qty_thousandths: Some(1_000_000),
            notes: None,
            tags: vec!["fridge".into(), "dairy".into()],
        })
        .await
        .unwrap();

    let got = repo.get(p.id).await.unwrap();
    assert_eq!(got.name, "Whole Milk");
    assert_eq!(got.brand.as_deref(), Some("Organic Valley"));
    assert_eq!(got.category.as_deref(), Some("dairy"));
    assert_eq!(got.barcode.as_deref(), Some("0123456789012"));
    assert_eq!(got.default_unit.as_deref(), Some("ml"));
    assert_eq!(got.default_qty_thousandths, Some(1_000_000));
    assert_eq!(got.tags, vec!["fridge".to_string(), "dairy".to_string()]);
}

#[tokio::test]
async fn pantry_item_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = PantryItemRepoLoro::new(&doc);

    let expires = Utc::now() + Duration::days(14);
    let item = repo
        .create(PantryItemCreate {
            product_id: None,
            name: "Eggs".into(),
            qty_thousandths: 12_000,
            unit: "ea".into(),
            location: Some("fridge".into()),
            expires_at: Some(expires),
            opened_at: None,
            notes: Some("free range".into()),
            tags: vec!["protein".into()],
        })
        .await
        .unwrap();

    let got = repo.get(item.id).await.unwrap();
    assert_eq!(got.name, "Eggs");
    assert_eq!(got.qty_thousandths, 12_000);
    assert_eq!(got.unit, "ea");
    assert_eq!(got.location.as_deref(), Some("fridge"));
    assert!(got.expires_at.is_some());
    assert_eq!(got.tags, vec!["protein".to_string()]);
}

#[tokio::test]
async fn shopping_list_item_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ShoppingListItemRepoLoro::new(&doc);

    let item = repo
        .create(ShoppingListItemCreate {
            product_id: None,
            name: "Bread".into(),
            qty_thousandths: 1_000,
            unit: "ea".into(),
            purchased: false,
            purchased_at: None,
            sort_index: 5,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let got = repo.get(item.id).await.unwrap();
    assert_eq!(got.name, "Bread");
    assert_eq!(got.qty_thousandths, 1_000);
    assert_eq!(got.unit, "ea");
    assert!(!got.purchased);
    assert_eq!(got.sort_index, 5);
}

#[tokio::test]
async fn all_three_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let products = FoodProductRepoLoro::new(&doc);
    let pantry = PantryItemRepoLoro::new(&doc);
    let shopping = ShoppingListItemRepoLoro::new(&doc);

    products
        .create(FoodProductCreate {
            name: "Salt".into(),
            brand: None,
            category: Some("spice".into()),
            barcode: None,
            default_unit: Some("g".into()),
            default_qty_thousandths: Some(500_000),
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    pantry
        .create(PantryItemCreate {
            product_id: None,
            name: "Pasta".into(),
            qty_thousandths: 500_000,
            unit: "g".into(),
            location: Some("pantry".into()),
            expires_at: None,
            opened_at: None,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    shopping
        .create(ShoppingListItemCreate {
            product_id: None,
            name: "Olive Oil".into(),
            qty_thousandths: 750_000,
            unit: "ml".into(),
            purchased: false,
            purchased_at: None,
            sort_index: 0,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    assert_eq!(
        products
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        pantry
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        shopping
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
}

#[tokio::test]
async fn replicas_converge_across_all_entities() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let pa = FoodProductRepoLoro::new(&a);
    let pb = FoodProductRepoLoro::new(&b);
    let ia = PantryItemRepoLoro::new(&a);
    let ib = PantryItemRepoLoro::new(&b);
    let sa = ShoppingListItemRepoLoro::new(&a);
    let sb = ShoppingListItemRepoLoro::new(&b);

    pa.create(FoodProductCreate {
        name: "A-product".into(),
        brand: None,
        category: None,
        barcode: None,
        default_unit: None,
        default_qty_thousandths: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    pb.create(FoodProductCreate {
        name: "B-product".into(),
        brand: None,
        category: None,
        barcode: None,
        default_unit: None,
        default_qty_thousandths: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    ia.create(PantryItemCreate {
        product_id: None,
        name: "A-pantry".into(),
        qty_thousandths: 1,
        unit: "ea".into(),
        location: None,
        expires_at: None,
        opened_at: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    ib.create(PantryItemCreate {
        product_id: None,
        name: "B-pantry".into(),
        qty_thousandths: 1,
        unit: "ea".into(),
        location: None,
        expires_at: None,
        opened_at: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    sa.create(ShoppingListItemCreate {
        product_id: None,
        name: "A-shop".into(),
        qty_thousandths: 1,
        unit: "ea".into(),
        purchased: false,
        purchased_at: None,
        sort_index: 0,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    sb.create(ShoppingListItemCreate {
        product_id: None,
        name: "B-shop".into(),
        qty_thousandths: 1,
        unit: "ea".into(),
        purchased: false,
        purchased_at: None,
        sort_index: 0,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    let ab = pa.doc().export(ExportMode::all_updates()).unwrap();
    let bb = pb.doc().export(ExportMode::all_updates()).unwrap();
    pb.doc().import(&ab).unwrap();
    pa.doc().import(&bb).unwrap();

    assert_eq!(
        pa.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        pb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        ia.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        ib.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        sa.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        sb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}
