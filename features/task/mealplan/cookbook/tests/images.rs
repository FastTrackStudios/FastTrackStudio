//! Recipe images: found by convention, and served carefully.
//!
//! `image` takes a caller-supplied path and reads it off disk, so the
//! guards matter more than the happy path.

use cookbook::{CookbookService, Store};
use std::path::Path;

fn fixture(root: &Path) -> Store {
    std::fs::create_dir_all(root.join("Cookbook")).unwrap();
    std::fs::write(
        root.join("Cookbook/pasta.cook"),
        ">> title: Pasta\n\nBoil @spaghetti{200%g}.\n\nDrain it.\n",
    )
    .unwrap();
    std::fs::write(root.join("Cookbook/pasta.jpg"), b"\xff\xd8\xff-title").unwrap();
    std::fs::write(root.join("Cookbook/pasta.1.png"), b"\x89PNG-step").unwrap();
    // A secret sitting outside the cookbook root.
    std::fs::write(root.parent().unwrap().join("secret.jpg"), b"nope").unwrap();
    Store::new(root.to_path_buf())
}

#[test]
fn images_are_found_by_naming_convention() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);

    let r = store.get("Cookbook/pasta.cook").unwrap();
    assert_eq!(r.images.len(), 2);

    let title = r.images.iter().find(|i| i.step_index.is_none()).unwrap();
    assert_eq!(title.path, "Cookbook/pasta.jpg");

    let step = r.images.iter().find(|i| i.step_index == Some(1)).unwrap();
    assert_eq!(step.path, "Cookbook/pasta.1.png", "`.1.` belongs to step 1");
}

#[test]
fn listing_carries_images_too() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);
    let all = store.list().unwrap();
    assert_eq!(all.len(), 1);
    assert_eq!(all[0].images.len(), 2, "list must match get");
}

#[test]
fn reads_an_image_it_owns() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);
    assert_eq!(
        store.image("Cookbook/pasta.jpg").unwrap(),
        b"\xff\xd8\xff-title"
    );
}

#[test]
fn refuses_to_climb_out_of_the_cookbook() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);
    assert!(
        store.image("../secret.jpg").is_err(),
        "a relative climb must not escape"
    );
    assert!(
        store.image("Cookbook/../../secret.jpg").is_err(),
        "nor a climb buried mid-path"
    );
}

#[test]
fn refuses_absolute_paths() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);
    assert!(store.image("/etc/hostname").is_err());
}

#[test]
fn refuses_anything_that_isnt_an_image() {
    let tmp = tempfile::tempdir().unwrap();
    let root = tmp.path().join("wiki");
    let store = fixture(&root);
    // The recipe sits in the same directory and is perfectly readable —
    // the extension allowlist is the whole reason this stays pictures.
    assert!(
        store.image("Cookbook/pasta.cook").is_err(),
        "an image endpoint should not hand out recipe sources"
    );
}
