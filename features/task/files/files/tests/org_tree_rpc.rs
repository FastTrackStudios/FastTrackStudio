//! The org tree resolver (issue #304) at the RPC seam: areas, the
//! Projects join (vault folder ⋈ File Root), the vault/wiki folder
//! trees straight through, and Assets hiding registered root dirs.

use architect::{LayerRouter, LocalServer, Scope};
use files::{FilesBackend, RootFlavor};
use files_proto::TreeNode;

fn router(backend: FilesBackend) -> LayerRouter {
    LayerRouter::new().merge(files::files_service_layer(backend))
}

fn names(node: &TreeNode) -> Vec<String> {
    match node {
        TreeNode::Listing(entries) => entries.iter().map(|e| e.name.clone()).collect(),
        TreeNode::Root { .. } => panic!("expected a listing, got a root handoff"),
    }
}

/// An org with a project (note + notes + a registered media root), an
/// album with a song, tagged vault + wiki pages, and a loose asset.
async fn rig() -> (
    tempfile::TempDir,
    files::FilesServiceClient,
    uuid::Uuid,
    LocalServer,
) {
    let dir = tempfile::tempdir().unwrap();
    let vault = dir.path().join("vault");

    // Vault: a project, an album with a song, a tagged record.
    let alpha = vault.join("Projects").join("Alpha");
    std::fs::create_dir_all(alpha.join("Notes")).unwrap();
    std::fs::write(
        alpha.join("Alpha.md"),
        "---\ntitle: Alpha\ntype: project\ntags: [client/acme]\n---\n# Alpha\n",
    )
    .unwrap();
    std::fs::write(alpha.join("Notes").join("Brief.md"), "# Brief\n").unwrap();
    let dusk = vault.join("Albums").join("Dusk");
    std::fs::create_dir_all(dusk.join("Song One")).unwrap();
    std::fs::write(dusk.join("Dusk.md"), "---\ntype: project\n---\n# Dusk\n").unwrap();
    std::fs::write(
        dusk.join("Song One").join("Song One.md"),
        "---\ntype: song\ntags:\n  - worship\n---\n# Song One\n",
    )
    .unwrap();
    std::fs::create_dir_all(vault.join("Records")).unwrap();
    std::fs::write(
        vault.join("Records").join("Sunday.md"),
        "---\ntags: [worship]\n---\n# Sunday\n",
    )
    .unwrap();

    // Wiki: one tagged page (sibling of the vault, like the org dir).
    let wiki = dir.path().join("wiki");
    std::fs::create_dir_all(&wiki).unwrap();
    std::fs::write(
        wiki.join("Runbook.md"),
        "---\ntags: [docs]\n---\n# Runbook\n",
    )
    .unwrap();

    // A loose asset in the Files area, beside the root dir.
    std::fs::write(dir.path().join("logo.png"), b"\x89PNG loose").unwrap();

    let backend = FilesBackend::new(dir.path(), &vault).unwrap();
    let scope = Scope::new();
    let local = LocalServer::serve(router(backend), scope.clone());
    let client: files::FilesServiceClient = local.establish().await.unwrap();

    // The project's media root, registered under the project's name.
    let root_dir = dir.path().join("alpha-media");
    std::fs::create_dir(&root_dir).unwrap();
    std::fs::write(root_dir.join("cut.mov"), vec![0x11u8; 512]).unwrap();
    let root = client
        .create_root(
            root_dir.to_string_lossy().into_owned(),
            "Alpha".into(),
            RootFlavor::Media,
        )
        .await
        .unwrap();
    client.checkpoint_now(root.id, None).await.unwrap();

    (dir, client, root.id, local)
}

#[tokio::test]
async fn the_tree_serves_areas_join_lenses_and_assets() {
    let (_dir, client, root_id, _local) = rig().await;

    // Areas.
    assert_eq!(
        names(&client.tree_browse("".into()).await.unwrap()),
        vec!["Projects", "Vault", "Wiki", "Assets"]
    );

    // Projects: both homes, name-sorted.
    assert_eq!(
        names(&client.tree_browse("Projects".into()).await.unwrap()),
        vec!["Alpha", "Dusk"]
    );

    // The join: Alpha has notes AND the virtual Media door…
    let alpha = names(&client.tree_browse("Projects/Alpha".into()).await.unwrap());
    assert!(alpha.contains(&"Alpha.md".to_string()), "{alpha:?}");
    assert!(alpha.contains(&"Notes".to_string()), "{alpha:?}");
    assert!(alpha.contains(&"Media".to_string()), "{alpha:?}");
    // …and Media hands off to the root explorer with the subpath.
    match client
        .tree_browse("Projects/Alpha/Media/takes".into())
        .await
        .unwrap()
    {
        TreeNode::Root { id, subpath } => {
            assert_eq!(id, root_id);
            assert_eq!(subpath, "takes");
        }
        TreeNode::Listing(_) => panic!("Media must resolve to the root"),
    }
    // A project with no registered root gets no Media entry.
    let dusk = names(&client.tree_browse("Projects/Dusk".into()).await.unwrap());
    assert!(!dusk.contains(&"Media".to_string()), "{dusk:?}");

    // Vault/Wiki: the physical folder tree, straight through.
    let vault_top = names(&client.tree_browse("Vault".into()).await.unwrap());
    assert!(vault_top.contains(&"Projects".to_string()), "{vault_top:?}");
    assert!(vault_top.contains(&"Records".to_string()), "{vault_top:?}");
    let records = names(&client.tree_browse("Vault/Records".into()).await.unwrap());
    assert_eq!(records, vec!["Sunday.md"]);
    let wiki = names(&client.tree_browse("Wiki".into()).await.unwrap());
    assert_eq!(wiki, vec!["Runbook.md"]);

    // Assets: loose files visible, registered root dirs hidden.
    let assets = names(&client.tree_browse("Assets".into()).await.unwrap());
    assert!(assets.contains(&"logo.png".to_string()), "{assets:?}");
    assert!(!assets.contains(&"alpha-media".to_string()), "{assets:?}");

    // Escapes and unknowns refuse.
    assert!(client.tree_browse("Vault/../..".into()).await.is_err());
    assert!(client.tree_browse("Nope".into()).await.is_err());
    assert!(client.tree_browse("Vault/Ghost".into()).await.is_err());
}
