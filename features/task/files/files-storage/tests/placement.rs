//! The Files placement layer end to end over an in-process
//! `architect::LocalServer` — the spec's Testing Decisions primary seam
//! ("the established idiom … the session facade's memory-link bootstrap
//! tests are the prior art"), the same one `files`' own
//! `tests/rpc_surface.rs` uses.
//!
//! All three lanes are mounted on one router here, exactly as a
//! deployment mounts them on three (operator on the server lane, org on
//! each org's, agent wherever agents connect) — the split is about who
//! can reach what, and this file asserts that split from the outside:
//! nothing below reaches into `StorageCore` to check private state.
//!
//! Covers every acceptance criterion of issue #262:
//!
//! 1. an operator registers a location; an org without a grant cannot
//!    place anything on it;
//! 2. grants enforce logical-byte quota and path prefix;
//! 3. an agent announces, is approved, and hosts a root's live tree +
//!    authoritative repo;
//! 4. a second location can hold blob replicas of the same root
//!    (placement is a separate axis from the live tree).

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use architect::{LayerRouter, LocalServer, Scope};
use files_storage::core::{in_server_announcement, server_volume};
use files_storage::proto::service::org::StorageServiceStreamSource as _;
use files_storage::{
    AgentAnnouncement, AgentDirective, AgentHosting, AgentStatus, AnnouncedVolume, CapabilityClass,
    DirectiveOutcome, GrantSpec, InServerAgent, LocationKind, PlacementStatus, StorageAdminBackend,
    StorageAdminServiceClient, StorageAgentBackend, StorageAgentServiceClient, StorageBackend,
    StorageCore, StorageError, StorageEvent, StorageServiceClient, StorageServiceStreamClient,
    storage_admin_layer, storage_agent_layer, storage_agent_stream_layer, storage_service_layer,
    storage_service_stream_layer,
};
use uuid::Uuid;

const ORG: &str = "acme";

/// One deployment: a registry, an in-server agent, and all three lanes on
/// one router.
struct Harness {
    _dir: tempfile::TempDir,
    volumes: PathBuf,
    agent_id: Uuid,
    agent: Arc<InServerAgent>,
    core: Arc<StorageCore>,
    scope: Arc<Scope>,
    admin: StorageAdminServiceClient,
    org: StorageServiceClient,
    agents: StorageAgentServiceClient,
    org_backend: StorageBackend,
    local: LocalServer,
}

impl Harness {
    async fn new() -> Self {
        let dir = tempfile::tempdir().expect("deployment tempdir");
        let volumes = dir.path().join("volumes");
        std::fs::create_dir_all(&volumes).unwrap();

        let core = StorageCore::open(dir.path().join("storage")).expect("registry");
        let agent_id = Uuid::new_v4();
        let agent = Arc::new(InServerAgent::new(agent_id));
        core.register_local_agent(agent.clone());

        let org_backend = StorageBackend::new(core.clone(), ORG);
        let router = LayerRouter::new()
            .merge(storage_admin_layer(StorageAdminBackend::new(core.clone())))
            .merge(storage_agent_layer(StorageAgentBackend::new(core.clone())))
            .merge(storage_agent_stream_layer(StorageAgentBackend::new(
                core.clone(),
            )))
            .merge(storage_service_layer(org_backend.clone()))
            .merge(storage_service_stream_layer(org_backend.clone()));

        let scope = Scope::new();
        let local = LocalServer::serve(router, scope.clone());
        let admin = local.establish().await.expect("admin client");
        let org = local.establish().await.expect("org client");
        let agents = local.establish().await.expect("agent client");

        Self {
            _dir: dir,
            volumes,
            agent_id,
            agent,
            core,
            scope,
            admin,
            org,
            agents,
            org_backend,
            local,
        }
    }

    /// A volume directory on this deployment's disk.
    fn volume(&self, key: &str, capabilities: Vec<CapabilityClass>) -> AnnouncedVolume {
        let root = self.volumes.join(key);
        std::fs::create_dir_all(&root).unwrap();
        AnnouncedVolume {
            key: key.to_string(),
            name: format!("{key} volume"),
            kind: LocationKind::ServerVolume,
            root_path: root.to_str().unwrap().to_string(),
            capabilities,
            capacity_bytes: None,
        }
    }

    async fn announce(&self, volumes: Vec<AnnouncedVolume>) {
        self.agents
            .announce(in_server_announcement(
                self.agent_id,
                "task-server",
                volumes,
            ))
            .await
            .expect("announce rpc");
    }

    async fn close(self) {
        self.agent.shutdown().await;
        drop(self.admin);
        drop(self.org);
        drop(self.agents);
        drop(self.org_backend);
        self.scope.close().await;
        drop(self.local);
        drop(self.core);
    }
}

/// Write a file into a hosted live tree through the agent's own
/// authoritative repo — the only handle allowed on that store — so the
/// root has real content to measure and replicate. Stands in for the
/// cadence engine (issue #260), which is what writes checkpoints for
/// real.
fn checkpoint_into(agent: &InServerAgent, live_tree: &Path, name: &str, content: &[u8]) {
    use jj_lib::repo::Repo as _;
    use task_files_version_store::checkpoint::{Change, checkpoint};

    let repo = agent.repo_at_head(live_tree).expect("authoritative repo");
    let parent = repo
        .view()
        .heads()
        .iter()
        .next()
        .cloned()
        .unwrap_or_else(|| repo.store().root_commit_id().clone());
    let path = jj_lib::repo_path::RepoPathBuf::from_internal_string(name).unwrap();
    pollster::block_on(checkpoint(
        &repo,
        parent,
        vec![Change::Write {
            path,
            content: content.to_vec(),
        }],
        "test content",
    ))
    .expect("checkpoint");
}

/// The application error behind a failed RPC call. Every lane's errors
/// arrive wrapped in vox's transport envelope; a transport-level failure
/// in these tests is a bug in the harness, not an expected outcome.
fn app_err<T: std::fmt::Debug>(result: Result<T, vox::VoxError<StorageError>>) -> StorageError {
    match result {
        Err(vox::VoxError::User(err)) => *err,
        other => panic!("expected an application error, got {other:?}"),
    }
}

async fn next_event(rx: &mut vox::Rx<StorageEvent>) -> StorageEvent {
    let frame = tokio::time::timeout(Duration::from_secs(10), rx.recv())
        .await
        .expect("timed out waiting for a StorageEvent")
        .expect("event channel errored")
        .expect("event stream closed early");
    let mut copied = None;
    let _ = frame.map(|ev| copied = Some(ev));
    copied.expect("SelfRef::map ran")
}

async fn next_directive(rx: &mut vox::Rx<AgentDirective>) -> AgentDirective {
    let frame = tokio::time::timeout(Duration::from_secs(10), rx.recv())
        .await
        .expect("timed out waiting for an AgentDirective")
        .expect("directive channel errored")
        .expect("directive stream closed early");
    let mut copied = None;
    let _ = frame.map(|d| copied = Some(d));
    copied.expect("SelfRef::map ran")
}

/// Acceptance criteria 1 and 3: an agent announces, the operator
/// approves and thereby registers its volume, and only then — and only
/// with a grant — can an org place a root's live tree on it, repo and
/// all.
#[tokio::test(flavor = "multi_thread")]
async fn agent_approval_grant_then_hosting() {
    let h = Harness::new().await;
    let root_id = Uuid::new_v4();

    // The agent announces. Nothing is a location yet.
    h.announce(vec![h.volume(
        "primary",
        vec![CapabilityClass::LiveTrees, CapabilityClass::Blobs],
    )])
    .await;
    let announced = h.admin.list_agents().await.expect("list_agents rpc");
    assert_eq!(announced.len(), 1);
    assert_eq!(announced[0].status, AgentStatus::Pending);
    assert_eq!(announced[0].hosting, AgentHosting::InServer);
    assert!(
        h.admin
            .list_locations()
            .await
            .expect("list_locations rpc")
            .is_empty(),
        "an unapproved agent's volumes are not locations"
    );

    // The operator approves — that is what registers the location.
    let approved = h
        .admin
        .approve_agent(h.agent_id, true)
        .await
        .expect("approve_agent rpc");
    assert_eq!(approved.status, AgentStatus::Approved);
    let locations = h.admin.list_locations().await.expect("list_locations rpc");
    assert_eq!(locations.len(), 1, "approval registered the volume");
    let location = locations[0].clone();

    // Criterion 1: an org with no grant sees nothing and can place
    // nothing — even naming the location id directly.
    assert!(
        h.org
            .list_locations()
            .await
            .expect("org list_locations rpc")
            .is_empty(),
        "an ungranted location is invisible to the org lane"
    );
    let ungranted = h
        .org
        .place_root(root_id, location.id, "mix-session".to_string())
        .await;
    let err_ungranted = app_err(ungranted);
    assert!(
        matches!(err_ungranted, StorageError::NotGranted(_)),
        "placing without a grant must be refused: {err_ungranted:?}"
    );

    // Subscribe to the org's events before the grant lands.
    let stream: StorageServiceStreamClient = h.local.establish().await.expect("stream client");
    let (tx, mut rx) = vox::channel::<StorageEvent>();
    let subscription = tokio::spawn(async move {
        stream
            .events(tx)
            .await
            .expect("subscribe to storage events");
    });
    let hub = h.org_backend.events_hub().clone();
    tokio::time::timeout(Duration::from_secs(10), async {
        while hub.subscriber_count() == 0 {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .expect("subscriber sink never reached the org hub");

    // The operator admits the org: live trees only, 1 MiB, under its own
    // prefix.
    let grant = h
        .admin
        .issue_grant(GrantSpec {
            org: ORG.to_string(),
            location_id: location.id,
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 1024 * 1024,
            path_prefix: "orgs/acme".to_string(),
        })
        .await
        .expect("issue_grant rpc");
    assert_eq!(grant.used_bytes, 0);
    match next_event(&mut rx).await {
        StorageEvent::GrantIssued(g) => assert_eq!(g.id, grant.id),
        other => panic!("expected GrantIssued, got {other:?}"),
    }
    assert_eq!(
        h.org.list_locations().await.expect("org locations").len(),
        1,
        "a granted location becomes visible"
    );

    // Criterion 3: placement hosts the live tree AND its authoritative
    // repo, under the grant's prefix.
    let placement = h
        .org
        .place_root(root_id, location.id, "mix-session".to_string())
        .await
        .expect("place_root rpc");
    assert_eq!(placement.status, PlacementStatus::Hosted);
    let live_tree = placement.live_tree.clone().expect("live tree bound");
    assert_eq!(live_tree.location_id, location.id);
    assert!(
        live_tree.repo_initialized,
        "the hosting agent initialized the authoritative repo"
    );
    let tree_path = PathBuf::from(&live_tree.absolute_path);
    assert!(tree_path.is_dir(), "the live tree exists on the volume");
    assert!(
        tree_path.join(".fts-files").join("store").exists(),
        "the authoritative version-store repo lives with the live tree"
    );
    assert!(
        tree_path.starts_with(Path::new(&location.root_path).join("orgs/acme")),
        "the live tree sits under the grant's path prefix: {tree_path:?}"
    );
    match next_event(&mut rx).await {
        StorageEvent::PlacementChanged(p) => assert_eq!(p.root_id, root_id),
        other => panic!("expected PlacementChanged, got {other:?}"),
    }

    // A second live tree for the same root is refused — a root's live
    // tree sits wholly on one location.
    let again = h
        .org
        .place_root(root_id, location.id, "mix-session-2".to_string())
        .await;
    let err_again = app_err(again);
    assert!(
        matches!(err_again, StorageError::AlreadyExists(_)),
        "a root already has its live tree: {err_again:?}"
    );

    // Revoking admission closes the lane again without touching data.
    h.admin
        .revoke_grant(grant.id)
        .await
        .expect("revoke_grant rpc");
    assert!(
        matches!(
            app_err(
                h.org
                    .place_root(Uuid::new_v4(), location.id, "other".to_string())
                    .await
            ),
            StorageError::NotGranted(_)
        ),
        "a revoked grant places nothing"
    );
    assert!(
        tree_path.is_dir(),
        "revoking a grant never deletes what was already placed"
    );

    subscription.abort();
    h.close().await;
}

/// Acceptance criterion 2: the two grant terms with teeth — the path
/// prefix confines every path an org supplies, and the logical-byte
/// quota refuses growth past it.
#[tokio::test(flavor = "multi_thread")]
async fn grants_enforce_prefix_and_quota() {
    let h = Harness::new().await;
    h.announce(vec![h.volume(
        "primary",
        vec![CapabilityClass::LiveTrees, CapabilityClass::Blobs],
    )])
    .await;
    h.admin
        .approve_agent(h.agent_id, true)
        .await
        .expect("approve_agent rpc");
    let location = h.admin.list_locations().await.expect("locations")[0].clone();
    // A deliberately tiny quota: 4 KiB of logical bytes.
    h.admin
        .issue_grant(GrantSpec {
            org: ORG.to_string(),
            location_id: location.id,
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 4096,
            path_prefix: "orgs/acme".to_string(),
        })
        .await
        .expect("issue_grant rpc");

    // Path prefix: nothing an org sends resolves outside its subtree.
    for escape in ["../elsewhere", "/etc", "a/../../../elsewhere", ""] {
        let attempt = h
            .org
            .place_root(Uuid::new_v4(), location.id, escape.to_string())
            .await;
        let err_attempt = app_err(attempt);
        assert!(
            matches!(err_attempt, StorageError::BadRequest(_)),
            "{escape:?} must not escape the grant's prefix: {err_attempt:?}"
        );
    }
    assert!(
        !h.volumes.join("primary").join("elsewhere").exists(),
        "a refused placement creates nothing outside the prefix"
    );

    // Quota: place a root, fill it past the quota, and watch the next
    // placement be refused.
    let first = Uuid::new_v4();
    let placement = h
        .org
        .place_root(first, location.id, "big-session".to_string())
        .await
        .expect("place_root rpc");
    let live_tree = PathBuf::from(&placement.live_tree.unwrap().absolute_path);

    let payload = vec![b'a'; 8192];
    checkpoint_into(&h.agent, &live_tree, "session.wav", &payload);
    let measured = h.org.refresh_usage(first).await.expect("refresh_usage rpc");
    assert!(
        measured.logical_bytes >= payload.len() as u64,
        "the hosting agent measured the tree's logical bytes: {measured:?}"
    );

    let usage = h.org.usage(location.id).await.expect("usage rpc");
    assert_eq!(usage.quota_bytes, 4096);
    assert!(
        usage.used_bytes > usage.quota_bytes,
        "usage reports the over-quota position honestly: {usage:?}"
    );
    assert_eq!(usage.placements, 1);
    let grants = h.org.list_grants().await.expect("list_grants rpc");
    assert_eq!(
        grants[0].used_bytes, usage.used_bytes,
        "a grant's used_bytes is derived from placements, never a separate counter"
    );

    let refused = h
        .org
        .place_root(Uuid::new_v4(), location.id, "another-session".to_string())
        .await;
    let err_refused = app_err(refused);
    assert!(
        matches!(err_refused, StorageError::QuotaExceeded(_)),
        "a grant with no headroom takes no new placements: {err_refused:?}"
    );

    h.close().await;
}

/// Acceptance criterion 4: blob placement is a separate axis. A second
/// location that cannot host a live tree at all still holds a full blob
/// replica of the same root.
#[tokio::test(flavor = "multi_thread")]
async fn second_location_holds_blob_replicas() {
    let h = Harness::new().await;
    h.announce(vec![
        h.volume(
            "primary",
            vec![CapabilityClass::LiveTrees, CapabilityClass::Blobs],
        ),
        // Blobs only — an archive volume that can never hold a live tree.
        h.volume("archive", vec![CapabilityClass::Blobs]),
    ])
    .await;
    h.admin
        .approve_agent(h.agent_id, true)
        .await
        .expect("approve_agent rpc");
    let locations = h.admin.list_locations().await.expect("locations");
    let primary = locations
        .iter()
        .find(|l| l.volume_key == "primary")
        .expect("primary registered")
        .clone();
    let archive = locations
        .iter()
        .find(|l| l.volume_key == "archive")
        .expect("archive registered")
        .clone();

    for (location, capabilities) in [
        // Primary carries both classes on purpose: the reason a replica
        // may not live on the live tree's own location is that one
        // location holds one copy, not that the grant fell short.
        (
            primary.id,
            vec![CapabilityClass::LiveTrees, CapabilityClass::Blobs],
        ),
        (archive.id, vec![CapabilityClass::Blobs]),
    ] {
        h.admin
            .issue_grant(GrantSpec {
                org: ORG.to_string(),
                location_id: location,
                capabilities,
                quota_bytes: 1024 * 1024,
                path_prefix: "orgs/acme".to_string(),
            })
            .await
            .expect("issue_grant rpc");
    }

    // A grant may not exceed its location's own capabilities.
    let over_grant = h
        .admin
        .issue_grant(GrantSpec {
            org: "other".to_string(),
            location_id: archive.id,
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 1,
            path_prefix: "orgs/other".to_string(),
        })
        .await;
    let err_over_grant = app_err(over_grant);
    assert!(
        matches!(err_over_grant, StorageError::CapabilityDenied(_)),
        "a grant cannot offer what its location cannot do: {err_over_grant:?}"
    );

    let root_id = Uuid::new_v4();
    let placement = h
        .org
        .place_root(root_id, primary.id, "video-project".to_string())
        .await
        .expect("place_root rpc");
    let live_tree = PathBuf::from(&placement.live_tree.unwrap().absolute_path);
    checkpoint_into(&h.agent, &live_tree, "cut-01.mov", &vec![b'v'; 40_000]);
    checkpoint_into(&h.agent, &live_tree, "notes.txt", b"client feedback");
    let measured = h.org.refresh_usage(root_id).await.expect("refresh_usage");

    // The live tree's location cannot be its own replica…
    assert!(
        matches!(
            app_err(h.org.add_blob_replica(root_id, primary.id).await),
            StorageError::BadRequest(_)
        ),
        "a replica must live somewhere other than the live tree"
    );
    // …and the blob-only location cannot host a live tree.
    assert!(
        matches!(
            app_err(
                h.org
                    .place_root(Uuid::new_v4(), archive.id, "nope".to_string())
                    .await
            ),
            StorageError::CapabilityDenied(_)
        ),
        "a blob-only grant hosts no live tree"
    );

    // The replica itself.
    let replicated = h
        .org
        .add_blob_replica(root_id, archive.id)
        .await
        .expect("add_blob_replica rpc");
    assert_eq!(replicated.replicas.len(), 1);
    let replica = replicated.replicas[0].clone();
    assert_eq!(replica.location_id, archive.id);
    assert!(replica.synced_at.is_some(), "the replica synced");
    assert_eq!(
        replica.files_present, 2,
        "both saved files reached the replica: {replica:?}"
    );
    assert_eq!(
        replica.logical_bytes, measured.logical_bytes,
        "the replica holds the same logical bytes as the live tree"
    );
    assert!(
        PathBuf::from(&replica.absolute_path).is_dir(),
        "the replica's chunk store is on the archive volume"
    );
    assert!(
        PathBuf::from(&replica.absolute_path)
            .starts_with(Path::new(&archive.root_path).join("orgs/acme")),
        "the replica sits under the grant's prefix too"
    );

    // Both axes charge their own location, independently.
    let primary_usage = h.org.usage(primary.id).await.expect("primary usage");
    let archive_usage = h.org.usage(archive.id).await.expect("archive usage");
    assert_eq!(primary_usage.used_bytes, measured.logical_bytes);
    assert_eq!(archive_usage.used_bytes, measured.logical_bytes);
    assert_eq!(archive_usage.placements, 1);

    // The live tree is untouched by replication — one root, one live
    // tree, N blob copies.
    let after = h.org.placement(root_id).await.expect("placement rpc");
    assert_eq!(
        after.live_tree.as_ref().unwrap().location_id,
        primary.id,
        "replication never moves the live tree"
    );

    h.close().await;
}

/// The storage-agent protocol as the other two hostings will speak it: a
/// remote agent announces, is approved, receives its directive over the
/// `#[subscribe]` stream, and reports the outcome — which is what flips
/// the placement to hosted. Nothing about the coordinator's side differs
/// from the in-server case except that it waits.
#[tokio::test(flavor = "multi_thread")]
async fn remote_agent_receives_directives_and_reports_back() {
    let h = Harness::new().await;

    // A second agent, deliberately NOT registered in-process — this is
    // the desktop/standalone hosting from the coordinator's point of view.
    let remote_id = Uuid::new_v4();
    let remote_root = h.volumes.join("remote-drive");
    std::fs::create_dir_all(&remote_root).unwrap();
    let announced = h
        .agents
        .announce(AgentAnnouncement {
            agent_id: remote_id,
            hosting: AgentHosting::Standalone,
            label: "nas".to_string(),
            volumes: vec![AnnouncedVolume {
                key: "bulk".to_string(),
                name: "NAS bulk".to_string(),
                kind: LocationKind::ServerVolume,
                root_path: remote_root.to_str().unwrap().to_string(),
                capabilities: vec![CapabilityClass::LiveTrees],
                capacity_bytes: Some(1 << 40),
            }],
        })
        .await
        .expect("announce rpc");
    assert_eq!(announced.status, AgentStatus::Pending);

    // A pending agent's volume cannot be granted, because it is not a
    // location at all.
    let premature = h
        .admin
        .issue_grant(GrantSpec {
            org: ORG.to_string(),
            location_id: Uuid::new_v4(),
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 1024,
            path_prefix: "orgs/acme".to_string(),
        })
        .await;
    let err_premature = app_err(premature);
    assert!(
        matches!(err_premature, StorageError::NotFound(_)),
        "no location exists for an unapproved agent: {err_premature:?}"
    );
    // …and registering one of its volumes is refused by name, too.
    assert!(
        matches!(
            app_err(
                h.admin
                    .register_location(remote_id, "bulk".to_string())
                    .await
            ),
            StorageError::AgentNotApproved(_)
        ),
        "an unapproved agent's volume cannot be registered"
    );

    h.admin
        .approve_agent(remote_id, true)
        .await
        .expect("approve_agent rpc");
    let location = h
        .admin
        .list_locations()
        .await
        .expect("locations")
        .into_iter()
        .find(|l| l.agent_id == remote_id)
        .expect("the remote agent's volume is now a location");
    h.admin
        .issue_grant(GrantSpec {
            org: ORG.to_string(),
            location_id: location.id,
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 1 << 20,
            path_prefix: "orgs/acme".to_string(),
        })
        .await
        .expect("issue_grant rpc");

    // The agent subscribes to its directive stream, as an agent does on
    // connect.
    let stream: files_storage::StorageAgentServiceStreamClient =
        h.local.establish().await.expect("agent stream client");
    let (tx, mut rx) = vox::channel::<AgentDirective>();
    let subscription = tokio::spawn(async move {
        stream
            .directives(tx)
            .await
            .expect("subscribe to directives");
    });
    let hub = h.core.directives_hub().clone();
    tokio::time::timeout(Duration::from_secs(10), async {
        while hub.subscriber_count() == 0 {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .expect("subscriber sink never reached the directive hub");

    let root_id = Uuid::new_v4();
    let placement = h
        .org
        .place_root(root_id, location.id, "remote-session".to_string())
        .await
        .expect("place_root rpc");
    assert_eq!(
        placement.status,
        PlacementStatus::Pending,
        "a remote agent's placement waits for the agent"
    );

    let directive = next_directive(&mut rx).await;
    assert_eq!(directive.agent_id, remote_id);
    let absolute_path = match &directive.kind {
        files_storage::DirectiveKind::HostLiveTree { absolute_path, .. } => absolute_path.clone(),
        other => panic!("expected HostLiveTree, got {other:?}"),
    };
    assert!(
        absolute_path.starts_with(remote_root.to_str().unwrap()),
        "the directive names a path on the agent's own volume"
    );
    let outstanding = h
        .agents
        .pending_directives(remote_id)
        .await
        .expect("pending_directives rpc");
    assert_eq!(outstanding.len(), 1, "catch-up read sees the same work");
    assert_eq!(outstanding[0].id, directive.id);

    // Another agent may not answer for this one.
    assert!(
        matches!(
            app_err(
                h.agents
                    .complete_directive(
                        h.agent_id,
                        directive.id,
                        DirectiveOutcome::Hosted {
                            repo_initialized: true
                        },
                    )
                    .await
            ),
            StorageError::BadRequest(_)
        ),
        "a directive can only be completed by the agent it was issued to"
    );

    h.agents
        .complete_directive(
            remote_id,
            directive.id,
            DirectiveOutcome::Hosted {
                repo_initialized: true,
            },
        )
        .await
        .expect("complete_directive rpc");

    let hosted = h.org.placement(root_id).await.expect("placement rpc");
    assert_eq!(hosted.status, PlacementStatus::Hosted);
    assert!(hosted.live_tree.unwrap().repo_initialized);
    assert!(
        h.agents
            .pending_directives(remote_id)
            .await
            .expect("pending_directives rpc")
            .is_empty(),
        "a completed directive stops being outstanding"
    );

    subscription.abort();
    h.close().await;
}

/// The registry is deployment-scoped and survives a restart: a fresh
/// `StorageCore` over the same directory still knows the agent, the
/// location, the grant and the placement.
#[tokio::test(flavor = "multi_thread")]
async fn registry_survives_a_restart() {
    let dir = tempfile::tempdir().expect("deployment tempdir");
    let volumes = dir.path().join("volumes");
    std::fs::create_dir_all(volumes.join("primary")).unwrap();
    let agent_id = Uuid::new_v4();
    let root_id = Uuid::new_v4();

    {
        let core = StorageCore::open(dir.path().join("storage")).expect("registry");
        let agent = Arc::new(InServerAgent::new(agent_id));
        core.register_local_agent(agent.clone());
        core.announce(in_server_announcement(
            agent_id,
            "task-server",
            vec![server_volume(
                "primary",
                "Server primary",
                &volumes.join("primary"),
            )],
        ))
        .expect("announce");
        core.approve_agent(agent_id, true).expect("approve");
        let location = core.list_locations()[0].clone();
        core.issue_grant(GrantSpec {
            org: ORG.to_string(),
            location_id: location.id,
            capabilities: vec![CapabilityClass::LiveTrees],
            quota_bytes: 1 << 20,
            path_prefix: "orgs/acme".to_string(),
        })
        .expect("grant");
        core.place_root(ORG, root_id, location.id, "session")
            .expect("place");
        agent.shutdown().await;
    }

    let core = StorageCore::open(dir.path().join("storage")).expect("reopen registry");
    assert_eq!(core.list_agents().len(), 1);
    assert_eq!(core.list_agents()[0].status, AgentStatus::Approved);
    assert_eq!(core.list_locations().len(), 1);
    assert_eq!(core.list_grants(Some(ORG)).len(), 1);
    let placement = core.placement(ORG, root_id).expect("placement survived");
    assert_eq!(placement.status, PlacementStatus::Hosted);
    assert!(
        PathBuf::from(&placement.live_tree.unwrap().absolute_path).is_dir(),
        "the live tree is still where the registry says it is"
    );
}
