//! In-process vox repro: drive a `#[vox::service]` over a memory link
//! (no REAPER, no architect bridge) and compare a unit-returning method
//! against a complex-returning one. Mirrors vox's own
//! `service_macro_shared` end-to-end harness (rev 27eef573).

use std::time::Duration;

use session_proto::setlist::Setlist;
use vox::{BareConduit, acceptor_conduit, initiator_conduit, memory_link_pair};
use vox_types::{ConnectionSettings, HandshakeResult, Link, MetadataEntry, Parity, SessionRole};

#[vox::service]
trait Probe {
    /// Control: unit return (this shape works against the live extension).
    async fn ping(&self) -> u32;
    /// Suspect: complex return (this shape hangs against the live extension).
    async fn fetch(&self) -> Setlist;
    /// Isolation probe: large Vec<u8> payload — does the JIT codec survive a
    /// big byte vector (like the 43KB schema CborPayload that's getting lost)?
    async fn blob(&self, n: u32) -> Vec<u8>;
}

#[derive(Clone)]
struct ProbeBackend;

impl Probe for ProbeBackend {
    async fn ping(&self) -> u32 {
        42
    }
    async fn blob(&self, n: u32) -> Vec<u8> {
        (0..n).map(|i| (i % 251) as u8).collect()
    }
    async fn fetch(&self) -> Setlist {
        use session_proto::song::{Chart, Section, Song};
        use session_proto::{SectionId, SongId};
        let section = Section {
            section_id: SectionId::default(),
            id: Some(1),
            name: "Verse".into(),
            comment: None,
            section_type: keyflow::sections::SectionType::parse("Verse").unwrap(),
            start_seconds: 0.0,
            end_seconds: 16.0,
            number: Some(1),
            color: Some(0x336699),
        };
        let song = Song {
            id: SongId::default(),
            name: "Repro Song".into(),
            project_guid: "guid-1".into(),
            start_seconds: 0.0,
            end_seconds: 120.0,
            count_in_seconds: Some(4.0),
            sections: vec![section],
            comments: vec![],
            tempo: Some(120.0),
            time_signature: None,
            measure_positions: vec![],
            chart_text: Some("|: C G :|".into()),
            parsed_chart: Some(Chart::new()),
            detected_chords: vec![],
            chart_fingerprint: Some("fp".into()),
            advance_mode: None,
            color: Some(0x112233),
        };
        Setlist {
            id: Some("repro".into()),
            name: "Repro Setlist".into(),
            songs: vec![song],
            ..Default::default()
        }
    }
}

fn acceptor_handshake() -> HandshakeResult {
    HandshakeResult {
        role: SessionRole::Acceptor,
        our_settings: ConnectionSettings {
            parity: Parity::Even,
            max_concurrent_requests: 64,
            initial_channel_credit: 16,
        },
        peer_settings: ConnectionSettings {
            parity: Parity::Odd,
            max_concurrent_requests: 64,
            initial_channel_credit: 16,
        },
        peer_supports_retry: true,
        session_resume_key: None,
        peer_resume_key: None,
        our_schema: vec![],
        peer_schema: vec![],
        peer_metadata: vec![MetadataEntry::str("vox-service", "Probe")],
    }
}

fn initiator_handshake() -> HandshakeResult {
    HandshakeResult {
        role: SessionRole::Initiator,
        our_settings: ConnectionSettings {
            parity: Parity::Odd,
            max_concurrent_requests: 64,
            initial_channel_credit: 16,
        },
        peer_settings: ConnectionSettings {
            parity: Parity::Even,
            max_concurrent_requests: 64,
            initial_channel_credit: 16,
        },
        peer_supports_retry: true,
        session_resume_key: None,
        peer_resume_key: None,
        our_schema: vec![],
        peer_schema: vec![],
        peer_metadata: vec![MetadataEntry::str("vox-service", "Probe")],
    }
}

type MessageConduit<L> = BareConduit<vox_types::MessageFamily, L>;

async fn with_client<F, Fut>(body: F)
where
    F: FnOnce(ProbeClient) -> Fut,
    Fut: std::future::Future<Output = ()>,
{
    let (a, b) = memory_link_pair(64);
    let client_conduit: MessageConduit<_> = BareConduit::new(a);
    let server_conduit: MessageConduit<_> = BareConduit::new(b);

    let (ready_tx, ready_rx) = tokio::sync::oneshot::channel::<()>();
    let server = tokio::task::spawn(async move {
        let guard = acceptor_conduit(server_conduit, acceptor_handshake())
            .on_connection(ProbeDispatcher::new(ProbeBackend))
            .establish::<ProbeClient>()
            .await
            .expect("server handshake");
        let _ = ready_tx.send(());
        let _guard = guard;
        std::future::pending::<()>().await;
    });

    let client = initiator_conduit(client_conduit, initiator_handshake())
        .establish::<ProbeClient>()
        .await
        .expect("client handshake");
    ready_rx.await.expect("server ready");
    body(client).await;
    server.abort();
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn unit_return_roundtrips() {
    with_client(|client| async move {
        match tokio::time::timeout(Duration::from_secs(5), client.ping()).await {
            Ok(v) => assert_eq!(v.expect("ping rpc"), 42),
            Err(_) => panic!("ping (unit return) HUNG"),
        }
    })
    .await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn large_blob_roundtrips() {
    with_client(|client| async move {
        match tokio::time::timeout(Duration::from_secs(5), client.blob(50_000)).await {
            Ok(v) => {
                let b = v.expect("blob rpc");
                assert_eq!(b.len(), 50_000, "blob truncated: got {}", b.len());
                assert_eq!(b[49_999], (49_999u32 % 251) as u8, "blob corrupted");
            }
            Err(_) => panic!("blob HUNG"),
        }
    })
    .await;
}

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn complex_return_roundtrips() {
    with_client(|client| async move {
        match tokio::time::timeout(Duration::from_secs(5), client.fetch()).await {
            Ok(v) => {
                let s = v.expect("fetch rpc");
                assert_eq!(s.name, "Repro Setlist");
            }
            Err(_) => panic!("fetch (complex return) HUNG — reproduced the live bug"),
        }
    })
    .await;
}
