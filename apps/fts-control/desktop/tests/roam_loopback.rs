//! Roam loopback connection test.
//!
//! Verifies that a roam session can be established via both in-memory channels
//! and Unix sockets. This isolates protocol/version issues from REAPER.

use roam::{DriverCaller, DriverReplySink, ErasedCaller, Handler, ReplySink, RoamError, SelfRef};
use std::path::PathBuf;

/// A no-op handler that rejects all calls with `UnknownMethod`.
/// Good enough to test that the roam handshake + session setup works.
#[derive(Clone)]
struct NoopHandler;

impl Handler<DriverReplySink> for NoopHandler {
    async fn handle(&self, _call: SelfRef<roam::RequestCall<'static>>, reply: DriverReplySink) {
        reply
            .send_error(RoamError::<core::convert::Infallible>::UnknownMethod)
            .await;
    }
}

#[tokio::test]
async fn memory_channel_loopback() {
    let (client_link, server_link) = roam::memory_link_pair(64);

    // Server side
    let server = tokio::spawn(async move {
        let result = roam::acceptor(roam::BareConduit::new(server_link))
            .establish::<DriverCaller>(NoopHandler)
            .await;
        match &result {
            Ok(_) => println!("[memory] server session established"),
            Err(e) => eprintln!("[memory] server accept failed: {e:?}"),
        }
        result.is_ok()
    });

    // Client side
    let (caller, _session) = roam::initiator_conduit(roam::BareConduit::new(client_link))
        .establish::<DriverCaller>(())
        .await
        .expect("memory initiator should succeed");

    let _erased = ErasedCaller::new(caller);
    println!("[memory] client session established, ErasedCaller created");

    // Server should also have succeeded
    let server_ok = server.await.expect("server task panicked");
    assert!(server_ok, "server should have established successfully");
}

#[tokio::test]
async fn unix_socket_loopback() {
    // Use a unique temp path
    let sock_path = PathBuf::from(format!("/tmp/fts-roam-test-{}.sock", std::process::id()));
    let _ = std::fs::remove_file(&sock_path);

    let listener = tokio::net::UnixListener::bind(&sock_path).expect("failed to bind test socket");

    println!("[unix] listening on {}", sock_path.display());

    let sock_path_clone = sock_path.clone();

    // Server: accept one connection
    let server = tokio::spawn(async move {
        let (stream, _addr) = listener.accept().await.expect("accept failed");
        println!("[unix] server accepted connection");

        let link = roam_stream::StreamLink::unix(stream);
        let result = roam::acceptor(roam::BareConduit::new(link))
            .establish::<DriverCaller>(NoopHandler)
            .await;
        match &result {
            Ok(_) => println!("[unix] server session established"),
            Err(e) => eprintln!("[unix] server accept failed: {e:?}"),
        }
        result.is_ok()
    });

    // Client: connect
    let stream = tokio::net::UnixStream::connect(&sock_path_clone)
        .await
        .expect("failed to connect to test socket");
    println!("[unix] client connected");

    let link = roam_stream::StreamLink::unix(stream);
    let (caller, _session) = roam::initiator_conduit(roam::BareConduit::new(link))
        .max_concurrent_requests(64)
        .establish::<DriverCaller>(())
        .await
        .expect("unix initiator should succeed");

    let _erased = ErasedCaller::new(caller);
    println!("[unix] client session established, ErasedCaller created");

    let server_ok = server.await.expect("server task panicked");
    assert!(server_ok, "server should have established successfully");

    // Cleanup
    let _ = std::fs::remove_file(&sock_path);
}

/// Test replicating daw_cli::connect code path (session handle dropped).
#[tokio::test]
async fn connect_session_handle_dropped() {
    let sockets: Vec<_> = std::fs::read_dir("/tmp")
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_name()
                .to_str()
                .map_or(false, |n| n.starts_with("fts-daw-") && n.ends_with(".sock"))
        })
        .collect();

    if sockets.is_empty() {
        println!("[dropped] No REAPER socket found, skipping");
        return;
    }

    let sock_path = sockets[0].path();
    println!("[dropped] Using socket: {}", sock_path.display());

    // Replicate daw_cli::connect exactly: session handle is dropped
    let daw = {
        let stream = tokio::net::UnixStream::connect(&sock_path)
            .await
            .expect("connect failed");
        let link = roam_stream::StreamLink::unix(stream);
        let (caller, _session) = roam::initiator_conduit(roam::BareConduit::new(link))
            .establish::<DriverCaller>(())
            .await
            .expect("establish failed");
        // _session is dropped here!
        daw_control::Daw::new(ErasedCaller::new(caller))
    };

    println!("[dropped] Connected (session handle dropped). Trying healthcheck...");
    tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    let alive = daw.healthcheck().await;
    println!("[dropped] healthcheck = {alive}");
}

/// Test connecting to the real REAPER socket (if available).
/// This helps diagnose real connection issues.
#[tokio::test]
async fn connect_to_live_reaper() {
    // Find REAPER socket
    let sockets: Vec<_> = std::fs::read_dir("/tmp")
        .unwrap()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.file_name()
                .to_str()
                .map_or(false, |n| n.starts_with("fts-daw-") && n.ends_with(".sock"))
        })
        .collect();

    if sockets.is_empty() {
        println!("[reaper] No REAPER socket found, skipping live test");
        return;
    }

    let sock_path = sockets[0].path();
    println!("[reaper] Found socket: {}", sock_path.display());

    // Try connecting
    let stream = match tokio::net::UnixStream::connect(&sock_path).await {
        Ok(s) => {
            println!("[reaper] TCP connect succeeded");
            s
        }
        Err(e) => {
            println!("[reaper] TCP connect failed: {e}");
            return;
        }
    };

    let link = roam_stream::StreamLink::unix(stream);
    println!("[reaper] Starting roam initiator handshake...");

    match tokio::time::timeout(
        std::time::Duration::from_secs(5),
        roam::initiator_conduit(roam::BareConduit::new(link))
            .max_concurrent_requests(64)
            .establish::<DriverCaller>(()),
    )
    .await
    {
        Ok(Ok((caller, _session))) => {
            let erased = ErasedCaller::new(caller);
            println!("[reaper] SUCCESS: roam session established!");

            // Try a simple DAW call
            let daw = daw_control::Daw::new(erased);
            let alive = daw.healthcheck().await;
            println!("[reaper] healthcheck = {alive}");
        }
        Ok(Err(e)) => {
            println!("[reaper] FAIL: roam handshake error: {e:?}");
            panic!("Roam handshake failed: {e:?}");
        }
        Err(_) => {
            println!("[reaper] FAIL: roam handshake timed out (5s)");
            panic!("Roam handshake timed out");
        }
    }
}
