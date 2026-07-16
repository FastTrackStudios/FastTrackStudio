//! Read-only smoke: connect to PipeWire, mirror the graph, print it.
//!
//! `cargo run -p patchbay --example snapshot`

#[tokio::main]
async fn main() {
    let backend = patchbay::PatchbayBackend::new();
    tokio::time::sleep(std::time::Duration::from_secs(2)).await;

    use patchbay::proto::PatchbayService as _;
    let snap = backend.graph().await.expect("snapshot");
    println!(
        "nodes={} ports={} links={}",
        snap.nodes.len(),
        snap.ports.len(),
        snap.links.len()
    );
    for n in snap.nodes.iter().take(30) {
        let ports = snap.ports.iter().filter(|p| p.node_id == n.id).count();
        println!("  [{:>4}] {:<40} {:<24} {} ports", n.id, n.label, n.media_class, ports);
    }
    let clock = backend.clock().await.expect("clock");
    println!("clock: {clock:?}");
    let dante = backend.dante_status().await.expect("dante");
    println!(
        "dante: installed={} active={} units={}",
        dante.installed,
        dante.active,
        dante.units.len()
    );
}
