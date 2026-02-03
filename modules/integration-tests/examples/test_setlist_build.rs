//! Simple test to trigger setlist build and see results
//!
//! Run with: cargo run --example test_setlist_build

use host_client::HostConnector;
use session_proto::SetlistServiceClient;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Connect via WebSocket (through gateway-ws which has proper routing)
    println!("Connecting to ws://localhost:3030/ws...");

    let connector = HostConnector::websocket("ws://localhost:3030/ws");
    let conn = connector.connect().await?;

    println!("Connected!");

    // Create setlist client
    let setlist_client = SetlistServiceClient::new(conn.handle().clone());

    // Trigger build
    println!("Calling build_from_open_projects...");
    setlist_client.build_from_open_projects().await?;
    println!("Build complete!");

    // Get the setlist
    println!("Getting setlist...");
    let setlist = setlist_client.get_setlist().await?;

    match setlist {
        Some(s) => {
            println!("\nSetlist: {} ({} songs)", s.name, s.songs.len());
            for (i, song) in s.songs.iter().enumerate() {
                println!(
                    "  Song {}: {} ({} sections)",
                    i,
                    song.name,
                    song.sections.len()
                );
                println!(
                    "    Time: {:.1}s - {:.1}s",
                    song.start_seconds, song.end_seconds
                );
                for (j, section) in song.sections.iter().enumerate() {
                    println!(
                        "      Section {}: {} ({:.1}s - {:.1}s)",
                        j, section.name, section.start_seconds, section.end_seconds
                    );
                }
            }
        }
        None => {
            println!("No setlist returned!");
        }
    }

    Ok(())
}
