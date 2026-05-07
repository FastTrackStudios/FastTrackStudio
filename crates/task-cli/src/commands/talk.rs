#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum NcCommands {
    /// List users on the Nextcloud instance
    Users {
        #[arg(long)]
        json: bool,
    },
    /// Show display name for a specific user
    User { user_id: String },
    /// List Deck boards
    Boards {
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
pub(crate) enum TalkCommands {
    /// List rooms the user is a member of
    Rooms {
        #[arg(long)]
        json: bool,
    },
    /// Post a message to a room
    Send {
        /// Room token (from `talk rooms`)
        room: String,
        /// Message body
        message: String,
        /// Reply to a parent message id
        #[arg(long)]
        reply_to: Option<u64>,
    },
    /// Show recent messages in a room
    History {
        room: String,
        #[arg(long, short = 'n', default_value = "20")]
        limit: u32,
        #[arg(long)]
        json: bool,
    },
}

/// Run a Talk subcommand. Reads credentials from NEXTCLOUD_URL / NEXTCLOUD_USER /
/// NEXTCLOUD_PASSWORD. `as_user` overrides NEXTCLOUD_USER when provided —
/// useful for Hermes-style bot identities.
pub(crate) async fn run_nc(cmd: NcCommands, as_user: Option<String>) -> eyre::Result<()> {
    use task_core::provider::{NextcloudConfig, NextcloudProvider};

    let url =
        std::env::var("NEXTCLOUD_URL").map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user
        .clone()
        .or(env_user)
        .ok_or_else(|| eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user."))?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;

    let provider = NextcloudProvider::new(
        "nc",
        "Nextcloud",
        NextcloudConfig {
            url,
            username,
            password,
            projects_path: std::env::var("NEXTCLOUD_PROJECTS_PATH")
                .unwrap_or_else(|_| "Projects/".to_string()),
            calendar: None,
            deck_enabled: true,
            deck_boards: std::collections::HashMap::new(),
        },
    );

    match cmd {
        NcCommands::Users { json } => {
            let users = provider.list_users().await?;
            if json {
                let items: Vec<String> = users
                    .iter()
                    .map(|u| format!("\"{}\"", escape_json(u)))
                    .collect();
                println!("[{}]", items.join(","));
            } else {
                for u in &users {
                    println!("{u}");
                }
                println!("\n{} user(s)", users.len());
            }
        }
        NcCommands::User { user_id } => {
            let display = provider.get_user_display_name(&user_id).await?;
            println!("{user_id} → {display}");
        }
        NcCommands::Boards { json } => {
            let boards = provider.list_deck_boards().await?;
            if json {
                let items: Vec<String> = boards
                    .iter()
                    .map(|p| facet_json::to_string(p).unwrap_or_default())
                    .collect();
                println!("[{}]", items.join(","));
            } else {
                if boards.is_empty() {
                    println!("No Deck boards.");
                } else {
                    for p in &boards {
                        println!("- {} ({:?})", p.title, p.status);
                    }
                    println!("\n{} board(s)", boards.len());
                }
            }
        }
    }
    Ok(())
}

pub(crate) async fn run_talk(cmd: TalkCommands, as_user: Option<String>) -> eyre::Result<()> {
    use task_core::provider::{CommunicationChannelProvider, TalkClient, TalkConfig};

    let url =
        std::env::var("NEXTCLOUD_URL").map_err(|_| eyre::eyre!("Set NEXTCLOUD_URL env var."))?;
    let env_user = std::env::var("NEXTCLOUD_USER").ok();
    let username = as_user
        .clone()
        .or(env_user)
        .ok_or_else(|| eyre::eyre!("Set NEXTCLOUD_USER env var or pass --as-user."))?;
    let password = std::env::var("NEXTCLOUD_PASSWORD")
        .map_err(|_| eyre::eyre!("Set NEXTCLOUD_PASSWORD env var."))?;

    let client = TalkClient::new(TalkConfig {
        url,
        username,
        password,
    });

    match cmd {
        TalkCommands::Rooms { json } => {
            let rooms = client.list_conversations().await?;
            print_channel_rooms(&rooms, json);
        }
        TalkCommands::Send {
            room,
            message,
            reply_to,
        } => {
            let sent = CommunicationChannelProvider::send_message(
                &client,
                ChannelSendMessageRequest {
                    conversation_id: room,
                    body: message,
                    reply_to: reply_to.map(|id| id.to_string()),
                },
            )
            .await?;
            println!("Sent message {} to {}.", sent.id, sent.conversation_id);
        }
        TalkCommands::History { room, limit, json } => {
            let msgs = CommunicationChannelProvider::recent_messages(&client, &room, limit).await?;
            print_channel_history(&msgs, json);
        }
    }
    Ok(())
}
