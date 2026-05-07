#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

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
