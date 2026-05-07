#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum EmailCommands {
    /// List configured Nextcloud Mail accounts
    Accounts {
        #[arg(long)]
        json: bool,
    },
    /// List mailboxes (folders) in an account
    Mailboxes {
        #[arg(long)]
        account: i64,
        #[arg(long)]
        json: bool,
    },
    /// Search / list messages in a mailbox
    Search {
        #[arg(long)]
        mailbox: i64,
        /// Filter: free-text or `from:`, `to:`, `subject:`, `cc:`, `bcc:` tokens
        #[arg(long)]
        filter: Option<String>,
        #[arg(long, short = 'n', default_value = "25")]
        limit: u32,
        #[arg(long)]
        cursor: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show one message (headers + body)
    Show {
        id: i64,
        /// Also fetch the body
        #[arg(long)]
        body: bool,
        #[arg(long)]
        json: bool,
    },
    /// Link an email to a task or project. Bot-friendly — every field that
    /// isn't provided is left as None / empty.
    Link {
        /// "task" or "project"
        #[arg(long)]
        to: String,
        /// Task title/id or project title
        reference: String,
        /// RFC-2822 Message-ID (with or without angle brackets)
        #[arg(long)]
        message_id: String,
        #[arg(long)]
        subject: Option<String>,
        #[arg(long)]
        from: Option<String>,
        /// Comma-separated recipient list
        #[arg(long)]
        to_recipients: Option<String>,
        /// Send date (RFC3339 or "YYYY-MM-DD HH:MM")
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        snippet: Option<String>,
        #[arg(long)]
        account_id: Option<i64>,
        #[arg(long)]
        mailbox: Option<String>,
        #[arg(long)]
        imap_uid: Option<u32>,
        #[arg(long)]
        nc_db_id: Option<i64>,
        #[arg(long)]
        attachments: Option<u32>,
        /// Comma-separated categorization tags
        #[arg(long)]
        tags: Option<String>,
    },
    /// Unlink an email from a task or project
    Unlink {
        #[arg(long)]
        to: String,
        reference: String,
        #[arg(long)]
        message_id: String,
    },
    /// List emails linked to a task or project
    List {
        #[arg(long)]
        to: String,
        reference: String,
        #[arg(long)]
        json: bool,
    },
    /// Create a mailbox (folder). To create a Proton label, pass a name
    /// under `Labels/` — e.g. `Labels/project.acme`.
    FolderCreate {
        #[arg(long)]
        account: i64,
        /// Folder name (supports `/` for nesting, e.g. `Folders/clients/acme`)
        #[arg(long)]
        name: String,
        #[arg(long)]
        json: bool,
    },
    /// Delete a mailbox (folder) by id. Removes the Proton label if the
    /// folder is under `Labels/`.
    FolderDelete {
        #[arg(long)]
        mailbox: i64,
    },
    /// Move a message to another mailbox. This is a true move — the
    /// source loses the message. For Proton-style labels that keep a
    /// message in INBOX, use `task email tag set` instead.
    Move {
        #[arg(long, value_name = "ID")]
        email_id: i64,
        #[arg(long)]
        to_folder: i64,
    },
    /// Manage NC Mail tags (IMAP keywords, NC-local)
    Tag {
        #[command(subcommand)]
        cmd: TagCommands,
    },
    /// Return messages in an inbox that are not yet linked to a task
    /// or project and are not tagged `$processed`. Curator / Hermes
    /// use this to find unsorted mail. Output is JSON by default
    /// (agent-friendly).
    Sweep {
        #[arg(long)]
        account: i64,
        /// Mailbox id to scan (default: account's INBOX)
        #[arg(long)]
        mailbox: Option<i64>,
        /// Cap on messages scanned per call
        #[arg(long, default_value = "50")]
        limit: u32,
        /// Filter string (same shape as `search --filter`)
        #[arg(long)]
        filter: Option<String>,
        /// Print a human table instead of JSON
        #[arg(long)]
        table: bool,
    },
    /// Mark a message as triaged by the curator. Applies the
    /// `$processed` NC Mail tag (auto-creating it on first call).
    /// Subsequent sweeps skip tagged messages.
    MarkProcessed {
        #[arg(long, value_name = "ID")]
        email_id: i64,
        /// Optional short note, recorded in the audit log
        #[arg(long)]
        note: Option<String>,
    },
    /// Watch an IMAP mailbox via RFC-2177 IDLE and emit one JSON line
    /// per server-pushed event. Long-running. Intended to run on
    /// starcommand (where ProtonMail Bridge is on 127.0.0.1).
    ///
    /// Credentials: IMAP_PASSWORD env var. The rest are flags.
    Watch {
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        #[arg(long, default_value = "1143")]
        port: u16,
        #[arg(long)]
        user: String,
        #[arg(long, default_value = "INBOX")]
        mailbox: String,
        /// PEM bundle to verify the server cert against. On starcommand
        /// this is `/var/lib/nc-mail-trust/ca-bundle.crt`.
        #[arg(long)]
        ca_bundle: Option<std::path::PathBuf>,
        /// Disable cert verification. Only safe for loopback.
        #[arg(long)]
        insecure: bool,
    },
}

#[derive(Subcommand)]
pub(crate) enum TagCommands {
    /// List NC Mail tags
    List {
        #[arg(long)]
        json: bool,
    },
    /// Create an NC Mail tag
    Create {
        /// Display name, e.g. "project/acme"
        #[arg(long)]
        name: String,
        /// 7-char hex color, e.g. `#8b5cf6`
        #[arg(long, default_value = "#8b5cf6")]
        color: String,
        #[arg(long)]
        json: bool,
    },
    /// Delete an NC Mail tag
    Delete {
        #[arg(long)]
        account: i64,
        #[arg(long)]
        tag: i64,
    },
    /// Attach an existing tag to a message (by imapLabel).
    Set {
        imap_label: String,
        #[arg(long, value_name = "ID")]
        email_id: i64,
    },
    /// Remove a tag from a message
    Unset {
        imap_label: String,
        #[arg(long, value_name = "ID")]
        email_id: i64,
    },
}

pub(crate) async fn run_remote_email_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: EmailCommands,
) -> eyre::Result<()> {
    let mail = remote.mail().await?;
    match command {
        EmailCommands::Accounts { json } => {
            let accounts = mail.list_accounts().await?;
            if json {
                print_mail_accounts_json(&accounts);
            } else {
                print_mail_accounts_table(&accounts);
            }
        }
        EmailCommands::Mailboxes { account, json } => {
            let boxes = mail.list_mailboxes(account).await?;
            if json {
                print_mailboxes_json(&boxes);
            } else {
                print_mailboxes_table(&boxes);
            }
        }
        EmailCommands::Search {
            mailbox,
            filter,
            limit,
            cursor,
            json,
        } => {
            let messages = mail
                .list_messages(task_core::MailListMessagesRequest {
                    mailbox_id: mailbox,
                    filter,
                    limit,
                    cursor,
                })
                .await?;
            if json {
                print_mail_messages_json(&messages);
            } else {
                print_mail_messages_table(&messages);
            }
        }
        EmailCommands::Show { id, body, json } => {
            let msg = mail.get_message(id).await?;
            let body_text = if body {
                mail.get_body(id).await.ok()
            } else {
                None
            };
            if json {
                print_mail_detail_json(&msg, body_text.as_deref());
            } else {
                print_mail_detail(&msg, body_text.as_deref());
            }
        }
        EmailCommands::FolderCreate {
            account,
            name,
            json,
        } => {
            let mb = mail
                .create_mailbox(task_core::MailCreateMailboxRequest {
                    account_id: account,
                    name,
                })
                .await?;
            if json {
                print_mailboxes_json(&[mb]);
            } else {
                println!(
                    "Created mailbox {} (id {}, account {})",
                    mb.name, mb.id, mb.account_id
                );
            }
        }
        EmailCommands::FolderDelete { mailbox } => {
            mail.delete_mailbox(mailbox).await?;
            println!("Deleted mailbox {mailbox}.");
        }
        EmailCommands::Move {
            email_id,
            to_folder,
        } => {
            mail.move_message(task_core::MailMoveMessageRequest {
                message_id: email_id,
                dest_folder_id: to_folder,
            })
            .await?;
            println!("Moved message {email_id} to folder {to_folder}.");
        }
        EmailCommands::Tag { cmd } => match cmd {
            TagCommands::List { json } => {
                let tags = mail.list_tags().await?;
                if json {
                    print_mail_tags_json(&tags);
                } else {
                    print_mail_tags_table(&tags);
                }
            }
            TagCommands::Create { name, color, json } => {
                let tag = mail
                    .create_tag(task_core::MailCreateTagRequest {
                        display_name: name,
                        color,
                    })
                    .await?;
                if json {
                    print_mail_tags_json(&[tag]);
                } else {
                    println!(
                        "Created tag {} (id {}, imapLabel {})",
                        tag.display_name, tag.id, tag.imap_label
                    );
                }
            }
            TagCommands::Delete { account, tag } => {
                mail.delete_tag(task_core::MailDeleteTagRequest {
                    account_id: account,
                    tag_id: tag,
                })
                .await?;
                println!("Deleted tag {tag} on account {account}.");
            }
            TagCommands::Set {
                imap_label,
                email_id,
            } => {
                mail.set_tag(task_core::MailMessageTagRequest {
                    message_id: email_id,
                    imap_label: imap_label.clone(),
                })
                .await?;
                println!("Tagged message {email_id} with {imap_label}.");
            }
            TagCommands::Unset {
                imap_label,
                email_id,
            } => {
                mail.remove_tag(task_core::MailMessageTagRequest {
                    message_id: email_id,
                    imap_label: imap_label.clone(),
                })
                .await?;
                println!("Removed tag {imap_label} from message {email_id}.");
            }
        },
        EmailCommands::Sweep {
            account,
            mailbox,
            limit,
            filter,
            table,
        } => {
            let mailbox_id = match mailbox {
                Some(m) => m,
                None => {
                    mail.list_mailboxes(account)
                        .await?
                        .into_iter()
                        .find(|m| m.name.eq_ignore_ascii_case("INBOX"))
                        .ok_or_else(|| eyre::eyre!("No INBOX for account {account}"))?
                        .id
                }
            };
            let messages = mail
                .list_messages(task_core::MailListMessagesRequest {
                    mailbox_id,
                    filter,
                    limit,
                    cursor: None,
                })
                .await?;
            let linked: std::collections::HashSet<_> =
                mail.linked_message_ids().await?.into_iter().collect();
            let mut unprocessed: Vec<_> = messages
                .into_iter()
                .filter(|m| {
                    if m.tag_labels.iter().any(|t| t == "$processed") {
                        return false;
                    }
                    if let Some(mid) = m.message_id.as_deref() {
                        let key = normalize_message_id(mid);
                        if linked.contains(&key) {
                            return false;
                        }
                    }
                    true
                })
                .collect();
            unprocessed.sort_by_key(|m| m.date);
            if table {
                print_mail_messages_table(&unprocessed);
            } else {
                print_mail_messages_json(&unprocessed);
            }
        }
        EmailCommands::MarkProcessed { email_id, note } => {
            let tags = mail.list_tags().await?;
            let processed = tags.into_iter().find(|t| t.imap_label == "$processed");
            let tag = match processed {
                Some(t) => t,
                None => {
                    mail.create_tag(task_core::MailCreateTagRequest {
                        display_name: "processed".into(),
                        color: "#64748b".into(),
                    })
                    .await?
                }
            };
            mail.set_tag(task_core::MailMessageTagRequest {
                message_id: email_id,
                imap_label: tag.imap_label,
            })
            .await?;
            println!(
                "Marked message {email_id} processed{}",
                note.as_deref()
                    .map(|n| format!(" — {n}"))
                    .unwrap_or_default()
            );
        }
        EmailCommands::Link { .. } | EmailCommands::Unlink { .. } | EmailCommands::List { .. } => {
            run_remote_email_link_command(remote, actor, command).await?;
        }
        EmailCommands::Watch { .. } => unreachable!("email watch is dispatched earlier"),
    }
    Ok(())
}

pub(crate) async fn run_remote_email_link_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: EmailCommands,
) -> eyre::Result<()> {
    let mail = remote.mail().await?;
    match command {
        EmailCommands::Link {
            to,
            reference,
            message_id,
            subject,
            from,
            to_recipients,
            date,
            snippet,
            account_id,
            mailbox,
            imap_uid,
            nc_db_id,
            attachments,
            tags,
        } => {
            let now = chrono::Utc::now();
            let email = task_core::EmailRef {
                uuid: Uuid::new_v4(),
                message_id: message_id.clone(),
                subject: subject.unwrap_or_default(),
                from: from.unwrap_or_default(),
                to: to_recipients
                    .map(|s| {
                        s.split(',')
                            .map(|t| t.trim().to_string())
                            .filter(|t| !t.is_empty())
                            .collect::<Vec<_>>()
                            .into()
                    })
                    .unwrap_or_default(),
                date: date
                    .as_deref()
                    .map(parse_datetime)
                    .transpose()?
                    .unwrap_or(now),
                snippet,
                account_id,
                mailbox,
                imap_uid,
                nc_db_id,
                has_attachments: attachments.map(|n| n > 0).unwrap_or(false),
                attachment_count: attachments.unwrap_or(0),
                linked_by: actor.map(str::to_string),
                linked_at: Some(now),
                user_tags: tags
                    .map(|s| {
                        s.split(',')
                            .map(|t| t.trim().to_string())
                            .filter(|t| !t.is_empty())
                            .collect::<Vec<_>>()
                            .into()
                    })
                    .unwrap_or_default(),
            };
            let response = mail
                .link_email(task_core::EmailLinkRequest {
                    target_type: to.clone(),
                    reference,
                    email,
                    actor: actor.map(str::to_string),
                })
                .await?;
            println!(
                "Linked {} to {} '{}'. ({} emails total)",
                message_id, response.target_type, response.title, response.email_count
            );
        }
        EmailCommands::Unlink {
            to,
            reference,
            message_id,
        } => {
            mail.unlink_email(task_core::EmailUnlinkRequest {
                target_type: to.clone(),
                reference: reference.clone(),
                message_id: message_id.clone(),
                actor: actor.map(str::to_string),
            })
            .await?;
            println!("Unlinked {message_id} from {to} '{reference}'.");
        }
        EmailCommands::List {
            to,
            reference,
            json,
        } => {
            let emails = mail
                .list_linked_emails(task_core::EmailListRequest {
                    target_type: to,
                    reference,
                })
                .await?;
            if json {
                print_emails_json(&emails);
            } else {
                print_emails_table(&emails);
            }
        }
        _ => unreachable!("only email link commands are delegated here"),
    }
    Ok(())
}

pub(crate) fn normalize_message_id(id: &str) -> String {
    id.trim()
        .trim_start_matches('<')
        .trim_end_matches('>')
        .to_ascii_lowercase()
}
