#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum CommentCommands {
    /// Add a comment to a task
    Add {
        reference: String,
        #[arg(long)]
        body: String,
        /// Timecode like "2:34" or "2:30-2:36" (audio/video)
        #[arg(long)]
        timecode: Option<String>,
    },
    /// List comments on a task
    List {
        reference: String,
        #[arg(long)]
        json: bool,
    },
    /// Reply to an existing comment by id
    Reply {
        reference: String,
        parent_id: String,
        #[arg(long)]
        body: String,
    },
    /// Mark a comment resolved (by id)
    Resolve {
        reference: String,
        comment_id: String,
    },
    /// Unresolve a comment
    Reopen {
        reference: String,
        comment_id: String,
    },
}

pub(crate) async fn run_remote_comment_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: CommentCommands,
) -> eyre::Result<()> {
    let client = remote.task_repo().await?;
    match command {
        CommentCommands::Add {
            reference,
            body,
            timecode,
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let time_ref = match timecode.as_deref() {
                Some(tc) => Some(
                    task_core::workflows::parse_timecode(tc)
                        .ok_or_else(|| eyre::eyre!("Invalid timecode: {tc}"))?,
                ),
                None => None,
            };
            let now = chrono::Local::now().naive_local();
            let mentions = Comment::extract_mentions(&body);
            let new_comment = Comment {
                id: Comment::generate_id(&author, Some(now), &body),
                author,
                body,
                created_at: Some(now),
                time_ref,
                mentions,
                ..Default::default()
            };
            let mut comments = parse_comments(&task.body);
            comments.push(new_comment.clone());
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Comment added ({}).", new_comment.id);
        }
        CommentCommands::List { reference, json } => {
            let task = remote_find_task_with_client(&client, &reference).await?;
            let comments = parse_comments(&task.body);
            if json {
                print_comments_json(&comments);
            } else {
                print_comments_table(&comments);
            }
        }
        CommentCommands::Reply {
            reference,
            parent_id,
            body,
        } => {
            let author = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let mut comments = parse_comments(&task.body);
            if !comments.iter().any(|c| c.id == parent_id) {
                eyre::bail!("No comment with id {parent_id} on task {reference}");
            }
            let now = chrono::Local::now().naive_local();
            let mentions = Comment::extract_mentions(&body);
            let new_comment = Comment {
                id: Comment::generate_id(&author, Some(now), &body),
                author,
                body,
                created_at: Some(now),
                reply_to: Some(parent_id),
                mentions,
                ..Default::default()
            };
            comments.push(new_comment.clone());
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Reply added ({}).", new_comment.id);
        }
        CommentCommands::Resolve {
            reference,
            comment_id,
        } => {
            let resolver = require_actor(&actor.map(str::to_string))?;
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = true;
            c.resolved_by = Some(resolver);
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Resolved comment {comment_id}.");
        }
        CommentCommands::Reopen {
            reference,
            comment_id,
        } => {
            let mut task = remote_find_task_with_client(&client, &reference).await?;
            let mut comments = parse_comments(&task.body);
            let Some(c) = comments.iter_mut().find(|c| c.id == comment_id) else {
                eyre::bail!("No comment with id {comment_id}");
            };
            c.resolved = false;
            c.resolved_by = None;
            task.body = splice_comments(&task.body, &comments);
            remote_update_task_with_client(&client, &task).await?;
            println!("Reopened comment {comment_id}.");
        }
    }
    Ok(())
}

/// Splice a rendered `## Comments` block back into the body, replacing any
/// existing block or appending one.
pub(crate) fn splice_comments(body: &str, comments: &[Comment]) -> String {
    let rendered = render_comments(comments);

    let mut lines = body.lines().collect::<Vec<_>>();
    let start = lines.iter().position(|l| l.trim() == "## Comments");

    if let Some(s) = start {
        // Find the next `## ` heading after the comments section, or EOF.
        let end = lines
            .iter()
            .enumerate()
            .skip(s + 1)
            .find(|(_, l)| l.trim_start().starts_with("## "))
            .map(|(i, _)| i)
            .unwrap_or(lines.len());

        let mut out = lines[..s].join("\n");
        if !out.is_empty() && !out.ends_with('\n') {
            out.push('\n');
        }
        out.push_str(&rendered);
        if end < lines.len() {
            out.push('\n');
            lines.drain(0..end);
            out.push_str(&lines.join("\n"));
        }
        out
    } else {
        let mut out = body.trim_end().to_string();
        if !out.is_empty() {
            out.push_str("\n\n");
        }
        out.push_str(&rendered);
        out
    }
}
