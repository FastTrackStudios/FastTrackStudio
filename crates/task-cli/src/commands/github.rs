#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum GithubCommands {
    /// Sync GitHub Issues with the vault — pull new issues, push status changes
    Sync {
        /// Repository in owner/repo format (e.g. "FastTrackStudios/task")
        #[arg(long)]
        repo: String,
        /// GitHub token (defaults to GITHUB_TOKEN or GH_TOKEN env)
        #[arg(long, env = "GITHUB_TOKEN")]
        token: Option<String>,
        /// Print what would happen without making changes
        #[arg(long)]
        plan: bool,
        /// Output as JSON
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_github_command_remote(
    remote: &RemoteVoxConfig,
    command: GithubCommands,
) -> eyre::Result<()> {
    match command {
        GithubCommands::Sync {
            repo,
            token,
            plan,
            json,
        } => {
            use task_core::provider::github;

            let (owner, name) = github::parse_repo(&repo).map_err(|e| eyre::eyre!("{e}"))?;
            let token_val =
                github::resolve_token(token.as_deref()).map_err(|e| eyre::eyre!("{e}"))?;

            let task_client = remote.task_repo().await?;
            let all_tasks = remote_list_tasks_with_client(&task_client).await?;
            let gh_tasks: Vec<_> = all_tasks
                .into_iter()
                .filter(|t| t.external_source.as_deref() == Some("github"))
                .collect();

            let config = github::GitHubConfig::new(token_val, format!("{owner}/{name}"));

            if plan {
                let sync_client = github::GitHubSync::new(config);
                let remote_tasks = sync_client
                    .pull_issues()
                    .await
                    .map_err(|e| eyre::eyre!("GitHub pull failed: {e}"))?;
                let sync_plan = github::build_sync_plan(&gh_tasks, &remote_tasks);
                if json {
                    let actions: Vec<String> = sync_plan
                        .actions
                        .iter()
                        .map(|a| match a {
                            github::SyncAction::Pull {
                                issue_number,
                                title,
                            } => format!(
                                r#"{{"action":"pull","issue":{},"title":{}}}"#,
                                facet_json::to_string(issue_number).unwrap_or_default(),
                                facet_json::to_string(title).unwrap_or_default(),
                            ),
                            github::SyncAction::Push {
                                issue_number,
                                title,
                                new_state,
                            } => format!(
                                r#"{{"action":"push","issue":{},"title":{},"state":{}}}"#,
                                facet_json::to_string(issue_number).unwrap_or_default(),
                                facet_json::to_string(title).unwrap_or_default(),
                                facet_json::to_string(new_state).unwrap_or_default(),
                            ),
                        })
                        .collect();
                    println!("[{}]", actions.join(","));
                } else {
                    print!("{sync_plan}");
                }
                return Ok(());
            }

            let sync_client = github::GitHubSync::new(config);
            let result = sync_client
                .sync(&gh_tasks)
                .await
                .map_err(|e| eyre::eyre!("GitHub sync failed: {e}"))?;
            if json {
                println!(
                    r#"{{"issues_pulled":{},"tasks_created":{},"statuses_pushed":{},"errors":{}}}"#,
                    result.issues_pulled,
                    result.tasks_created,
                    result.statuses_pushed,
                    facet_json::to_string(&result.errors).unwrap_or_default(),
                );
            } else {
                println!("{}", github::format_sync_result(&result));
            }
        }
    }
    Ok(())
}

pub(crate) fn env_truthy(value: &str) -> bool {
    matches!(
        value.to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}
