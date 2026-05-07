#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum ServerCommands {
    /// Add or update a named server profile
    Add {
        name: String,
        #[arg(long)]
        url: String,
        #[arg(long)]
        session_token: Option<String>,
        #[arg(long)]
        organization_id: Option<String>,
        #[arg(long)]
        use_now: bool,
    },
    /// List configured server profiles
    List {
        #[arg(long)]
        json: bool,
    },
    /// Select the default server profile
    Use { name: String },
    /// Show the active/default server profile
    Current {
        #[arg(long)]
        json: bool,
    },
    /// Run doctor against a configured server profile
    Doctor {
        name: Option<String>,
        #[arg(long)]
        json: bool,
        #[arg(long)]
        deep: bool,
    },
}

pub(crate) async fn run_server_command(command: ServerCommands) -> eyre::Result<()> {
    match command {
        ServerCommands::Add {
            name,
            url,
            session_token,
            organization_id,
            use_now,
        } => {
            let mut profiles = load_server_profiles().unwrap_or_default();
            profiles.servers.retain(|profile| profile.name != name);
            profiles.servers.push(ServerProfile {
                name: name.clone(),
                url,
                session_token,
                organization_id,
            });
            if use_now || profiles.default.is_none() {
                profiles.default = Some(name.clone());
            }
            save_server_profiles(&profiles)?;
            println!("Saved server profile '{name}'.");
        }
        ServerCommands::List { json } => {
            let profiles = load_server_profiles().unwrap_or_default();
            if json {
                print_server_profiles_json(&profiles);
            } else if profiles.servers.is_empty() {
                println!("No server profiles configured.");
            } else {
                println!("{:<18} {:<8} URL", "NAME", "DEFAULT");
                println!("{}", "-".repeat(72));
                for profile in profiles.servers {
                    println!(
                        "{:<18} {:<8} {}",
                        profile.name,
                        if profiles.default.as_deref() == Some(&profile.name) {
                            "yes"
                        } else {
                            ""
                        },
                        profile.url
                    );
                }
            }
        }
        ServerCommands::Use { name } => {
            let mut profiles = load_server_profiles().unwrap_or_default();
            if profiles.servers.iter().any(|profile| profile.name == name) {
                profiles.default = Some(name.clone());
                save_server_profiles(&profiles)?;
                println!("Using server profile '{name}'.");
            } else {
                eyre::bail!("Unknown server profile: {name}");
            }
        }
        ServerCommands::Current { json } => {
            let profiles = load_server_profiles().unwrap_or_default();
            let current = profiles.current();
            if json {
                print_server_profile_json(current.as_ref());
            } else if let Some(profile) = current {
                println!("{} -> {}", profile.name, profile.url);
            } else {
                println!("No default server profile configured.");
            }
        }
        ServerCommands::Doctor { name, json, deep } => {
            let profiles = load_server_profiles().unwrap_or_default();
            let profile = name
                .as_deref()
                .and_then(|name| profiles.resolve(name))
                .or_else(|| profiles.current())
                .ok_or_else(|| eyre::eyre!("No server profile configured."))?;
            let remote =
                RemoteVoxConfig::new(profile.name, profile.session_token, profile.organization_id)?;
            run_remote_doctor(&remote, json, deep).await?;
        }
    }
    Ok(())
}

pub(crate) fn print_server_profiles_json(profiles: &ServerProfiles) {
    print!("{{\"default\":");
    match &profiles.default {
        Some(default) => print!("\"{}\"", escape_json(default)),
        None => print!("null"),
    }
    print!(",\"servers\":[");
    for (idx, profile) in profiles.servers.iter().enumerate() {
        if idx > 0 {
            print!(",");
        }
        print_server_profile_object(profile);
    }
    println!("]}}");
}

pub(crate) fn print_server_profile_json(profile: Option<&ServerProfile>) {
    match profile {
        Some(profile) => {
            print_server_profile_object(profile);
            println!();
        }
        None => println!("null"),
    }
}

pub(crate) fn print_server_profile_object(profile: &ServerProfile) {
    print!(
        "{{\"name\":\"{}\",\"url\":\"{}\",\"session_token_configured\":{},\"organization_id\":{}}}",
        escape_json(&profile.name),
        escape_json(&profile.url),
        profile.session_token.is_some(),
        profile
            .organization_id
            .as_deref()
            .map(|org| format!("\"{}\"", escape_json(org)))
            .unwrap_or_else(|| "null".into())
    );
}
