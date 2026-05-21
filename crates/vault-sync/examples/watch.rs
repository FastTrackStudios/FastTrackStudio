//! Small CLI for poking at a live task-server.
//!
//! Usage:
//!   cargo run -p vault-sync --example watch -- <base_url> <vault_id> [subcommand]
//!
//! Subcommands:
//!   manifest                       — list all files in the vault (default)
//!   get  <rel_path>                — print file contents to stdout
//!   put  <rel_path> <local_path>   — push a local file (IfMatch::Force)
//!   del  <rel_path>                — delete (IfMatch::Force)
//!   watch                          — subscribe and print events forever
//!
//! Start the server in another shell first:
//!   TASK_SERVER_VAULT_ROOT=/tmp/vaults cargo run -p task-server
//!
//! Then for example:
//!   cargo run -p vault-sync --example watch -- \
//!     http://127.0.0.1:9090 demo manifest

use std::env;
use std::process::ExitCode;

use vault_sync::{Bytes, IfMatch, VaultClient};

#[tokio::main]
async fn main() -> ExitCode {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("usage: watch <base_url> <vault_id> [manifest|get|put|del|watch] ...");
        return ExitCode::from(2);
    }
    let base = &args[1];
    let vault_id = &args[2];
    let subcmd = args.get(3).map(String::as_str).unwrap_or("manifest");

    let client = match VaultClient::new(base, vault_id.clone()) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("bad URL: {e}");
            return ExitCode::from(2);
        }
    };

    let result: Result<(), vault_sync::Error> = match subcmd {
        "manifest" => {
            let m = client.manifest().await;
            match m {
                Ok(m) => {
                    println!("vault {} — {} file(s)", m.vault_id, m.files.len());
                    for f in m.files {
                        println!(
                            "  {:>10} {} {}",
                            f.size,
                            &f.sha256[..f.sha256.len().min(12)],
                            f.path
                        );
                    }
                    Ok(())
                }
                Err(e) => Err(e),
            }
        }
        "get" => match args.get(4) {
            Some(rel) => match client.get_file(rel).await {
                Ok(bytes) => {
                    use std::io::Write;
                    std::io::stdout().write_all(&bytes).ok();
                    Ok(())
                }
                Err(e) => Err(e),
            },
            None => {
                eprintln!("usage: get <rel_path>");
                return ExitCode::from(2);
            }
        },
        "put" => match (args.get(4), args.get(5)) {
            (Some(rel), Some(local)) => match std::fs::read(local) {
                Ok(bytes) => match client
                    .put_file(rel, Bytes::from(bytes), IfMatch::Force)
                    .await
                {
                    Ok(sha) => {
                        println!("ok {sha}");
                        Ok(())
                    }
                    Err(e) => Err(e),
                },
                Err(e) => {
                    eprintln!("read {local}: {e}");
                    return ExitCode::from(1);
                }
            },
            _ => {
                eprintln!("usage: put <rel_path> <local_path>");
                return ExitCode::from(2);
            }
        },
        "del" => match args.get(4) {
            Some(rel) => client.delete_file(rel, IfMatch::Force).await,
            None => {
                eprintln!("usage: del <rel_path>");
                return ExitCode::from(2);
            }
        },
        "watch" => {
            let mut sub = match client.subscribe().await {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("subscribe: {e}");
                    return ExitCode::from(1);
                }
            };
            println!("subscribed to {base}/vault/{vault_id} — Ctrl-C to exit");
            loop {
                match sub.next_event().await {
                    Ok(Some(evt)) => println!("{}", serde_json::to_string(&evt).unwrap()),
                    Ok(None) => {
                        println!("(server closed)");
                        return ExitCode::SUCCESS;
                    }
                    Err(e) => {
                        eprintln!("ws: {e}");
                        return ExitCode::from(1);
                    }
                }
            }
        }
        other => {
            eprintln!("unknown subcommand: {other}");
            return ExitCode::from(2);
        }
    };

    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::from(1)
        }
    }
}
