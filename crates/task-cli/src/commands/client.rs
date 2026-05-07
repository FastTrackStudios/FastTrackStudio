#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum ClientCommands {
    /// Create or update a client note (upserts by name)
    Add {
        name: String,
        /// Default hourly rate in cents (e.g. 12000 = $120/hr)
        #[arg(long)]
        rate: Option<u32>,
        /// ISO 4217 currency code, e.g. "USD", "EUR"
        #[arg(long)]
        currency: Option<String>,
        /// Net payment terms in days
        #[arg(long)]
        terms_days: Option<u32>,
        #[arg(long)]
        email: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long)]
        phone: Option<String>,
        /// Invoice Ninja client hashed id (set after sync)
        #[arg(long)]
        invoice_ninja_id: Option<String>,
    },
    /// List all clients
    List {
        #[arg(long)]
        json: bool,
    },
    /// Show a single client
    Show {
        name: String,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_remote_client_command(
    remote: &RemoteVoxConfig,
    command: ClientCommands,
) -> eyre::Result<()> {
    let client = remote.client_repo().await?;
    match command {
        ClientCommands::Add {
            name,
            rate,
            currency,
            terms_days,
            email,
            contact,
            phone,
            invoice_ninja_id,
        } => {
            let existing = remote_find_client_with_client(&client, &name).await?;
            let mut item = existing.unwrap_or_else(|| task_core::Client {
                name: name.clone(),
                ..Default::default()
            });
            if let Some(r) = rate {
                item.default_hourly_rate = Some(r);
            }
            if let Some(c) = currency {
                item.currency_code = c;
            }
            if let Some(d) = terms_days {
                item.payment_terms_days = Some(d);
            }
            if let Some(e) = email {
                item.email = Some(e);
            }
            if let Some(c) = contact {
                item.contact_name = Some(c);
            }
            if let Some(p) = phone {
                item.phone = Some(p);
            }
            if let Some(id) = invoice_ninja_id {
                item.invoice_ninja_id = Some(id);
            }
            let saved = remote_save_client_with_client(&client, &item).await?;
            println!(
                "Saved client '{}' (rate {}¢/hr).",
                saved.name,
                saved.default_hourly_rate.unwrap_or(0)
            );
        }
        ClientCommands::List { json } => {
            let clients = remote_list_clients_with_client(&client).await?;
            if json {
                print_clients_json(&clients);
            } else {
                print_clients_table(&clients);
            }
        }
        ClientCommands::Show { name, json } => {
            let item = remote_find_client_with_client(&client, &name)
                .await?
                .ok_or_else(|| eyre::eyre!("Client not found: {name}"))?;
            if json {
                println!("{}", facet_json::to_string(&item).unwrap_or_default());
            } else {
                print_client_detail(&item);
            }
        }
    }
    Ok(())
}
