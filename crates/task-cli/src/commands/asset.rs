#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum AssetCommands {
    /// Create a new asset
    Create {
        name: String,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        manufacturer: Option<String>,
        #[arg(long)]
        model: Option<String>,
        #[arg(long = "serial-number")]
        serial_number: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long = "assigned-to")]
        assigned_to: Option<String>,
        #[arg(long = "purchase-date")]
        purchase_date: Option<String>,
        #[arg(long = "warranty-until")]
        warranty_until: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List assets
    List {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long = "needs-repair-only")]
        needs_repair_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show a single asset
    Show {
        id: String,
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show an asset inventory report
    Report {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long = "needs-repair-only")]
        needs_repair_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update an asset
    Update {
        id: String,
        #[arg(long)]
        name: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        manufacturer: Option<String>,
        #[arg(long)]
        model: Option<String>,
        #[arg(long = "serial-number")]
        serial_number: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long = "assigned-to")]
        assigned_to: Option<String>,
        #[arg(long = "purchase-date")]
        purchase_date: Option<String>,
        #[arg(long = "warranty-until")]
        warranty_until: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<Option<u64>>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Move an asset to a location / space / rack or case
    Move {
        id: String,
        #[arg(long = "to")]
        location: String,
        #[arg(long)]
        space: Option<String>,
        #[arg(long = "rack-or-case")]
        rack_or_case: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Change an asset status
    Status {
        id: String,
        status: String,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Record maintenance for an asset
    Maintain {
        id: String,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        issue: String,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        warranty: bool,
        #[arg(long)]
        rma: Option<String>,
        #[arg(long)]
        task: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Open repair work for an asset and link the created task
    Repair {
        #[command(subcommand)]
        command: AssetRepairCommands,
    },
    /// Reserve an asset for an event, booking, project, or freeform reference
    Reserve {
        id: String,
        #[arg(long = "for")]
        reference: String,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long = "reserved-by")]
        reserved_by: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        force: bool,
        #[arg(long)]
        json: bool,
    },
    /// Release an asset reservation by id or reference
    Release {
        id: String,
        reservation: String,
        #[arg(long)]
        json: bool,
    },
    /// List reservation and availability conflicts
    Conflicts {
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        space: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        query: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete an asset
    Delete { id: String },
}

#[derive(Subcommand)]
pub(crate) enum AssetRepairCommands {
    /// Create and link a repair task
    Open {
        id: String,
        #[arg(long)]
        title: String,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        contact: Option<String>,
        #[arg(long = "cost-cents")]
        cost_cents: Option<u64>,
        #[arg(long)]
        warranty: bool,
        #[arg(long)]
        rma: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_remote_asset_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: AssetCommands,
) -> eyre::Result<()> {
    let client = remote.asset_repo().await?;
    match command {
        AssetCommands::Create {
            name,
            status,
            manufacturer,
            model,
            serial_number,
            category,
            organization,
            location,
            space,
            rack_or_case,
            assigned_to,
            purchase_date,
            warranty_until,
            vendor,
            cost_cents,
            notes,
            json,
        } => {
            let created = remote_create_asset_with_client(
                &client,
                AssetCreateRequest {
                    name,
                    status,
                    manufacturer,
                    model,
                    serial_number,
                    category,
                    organization,
                    location,
                    space,
                    rack_or_case,
                    assigned_to,
                    purchase_date: purchase_date.as_deref().map(str::parse).transpose()?,
                    warranty_until: warranty_until.as_deref().map(str::parse).transpose()?,
                    vendor,
                    cost_cents,
                    notes,
                    actor: actor.map(str::to_string),
                },
            )
            .await?;
            print_asset_result(&created, json);
        }
        AssetCommands::List {
            location,
            space,
            status,
            category,
            organization,
            query,
            needs_repair_only,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only,
                },
            )
            .await?;
            print_assets(&assets, json);
        }
        AssetCommands::Show { id, md, json } => {
            let asset = remote_find_asset_with_client(&client, &id).await?;
            if md {
                println!("{}", task_core::asset::render_asset_body(&asset));
            } else {
                print_asset_result(&asset, json);
            }
        }
        AssetCommands::Report {
            location,
            space,
            status,
            category,
            organization,
            query,
            needs_repair_only,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only,
                },
            )
            .await?;
            let report = task_core::asset::build_asset_report(&assets, Utc::now().date_naive());
            if json {
                println!("{}", facet_json::to_string(&report).unwrap_or_default());
            } else {
                println!("Assets: {}", report.asset_count);
                print_assets(&report.assets, false);
            }
        }
        AssetCommands::Update {
            id,
            name,
            status,
            manufacturer,
            model,
            serial_number,
            category,
            organization,
            location,
            space,
            rack_or_case,
            assigned_to,
            purchase_date,
            warranty_until,
            vendor,
            cost_cents,
            notes,
            json,
        } => {
            let patch = AssetPatch {
                name,
                status,
                manufacturer,
                model,
                serial_number,
                category,
                organization,
                location,
                space,
                rack_or_case,
                assigned_to,
                purchase_date,
                warranty_until,
                vendor,
                cost_cents,
                notes,
            };
            let updated = remote_update_asset_with_client(&client, &id, patch).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Move {
            id,
            location,
            space,
            rack_or_case,
            json,
        } => {
            let updated = remote_update_asset_with_client(
                &client,
                &id,
                AssetPatch {
                    location: Some(location),
                    space,
                    rack_or_case,
                    ..AssetPatch::default()
                },
            )
            .await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Status {
            id,
            status,
            notes,
            json,
        } => {
            let updated = remote_update_asset_with_client(
                &client,
                &id,
                AssetPatch {
                    status: Some(status),
                    notes,
                    ..AssetPatch::default()
                },
            )
            .await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Maintain {
            id,
            date,
            issue,
            vendor,
            contact,
            cost_cents,
            warranty,
            rma,
            task,
            notes,
            json,
        } => {
            let mut asset = remote_find_asset_with_client(&client, &id).await?;
            asset.maintenance.push(AssetMaintenanceRecord {
                date: date
                    .as_deref()
                    .map(str::parse)
                    .transpose()?
                    .unwrap_or_else(|| Utc::now().date_naive()),
                issue,
                vendor,
                contact,
                cost_cents: cost_cents.map(|v| v as i64),
                warranty,
                rma,
                task: task.map(WikiLink),
                notes,
            });
            if asset.status == AssetStatus::Available {
                asset.status = AssetStatus::MaintenanceDue;
            }
            asset.date_modified = Some(Utc::now());
            let updated = remote_replace_asset_with_client(&client, asset).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Repair { command } => match command {
            AssetRepairCommands::Open {
                id,
                title,
                notes,
                vendor,
                contact,
                cost_cents,
                warranty,
                rma,
                json,
            } => {
                let mut asset = remote_find_asset_with_client(&client, &id).await?;
                asset.maintenance.push(AssetMaintenanceRecord {
                    date: Utc::now().date_naive(),
                    issue: title,
                    vendor,
                    contact,
                    cost_cents: cost_cents.map(|v| v as i64),
                    warranty,
                    rma,
                    notes,
                    ..AssetMaintenanceRecord::default()
                });
                asset.status = AssetStatus::NeedsRepair;
                asset.date_modified = Some(Utc::now());
                let updated = remote_replace_asset_with_client(&client, asset).await?;
                print_asset_result(&updated, json);
            }
        },
        AssetCommands::Reserve {
            id,
            reference,
            from,
            to,
            reserved_by,
            notes,
            force,
            json,
        } => {
            let response = remote_reserve_asset_with_client(
                &client,
                &id,
                AssetReserveRequest {
                    reference,
                    starts_at: from.as_deref().map(parse_datetime).transpose()?,
                    ends_at: to.as_deref().map(parse_datetime).transpose()?,
                    reserved_by,
                    notes,
                    force,
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&response).unwrap_or_default());
            } else {
                println!(
                    "Reserved {} for {}.",
                    response.asset.name, response.reservation.reference.0
                );
                print_asset_conflicts(&response.conflicts, false);
            }
        }
        AssetCommands::Release {
            id,
            reservation,
            json,
        } => {
            let updated =
                remote_release_asset_reservation_with_client(&client, &id, &reservation).await?;
            print_asset_result(&updated, json);
        }
        AssetCommands::Conflicts {
            location,
            space,
            status,
            category,
            organization,
            query,
            json,
        } => {
            let assets = remote_list_assets_filtered(
                &client,
                AssetFilter {
                    location,
                    space,
                    status,
                    category,
                    organization,
                    query,
                    needs_repair_only: false,
                },
            )
            .await?;
            let conflicts = task_core::asset::collect_asset_conflicts(&assets);
            print_asset_conflicts(&conflicts, json);
        }
        AssetCommands::Delete { id } => {
            let asset = remote_find_asset_with_client(&client, &id).await?;
            client.delete_asset(asset.uuid.to_string()).await?;
            println!("Deleted asset: {id}");
        }
    }
    Ok(())
}

pub(crate) async fn remote_list_assets_with_client(
    client: &task_core::asset::AssetRepoClient,
) -> eyre::Result<Vec<Asset>> {
    let rows = client.list_assets(None, None, None, Some(10_000)).await?;
    rows.into_iter().map(api_to_model).collect()
}

pub(crate) async fn remote_list_assets_filtered(
    client: &task_core::asset::AssetRepoClient,
    filter: AssetFilter,
) -> eyre::Result<Vec<Asset>> {
    let mut assets = remote_list_assets_with_client(client).await?;
    assets.retain(|asset| task_core::asset::matches_asset_filter(asset, &filter));
    assets.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.number.cmp(&b.number)));
    Ok(assets)
}

pub(crate) async fn remote_find_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
) -> eyre::Result<Asset> {
    remote_list_assets_with_client(client)
        .await?
        .into_iter()
        .find(|asset| {
            asset.uuid.to_string() == reference
                || asset.id.eq_ignore_ascii_case(reference)
                || asset.name.eq_ignore_ascii_case(reference)
        })
        .ok_or_else(|| eyre::eyre!("Asset not found: {reference}"))
}

pub(crate) async fn remote_create_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    request: AssetCreateRequest,
) -> eyre::Result<Asset> {
    let now = Utc::now();
    let number = remote_list_assets_with_client(client)
        .await?
        .into_iter()
        .filter(|asset| asset.id.starts_with(&format!("AST-{:04}-", now.year())))
        .map(|asset| asset.number)
        .max()
        .unwrap_or(0)
        + 1;
    let status = request
        .status
        .as_deref()
        .and_then(task_core::asset::parse_asset_status)
        .unwrap_or_default();
    let asset = Asset {
        uuid: Uuid::new_v4(),
        id: task_core::asset::format_asset_id(now.year(), number),
        number,
        name: request.name,
        status,
        manufacturer: request.manufacturer,
        model: request.model,
        serial_number: request.serial_number,
        category: request.category,
        organization: request.organization,
        location: request.location.map(WikiLink),
        space: request.space.map(WikiLink),
        rack_or_case: request.rack_or_case,
        assigned_to: request.assigned_to,
        purchase_date: request.purchase_date,
        warranty_until: request.warranty_until,
        vendor: request.vendor,
        cost_cents: request.cost_cents.map(|value| value as i64),
        notes: request.notes,
        created_by: request.actor,
        date_created: Some(now),
        date_modified: Some(now),
        ..Asset::default()
    };
    let create: task_core::asset::AssetApiCreate = model_to_api(&asset)?;
    api_to_model(client.create_asset(create).await?)
}

pub(crate) async fn remote_update_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    patch: AssetPatch,
) -> eyre::Result<Asset> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    apply_asset_patch(&mut asset, patch)?;
    remote_replace_asset_with_client(client, asset).await
}

pub(crate) async fn remote_replace_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    asset: Asset,
) -> eyre::Result<Asset> {
    let update: task_core::asset::AssetApiUpdate = model_to_api(&asset)?;
    api_to_model(client.update_asset(asset.uuid.to_string(), update).await?)
}

pub(crate) async fn remote_reserve_asset_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    request: AssetReserveRequest,
) -> eyre::Result<AssetReservationResponse> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    let reservation = AssetReservationRecord {
        id: Uuid::new_v4().to_string(),
        reference: WikiLink(request.reference),
        starts_at: request.starts_at,
        ends_at: request.ends_at,
        reserved_by: request.reserved_by,
        notes: request.notes,
    };
    let conflicts = task_core::asset::conflicts_for_reservation(&asset, &reservation);
    if !request.force && !conflicts.is_empty() {
        eyre::bail!(
            "asset reservation has {} conflict(s); rerun with --force to record anyway",
            conflicts.len()
        );
    }
    asset.reservations.push(reservation.clone());
    if asset.status == AssetStatus::Available {
        asset.status = AssetStatus::Reserved;
    }
    asset.date_modified = Some(Utc::now());
    let asset = remote_replace_asset_with_client(client, asset).await?;
    Ok(AssetReservationResponse {
        asset,
        reservation,
        conflicts,
    })
}

pub(crate) async fn remote_release_asset_reservation_with_client(
    client: &task_core::asset::AssetRepoClient,
    reference: &str,
    reservation_ref: &str,
) -> eyre::Result<Asset> {
    let mut asset = remote_find_asset_with_client(client, reference).await?;
    let before = asset.reservations.len();
    asset.reservations.retain(|reservation| {
        reservation.id != reservation_ref
            && !reservation
                .reference
                .0
                .eq_ignore_ascii_case(reservation_ref)
    });
    if asset.reservations.len() == before {
        eyre::bail!("Asset reservation not found: {reservation_ref}");
    }
    if asset.status == AssetStatus::Reserved && asset.reservations.is_empty() {
        asset.status = AssetStatus::Available;
    }
    asset.date_modified = Some(Utc::now());
    remote_replace_asset_with_client(client, asset).await
}

pub(crate) fn apply_asset_patch(asset: &mut Asset, patch: AssetPatch) -> eyre::Result<()> {
    if let Some(value) = patch.name {
        asset.name = value;
    }
    if let Some(value) = patch.status {
        asset.status = task_core::asset::parse_asset_status(&value)
            .ok_or_else(|| eyre::eyre!("invalid asset status: {value}"))?;
    }
    if let Some(value) = patch.manufacturer {
        asset.manufacturer = empty_to_none(value);
    }
    if let Some(value) = patch.model {
        asset.model = empty_to_none(value);
    }
    if let Some(value) = patch.serial_number {
        asset.serial_number = empty_to_none(value);
    }
    if let Some(value) = patch.category {
        asset.category = empty_to_none(value);
    }
    if let Some(value) = patch.organization {
        asset.organization = empty_to_none(value);
    }
    if let Some(value) = patch.location {
        asset.location = empty_to_none(value).map(WikiLink);
    }
    if let Some(value) = patch.space {
        asset.space = empty_to_none(value).map(WikiLink);
    }
    if let Some(value) = patch.rack_or_case {
        asset.rack_or_case = empty_to_none(value);
    }
    if let Some(value) = patch.assigned_to {
        asset.assigned_to = empty_to_none(value);
    }
    if let Some(value) = patch.purchase_date {
        asset.purchase_date = if value.trim().is_empty() || value == "clear" {
            None
        } else {
            Some(value.parse()?)
        };
    }
    if let Some(value) = patch.warranty_until {
        asset.warranty_until = if value.trim().is_empty() || value == "clear" {
            None
        } else {
            Some(value.parse()?)
        };
    }
    if let Some(value) = patch.vendor {
        asset.vendor = empty_to_none(value);
    }
    if let Some(value) = patch.cost_cents {
        asset.cost_cents = value.map(|amount| amount as i64);
    }
    if let Some(value) = patch.notes {
        asset.notes = empty_to_none(value);
    }
    asset.date_modified = Some(Utc::now());
    Ok(())
}

pub(crate) fn empty_to_none(value: String) -> Option<String> {
    if value.trim().is_empty() || value == "clear" {
        None
    } else {
        Some(value)
    }
}
