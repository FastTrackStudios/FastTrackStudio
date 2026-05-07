#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum CalendarCommands {
    /// List calendar events, optionally filtered by an RFC3339 or date range
    List {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a calendar event by id or exact title
    Show {
        reference: String,
        #[arg(long)]
        json: bool,
    },
    /// Create a calendar event
    Add {
        #[arg(long)]
        title: String,
        /// Start time: RFC3339, "YYYY-MM-DDTHH:MM", or "YYYY-MM-DD HH:MM"
        #[arg(long)]
        start: String,
        /// End time in the same formats as --start
        #[arg(long)]
        end: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        venue: Option<String>,
        #[arg(long)]
        space: Vec<String>,
        #[arg(long)]
        all_day: bool,
        /// confirmed, tentative, or cancelled
        #[arg(long, default_value = "confirmed")]
        status: String,
        #[arg(long)]
        recurrence: Option<String>,
        #[arg(long)]
        attendee: Vec<String>,
        #[arg(long)]
        asset: Vec<String>,
        #[arg(long = "force-assets")]
        force_assets: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update mutable calendar event fields
    Update {
        reference: String,
        #[arg(long)]
        title: Option<String>,
        #[arg(long)]
        start: Option<String>,
        /// End time, or "clear"
        #[arg(long)]
        end: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        venue: Option<String>,
        #[arg(long)]
        space: Vec<String>,
        #[arg(long)]
        all_day: Option<bool>,
        /// confirmed, tentative, or cancelled
        #[arg(long)]
        status: Option<String>,
        /// RRULE string, or "clear"
        #[arg(long)]
        recurrence: Option<String>,
        /// Replace attendees with comma-separated list. Pass "" to clear.
        #[arg(long)]
        attendees: Option<String>,
        /// Replace markdown body
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete a calendar event by id or exact title
    Delete { reference: String },
    /// CardDAV addressbook controls
    Carddav {
        #[command(subcommand)]
        command: CardDavCommands,
    },
}

#[derive(Subcommand)]
pub(crate) enum CardDavCommands {
    /// Discover addressbooks
    Discover {
        #[arg(long)]
        json: bool,
    },
    /// Sync an addressbook and print vCard objects
    Sync {
        #[arg(long, default_value = "contacts")]
        addressbook: String,
        #[arg(long)]
        sync_token: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_remote_calendar_command(
    remote: &RemoteVoxConfig,
    command: CalendarCommands,
) -> eyre::Result<()> {
    let client = remote.calendar().await?;
    let repo = remote.calendar_event_repo().await?;
    match command {
        CalendarCommands::List { from, to, json } => {
            let events =
                remote_calendar_events_for_range(&client, from.as_deref(), to.as_deref()).await?;
            if json {
                print_calendar_events_json(&events);
            } else {
                print_calendar_events_table(&events);
            }
        }
        CalendarCommands::Show { reference, json } => {
            let event = remote_find_calendar_event(&client, &reference).await?;
            if json {
                println!("{}", facet_json::to_string(&event).unwrap_or_default());
            } else {
                print_calendar_event_detail(&event);
            }
        }
        CalendarCommands::Add {
            title,
            start,
            end,
            description,
            location,
            venue,
            space,
            all_day,
            status,
            recurrence,
            attendee,
            asset,
            force_assets,
            json,
        } => {
            let start = parse_datetime(&start)?;
            let end = end.as_deref().map(parse_datetime).transpose()?;
            let event = CalendarEvent {
                title,
                description,
                location,
                venue: venue.map(WikiLink),
                spaces: space.into_iter().map(WikiLink).collect(),
                start,
                end,
                all_day,
                status: parse_calendar_status(&status)?,
                recurrence,
                attendees: attendee.into(),
                ..CalendarEvent::default()
            };
            let create: task_core::calendar_event::CalendarEventApiCreate = model_to_api(&event)?;
            let created: CalendarEvent = api_to_model(repo.create_calendar_event(create).await?)?;
            let asset_reservations = if asset.is_empty() {
                Vec::new()
            } else {
                let asset_repo = remote.asset_repo().await?;
                let reference = created.id.clone().unwrap_or_else(|| created.title.clone());
                let mut reservations = Vec::new();
                for asset_ref in asset {
                    reservations.push(
                        crate::commands::asset::remote_reserve_asset_with_client(
                            &asset_repo,
                            &asset_ref,
                            AssetReserveRequest {
                                reference: reference.clone(),
                                starts_at: Some(created.start),
                                ends_at: created.end,
                                force: force_assets,
                                ..AssetReserveRequest::default()
                            },
                        )
                        .await?,
                    );
                }
                reservations
            };
            if json {
                println!(
                    "{{\"event\":{},\"asset_reservations\":{}}}",
                    facet_json::to_string(&created).unwrap_or_default(),
                    facet_json::to_string(&asset_reservations).unwrap_or_default()
                );
            } else {
                println!("Created calendar event: {}", created.title);
                println!("  id: {}", created.id.as_deref().unwrap_or("—"));
                for reservation in &asset_reservations {
                    println!(
                        "  reserved asset: {} ({})",
                        reservation.asset.name, reservation.asset.id
                    );
                    print_asset_conflicts(&reservation.conflicts, false);
                }
            }
        }
        CalendarCommands::Update {
            reference,
            title,
            start,
            end,
            description,
            location,
            venue,
            space,
            all_day,
            status,
            recurrence,
            attendees,
            body,
            json,
        } => {
            let patch = build_calendar_patch(CalendarPatchInput {
                title,
                start,
                end,
                description,
                location,
                venue,
                space,
                all_day,
                status,
                recurrence,
                attendees,
                body,
            })?;
            let mut event = remote_find_calendar_event_with_client(&repo, &reference).await?;
            apply_remote_calendar_patch(&mut event, patch);
            let updated = remote_update_calendar_event_with_client(&repo, &event).await?;
            if json {
                println!("{}", facet_json::to_string(&updated).unwrap_or_default());
            } else {
                println!("Updated calendar event: {}", updated.title);
            }
        }
        CalendarCommands::Delete { reference } => {
            let event = remote_find_calendar_event_with_client(&repo, &reference).await?;
            repo.delete_calendar_event(event.uuid.to_string()).await?;
            println!("Deleted calendar event: {reference}");
        }
        CalendarCommands::Carddav { command } => match command {
            CardDavCommands::Discover { json } => {
                let discovery = client.discover_carddav().await?;
                if json {
                    println!("{}", facet_json::to_string(&discovery).unwrap_or_default());
                } else {
                    println!("Addressbook home: {}", discovery.addressbook_home_set);
                    for book in discovery.addressbooks {
                        println!(
                            "{}\t{}",
                            book.name,
                            book.display_name.as_deref().unwrap_or("—")
                        );
                    }
                }
            }
            CardDavCommands::Sync {
                addressbook,
                sync_token,
                json,
            } => {
                let sync = client
                    .addressbook_sync_collection(CardDavSyncCollectionRequest {
                        addressbook,
                        sync_token,
                    })
                    .await?;
                if json {
                    println!("{}", facet_json::to_string(&sync).unwrap_or_default());
                } else {
                    println!("sync-token: {}", sync.sync_token.as_deref().unwrap_or("—"));
                    for object in sync.objects {
                        let name = object
                            .contact
                            .as_ref()
                            .and_then(|contact| contact.full_name.as_deref())
                            .unwrap_or(object.href.as_str());
                        println!("{}\t{}", object.href, name);
                    }
                }
            }
        },
    }
    Ok(())
}

pub(crate) async fn remote_calendar_events_for_range(
    client: &task_core::service::CalendarServiceClient,
    from: Option<&str>,
    to: Option<&str>,
) -> eyre::Result<Vec<CalendarEvent>> {
    let from = from
        .map(parse_calendar_boundary_start)
        .transpose()?
        .unwrap_or_else(|| parse_datetime("1970-01-01T00:00:00Z").unwrap())
        .to_rfc3339();
    let to = to
        .map(parse_calendar_boundary_end)
        .transpose()?
        .unwrap_or_else(|| parse_datetime("9999-12-31T23:59:59Z").unwrap())
        .to_rfc3339();
    Ok(client.events_between(from, to).await?)
}

pub(crate) async fn remote_find_calendar_event(
    client: &task_core::service::CalendarServiceClient,
    reference: &str,
) -> eyre::Result<CalendarEvent> {
    remote_calendar_events_for_range(client, None, None)
        .await?
        .into_iter()
        .find(|e| e.id.as_deref() == Some(reference) || e.title == reference)
        .ok_or_else(|| eyre::eyre!("Calendar event not found: {reference}"))
}

/// Typed inputs for [`build_calendar_patch`].
pub(crate) struct CalendarPatchInput {
    title: Option<String>,
    start: Option<String>,
    end: Option<String>,
    description: Option<String>,
    location: Option<String>,
    venue: Option<String>,
    space: Vec<String>,
    all_day: Option<bool>,
    status: Option<String>,
    recurrence: Option<String>,
    attendees: Option<String>,
    body: Option<String>,
}

pub(crate) fn build_calendar_patch(input: CalendarPatchInput) -> eyre::Result<CalendarEventPatch> {
    let CalendarPatchInput {
        title,
        start,
        end,
        description,
        location,
        venue,
        space,
        all_day,
        status,
        recurrence,
        attendees,
        body,
    } = input;
    Ok(CalendarEventPatch {
        title,
        description: description.map(optional_string_field),
        location: location.map(optional_string_field),
        venue: venue.map(|venue| {
            if venue == "clear" || venue.is_empty() {
                None
            } else {
                Some(WikiLink(venue))
            }
        }),
        spaces: if space.is_empty() {
            None
        } else {
            Some(space.into_iter().map(WikiLink).collect())
        },
        start: start.as_deref().map(parse_datetime).transpose()?,
        end: match end {
            Some(s) if s == "clear" || s.is_empty() => Some(None),
            Some(s) => Some(Some(parse_datetime(&s)?)),
            None => None,
        },
        all_day,
        status: status.as_deref().map(parse_calendar_status).transpose()?,
        recurrence: recurrence.map(|s| {
            if s == "clear" || s.is_empty() {
                None
            } else {
                Some(s)
            }
        }),
        attendees: attendees.map(|s| {
            if s.is_empty() {
                Vec::new()
            } else {
                s.split(',').map(|a| a.trim().to_string()).collect()
            }
        }),
        body,
    })
}

pub(crate) fn apply_remote_calendar_patch(event: &mut CalendarEvent, patch: CalendarEventPatch) {
    if let Some(value) = patch.title {
        event.title = value;
    }
    if let Some(value) = patch.description {
        event.description = value;
    }
    if let Some(value) = patch.location {
        event.location = value;
    }
    if let Some(value) = patch.venue {
        event.venue = value;
    }
    if let Some(value) = patch.spaces {
        event.spaces = value.into();
    }
    if let Some(value) = patch.start {
        event.start = value;
    }
    if let Some(value) = patch.end {
        event.end = value;
    }
    if let Some(value) = patch.all_day {
        event.all_day = value;
    }
    if let Some(value) = patch.status {
        event.status = value;
    }
    if let Some(value) = patch.recurrence {
        event.recurrence = value;
    }
    if let Some(value) = patch.attendees {
        event.attendees = value.into();
    }
    if let Some(value) = patch.body {
        event.body = value;
    }
    event.date_modified = Some(Utc::now());
}
