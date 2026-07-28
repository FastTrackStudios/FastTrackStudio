//! `task finance …` — reports + invoice generation.
//!
//! Moved verbatim out of `main.rs`; behaviour unchanged.

use clap::Subcommand;

use crate::timer::project_path_for;

#[derive(Subcommand)]
pub(crate) enum FinanceCmd {
    /// Print the weekly summary (hours + billable amount per
    /// project) as markdown. Reads the timer DB.
    Weekly {
        /// Any date inside the target week. Defaults to today.
        #[arg(long)]
        week_of: Option<chrono::NaiveDate>,
        /// Emit the summary as JSON instead of markdown.
        #[arg(long)]
        json: bool,
    },
    /// Per-project hours rollup for a range. Defaults to
    /// the last 7 days.
    Project {
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Emit the rollup rows as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Build + render an invoice from billable sessions on
    /// one project. By default writes both a PDF and a
    /// markdown stub into the vault's `Reports/Invoices/`
    /// directory (PDF under `Reports/Invoices/pdfs/`, MD at
    /// `Reports/Invoices/<num>.md` wikilinking the PDF).
    /// Use `--out` to override the PDF location and skip the
    /// vault export.
    Invoice {
        /// Project frontmatter uuid. Omit to bill every
        /// billable session in the range regardless of
        /// project (including unscoped time).
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Inclusive lower bound on `start_time`.
        #[arg(long)]
        since: chrono::DateTime<chrono::Utc>,
        /// Exclusive upper bound on `start_time`.
        #[arg(long)]
        until: chrono::DateTime<chrono::Utc>,
        /// Explicit invoice number, e.g. `INV-2026-0042`.
        /// Mutually exclusive with `--prefix`.
        #[arg(long, conflicts_with = "prefix")]
        number: Option<String>,
        /// Auto-increment from the highest existing
        /// `<prefix>NNN` (zero-padded `--pad` digits, default
        /// 3). Example: `--prefix TBM-2026-` → finds the
        /// next free `TBM-2026-001`, `TBM-2026-002`…
        #[arg(long)]
        prefix: Option<String>,
        /// Width of the numeric suffix when using `--prefix`.
        #[arg(long, default_value_t = 3)]
        pad: usize,
        /// Net N days for due date. Default 30.
        #[arg(long, default_value_t = 30)]
        net_days: i64,
        /// Free-text bill-to (display name). Used because we
        /// don't have a Party row yet in the local CLI flow.
        /// Once finance-db is mounted this becomes
        /// `--party-id <uuid>`.
        #[arg(long, default_value = "Bill-to")]
        client_name: String,
        /// Override PDF path. When set, skips the vault
        /// export and writes only this file. When omitted,
        /// the PDF lands at
        /// `<vault>/Reports/Invoices/pdfs/<num>.pdf` and a
        /// companion markdown stub goes to
        /// `<vault>/Reports/Invoices/<num>.md`.
        #[arg(long, short)]
        out: Option<std::path::PathBuf>,
        /// Render the PDF without persisting the invoice to
        /// `finance.sqlite` or stamping
        /// `work_sessions.invoice_id`. Use for previews.
        /// Without this flag (the default), the same
        /// `--since/--until` window won't re-bill the same
        /// hours on a later run.
        #[arg(long, default_value_t = false)]
        no_commit: bool,
        /// Emit the build/persist outcome as JSON.
        #[arg(long)]
        json: bool,
    },
    /// List persisted invoices in `finance.sqlite`.
    Invoices {
        /// Filter by status slug (draft / sent / paid /
        /// void / etc). Case-insensitive.
        #[arg(long)]
        status: Option<String>,
        /// Filter by party id.
        #[arg(long)]
        party: Option<uuid::Uuid>,
        /// Cap the output at this many rows (newest issued
        /// first).
        #[arg(long, default_value_t = 50)]
        limit: u64,
        /// Emit the invoices as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Show one persisted invoice in detail — header,
    /// totals, line items, and the contributing session
    /// ids stamped to it.
    InvoiceShow {
        /// Invoice number.
        number: String,
        /// Emit the invoice (+ stamped sessions) as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Record a payment and update the invoice's balance.
    /// `--amount` is in minor units (cents). Sets the
    /// invoice to Paid if balance reaches zero,
    /// PartiallyPaid otherwise.
    InvoiceMarkPaid {
        /// Invoice number.
        number: String,
        /// Payment amount in minor units (cents). Omit to
        /// pay the full outstanding balance.
        #[arg(long)]
        amount: Option<i64>,
        /// ISO 8601 date (YYYY-MM-DD) the payment landed.
        /// Defaults to today.
        #[arg(long)]
        on: Option<chrono::NaiveDate>,
        /// Free-text note (cheque #, wire ref, …).
        #[arg(long, default_value = "")]
        memo: String,
        /// Emit the payment outcome as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Cancel an invoice + un-stamp the contributing
    /// sessions so they can be re-billed. Idempotent on a
    /// missing invoice; refuses if the invoice already has
    /// payments against it (use a credit note instead).
    InvoiceVoid {
        /// Invoice number.
        number: String,
        /// Emit the void outcome as JSON.
        #[arg(long)]
        json: bool,
    },
}

pub(crate) async fn run_finance(cmd: FinanceCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    let ctx = crate::org_ctx::resolve_active(org_override)?;
    // `TASK_TIMER_DB` still wins as a hard override (lets a
    // fixture point at a fresh sqlite); else use the org's
    // resolver.
    let db_url = std::env::var("TASK_TIMER_DB")
        .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
    let timer_conn = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect timer db `{db_url}`: {e}"))?;
    timer::Migrator::up(&timer_conn, None).await.ok();
    // `TASK_VAULT_ROOT` is a fixture override; the real
    // default is the active org's vault. (Was a cwd-relative
    // `examples/vault` fallback in the invoice arm, which
    // silently exported invoices into whatever repo you
    // happened to run from.)
    let vault_root = std::env::var("TASK_VAULT_ROOT")
        .map_or_else(|_| ctx.root.vault_dir(), std::path::PathBuf::from);

    match cmd {
        FinanceCmd::Weekly { week_of, json } => {
            let day = week_of.unwrap_or_else(|| chrono::Utc::now().date_naive());
            let summary = finance::reports::weekly_summary(&timer_conn, None, day)
                .await
                .map_err(|e| eyre::eyre!("weekly: {e}"))?;
            if json {
                crate::json_out::print_json(&summary)?;
            } else {
                print!("{}", summary.to_markdown());
            }
        }
        FinanceCmd::Project { since, until, json } => {
            use finance::reports::DateRange;
            // Each bound defaults independently: missing
            // `--until` means "now", missing `--since` means
            // 7 days before until. Previously `--since`
            // alone was silently ignored (full fallback to
            // last-7-days).
            let range = {
                let u = until.unwrap_or_else(chrono::Utc::now);
                let s = since.unwrap_or(u - chrono::Duration::days(7));
                DateRange { since: s, until: u }
            };
            let rows = finance::reports::hours_by_project(&timer_conn, None, range)
                .await
                .map_err(|e| eyre::eyre!("project: {e}"))?;
            if json {
                // Rollup rows + the same resolved display label
                // the human rendering computes.
                let out: Vec<serde_json::Value> = rows
                    .iter()
                    .map(|r| {
                        let mut v = serde_json::to_value(r).unwrap_or(serde_json::Value::Null);
                        if let serde_json::Value::Object(map) = &mut v {
                            let label = if !r.project_path.is_empty() {
                                r.project_path.clone()
                            } else if let Some(pid) = r.project_id {
                                let resolved = project_path_for(&vault_root, Some(pid));
                                if resolved.is_empty() {
                                    format!("(project {pid})")
                                } else {
                                    resolved
                                }
                            } else {
                                "(unscoped)".to_string()
                            };
                            map.insert("project".into(), label.into());
                        }
                        v
                    })
                    .collect();
                crate::json_out::print_json(&out)?;
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no closed sessions in range)");
            }
            for r in rows {
                // Older sessions may carry a project_id but
                // an empty project_path (the path resolver
                // used to miss nested project folders), so
                // fall back to a vault lookup before
                // declaring the bucket unscoped.
                let project = if !r.project_path.is_empty() {
                    r.project_path.clone()
                } else if let Some(pid) = r.project_id {
                    let resolved = project_path_for(&vault_root, Some(pid));
                    if resolved.is_empty() {
                        format!("(project {pid})")
                    } else {
                        resolved
                    }
                } else {
                    "(unscoped)".to_string()
                };
                println!(
                    "{project}\n  sessions: {}\n  total:    {}\n  billable: {} ({} {})",
                    r.session_count,
                    fmt_seconds(r.total_seconds),
                    fmt_seconds(r.billable_seconds),
                    fmt_minor(r.billable_amount_minor),
                    if r.currency.is_empty() {
                        "(no currency)".to_string()
                    } else {
                        r.currency
                    },
                );
            }
        }
        FinanceCmd::Invoice {
            project,
            since,
            until,
            number,
            prefix,
            pad,
            net_days,
            client_name,
            out,
            no_commit,
            json,
        } => {
            if number.is_none() && prefix.is_none() {
                return Err(eyre::eyre!(
                    "pass either --number <explicit> or --prefix <auto>"
                ));
            }
            // Stable per-org / per-client UUIDv5 ids so
            // repeated invoices share a single Book and
            // Party row in finance.sqlite. Avoids the
            // FK-constraint failure that hits when book_id /
            // party_id are nil, and keeps the schema sane
            // until a real CLI surface for Books + Parties
            // lands.
            let book_id = uuid::Uuid::new_v5(
                &uuid::Uuid::NAMESPACE_DNS,
                format!("task-finance-book/{}", ctx.root.slug()).as_bytes(),
            );
            let party_id = uuid::Uuid::new_v5(
                &uuid::Uuid::NAMESPACE_DNS,
                format!("task-finance-party/{}/{}", ctx.root.slug(), client_name).as_bytes(),
            );
            let book = finance_proto::book::Book {
                id: book_id,
                name: format!("{} Book", ctx.root.slug()),
                kind: finance_proto::book::BookKind::Personal,
                base_currency: "USD".into(),
                settings_json: "{}".into(),
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
            };
            let party = finance_proto::party::Party {
                id: party_id,
                book_id: book.id,
                kind: finance_proto::party::PartyKind::Client,
                display_name: client_name.clone(),
                legal_name: client_name.clone(),
                email: String::new(),
                phone: String::new(),
                address: String::new(),
                tax_id: String::new(),
                default_currency: "USD".into(),
                default_net_days: net_days.try_into().unwrap_or(30),
                default_rate_minor_per_hour: 0,
                notes: String::new(),
                is_archived: false,
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
            };
            // Open the org's finance.sqlite up-front (even
            // for --no-commit) so we can pre-check the
            // invoice number against the unique index and
            // fail before spending render time on a dupe.
            let finance_conn = {
                use sea_orm_migration::MigratorTrait;
                let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
                let conn = Database::connect(&url)
                    .await
                    .map_err(|e| eyre::eyre!("connect finance db `{url}`: {e}"))?;
                finance_db::Migrator::up(&conn, None)
                    .await
                    .map_err(|e| eyre::eyre!("finance migrations: {e}"))?;
                conn
            };
            // Resolve the final invoice number: explicit
            // --number, or auto-incremented from --prefix.
            let final_number: String = if let Some(n) = number.clone() {
                use finance_db::entity::{InvoiceColumn, InvoiceEntity};
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                let existing = InvoiceEntity::find()
                    .filter(InvoiceColumn::Number.eq(n.clone()))
                    .one(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("check invoice number: {e}"))?;
                if existing.is_some() {
                    return Err(eyre::eyre!(
                        "invoice number `{n}` is already in finance.sqlite. Pick a new --number, or pass --no-commit to render-only."
                    ));
                }
                n
            } else {
                let p = prefix.clone().expect("validated above");
                next_invoice_number(&finance_conn, &p, pad).await?
            };

            // When `--project` is set we delegate to the
            // pipeline's per-engagement query. Without it,
            // load every billable + uninvoiced session in
            // the window and hand the list to
            // `build_from_models`.
            let build = if let Some(pid) = project {
                finance::invoice_from_sessions::build_invoice_from_sessions(
                    &timer_conn,
                    finance::invoice_from_sessions::BuildInvoiceArgs {
                        book: book.clone(),
                        party: party.clone(),
                        project_id: pid,
                        since,
                        until,
                        net_days,
                        number: final_number.clone(),
                        notes_public: String::new(),
                        notes_private: String::new(),
                        terms: String::new(),
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("build invoice: {e}"))?
            } else {
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                let sessions = WorkSessionEntity::find()
                    .filter(WorkSessionColumn::Billable.eq(true))
                    .filter(WorkSessionColumn::EndTime.is_not_null())
                    .filter(WorkSessionColumn::InvoiceId.is_null())
                    .filter(WorkSessionColumn::StartTime.gte(since))
                    .filter(WorkSessionColumn::StartTime.lt(until))
                    .all(&timer_conn)
                    .await
                    .map_err(|e| eyre::eyre!("query sessions: {e}"))?;
                finance::invoice_from_sessions::build_from_models(
                    book.clone(),
                    party.clone(),
                    sessions,
                    net_days,
                    final_number.clone(),
                    String::new(),
                    String::new(),
                    String::new(),
                )
                .map_err(|e| eyre::eyre!("build invoice: {e}"))?
            };

            // Issuer ("From" block): `<org>/issuer.toml` is
            // the durable source; `TASK_ISSUER_*` env vars
            // override per-field for fixtures. "Your Name"
            // placeholder only when neither is set.
            let stored = org_proto::IssuerProfile::load(&ctx.root.issuer_path())
                .map_err(|e| eyre::eyre!("issuer.toml: {e}"))?
                .unwrap_or_default();
            let field = |env: &str, file: String, default: &str| {
                std::env::var(env).unwrap_or(if file.is_empty() {
                    default.to_string()
                } else {
                    file
                })
            };
            let issuer = finance::pdf_adapter::IssuerProfile {
                name: field("TASK_ISSUER_NAME", stored.name, "Your Name"),
                address: field("TASK_ISSUER_ADDRESS", stored.address, ""),
                email: field("TASK_ISSUER_EMAIL", stored.email, ""),
                phone: field("TASK_ISSUER_PHONE", stored.phone, ""),
                tax_id: field("TASK_ISSUER_TAX_ID", stored.tax_id, ""),
            };
            let mut ifp = finance::pdf_adapter::invoice_for_pdf(&build.invoice, &issuer, &party);
            // Resolve user_id → display name from the org's
            // auth.sqlite. Missing rows fall back to a
            // short-id label so a stranded id still reads.
            let mut names_by_id = {
                use architect_auth::db::{AuthUserColumn, AuthUserEntity};
                use sea_orm::{ColumnTrait, Database, EntityTrait, QueryFilter};
                let auth_path = ctx.root.auth_db();
                let mut map: std::collections::HashMap<uuid::Uuid, String> =
                    std::collections::HashMap::new();
                let ids: Vec<uuid::Uuid> = build.line_meta.iter().map(|m| m.user_id).collect();
                if !ids.is_empty() && auth_path.exists() {
                    let url = format!("sqlite://{}?mode=ro", auth_path.display());
                    if let Ok(db) = Database::connect(&url).await {
                        if let Ok(rows) = AuthUserEntity::find()
                            .filter(AuthUserColumn::Id.is_in(ids.clone()))
                            .all(&db)
                            .await
                        {
                            for r in rows {
                                let label = r
                                    .name
                                    .filter(|s| !s.is_empty())
                                    .or(r.email)
                                    .unwrap_or_else(|| r.id.simple().to_string());
                                map.insert(r.id, label);
                            }
                        }
                    }
                }
                map
            };
            // Manual override: `TASK_MEMBER_NAMES="<uuid>=Name;<uuid>=Name"`
            // wins over the auth.sqlite lookup — and seeds a display name
            // when there's no auth row at all (CLI-only invoices where the
            // member id is a local stand-in, not a signed-up account).
            if let Ok(raw) = std::env::var("TASK_MEMBER_NAMES") {
                for pair in raw.split([';', ',']) {
                    if let Some((id, name)) = pair.split_once('=') {
                        if let (Ok(uid), name) = (id.trim().parse::<uuid::Uuid>(), name.trim()) {
                            if !name.is_empty() {
                                names_by_id.insert(uid, name.to_string());
                            }
                        }
                    }
                }
            }
            enrich_invoice_with_assignees(&mut ifp, &build.line_meta, &names_by_id);
            // User asked to drop the due-date row; keep
            // `Invoice.due_date` in the proto for accounting
            // semantics, just hide it on the PDF.
            ifp.due_date.clear();
            // Same idea for the status pill — the proto
            // still says "Draft" until we mount a real
            // posting flow, but the PDF doesn't need to
            // shout that at the recipient.
            ifp.status.clear();
            // Period the invoice spans — drives the
            // "Period:" row in the header so a reader
            // doesn't have to scan line dates.
            ifp.period_start = since.format("%Y-%m-%d").to_string();
            ifp.period_end = until.format("%Y-%m-%d").to_string();
            // Decide PDF path: explicit --out wins; else vault-export under
            // `<vault>/Reports/Invoices/pdfs/<num>.pdf`.
            let do_vault_export = out.is_none();
            let pdf_path: std::path::PathBuf = if let Some(p) = out {
                p
            } else {
                let dir = vault_root.join("Reports").join("Invoices").join("pdfs");
                std::fs::create_dir_all(&dir)
                    .map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
                dir.join(format!("{}.pdf", build.invoice.number))
            };
            // Shell out to the `task-pdf-render` binary (in
            // libs/pdf). Fulgur's compile tree triggers a
            // stylo recursion-limit issue when pulled into
            // the CLI's larger graph; isolating it to a
            // standalone binary keeps both compiles clean.
            let request = serde_json::json!({
                "mode": "invoice",
                "data": ifp,
            });
            let render_bin = std::env::var("TASK_PDF_RENDER_BIN")
                .unwrap_or_else(|_| "task-pdf-render".to_string());
            let mut child = std::process::Command::new(&render_bin)
                .arg("--out")
                .arg(&pdf_path)
                .stdin(std::process::Stdio::piped())
                .stderr(std::process::Stdio::inherit())
                .spawn()
                .map_err(|e| {
                    eyre::eyre!(
                        "spawn `{render_bin}`: {e}. Build with `cargo build -p pdf` and put it on PATH, or set TASK_PDF_RENDER_BIN."
                    )
                })?;
            {
                let stdin = child
                    .stdin
                    .as_mut()
                    .ok_or_else(|| eyre::eyre!("render: no stdin"))?;
                serde_json::to_writer(stdin, &request)
                    .map_err(|e| eyre::eyre!("write request: {e}"))?;
            }
            let status = child.wait().map_err(|e| eyre::eyre!("wait: {e}"))?;
            if !status.success() {
                return Err(eyre::eyre!("`{render_bin}` exited with {status}"));
            }
            let bytes_len = std::fs::metadata(&pdf_path).map(|m| m.len()).unwrap_or(0);

            // Vault export: companion markdown stub at
            // `Reports/Invoices/<num>.md` wikilinking the
            // PDF. Skipped when caller passes --out.
            let mut md_out: Option<std::path::PathBuf> = None;
            if do_vault_export {
                let md_path = vault_root
                    .join("Reports")
                    .join("Invoices")
                    .join(format!("{}.md", build.invoice.number));
                if let Some(parent) = md_path.parent() {
                    std::fs::create_dir_all(parent)
                        .map_err(|e| eyre::eyre!("create {}: {e}", parent.display()))?;
                }
                let rel_pdf = format!("pdfs/{}.pdf", build.invoice.number);
                let md = render_invoice_markdown(
                    &build.invoice,
                    &party,
                    &rel_pdf,
                    build.source_session_ids.len(),
                    since,
                    until,
                    &ifp.people,
                    &ifp.assignees,
                );
                std::fs::write(&md_path, md)
                    .map_err(|e| eyre::eyre!("write {}: {e}", md_path.display()))?;
                if !json {
                    println!("Wrote {}", md_path.display());
                }
                md_out = Some(md_path);
            }
            // Persist to finance.sqlite + stamp the
            // contributing sessions so the same range can't
            // re-bill the same hours. SQLite-per-DB means
            // we can't span a tx across the two; finance
            // first (atomic insert), then timer stamp. If
            // the stamp fails mid-way the worst case is a
            // partial set of sessions linked to a real
            // invoice — re-running `--no-commit=false` will
            // pick up the leftovers next time because the
            // invoice number now collides.
            let mut stamped_sessions: u64 = 0;
            if no_commit {
                if !json {
                    println!("Skipped commit (--no-commit). Sessions remain unbilled.");
                }
            } else {
                use finance_db::entity::{
                    BookColumn, BookEntity, InvoiceEntity, PartyColumn, PartyEntity,
                };
                use sea_orm::sea_query::OnConflict;
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                // Insert-if-missing book + party (do-nothing
                // on conflict). The first invoice in a fresh
                // finance.sqlite is what creates these.
                BookEntity::insert(finance::billing::book_to_active(&book))
                    .on_conflict(OnConflict::column(BookColumn::Id).do_nothing().to_owned())
                    .do_nothing()
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("upsert book: {e}"))?;
                PartyEntity::insert(finance::billing::party_to_active(&party))
                    .on_conflict(OnConflict::column(PartyColumn::Id).do_nothing().to_owned())
                    .do_nothing()
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("upsert party: {e}"))?;
                let active = finance::billing::invoice_to_active(&build.invoice);
                InvoiceEntity::insert(active)
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("insert invoice: {e}"))?;
                let stamped = WorkSessionEntity::update_many()
                    .col_expr(
                        WorkSessionColumn::InvoiceId,
                        sea_orm::sea_query::Expr::value(build.invoice.id),
                    )
                    .col_expr(
                        WorkSessionColumn::UpdatedAt,
                        sea_orm::sea_query::Expr::value(chrono::Utc::now()),
                    )
                    .filter(WorkSessionColumn::Id.is_in(build.source_session_ids.clone()))
                    .exec(&timer_conn)
                    .await
                    .map_err(|e| eyre::eyre!("stamp sessions: {e}"))?;
                stamped_sessions = stamped.rows_affected;
                if !json {
                    println!(
                        "Persisted invoice {} + stamped {} session(s).",
                        build.invoice.id, stamped.rows_affected
                    );
                }
            }
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "id": build.invoice.id,
                    "number": build.invoice.number,
                    "currency": build.invoice.currency,
                    "subtotal_minor": build.invoice.subtotal_minor,
                    "total_minor": build.invoice.total_minor,
                    "sessions": build.source_session_ids,
                    "pdf_path": pdf_path,
                    "pdf_bytes": bytes_len,
                    "markdown_path": md_out,
                    "committed": !no_commit,
                    "stamped_sessions": stamped_sessions,
                }))?;
            } else {
                println!(
                    "Wrote {} ({bytes_len} bytes, {} sessions, {} {})",
                    pdf_path.display(),
                    build.source_session_ids.len(),
                    fmt_minor(build.invoice.total_minor),
                    build.invoice.currency,
                );
            }
        }
        FinanceCmd::Invoices {
            status,
            party,
            limit,
            json,
        } => {
            use finance_db::entity::{InvoiceColumn, InvoiceEntity};
            use sea_orm::{ColumnTrait, EntityTrait, QueryFilter, QueryOrder, QuerySelect};
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let mut q = InvoiceEntity::find()
                .order_by_desc(InvoiceColumn::IssueDate)
                .order_by_desc(InvoiceColumn::CreatedAt)
                .limit(limit);
            if let Some(p) = party {
                q = q.filter(InvoiceColumn::PartyId.eq(p));
            }
            let rows = q
                .all(&conn)
                .await
                .map_err(|e| eyre::eyre!("list invoices: {e}"))?;
            let status_needle = status.map(|s| s.to_lowercase());
            let filtered: Vec<_> = rows
                .into_iter()
                .filter(|r| {
                    status_needle
                        .as_ref()
                        .is_none_or(|n| format!("{:?}", r.status).to_lowercase() == *n)
                })
                .collect();
            if json {
                let out: Vec<serde_json::Value> =
                    filtered.iter().map(crate::json_out::invoice_json).collect();
                crate::json_out::print_json(&out)?;
                return Ok(());
            }
            if filtered.is_empty() {
                println!("(no invoices)");
            }
            println!(
                "{:<24}  {:<11}  {:>12}  {:>12}  {:<10}",
                "number", "issued", "total", "balance", "status"
            );
            for r in filtered {
                println!(
                    "{:<24}  {:<11}  {:>12}  {:>12}  {:<10}",
                    r.number,
                    r.issue_date,
                    fmt_minor(r.total_minor),
                    fmt_minor(r.balance_minor),
                    format!("{:?}", r.status).to_lowercase(),
                );
            }
        }
        FinanceCmd::InvoiceShow { number, json } => {
            use finance_db::entity::{InvoiceColumn, InvoiceEntity};
            use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            // Sessions stamped to this invoice (best-effort).
            let sessions = {
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                WorkSessionEntity::find()
                    .filter(WorkSessionColumn::InvoiceId.eq(row.id))
                    .all(&timer_conn)
                    .await
                    .unwrap_or_default()
            };
            if json {
                let mut v = crate::json_out::invoice_json(&row);
                if let serde_json::Value::Object(map) = &mut v {
                    let rows: Vec<serde_json::Value> = sessions
                        .into_iter()
                        .map(|m| crate::json_out::session_json(&timer_proto::WorkSession::from(m)))
                        .collect();
                    map.insert("sessions".into(), serde_json::Value::Array(rows));
                }
                crate::json_out::print_json(&v)?;
                return Ok(());
            }
            println!("Invoice {}", row.number);
            println!("  id:          {}", row.id);
            println!("  status:      {:?}", row.status);
            println!("  issued:      {}", row.issue_date);
            println!("  due:         {}", row.due_date);
            println!("  currency:    {}", row.currency);
            println!("  subtotal:    {}", fmt_minor(row.subtotal_minor));
            println!("  total:       {}", fmt_minor(row.total_minor));
            println!("  paid:        {}", fmt_minor(row.amount_paid_minor));
            println!("  balance:     {}", fmt_minor(row.balance_minor));
            println!("  party_id:    {}", row.party_id);
            println!("  book_id:     {}", row.book_id);
            println!("  line items:  {}", row.line_items.0.len());
            for li in &row.line_items.0 {
                println!(
                    "    - {}  qty={:.2}h  amount={}",
                    li.description,
                    (li.quantity_milli as f64) / 1000.0,
                    fmt_minor(li.line_total_minor),
                );
            }
            println!("  sessions:    {}", sessions.len());
            for s in sessions {
                println!(
                    "    - {}  {}",
                    s.start_time.format("%Y-%m-%d %H:%M"),
                    s.description
                );
            }
        }
        FinanceCmd::InvoiceMarkPaid {
            number,
            amount,
            on,
            memo,
            json,
        } => {
            use finance_db::entity::{InvoiceActive, InvoiceColumn, InvoiceEntity};
            use sea_orm::{
                ActiveModelTrait, ActiveValue::Set, ColumnTrait, EntityTrait, QueryFilter,
            };
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            let outstanding = row.balance_minor;
            if outstanding <= 0 {
                return Err(eyre::eyre!(
                    "invoice `{number}` already has zero balance ({})",
                    fmt_minor(row.amount_paid_minor)
                ));
            }
            let pay = amount.unwrap_or(outstanding);
            if pay <= 0 {
                return Err(eyre::eyre!("--amount must be positive"));
            }
            if pay > outstanding {
                return Err(eyre::eyre!(
                    "--amount {} exceeds outstanding balance {}",
                    fmt_minor(pay),
                    fmt_minor(outstanding)
                ));
            }
            let new_paid = row.amount_paid_minor + pay;
            let new_balance = outstanding - pay;
            let new_status = if new_balance == 0 {
                finance_proto::invoice::InvoiceStatus::Paid
            } else {
                finance_proto::invoice::InvoiceStatus::PartiallyPaid
            };
            let on_date = on.unwrap_or_else(|| chrono::Utc::now().date_naive());
            let id = row.id;
            let mut active: InvoiceActive = row.into();
            active.amount_paid_minor = Set(new_paid);
            active.balance_minor = Set(new_balance);
            active.status = Set(new_status);
            active.updated_at = Set(chrono::Utc::now());
            active
                .update(&conn)
                .await
                .map_err(|e| eyre::eyre!("update invoice: {e}"))?;
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "id": id,
                    "number": number,
                    "payment_minor": pay,
                    "on": on_date,
                    "memo": memo,
                    "status": format!("{new_status:?}").to_lowercase(),
                    "amount_paid_minor": new_paid,
                    "balance_minor": new_balance,
                }))?;
            } else {
                println!(
                    "Recorded payment of {} on {} ({}). status={:?}, paid={}, balance={}",
                    fmt_minor(pay),
                    on_date,
                    if memo.is_empty() { "no memo" } else { &memo },
                    new_status,
                    fmt_minor(new_paid),
                    fmt_minor(new_balance),
                );
            }
        }
        FinanceCmd::InvoiceVoid { number, json } => {
            use finance_db::entity::{InvoiceActive, InvoiceColumn, InvoiceEntity};
            use sea_orm::{
                ActiveModelTrait, ActiveValue::Set, ColumnTrait, EntityTrait, QueryFilter,
            };
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            if row.amount_paid_minor > 0 {
                return Err(eyre::eyre!(
                    "invoice `{number}` has payments against it ({}). Issue a credit note instead.",
                    fmt_minor(row.amount_paid_minor)
                ));
            }
            let invoice_id = row.id;
            let mut active: InvoiceActive = row.into();
            active.status = Set(finance_proto::invoice::InvoiceStatus::Cancelled);
            active.updated_at = Set(chrono::Utc::now());
            active
                .update(&conn)
                .await
                .map_err(|e| eyre::eyre!("update invoice: {e}"))?;
            // Un-stamp the contributing sessions so they
            // become re-billable.
            use sea_orm::Database;
            let timer_url = std::env::var("TASK_TIMER_DB")
                .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
            let tc = Database::connect(&timer_url)
                .await
                .map_err(|e| eyre::eyre!("connect timer db: {e}"))?;
            use timer::entity::{WorkSessionColumn, WorkSessionEntity};
            let cleared = WorkSessionEntity::update_many()
                .col_expr(
                    WorkSessionColumn::InvoiceId,
                    sea_orm::sea_query::Expr::value(Option::<uuid::Uuid>::None),
                )
                .col_expr(
                    WorkSessionColumn::UpdatedAt,
                    sea_orm::sea_query::Expr::value(chrono::Utc::now()),
                )
                .filter(WorkSessionColumn::InvoiceId.eq(invoice_id))
                .exec(&tc)
                .await
                .map_err(|e| eyre::eyre!("un-stamp sessions: {e}"))?;
            if json {
                crate::json_out::print_json(&serde_json::json!({
                    "id": invoice_id,
                    "number": number,
                    "status": "cancelled",
                    "sessions_unstamped": cleared.rows_affected,
                }))?;
            } else {
                println!(
                    "Voided `{number}` and un-stamped {} session(s).",
                    cleared.rows_affected
                );
            }
        }
    }
    Ok(())
}

fn fmt_seconds(s: i64) -> String {
    let h = s / 3600;
    let m = (s % 3600) / 60;
    if h > 0 {
        format!("{h}h{m:02}m")
    } else {
        format!("{m}m")
    }
}

fn fmt_minor(c: i64) -> String {
    let neg = c < 0;
    let abs = c.unsigned_abs();
    format!(
        "{}{}.{:02}",
        if neg { "-" } else { "" },
        abs / 100,
        abs % 100
    )
}

/// Stitch assignee labels onto every line, sort by
/// (assignee → date), and synthesize the per-assignee
/// summary block + the two chart SVGs that the template
/// embeds verbatim.
///
/// Single-assignee invoices are left untouched (no column,
/// no summary, no charts) — the breakdown is only useful
/// when the work is split across people.
/// Scan `finance_invoices.number` for rows whose number
/// starts with `prefix` and whose suffix parses as an
/// integer; return `<prefix><next>` zero-padded to `pad`.
/// Starts at 1 if no match exists.
async fn next_invoice_number(
    conn: &sea_orm::DatabaseConnection,
    prefix: &str,
    pad: usize,
) -> eyre::Result<String> {
    use finance_db::entity::{InvoiceColumn, InvoiceEntity};
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    let rows = InvoiceEntity::find()
        .filter(InvoiceColumn::Number.starts_with(prefix))
        .all(conn)
        .await
        .map_err(|e| eyre::eyre!("scan invoice numbers: {e}"))?;
    let highest = rows
        .iter()
        .filter_map(|r| {
            r.number
                .strip_prefix(prefix)
                .and_then(|s| s.parse::<u64>().ok())
        })
        .max()
        .unwrap_or(0);
    let next = highest + 1;
    Ok(format!("{prefix}{next:0>pad$}"))
}

fn enrich_invoice_with_assignees(
    ifp: &mut finance::pdf_adapter::InvoiceForPdf,
    line_meta: &[finance::invoice_from_sessions::LineMeta],
    names_by_id: &std::collections::HashMap<uuid::Uuid, String>,
) {
    if ifp.lines.len() != line_meta.len() || line_meta.is_empty() {
        return;
    }
    // Distinct chart-friendly palette. Reused mod-N for
    // unusually large teams.
    const PALETTE: &[&str] = &[
        "#3b82f6", "#f97316", "#10b981", "#a855f7", "#ef4444", "#eab308", "#0ea5e9", "#ec4899",
    ];
    let label_for = |uid: uuid::Uuid| -> String {
        names_by_id
            .get(&uid)
            .cloned()
            .unwrap_or_else(|| format!("user {}", &uid.simple().to_string()[..8]))
    };

    // Tag each line with its assignee name + carry the
    // matching meta through the sort so downstream
    // aggregations stay aligned with the rendered lines.
    let mut tagged: Vec<(
        usize,
        String,
        finance::pdf_adapter::InvoiceLineForPdf,
        finance::invoice_from_sessions::LineMeta,
    )> = ifp
        .lines
        .drain(..)
        .zip(line_meta.iter().copied())
        .enumerate()
        .map(|(i, (mut line, meta))| {
            let name = label_for(meta.user_id);
            line.assignee = name.clone();
            (i, name, line, meta)
        })
        .collect();
    tagged.sort_by(|a, b| a.1.cmp(&b.1).then(a.0.cmp(&b.0)));

    // Hide the per-line assignee column on
    // single-assignee invoices (no useful signal), but
    // still produce the per-task breakdown below.
    let distinct: std::collections::BTreeSet<&str> =
        tagged.iter().map(|(_, n, _, _)| n.as_str()).collect();
    let single_assignee = distinct.len() <= 1;
    if single_assignee {
        for (_, _, line, _) in &mut tagged {
            line.assignee.clear();
        }
    }

    // Aggregate by task (case-folded description) AND by
    // person from the sorted tuples — meta is paired with
    // its line so totals can't drift if sort order changes.
    let mut totals: std::collections::BTreeMap<String, (i64, i64)> =
        std::collections::BTreeMap::new();
    let mut by_person_raw: std::collections::BTreeMap<String, (i64, i64)> =
        std::collections::BTreeMap::new();
    for (_, _name, line, meta) in &tagged {
        let key = canonical_task_label(&line.description);
        let t = totals.entry(key).or_insert((0, 0));
        t.0 += meta.secs;
        t.1 += meta.cents;
        // Use the user-id directly so the per-person split
        // is correct even when we've hidden the column.
        let person = label_for(meta.user_id);
        if !person.is_empty() {
            let p = by_person_raw.entry(person).or_insert((0, 0));
            p.0 += meta.secs;
            p.1 += meta.cents;
        }
    }
    ifp.lines = tagged.into_iter().map(|(_, _, l, _)| l).collect();
    if totals.len() <= 1 {
        return;
    }
    let total_secs: i64 = totals.values().map(|(s, _)| *s).sum();
    let total_secs_f = total_secs.max(1) as f64;

    let tasks: Vec<finance::pdf_adapter::AssigneeSummary> = totals
        .iter()
        .enumerate()
        .map(|(i, (name, (secs, cents)))| {
            let hours = *secs as f64 / 3600.0;
            let pct = (*secs as f64) * 100.0 / total_secs_f;
            finance::pdf_adapter::AssigneeSummary {
                name: name.clone(),
                hours: format!("{hours:.2}"),
                amount: fmt_minor(*cents),
                pct: format!("{pct:.1}"),
                color: PALETTE[i % PALETTE.len()].to_string(),
            }
        })
        .collect();

    ifp.donut_svg = build_donut_svg(&tasks, &totals, total_secs);
    ifp.bars_svg = build_bars_svg(&tasks, &totals);
    ifp.assignees = tasks;

    // Per-person concise roll-up, computed above from the
    // sorted (line, meta) tuples — guaranteed aligned.
    let total_p_secs = by_person_raw.values().map(|(s, _)| *s).sum::<i64>().max(1) as f64;
    ifp.people = by_person_raw
        .into_iter()
        .enumerate()
        .map(
            |(i, (name, (secs, cents)))| finance::pdf_adapter::AssigneeSummary {
                name,
                hours: format!("{:.2}", secs as f64 / 3600.0),
                amount: fmt_minor(cents),
                pct: format!("{:.1}", (secs as f64) * 100.0 / total_p_secs),
                color: PALETTE[i % PALETTE.len()].to_string(),
            },
        )
        .collect();
}

/// Pull a stable task label out of a line description.
/// Lines are formatted as `"{date_prefix}  {description}"`
/// — the date prefix is either `YYYY-MM-DD` or
/// `YYYY-MM-DD – MM-DD`. Strip it, normalise the
/// remainder, and case-fold for grouping.
fn canonical_task_label(line_desc: &str) -> String {
    let trimmed = line_desc.trim_start();
    // Date prefix always starts with 10 chars of date —
    // skip until the first run of two spaces, which is
    // how the prefix is separated from the description.
    let body = trimmed.split_once("  ").map_or(trimmed, |(_, rest)| rest);
    let body = body.trim().trim_end_matches(" (mixed rates)");
    let mut out = String::with_capacity(body.len());
    let mut prev_was_space = false;
    for ch in body.chars() {
        if ch.is_whitespace() {
            if !prev_was_space {
                out.push(' ');
            }
            prev_was_space = true;
        } else {
            for c in ch.to_lowercase() {
                out.push(c);
            }
            prev_was_space = false;
        }
    }
    // Title-case the first letter so the legend reads
    // naturally ("Video editing" vs "video editing").
    let mut chars = out.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => "Untitled".to_string(),
    }
}

/// SVG donut showing each assignee's share of total hours.
/// Inline + self-contained — fulgur fetches no externals.
fn build_donut_svg(
    summaries: &[finance::pdf_adapter::AssigneeSummary],
    totals: &std::collections::BTreeMap<String, (i64, i64)>,
    total_secs: i64,
) -> String {
    const SIZE: f64 = 110.0;
    const CX: f64 = SIZE / 2.0;
    const CY: f64 = SIZE / 2.0;
    const R_OUTER: f64 = 48.0;
    const R_INNER: f64 = 28.0;
    let total = total_secs.max(1) as f64;
    let mut start = -std::f64::consts::FRAC_PI_2; // 12 o'clock
    let mut paths = String::new();
    for s in summaries {
        let secs = totals.get(&s.name).map_or(0, |(sec, _)| *sec) as f64;
        let frac = secs / total;
        let sweep = frac * std::f64::consts::TAU;
        let end = start + sweep;
        // Single-slice (100%) needs a full-circle path
        // rather than two arcs that share both endpoints.
        let path = if (frac - 1.0).abs() < 1e-6 {
            format!(
                "M {x1:.3} {y1:.3} A {ro} {ro} 0 1 1 {x2:.3} {y2:.3} \
                 M {x3:.3} {y3:.3} A {ri} {ri} 0 1 0 {x4:.3} {y4:.3} Z",
                x1 = CX + R_OUTER,
                y1 = CY,
                x2 = CX + R_OUTER - 0.001,
                y2 = CY,
                x3 = CX + R_INNER,
                y3 = CY,
                x4 = CX + R_INNER - 0.001,
                y4 = CY,
                ro = R_OUTER,
                ri = R_INNER,
            )
        } else {
            let large = i32::from(sweep > std::f64::consts::PI);
            let (sx, sy) = (CX + R_OUTER * start.cos(), CY + R_OUTER * start.sin());
            let (ex, ey) = (CX + R_OUTER * end.cos(), CY + R_OUTER * end.sin());
            let (isx, isy) = (CX + R_INNER * end.cos(), CY + R_INNER * end.sin());
            let (iex, iey) = (CX + R_INNER * start.cos(), CY + R_INNER * start.sin());
            format!(
                "M {sx:.3} {sy:.3} A {R_OUTER} {R_OUTER} 0 {large} 1 {ex:.3} {ey:.3} \
                 L {isx:.3} {isy:.3} A {R_INNER} {R_INNER} 0 {large} 0 {iex:.3} {iey:.3} Z",
            )
        };
        paths.push_str(&format!("<path d=\"{path}\" fill=\"{}\" />", s.color));
        start = end;
    }
    format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{SIZE}\" height=\"{SIZE}\" viewBox=\"0 0 {SIZE} {SIZE}\">{paths}</svg>"
    )
}

/// Horizontal bars — amount billed per assignee, ranked
/// high-to-low. Pairs with the donut (hours share) so the
/// reader sees rate-weighted contribution too.
fn build_bars_svg(
    summaries: &[finance::pdf_adapter::AssigneeSummary],
    totals: &std::collections::BTreeMap<String, (i64, i64)>,
) -> String {
    const ROW_H: f64 = 16.0;
    const PAD_X: f64 = 4.0;
    const BAR_AREA_W: f64 = 110.0;
    let mut ranked: Vec<_> = summaries.iter().collect();
    ranked.sort_by(|a, b| {
        let av = totals.get(&a.name).map_or(0, |(_, c)| *c);
        let bv = totals.get(&b.name).map_or(0, |(_, c)| *c);
        bv.cmp(&av)
    });
    let max_cents = ranked
        .iter()
        .map(|a| totals.get(&a.name).map_or(0, |(_, c)| *c))
        .max()
        .unwrap_or(0)
        .max(1) as f64;
    let h = ROW_H * ranked.len() as f64 + 4.0;
    let w = BAR_AREA_W + PAD_X * 2.0;
    let mut bars = String::new();
    for (i, s) in ranked.iter().enumerate() {
        let cents = totals.get(&s.name).map_or(0, |(_, c)| *c) as f64;
        let bar_w = (cents / max_cents) * BAR_AREA_W;
        let y = i as f64 * ROW_H + 4.0;
        bars.push_str(&format!(
            "<rect x=\"{PAD_X}\" y=\"{y:.2}\" width=\"{bar_w:.2}\" height=\"8\" rx=\"2\" fill=\"{}\" />\
             <text x=\"{tx:.2}\" y=\"{ty:.2}\" font-size=\"6.5\" font-family=\"Helvetica,Arial,sans-serif\" fill=\"#222\">${}</text>",
            s.color,
            s.amount,
            tx = PAD_X + bar_w + 3.0,
            ty = y + 7.0,
        ));
    }
    format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{w}\" height=\"{h:.2}\" viewBox=\"0 0 {w} {h:.2}\">{bars}</svg>"
    )
}

/// Companion markdown stub for an invoice. Wikilinks the
/// PDF (Obsidian-style `![[pdfs/INV-...pdf]]` embed) so a
/// vault viewer can open the file inline. Frontmatter makes
/// the page queryable in `Reports/Invoices/*.base`.
#[allow(clippy::too_many_arguments)]
fn render_invoice_markdown(
    invoice: &finance_proto::invoice::Invoice,
    party: &finance_proto::party::Party,
    rel_pdf_path: &str,
    session_count: usize,
    period_start: chrono::DateTime<chrono::Utc>,
    period_end: chrono::DateTime<chrono::Utc>,
    people: &[finance::pdf_adapter::AssigneeSummary],
    tasks: &[finance::pdf_adapter::AssigneeSummary],
) -> String {
    let mut out = String::new();
    out.push_str("---\n");
    out.push_str("type: invoice\n");
    out.push_str(&format!("number: {}\n", invoice.number));
    out.push_str(&format!("status: {:?}\n", invoice.status).to_lowercase());
    out.push_str(&format!("issueDate: {}\n", invoice.issue_date));
    out.push_str(&format!("dueDate: {}\n", invoice.due_date));
    out.push_str(&format!(
        "periodStart: {}\n",
        period_start.format("%Y-%m-%d")
    ));
    out.push_str(&format!("periodEnd: {}\n", period_end.format("%Y-%m-%d")));
    out.push_str(&format!("currency: {}\n", invoice.currency));
    out.push_str(&format!("totalMinor: {}\n", invoice.total_minor));
    out.push_str(&format!("balanceMinor: {}\n", invoice.balance_minor));
    out.push_str(&format!("party: \"{}\"\n", party.display_name));
    out.push_str(&format!("sessions: {session_count}\n"));
    out.push_str(&format!("pdf: \"{rel_pdf_path}\"\n"));
    out.push_str("tags: [invoice]\n");
    out.push_str("---\n\n");
    out.push_str(&format!("# Invoice {}\n\n", invoice.number));
    out.push_str(&format!(
        "**To:** {}  \n**Issued:** {}  \n**Period:** {} → {}  \n**Total:** {} {}\n\n",
        party.display_name,
        invoice.issue_date,
        period_start.format("%Y-%m-%d"),
        period_end.format("%Y-%m-%d"),
        fmt_minor(invoice.total_minor),
        invoice.currency,
    ));
    out.push_str("## PDF\n\n");
    out.push_str(&format!("![[{rel_pdf_path}]]\n\n"));
    if !people.is_empty() {
        out.push_str("## Per person\n\n");
        out.push_str("| Member | Hours | Share | Amount |\n");
        out.push_str("|---|---:|---:|---:|\n");
        for p in people {
            out.push_str(&format!(
                "| {} | {} | {}% | {} |\n",
                p.name, p.hours, p.pct, p.amount
            ));
        }
        out.push('\n');
    }
    if !tasks.is_empty() {
        out.push_str("## Time by task\n\n");
        out.push_str("| Task | Hours | Share | Amount |\n");
        out.push_str("|---|---:|---:|---:|\n");
        for t in tasks {
            out.push_str(&format!(
                "| {} | {} | {}% | {} |\n",
                t.name, t.hours, t.pct, t.amount
            ));
        }
        out.push('\n');
    }
    out.push_str("## Line items\n\n");
    out.push_str("| Description | Quantity | Unit price | Amount |\n");
    out.push_str("|---|---:|---:|---:|\n");
    for li in &invoice.line_items.0 {
        let qty_hours = (li.quantity_milli as f64) / 1000.0;
        out.push_str(&format!(
            "| {} | {:.2} hr | {} | {} |\n",
            li.description,
            qty_hours,
            fmt_minor(li.unit_price_minor),
            fmt_minor(li.line_total_minor),
        ));
    }
    out.push_str(&format!(
        "\n**Subtotal:** {} {}  \n",
        fmt_minor(invoice.subtotal_minor),
        invoice.currency,
    ));
    if invoice.tax_total_minor != 0 {
        out.push_str(&format!(
            "**Tax:** {} {}  \n",
            fmt_minor(invoice.tax_total_minor),
            invoice.currency,
        ));
    }
    out.push_str(&format!(
        "**Total:** {} {}\n",
        fmt_minor(invoice.total_minor),
        invoice.currency,
    ));
    if !invoice.notes_public.is_empty() {
        out.push_str(&format!("\n## Notes\n\n{}\n", invoice.notes_public));
    }
    out
}
