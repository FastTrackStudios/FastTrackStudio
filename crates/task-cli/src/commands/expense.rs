#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

#[derive(Subcommand)]
pub(crate) enum ExpenseCommands {
    /// Create a new expense
    Create {
        description: String,
        #[arg(long)]
        amount: u64,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        currency: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        receipt: Option<String>,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        reimbursable: bool,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List expenses (newest first)
    List {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        reimbursable_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show a single expense
    Show {
        id: String,
        #[arg(long)]
        md: bool,
        #[arg(long)]
        json: bool,
    },
    /// Show an expense roll-up report
    Report {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        reimbursable_only: bool,
        #[arg(long)]
        json: bool,
    },
    /// Update an expense
    Update {
        id: String,
        #[arg(long)]
        amount: Option<u64>,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        currency: Option<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        client: Option<String>,
        #[arg(long)]
        deliverable: Option<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long)]
        vendor: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        receipt: Option<String>,
        #[arg(long)]
        reference: Option<String>,
        #[arg(long)]
        reimbursable: Option<bool>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Delete an expense
    Delete { id: String },
}

pub(crate) async fn run_remote_expense_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: ExpenseCommands,
) -> eyre::Result<()> {
    let service: task_core::service::ExpenseServiceClient = remote.connect().await?;
    let repo = remote.expense_repo().await?;
    let parse_date = |s: &str| -> eyre::Result<NaiveDate> {
        s.parse::<NaiveDate>()
            .map_err(|_| eyre::eyre!("Invalid date: {s}"))
    };

    match command {
        ExpenseCommands::Create {
            description,
            amount,
            date,
            currency,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            receipt,
            reference,
            reimbursable,
            status,
            notes,
            json,
        } => {
            let expense = remote_create_expense_with_client(
                &repo,
                ExpenseCreateRequest {
                    description,
                    amount_cents: amount.try_into()?,
                    date: date.as_deref().map(parse_date).transpose()?,
                    currency_code: currency,
                    project,
                    client: client_name,
                    deliverable,
                    category,
                    vendor,
                    receipt,
                    reference,
                    reimbursable,
                    status,
                    notes,
                    actor: actor.map(str::to_string),
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!(
                    "Created expense {} — ${:.2}",
                    expense.id,
                    expense.amount_cents as f64 / 100.0
                );
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::List {
            from,
            to,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            status,
            reimbursable_only,
            json,
        } => {
            let filter = ExpenseFilter {
                from: from.as_deref().map(parse_date).transpose()?,
                to: to.as_deref().map(parse_date).transpose()?,
                project,
                client: client_name,
                deliverable,
                category,
                vendor,
                status,
                reimbursable_only,
            };
            let expenses: Vec<_> = remote_list_expenses_with_client(&repo)
                .await?
                .into_iter()
                .filter(|expense| task_core::expense::matches_expense_filter(expense, &filter))
                .collect();
            if json {
                println!("{}", facet_json::to_string(&expenses).unwrap_or_default());
            } else if expenses.is_empty() {
                println!("No expenses.");
            } else {
                for expense in expenses {
                    println!(
                        "{}  ${:.2}  {:<10}  {}",
                        expense.date,
                        expense.amount_cents as f64 / 100.0,
                        format!("{:?}", expense.status),
                        expense.description
                    );
                }
            }
        }
        ExpenseCommands::Show { id, md: _, json } => {
            let expense = remote_find_expense_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::Report {
            from,
            to,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            status,
            reimbursable_only,
            json,
        } => {
            let filter = ExpenseFilter {
                from: from.as_deref().map(parse_date).transpose()?,
                to: to.as_deref().map(parse_date).transpose()?,
                project,
                client: client_name,
                deliverable,
                category,
                vendor,
                status,
                reimbursable_only,
            };
            let report = service.expense_report(filter).await?;
            if json {
                println!("{}", facet_json::to_string(&report).unwrap_or_default());
            } else {
                println!("{}", render_expense_report(&report));
            }
        }
        ExpenseCommands::Update {
            id,
            amount,
            date,
            currency,
            project,
            client: client_name,
            deliverable,
            category,
            vendor,
            description,
            receipt,
            reference,
            reimbursable,
            status,
            notes,
            json,
        } => {
            let expense = remote_update_expense_with_client(
                &repo,
                &id,
                ExpensePatch {
                    status,
                    date,
                    amount_cents: amount.map(i64::try_from).transpose()?,
                    currency_code: currency,
                    project,
                    client: client_name,
                    deliverable,
                    category,
                    vendor,
                    description,
                    receipt,
                    reference,
                    reimbursable,
                    notes,
                },
            )
            .await?;
            if json {
                println!("{}", facet_json::to_string(&expense).unwrap_or_default());
            } else {
                println!("Updated expense {}.", expense.id);
                println!("{}", render_expense_body(&expense));
            }
        }
        ExpenseCommands::Delete { id } => {
            let expense = remote_find_expense_with_client(&repo, &id)
                .await?
                .ok_or_else(|| eyre::eyre!("Expense not found: {id}"))?;
            repo.delete_expense(expense.uuid.to_string()).await?;
            println!("Deleted expense {id}.");
        }
    }

    Ok(())
}
