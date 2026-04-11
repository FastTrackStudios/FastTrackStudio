use dioxus::prelude::*;
use fts_ui::prelude::*;
use vault_core::Status;

#[derive(Props, Clone, PartialEq)]
pub struct TaskStatusBadgeProps {
    pub status: Status,
}

#[component]
pub fn TaskStatusBadge(props: TaskStatusBadgeProps) -> Element {
    let (variant, label) = match &props.status {
        Status::None | Status::Open => (StatusBadgeVariant::Neutral, "Open"),
        Status::InProgress => (StatusBadgeVariant::Success, "In Progress"),
        Status::OnHold => (StatusBadgeVariant::Warning, "On Hold"),
        Status::Planned => (StatusBadgeVariant::Neutral, "Planned"),
        Status::Done => (StatusBadgeVariant::Success, "Done"),
        Status::Cancelled => (StatusBadgeVariant::Danger, "Cancelled"),
        Status::Archived => (StatusBadgeVariant::Neutral, "Archived"),
    };

    rsx! {
        StatusBadge { variant, label: label.to_string() }
    }
}
