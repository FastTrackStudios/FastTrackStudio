//! Saved views — persistent filter + sort + display configurations.
//!
//! A view is a named set of filters, grouping, sorting, and column visibility
//! that can be saved, shared, and reused. Like Plane's IssueView.
//!
//! Stored as `.md` files in `views/` directory:
//! ```yaml
//! title: "My Urgent Tasks"
//! filters:
//!   priority: [Urgent, High]
//!   status: [Open, InProgress]
//!   assignee: [cody]
//! display:
//!   group_by: status
//!   order_by: priority
//!   layout: list
//! ```

use facet::Facet;

/// A saved view configuration.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct SavedView {
    pub title: String,
    pub description: Option<String>,

    /// Filter criteria.
    #[facet(default)]
    pub filters: ViewFilters,

    /// Display settings.
    #[facet(default)]
    pub display: ViewDisplay,

    /// Who created this view.
    pub created_by: Option<String>,

    /// Whether this view is shared with the team or personal.
    pub is_shared: bool,

    /// Sort order for the views list.
    pub sort_order: Option<f64>,
}

/// Filter criteria for a view.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ViewFilters {
    #[facet(default)]
    pub status: Vec<String>,
    #[facet(default)]
    pub priority: Vec<String>,
    #[facet(default)]
    pub assignee: Vec<String>,
    #[facet(default)]
    pub tags: Vec<String>,
    #[facet(default)]
    pub project: Vec<String>,
    #[facet(default)]
    pub issue_type: Vec<String>,
    pub due_before: Option<String>,
    pub due_after: Option<String>,
    pub created_before: Option<String>,
    pub created_after: Option<String>,
    /// Whether to include sub-tasks.
    pub include_subtasks: Option<bool>,
    /// Full-text search query.
    pub search: Option<String>,
}

/// Display/presentation settings for a view.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ViewDisplay {
    /// Layout: list, kanban, calendar, gantt, table.
    pub layout: Option<String>,
    /// Group by field: status, priority, assignee, project, none.
    pub group_by: Option<String>,
    /// Sort by field: priority, due, created, title, updated.
    pub order_by: Option<String>,
    /// Sort direction: asc, desc.
    pub order_direction: Option<String>,
    /// Show empty groups in kanban/grouped views.
    pub show_empty_groups: Option<bool>,
    /// Visible properties/columns.
    #[facet(default)]
    pub visible_properties: Vec<String>,
}
