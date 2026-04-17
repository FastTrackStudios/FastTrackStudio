//! Universal approval workflow — entity-agnostic.
//!
//! Any entity (output, deliverable, design, mix) can have an approval flow:
//! Draft → Submitted → Review → ChangesRequested → Approved
//!
//! Each state transition is logged with who/when/why.
//!
//! ## Nextcloud sync
//! Maps to Deck card labels:
//! - "Draft" → no label
//! - "Review" → "review" label
//! - "Changes Requested" → "changes-requested" label
//! - "Approved" → "approved" label

use chrono::NaiveDateTime;
use facet::Facet;

/// The approval status of a deliverable.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum ApprovalStatus {
    #[default]
    Draft,
    Submitted,
    Review,
    ChangesRequested,
    Approved,
    Rejected,
}

impl ApprovalStatus {
    pub fn is_terminal(&self) -> bool {
        matches!(self, Self::Approved | Self::Rejected)
    }

    pub fn label(&self) -> &'static str {
        match self {
            Self::Draft => "Draft",
            Self::Submitted => "Submitted",
            Self::Review => "In Review",
            Self::ChangesRequested => "Changes Requested",
            Self::Approved => "Approved",
            Self::Rejected => "Rejected",
        }
    }
}

/// A single state transition in the approval history.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ApprovalEvent {
    pub from: ApprovalStatus,
    pub to: ApprovalStatus,
    pub by: String,
    pub at: Option<NaiveDateTime>,
    pub reason: Option<String>,
}

/// Approval state attached to any entity.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Approval {
    pub status: ApprovalStatus,
    /// Who needs to approve.
    #[facet(default)]
    pub reviewers: Vec<String>,
    /// Who has already approved (for multi-reviewer flows).
    #[facet(default)]
    pub approved_by: Vec<String>,
    /// Full history of state changes.
    #[facet(default)]
    pub history: Vec<ApprovalEvent>,
}

impl Approval {
    pub fn submit(&mut self, by: &str) {
        self.transition(ApprovalStatus::Submitted, by, None);
    }

    pub fn request_review(&mut self, by: &str) {
        self.transition(ApprovalStatus::Review, by, None);
    }

    pub fn request_changes(&mut self, by: &str, reason: &str) {
        self.transition(ApprovalStatus::ChangesRequested, by, Some(reason));
    }

    pub fn approve(&mut self, by: &str) {
        if !self.approved_by.contains(&by.to_string()) {
            self.approved_by.push(by.to_string());
        }
        // Auto-approve if all reviewers have approved, or if no reviewers set
        if self.reviewers.is_empty() || self.reviewers.iter().all(|r| self.approved_by.contains(r)) {
            self.transition(ApprovalStatus::Approved, by, None);
        }
    }

    pub fn reject(&mut self, by: &str, reason: &str) {
        self.transition(ApprovalStatus::Rejected, by, Some(reason));
    }

    fn transition(&mut self, to: ApprovalStatus, by: &str, reason: Option<&str>) {
        let event = ApprovalEvent {
            from: self.status.clone(),
            to: to.clone(),
            by: by.to_string(),
            at: Some(chrono::Local::now().naive_local()),
            reason: reason.map(|r| r.to_string()),
        };
        self.history.push(event);
        self.status = to;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_reviewer_flow() {
        let mut a = Approval::default();
        assert_eq!(a.status, ApprovalStatus::Draft);

        a.submit("cody");
        assert_eq!(a.status, ApprovalStatus::Submitted);

        a.request_review("cody");
        assert_eq!(a.status, ApprovalStatus::Review);

        a.request_changes("amy", "Kick too loud");
        assert_eq!(a.status, ApprovalStatus::ChangesRequested);
        assert_eq!(a.history.last().unwrap().reason.as_deref(), Some("Kick too loud"));

        a.submit("cody");
        a.approve("amy");
        assert_eq!(a.status, ApprovalStatus::Approved);
        assert_eq!(a.history.len(), 5);
    }

    #[test]
    fn multi_reviewer_requires_all() {
        let mut a = Approval {
            reviewers: vec!["amy".into(), "carter".into()],
            ..Default::default()
        };
        a.submit("cody");
        a.request_review("cody");

        // First reviewer approves — not done yet
        a.approve("amy");
        assert_eq!(a.status, ApprovalStatus::Review); // still in review
        assert_eq!(a.approved_by, vec!["amy"]);

        // Second reviewer approves — now done
        a.approve("carter");
        assert_eq!(a.status, ApprovalStatus::Approved);
        assert_eq!(a.approved_by, vec!["amy", "carter"]);
    }

    #[test]
    fn reject_is_terminal() {
        let mut a = Approval::default();
        a.submit("cody");
        a.reject("amy", "Not up to standard");
        assert!(a.status.is_terminal());
        assert_eq!(a.status, ApprovalStatus::Rejected);
    }
}
