//! Setlist service context for dependency injection

use fts::setlist::{LocalSetlistClient, MockSetlist};

crate::define_service_context! {
    /// Setlist service context for dependency injection
    name: Setlist,
    client_type: LocalSetlistClient<MockSetlist>,
}
