//! Dioxus implementation of SetlistDataSource trait
//!
//! This implementation uses Dioxus global signals to provide setlist and transport state.

#[cfg(feature = "dioxus")]
mod dioxus_impl {
    use crate::setlist::core::Setlist;
    use crate::setlist::infra::api_trait::{SetlistDataSource, TransportState, AppState};
    use crate::setlist::infra::dioxus::*;

    /// Dioxus-based implementation of SetlistDataSource
    ///
    /// This implementation reads from Dioxus global signals.
    pub struct DioxusSetlistDataSource;

    impl SetlistDataSource for DioxusSetlistDataSource {
        fn get_setlist(&self) -> Option<Setlist> {
            SETLIST().as_ref().map(|api| api.get_setlist().clone())
        }

        fn get_active_song_index(&self) -> Option<usize> {
            SETLIST().as_ref().and_then(|api| api.active_song_index())
        }

        fn get_transport_state(&self) -> TransportState {
            TransportState {
                is_playing: IS_PLAYING(),
                is_recording: IS_RECORDING(),
                current_position_seconds: CURRENT_POSITION_SECONDS(),
                current_position_beats: CURRENT_POSITION_BEATS(),
                tempo: CURRENT_TEMPO(),
                time_sig_numerator: CURRENT_TIME_SIG_NUMERATOR(),
                time_sig_denominator: CURRENT_TIME_SIG_DENOMINATOR(),
            }
        }

        fn get_app_state(&self) -> AppState {
            AppState {
                is_counting_in: IS_COUNTING_IN(),
                loop_enabled: LOOP_ENABLED(),
                loop_start: LOOP_START(),
                loop_end: LOOP_END(),
                queued_name: QUEUED_NAME().clone(),
                queued_index: QUEUED_INDEX().clone(),
            }
        }
    }
}

#[cfg(feature = "dioxus")]
pub use dioxus_impl::DioxusSetlistDataSource;
