//! Web client handler — receives pushed events from the desktop app.
//!
//! Implements `WebClientService` so the desktop gateway can call `push_event()`
//! on this handler via roam RPC. Events are written into the same `GlobalSignal`s
//! that the UI components read from.

use dioxus::prelude::*;
use session::{SetlistEvent, WebClientService};
use session_ui::{
    TransportState, ACTIVE_INDICES, ACTIVE_PLAYBACK_IS_PLAYING, ACTIVE_PLAYBACK_MUSICAL,
    AUDIO_LATENCY_SECONDS, PLAYBACK_STATE, SETLIST_STRUCTURE, SONG_CHARTS, SONG_TRANSPORT,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

/// Handler for events pushed from the desktop app.
#[derive(Clone)]
pub struct WebClientHandler;

impl WebClientService for WebClientHandler {
    async fn push_event(&self, event: SetlistEvent) {
        match event {
            SetlistEvent::SetlistChanged(setlist) => {
                log(&format!(
                    "[fts-control] Setlist changed: {} songs",
                    setlist.songs.len()
                ));
                let valid_guids: std::collections::HashSet<String> = setlist
                    .songs
                    .iter()
                    .map(|song| song.project_guid.clone())
                    .collect();
                SONG_CHARTS
                    .write()
                    .retain(|guid, _| valid_guids.contains(guid));
                *SETLIST_STRUCTURE.write() = setlist;
            }

            SetlistEvent::SongHydrated { index, song, .. } => {
                let mut setlist = SETLIST_STRUCTURE.write();
                if index < setlist.songs.len() {
                    setlist.songs[index] = song;
                }
            }

            SetlistEvent::SongChartHydrated { chart, .. } => {
                SONG_CHARTS
                    .write()
                    .insert(chart.project_guid.clone(), chart);
            }

            SetlistEvent::ActiveIndicesChanged(indices) => {
                if indices.is_playing {
                    *PLAYBACK_STATE.write() = daw::service::PlayState::Playing;
                } else {
                    *PLAYBACK_STATE.write() = daw::service::PlayState::Stopped;
                }
                *ACTIVE_INDICES.write() = indices;
            }

            SetlistEvent::TransportUpdate(transports) => {
                let active_song_index = ACTIVE_INDICES.read().song_index;
                let audio_latency = *AUDIO_LATENCY_SECONDS.read();

                let mut transport_updates: Vec<(usize, TransportState)> =
                    Vec::with_capacity(transports.len());
                let mut active_transport_update = None;

                {
                    let setlist = SETLIST_STRUCTURE.read();
                    let existing_transports = SONG_TRANSPORT.read();

                    for transport in &transports {
                        let compensated_position = if transport.is_playing && audio_latency > 0.0 {
                            let compensated_time = transport.position.time.map(|t| {
                                daw::service::TimePosition::from_seconds(
                                    t.as_seconds() + audio_latency,
                                )
                            });
                            daw::service::Position::new(
                                transport.position.musical.clone(),
                                compensated_time,
                                transport.position.midi.clone(),
                            )
                        } else {
                            transport.position.clone()
                        };

                        let loop_region_percent =
                            transport.loop_region.as_ref().and_then(|region| {
                                setlist.songs.get(transport.song_index).map(|song| {
                                    let song_duration = song.duration();
                                    if song_duration > 0.0 {
                                        (
                                            (region.start_seconds / song_duration).clamp(0.0, 1.0),
                                            (region.end_seconds / song_duration).clamp(0.0, 1.0),
                                        )
                                    } else {
                                        (0.0, 1.0)
                                    }
                                })
                            });

                        let next_state = TransportState {
                            position: compensated_position,
                            bpm: transport.bpm,
                            time_sig_num: transport.time_sig_num as i32,
                            time_sig_denom: transport.time_sig_denom as i32,
                            is_playing: transport.is_playing,
                            is_looping: transport.is_looping,
                            loop_region: loop_region_percent,
                        };

                        let changed = existing_transports
                            .get(&transport.song_index)
                            .map(|existing| *existing != next_state)
                            .unwrap_or(true);

                        if changed {
                            transport_updates.push((transport.song_index, next_state));
                        }

                        if Some(transport.song_index) == active_song_index {
                            active_transport_update = Some((
                                transport.progress,
                                transport.section_progress,
                                transport.section_index,
                                transport.is_playing,
                                transport.is_looping,
                                transport.position.musical.clone(),
                            ));
                        }
                    }
                }

                if !transport_updates.is_empty() {
                    let mut song_transport = SONG_TRANSPORT.write();
                    for (song_index, state) in transport_updates {
                        song_transport.insert(song_index, state);
                    }
                }

                if let Some((
                    song_progress,
                    section_progress,
                    section_index,
                    is_playing,
                    is_looping,
                    musical,
                )) = active_transport_update
                {
                    if *ACTIVE_PLAYBACK_MUSICAL.peek() != musical {
                        *ACTIVE_PLAYBACK_MUSICAL.write() = musical;
                    }
                    if *ACTIVE_PLAYBACK_IS_PLAYING.peek() != is_playing {
                        *ACTIVE_PLAYBACK_IS_PLAYING.write() = is_playing;
                    }

                    let old_playing = *PLAYBACK_STATE.read();
                    let new_playing = if is_playing {
                        daw::service::PlayState::Playing
                    } else {
                        daw::service::PlayState::Stopped
                    };

                    if old_playing != new_playing {
                        *PLAYBACK_STATE.write() = new_playing;
                    }

                    let indices_changed = {
                        let current = ACTIVE_INDICES.read();
                        current.song_progress != Some(song_progress)
                            || current.section_progress != section_progress
                            || current.section_index != section_index
                            || current.is_playing != is_playing
                            || current.looping != is_looping
                    };

                    if indices_changed {
                        let mut indices = ACTIVE_INDICES.write();
                        indices.song_progress = Some(song_progress);
                        indices.section_progress = section_progress;
                        indices.section_index = section_index;
                        indices.is_playing = is_playing;
                        indices.looping = is_looping;
                    }
                }
            }

            SetlistEvent::SongEntered { index, song, .. } => {
                log(&format!(
                    "[fts-control] Entered song {}: {}",
                    index, song.name
                ));
            }

            SetlistEvent::SongExited { index, .. } => {
                log(&format!("[fts-control] Exited song {}", index));
            }

            SetlistEvent::SectionEntered {
                song_index,
                section_index,
                section,
                ..
            } => {
                log(&format!(
                    "[fts-control] Entered section {}.{}: {}",
                    song_index, section_index, section.name
                ));
            }

            SetlistEvent::SectionExited {
                song_index,
                section_index,
                ..
            } => {
                log(&format!(
                    "[fts-control] Exited section {}.{}",
                    song_index, section_index
                ));
            }

            SetlistEvent::PositionChanged { .. } => {
                // Legacy event, ignored — we use TransportUpdate now
            }
        }
    }
}
