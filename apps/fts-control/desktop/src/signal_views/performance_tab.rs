use dioxus::prelude::*;

#[component]
pub(crate) fn SignalPerformanceTab() -> Element {
    let signal = signal_ui::use_signal_service();
    use signal_ui::views::{PerfSceneTile, PerformanceView, RigStatus, SnapshotSlot, SongNavState};

    let mut rig_data = use_signal(|| None::<signal::rig::Rig>);
    let mut active_scene_id = use_signal(|| None::<String>);
    let mut morph_value = use_signal(|| 0.0_f64);
    let mut song_data = use_signal(|| None::<signal::song::Song>);
    let mut section_index = use_signal(|| 0_usize);

    // Fetch first rig + first song
    {
        let signal = signal.clone();
        use_effect(move || {
            let signal = signal.clone();
            spawn(async move {
                let rigs = signal.rigs().list().await.unwrap_or_default();
                if let Some(first) = rigs.first() {
                    if let Some(rig) = signal
                        .rigs()
                        .load(first.id.to_string())
                        .await
                        .ok()
                        .flatten()
                    {
                        if let Some(v) = rig.variants.first() {
                            active_scene_id.set(Some(v.id.to_string()));
                        }
                        rig_data.set(Some(rig));
                    }
                }
                let songs = signal.songs().list().await.unwrap_or_default();
                if let Some(first) = songs.first() {
                    if let Some(song) = signal
                        .songs()
                        .load(first.id.to_string())
                        .await
                        .ok()
                        .flatten()
                    {
                        song_data.set(Some(song));
                    }
                }
            });
        });
    }

    // Map rig → props
    let (status, scenes, snapshot_slots, morph_a, morph_b) = if let Some(rig) = rig_data() {
        let active_id = active_scene_id();
        let active_name = active_id
            .as_ref()
            .and_then(|id| rig.variants.iter().find(|v| v.id.to_string() == *id))
            .map(|v| v.name.clone())
            .unwrap_or_else(|| "None".to_string());

        let status = RigStatus {
            rig_name: rig.name.clone(),
            engine_count: rig.engine_ids.len(),
            layer_count: 0,
            active_scene_name: active_name,
        };

        let scenes: Vec<PerfSceneTile> = rig
            .variants
            .iter()
            .map(|v| PerfSceneTile {
                id: v.id.to_string(),
                name: v.name.clone(),
                is_active: active_id.as_deref() == Some(&v.id.to_string()),
                summary: format!("{} engines", v.engine_selections.len()),
            })
            .collect();

        let slots: Vec<SnapshotSlot> = (0..8)
            .map(|i| SnapshotSlot {
                index: i,
                name: None,
                is_a: i < 4,
                is_active: false,
            })
            .collect();

        let morph_a = scenes
            .first()
            .map(|s| s.name.clone())
            .unwrap_or_else(|| "A".to_string());
        let morph_b = scenes
            .get(1)
            .map(|s| s.name.clone())
            .unwrap_or_else(|| "B".to_string());

        (Some(status), scenes, slots, morph_a, morph_b)
    } else {
        (None, vec![], vec![], "A".to_string(), "B".to_string())
    };

    let song_nav = song_data().map(|song| {
        let idx = section_index();
        let section = song.sections.get(idx);
        SongNavState {
            song_name: song.name.clone(),
            section_name: section
                .map(|s| s.name.clone())
                .unwrap_or_else(|| "—".to_string()),
            section_index: idx,
            section_count: song.sections.len(),
            tempo: None,
            key_signature: None,
        }
    });

    rsx! {
        div { class: "h-full overflow-auto",
            if let Some(rig_status) = status {
                PerformanceView {
                    status: rig_status,
                    scenes,
                    morph_value: morph_value(),
                    morph_scene_a: morph_a,
                    morph_scene_b: morph_b,
                    snapshot_slots,
                    song_nav,
                    on_scene_select: move |id: String| { active_scene_id.set(Some(id)); },
                    on_morph_change: move |val: f64| { morph_value.set(val); },
                    on_prev_section: move |_| {
                        let idx = section_index();
                        if idx > 0 { section_index.set(idx - 1); }
                    },
                    on_next_section: move |_| {
                        let idx = section_index();
                        if let Some(song) = song_data() {
                            if idx + 1 < song.sections.len() { section_index.set(idx + 1); }
                        }
                    },
                }
            } else {
                div { class: "flex items-center justify-center h-full",
                    p { class: "text-sm text-muted-foreground", "Loading rig data..." }
                }
            }
        }
    }
}
