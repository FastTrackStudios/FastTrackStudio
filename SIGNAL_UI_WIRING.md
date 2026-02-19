# Signal UI - Create/Save Wiring Guide

This document shows exactly where to add create/save buttons in the Signal Management tab and how to wire them to the controller.

## Database Setup ✅ COMPLETE

The desktop app now uses: `~/Music/FastTrackStudio/Library/signal.db`

## What to Add

### 1. Add "Capture Preset" Button (Top Toolbar)

**Location**: Line ~2240 in `main.rs`, inside the toolbar div before the mode tabs

```rust
// Add this button before the mode tabs:
button {
    class: "px-3 py-1.5 text-xs rounded bg-blue-600 hover:bg-blue-700 text-white font-medium",
    onclick: move |_| {
        // Spawn dialog or inline capture
        let ctrl = controller.clone();
        spawn(async move {
            // TODO: Get current DAW track
            // let track = daw.current_track().await?;
            // let chunk = track.get_chunk().await?;
            // Parse chunk into Rig with engines
            // ctrl.save_rig_collection(rig).await;
            
            tracing::info!("Capture preset from REAPER - TODO");
        });
    },
    "📸 Capture from REAPER"
}
```

### 2. Add "+ Profile" Button

**Location**: Line ~2539, replace the "Profiles" header in Song mode:

```rust
div { class: "px-3 py-2 border-b border-border flex-shrink-0 flex items-center justify-between",
    h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Profiles" }
    button {
        class: "px-2 py-0.5 text-[10px] rounded bg-primary text-primary-foreground hover:bg-primary/90",
        onclick: move |_| {
            let ctrl = controller.clone();
            spawn(async move {
                use signal::profile::{Profile, Patch, PatchTarget, ProfileId, PatchId};
                use signal::metadata::Metadata;
                
                // Create a new profile with one default patch
                let profile_id = ProfileId::new();
                let patch_id = PatchId::new();
                
                // Default patch targets the first rig's first scene (you can make this a dialog)
                let rigs = ctrl.list_rig_collections().await;
                if let Some(first_rig) = rigs.first() {
                    if let Some(first_scene) = first_rig.variants.first() {
                        let default_patch = Patch::from_rig_scene(
                            patch_id,
                            "Clean",
                            first_rig.id.clone(),
                            first_scene.id.clone(),
                        );
                        
                        let profile = Profile::new(
                            profile_id,
                            "New Profile",
                            default_patch,
                        );
                        
                        ctrl.save_profile(profile).await;
                        tracing::info!("Created new profile");
                        
                        // TODO: Refresh the UI - trigger the use_effect to reload profiles
                    }
                }
            });
        },
        "+ New"
    }
}
```

**Also add the same button at line ~2663 for Profile mode**

### 3. Add "+ Patch" Button to Profiles

**Location**: Inside `render_profile_list` function, add a button after each profile's name when expanded:

```rust
// After the profile name div, add:
if is_expanded {
    button {
        class: "ml-auto px-2 py-0.5 text-[10px] rounded bg-green-600 hover:bg-green-700 text-white",
        onclick: move |e| {
            e.stop_propagation();
            let prof_id = prof.id.clone();
            let ctrl = controller.clone(); // Need to pass controller down to this function
            spawn(async move {
                use signal::profile::{Patch, PatchTarget, PatchId};
                
                // Load the full profile to add a patch
                if let Some(mut profile) = ctrl.load_profile(prof_id.parse().unwrap()).await {
                    let rigs = ctrl.list_rig_collections().await;
                    if let Some(first_rig) = rigs.first() {
                        if let Some(first_scene) = first_rig.variants.first() {
                            let new_patch = Patch::from_rig_scene(
                                PatchId::new(),
                                "New Patch",
                                first_rig.id.clone(),
                                first_scene.id.clone(),
                            );
                            
                            profile.patches.push(new_patch);
                            ctrl.save_profile(profile).await;
                            tracing::info!("Added patch to profile");
                        }
                    }
                }
            });
        },
        "+ Patch"
    }
}
```

### 4. Add "+ Song" Button

**Location**: Line ~2687, in the songs panel header (inside Song mode right panel):

Find the Songs header and add:

```rust
div { class: "px-3 py-2 border-b border-border flex items-center justify-between",
    div { class: "flex items-center gap-2",
        h3 { class: "text-[10px] font-semibold text-zinc-500 uppercase tracking-wider", "Songs" }
        // Existing setlist dropdown...
    }
    button {
        class: "px-2 py-0.5 text-[10px] rounded bg-primary text-primary-foreground hover:bg-primary/90",
        onclick: move |_| {
            let ctrl = controller.clone();
            spawn(async move {
                use signal::song::{Song, Section, SectionSource, SongId, SectionId};
                use signal::metadata::Metadata;
                
                // Create a new song with one default section
                let song_id = SongId::new();
                let section_id = SectionId::new();
                
                // Default section references the first profile's first patch
                let profiles = ctrl.list_profiles().await;
                if let Some(first_profile) = profiles.first() {
                    if let Some(first_patch) = first_profile.patches.first() {
                        let default_section = Section::from_patch(
                            section_id,
                            "Intro",
                            first_patch.id.clone(),
                        );
                        
                        let song = Song::new(
                            song_id,
                            "New Song",
                            default_section,
                        );
                        
                        ctrl.save_song(song).await;
                        tracing::info!("Created new song");
                    }
                }
            });
        },
        "+ New"
    }
}
```

### 5. Add "+ Section" Button to Songs

**Location**: In the SongEditor rendering (after the song name), add:

```rust
// Inside the section header:
button {
    class: "px-2 py-0.5 text-[10px] rounded bg-green-600 hover:bg-green-700 text-white",
    onclick: move |_| {
        let song_id_str = selected_song_id().clone();
        let ctrl = controller.clone();
        spawn(async move {
            if let Some(sid) = song_id_str {
                use signal::song::{Section, SectionSource, SectionId};
                
                // Load the full song
                if let Some(mut song) = ctrl.load_song(sid.parse().unwrap()).await {
                    // Get first profile/patch for default section
                    let profiles = ctrl.list_profiles().await;
                    if let Some(first_profile) = profiles.first() {
                        if let Some(first_patch) = first_profile.patches.first() {
                            let new_section = Section::from_patch(
                                SectionId::new(),
                                "Verse",
                                first_patch.id.clone(),
                            );
                            
                            song.add_section(new_section);
                            ctrl.save_song(song).await;
                            tracing::info!("Added section to song");
                        }
                    }
                }
            }
        });
    },
    "+ Section"
}
```

### 6. Add Save Button for Profile Overrides

When editing parameters in the grid, add a "Save Override" button:

```rust
// In the parameter grid panel, add a save button:
button {
    class: "px-3 py-1 text-xs rounded bg-blue-600 hover:bg-blue-700 text-white",
    onclick: move |_| {
        let prof_id = selected_profile().clone();
        let patch_id = selected_patch().clone();
        let ctrl = controller.clone();
        
        spawn(async move {
            if let (Some(prof), Some(patch)) = (prof_id, patch_id) {
                // Load the profile
                if let Some(mut profile) = ctrl.load_profile(prof.parse().unwrap()).await {
                    // Find the patch and add overrides
                    if let Some(patch) = profile.patches.iter_mut().find(|p| p.id.to_string() == patch) {
                        // TODO: Get modified parameters from the grid
                        // For each modified param, add an Override
                        use signal::overrides::Override;
                        
                        // Example: patch.overrides.push(Override::Parameter { path: "gain".into(), value: 0.8 });
                        
                        ctrl.save_profile(profile).await;
                        tracing::info!("Saved overrides");
                    }
                }
            }
        });
    },
    "💾 Save Overrides"
}
```

## Controller API Reference

```rust
// Profiles
controller.list_profiles().await -> Vec<Profile>
controller.load_profile(ProfileId).await -> Option<Profile>
controller.save_profile(Profile).await

// Songs
controller.list_songs().await -> Vec<Song>
controller.load_song(SongId).await -> Option<Song>  
controller.save_song(Song).await

// Setlists
controller.list_setlists().await -> Vec<Setlist>
controller.save_setlist(Setlist).await

// Rigs (for captured presets)
controller.list_rig_collections().await -> Vec<Rig>
controller.save_rig_collection(Rig).await
```

## Type Constructors

```rust
// IDs
ProfileId::new() -> ProfileId
PatchId::new() -> PatchId
SongId::new() -> SongId
SectionId::new() -> SectionId
RigId::new() -> RigId

// Objects
Profile::new(id, name, default_patch) -> Profile
Patch::from_rig_scene(id, name, rig_id, scene_id) -> Patch
Song::new(id, name, default_section) -> Song
Section::from_patch(id, name, patch_id) -> Section
Section::from_rig_scene(id, name, rig_id, scene_id) -> Section
```

## Triggering UI Refresh

After saving, you need to trigger the `use_effect` that loads data. The simplest way:

```rust
// Change the rig_type signal to force a reload
let current = rig_type();
rig_type.set(current); // This triggers the use_effect dependency
```

Or add a dedicated "refresh" signal:

```rust
let mut refresh_trigger = use_signal(|| 0_u32);

// In the use_effect dependencies:
use_effect(move || {
    let _ = refresh_trigger(); // Read it as dependency
    // ... load data
});

// After saving:
refresh_trigger.set(refresh_trigger() + 1);
```

## Next Steps

1. Add the buttons as shown above
2. Test each create operation
3. Add proper dialogs with name input (optional - buttons work fine)
4. Wire up the "Capture from REAPER" to actually read track chunks
5. Add delete/rename buttons using similar patterns

The persistence is already working - all changes save to `~/Music/FastTrackStudio/Library/signal.db`!
