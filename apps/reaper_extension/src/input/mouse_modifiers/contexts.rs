//! Mouse modifier contexts
//!
//! Complete list of all REAPER mouse modifier contexts.
//! Uses internal MM_CTX_* names which are the most reliable format.
//! These correspond to the contexts available in Options > Preferences > Mouse Modifiers

/// All mouse modifier contexts in REAPER
/// Using internal MM_CTX_* names for reliability
/// Based on sockmonkey72's MouseMaps.lua and REAPER's reaper-mouse.ini
pub const ALL_CONTEXTS: &[&str] = &[
    // Arrange view contexts
    "MM_CTX_ARRANGE_MMOUSE",     // Arrange view (middle drag)
    "MM_CTX_ARRANGE_MMOUSE_CLK", // Arrange view (middle click)
    "MM_CTX_ARRANGE_RMOUSE",     // Arrange view (right drag)
    // Automation contexts
    "MM_CTX_POOLEDENV",        // Automation item (left drag)
    "MM_CTX_POOLEDENVEDGE",    // Automation item edge (left drag)
    "MM_CTX_POOLEDENV_DBLCLK", // Automation item (double click)
    // Edit cursor
    "MM_CTX_CURSORHANDLE", // Edit cursor handle (left drag)
    // Envelope contexts
    "MM_CTX_ENVCP_DBLCLK",   // Envelope control panel (double click)
    "MM_CTX_ENVLANE",        // Envelope lane (left drag)
    "MM_CTX_ENVLANE_DBLCLK", // Envelope lane (double click)
    "MM_CTX_ENVPT",          // Envelope point (left drag)
    "MM_CTX_ENVPT_DBLCLK",   // Envelope point (double click)
    "MM_CTX_ENVSEG",         // Envelope segment (left drag)
    "MM_CTX_ENVSEG_DBLCLK",  // Envelope segment (double click)
    // Fixed lane contexts
    "MM_CTX_FIXEDLANETAB_CLK", // Fixed lane header button (left click)
    "MM_CTX_LINKEDLANE",       // Fixed lane when comping is enabled (left drag)
    "MM_CTX_LINKEDLANE_CLK",   // Fixed lane comp area (left click)
    // MIDI contexts
    "MM_CTX_MIDI_CCEVT",            // MIDI CC event (left click/drag)
    "MM_CTX_MIDI_CCEVT_DBLCLK",     // MIDI CC event (double click)
    "MM_CTX_MIDI_CCLANE",           // MIDI CC lane (left click/drag)
    "MM_CTX_MIDI_CCLANE_DBLCLK",    // MIDI CC lane (double click)
    "MM_CTX_MIDI_CCSEG",            // MIDI CC segment (left click/drag)
    "MM_CTX_MIDI_CCSEG_DBLCLK",     // MIDI CC segment (double click)
    "MM_CTX_MIDI_ENDPTR",           // MIDI source loop end marker (left drag)
    "MM_CTX_MIDI_MARKERLANES",      // MIDI marker/region lanes (left drag)
    "MM_CTX_MIDI_NOTE",             // MIDI note (left drag)
    "MM_CTX_MIDI_NOTEEDGE",         // MIDI note edge (left drag)
    "MM_CTX_MIDI_NOTE_CLK",         // MIDI note (left click)
    "MM_CTX_MIDI_NOTE_DBLCLK",      // MIDI note (double click)
    "MM_CTX_MIDI_PIANOROLL",        // MIDI piano roll (left drag)
    "MM_CTX_MIDI_PIANOROLL_CLK",    // MIDI piano roll (left click)
    "MM_CTX_MIDI_PIANOROLL_DBLCLK", // MIDI piano roll (double click)
    "MM_CTX_MIDI_RMOUSE",           // MIDI editor (right drag)
    "MM_CTX_MIDI_RULER",            // MIDI ruler (left drag)
    "MM_CTX_MIDI_RULER_CLK",        // MIDI ruler (left click)
    "MM_CTX_MIDI_RULER_DBLCLK",     // MIDI ruler (double click)
    // Media item contexts
    "MM_CTX_ITEM",                     // Media item (left drag)
    "MM_CTX_ITEM_CLK",                 // Media item (left click)
    "MM_CTX_ITEM_DBLCLK",              // Media item (double click)
    "MM_CTX_ITEMEDGE",                 // Media item edge (left drag)
    "MM_CTX_ITEMEDGE_DBLCLK",          // Media item edge (double click)
    "MM_CTX_ITEMFADE",                 // Media item fade/autocrossfade (left drag)
    "MM_CTX_ITEMFADE_CLK",             // Media item fade/autocrossfade (left click)
    "MM_CTX_ITEMFADE_DBLCLK",          // Media item fade/autocrossfade (double click)
    "MM_CTX_ITEMLOWER",                // Media item bottom half (left drag)
    "MM_CTX_ITEMLOWER_CLK",            // Media item bottom half (left click)
    "MM_CTX_ITEMLOWER_DBLCLK",         // Media item bottom half (double click)
    "MM_CTX_ITEMSTRETCHMARKER",        // Media item stretch marker (left drag)
    "MM_CTX_ITEMSTRETCHMARKERRATE",    // Media item stretch marker rate (left drag)
    "MM_CTX_ITEMSTRETCHMARKER_DBLCLK", // Media item stretch marker (double click)
    "MM_CTX_CROSSFADE",                // Media item fade intersection (left drag)
    "MM_CTX_CROSSFADE_CLK",            // Media item fade intersection (left click)
    "MM_CTX_CROSSFADE_DBLCLK",         // Media item fade intersection (double click)
    // Mixer contexts
    "MM_CTX_MCP_DBLCLK", // Mixer control panel (double click)
    // Project marker/region contexts
    "MM_CTX_MARKERLANES",       // Project marker/region lane (left drag)
    "MM_CTX_MARKER_REGIONEDGE", // Project marker/region edge (left drag)
    "MM_CTX_REGION",            // Project region (left drag)
    "MM_CTX_TEMPOMARKER",       // Project tempo/time signature marker (left drag)
    // Razor edit contexts
    "MM_CTX_AREASEL",      // Razor edit area (left drag)
    "MM_CTX_AREASEL_CLK",  // Razor edit area (left click)
    "MM_CTX_AREASEL_EDGE", // Razor edit edge (left drag)
    "MM_CTX_AREASEL_ENV",  // Razor edit envelope area (left drag)
    // Ruler contexts
    "MM_CTX_RULER",        // Ruler (left drag)
    "MM_CTX_RULER_CLK",    // Ruler (left click)
    "MM_CTX_RULER_DBLCLK", // Ruler (double click)
    // Track contexts
    "MM_CTX_TRACK",      // Track (left drag)
    "MM_CTX_TRACK_CLK",  // Track (left click)
    "MM_CTX_TCP_DBLCLK", // Track control panel (double click)
];

/// Get display name for a context (for logging/UI)
pub fn get_display_name(context: &str) -> &str {
    match context {
        "MM_CTX_ARRANGE_MMOUSE" => "Arrange view (middle drag)",
        "MM_CTX_ARRANGE_MMOUSE_CLK" => "Arrange view (middle click)",
        "MM_CTX_ARRANGE_RMOUSE" => "Arrange view (right drag)",
        "MM_CTX_POOLEDENV" => "Automation item (left drag)",
        "MM_CTX_POOLEDENVEDGE" => "Automation item edge (left drag)",
        "MM_CTX_POOLEDENV_DBLCLK" => "Automation item (double click)",
        "MM_CTX_CURSORHANDLE" => "Edit cursor handle (left drag)",
        "MM_CTX_ENVCP_DBLCLK" => "Envelope control panel (double click)",
        "MM_CTX_ENVLANE" => "Envelope lane (left drag)",
        "MM_CTX_ENVLANE_DBLCLK" => "Envelope lane (double click)",
        "MM_CTX_ENVPT" => "Envelope point (left drag)",
        "MM_CTX_ENVPT_DBLCLK" => "Envelope point (double click)",
        "MM_CTX_ENVSEG" => "Envelope segment (left drag)",
        "MM_CTX_ENVSEG_DBLCLK" => "Envelope segment (double click)",
        "MM_CTX_FIXEDLANETAB_CLK" => "Fixed lane header button (left click)",
        "MM_CTX_LINKEDLANE" => "Fixed lane when comping is enabled (left drag)",
        "MM_CTX_LINKEDLANE_CLK" => "Fixed lane comp area (left click)",
        "MM_CTX_MIDI_CCEVT" => "MIDI CC event (left click/drag)",
        "MM_CTX_MIDI_CCEVT_DBLCLK" => "MIDI CC event (double click)",
        "MM_CTX_MIDI_CCLANE" => "MIDI CC lane (left click/drag)",
        "MM_CTX_MIDI_CCLANE_DBLCLK" => "MIDI CC lane (double click)",
        "MM_CTX_MIDI_CCSEG" => "MIDI CC segment (left click/drag)",
        "MM_CTX_MIDI_CCSEG_DBLCLK" => "MIDI CC segment (double click)",
        "MM_CTX_MIDI_ENDPTR" => "MIDI source loop end marker (left drag)",
        "MM_CTX_MIDI_MARKERLANES" => "MIDI marker/region lanes (left drag)",
        "MM_CTX_MIDI_NOTE" => "MIDI note (left drag)",
        "MM_CTX_MIDI_NOTEEDGE" => "MIDI note edge (left drag)",
        "MM_CTX_MIDI_NOTE_CLK" => "MIDI note (left click)",
        "MM_CTX_MIDI_NOTE_DBLCLK" => "MIDI note (double click)",
        "MM_CTX_MIDI_PIANOROLL" => "MIDI piano roll (left drag)",
        "MM_CTX_MIDI_PIANOROLL_CLK" => "MIDI piano roll (left click)",
        "MM_CTX_MIDI_PIANOROLL_DBLCLK" => "MIDI piano roll (double click)",
        "MM_CTX_MIDI_RMOUSE" => "MIDI editor (right drag)",
        "MM_CTX_MIDI_RULER" => "MIDI ruler (left drag)",
        "MM_CTX_MIDI_RULER_CLK" => "MIDI ruler (left click)",
        "MM_CTX_MIDI_RULER_DBLCLK" => "MIDI ruler (double click)",
        "MM_CTX_ITEM" => "Media item (left drag)",
        "MM_CTX_ITEM_CLK" => "Media item (left click)",
        "MM_CTX_ITEM_DBLCLK" => "Media item (double click)",
        "MM_CTX_ITEMEDGE" => "Media item edge (left drag)",
        "MM_CTX_ITEMEDGE_DBLCLK" => "Media item edge (double click)",
        "MM_CTX_ITEMFADE" => "Media item fade/autocrossfade (left drag)",
        "MM_CTX_ITEMFADE_CLK" => "Media item fade/autocrossfade (left click)",
        "MM_CTX_ITEMFADE_DBLCLK" => "Media item fade/autocrossfade (double click)",
        "MM_CTX_ITEMLOWER" => "Media item bottom half (left drag)",
        "MM_CTX_ITEMLOWER_CLK" => "Media item bottom half (left click)",
        "MM_CTX_ITEMLOWER_DBLCLK" => "Media item bottom half (double click)",
        "MM_CTX_ITEMSTRETCHMARKER" => "Media item stretch marker (left drag)",
        "MM_CTX_ITEMSTRETCHMARKERRATE" => "Media item stretch marker rate (left drag)",
        "MM_CTX_ITEMSTRETCHMARKER_DBLCLK" => "Media item stretch marker (double click)",
        "MM_CTX_CROSSFADE" => "Media item fade intersection (left drag)",
        "MM_CTX_CROSSFADE_CLK" => "Media item fade intersection (left click)",
        "MM_CTX_CROSSFADE_DBLCLK" => "Media item fade intersection (double click)",
        "MM_CTX_MCP_DBLCLK" => "Mixer control panel (double click)",
        "MM_CTX_MARKERLANES" => "Project marker/region lane (left drag)",
        "MM_CTX_MARKER_REGIONEDGE" => "Project marker/region edge (left drag)",
        "MM_CTX_REGION" => "Project region (left drag)",
        "MM_CTX_TEMPOMARKER" => "Project tempo/time signature marker (left drag)",
        "MM_CTX_AREASEL" => "Razor edit area (left drag)",
        "MM_CTX_AREASEL_CLK" => "Razor edit area (left click)",
        "MM_CTX_AREASEL_EDGE" => "Razor edit edge (left drag)",
        "MM_CTX_AREASEL_ENV" => "Razor edit envelope area (left drag)",
        "MM_CTX_RULER" => "Ruler (left drag)",
        "MM_CTX_RULER_CLK" => "Ruler (left click)",
        "MM_CTX_RULER_DBLCLK" => "Ruler (double click)",
        "MM_CTX_TRACK" => "Track (left drag)",
        "MM_CTX_TRACK_CLK" => "Track (left click)",
        "MM_CTX_TCP_DBLCLK" => "Track control panel (double click)",
        _ => context, // Return as-is if not found
    }
}

/// Categorize contexts for organization
pub mod categories {
    use super::ALL_CONTEXTS;

    pub fn arrange_view() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_ARRANGE"))
            .copied()
            .collect()
    }

    pub fn automation() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_POOLEDENV"))
            .copied()
            .collect()
    }

    pub fn envelope() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_ENV"))
            .copied()
            .collect()
    }

    pub fn fixed_lane() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| {
                ctx.starts_with("MM_CTX_FIXEDLANE") || ctx.starts_with("MM_CTX_LINKEDLANE")
            })
            .copied()
            .collect()
    }

    pub fn midi() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_MIDI"))
            .copied()
            .collect()
    }

    pub fn media_item() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_ITEM") || ctx.starts_with("MM_CTX_CROSSFADE"))
            .copied()
            .collect()
    }

    pub fn mixer() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_MCP"))
            .copied()
            .collect()
    }

    pub fn project() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| {
                ctx.starts_with("MM_CTX_MARKER")
                    || ctx.starts_with("MM_CTX_REGION")
                    || ctx.starts_with("MM_CTX_TEMPOMARKER")
            })
            .copied()
            .collect()
    }

    pub fn razor_edit() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_AREASEL"))
            .copied()
            .collect()
    }

    pub fn ruler() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_RULER") && !ctx.starts_with("MM_CTX_MIDI_RULER"))
            .copied()
            .collect()
    }

    pub fn track() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| ctx.starts_with("MM_CTX_TRACK") || ctx.starts_with("MM_CTX_TCP"))
            .copied()
            .collect()
    }

    pub fn other() -> Vec<&'static str> {
        ALL_CONTEXTS
            .iter()
            .filter(|ctx| {
                !ctx.starts_with("MM_CTX_ARRANGE")
                    && !ctx.starts_with("MM_CTX_POOLEDENV")
                    && !ctx.starts_with("MM_CTX_ENV")
                    && !ctx.starts_with("MM_CTX_FIXEDLANE")
                    && !ctx.starts_with("MM_CTX_LINKEDLANE")
                    && !ctx.starts_with("MM_CTX_MIDI")
                    && !ctx.starts_with("MM_CTX_ITEM")
                    && !ctx.starts_with("MM_CTX_CROSSFADE")
                    && !ctx.starts_with("MM_CTX_MCP")
                    && !ctx.starts_with("MM_CTX_MARKER")
                    && !ctx.starts_with("MM_CTX_REGION")
                    && !ctx.starts_with("MM_CTX_TEMPOMARKER")
                    && !ctx.starts_with("MM_CTX_AREASEL")
                    && !ctx.starts_with("MM_CTX_RULER")
                    && !ctx.starts_with("MM_CTX_TRACK")
                    && !ctx.starts_with("MM_CTX_TCP")
            })
            .copied()
            .collect()
    }
}
