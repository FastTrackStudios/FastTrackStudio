//! App-wide keyboard shortcuts, driven by the `input` crate's
//! trie-based [`InputProcessor`] (multi-key sequences, modal-ready).
//!
//! ## Architecture
//!
//! ONE canonical keymap ([`SHORTCUTS`]) is shared across desktop, web
//! and mobile builds. Each entry names its canonical chord plus an
//! optional **web alternate** for combos the browser refuses to yield
//! (see the reserved list below). [`keymap_config`] compiles the table
//! into an `input::KeymapConfig`; the processor resolves sequences —
//! including multi-key ones like `g h` — through its prefix trie.
//!
//! The engine mounts via [`use_app_shortcuts`], called from
//! [`crate::palette::CommandPalette`]: the shortcut targets (palette /
//! fleeting-capture / shell-panel contexts) and the router's
//! `Navigator` only exist under the `AppShell` layout, and the palette
//! is the one app-long component there that this subtask owns. The
//! wasm build installs **capture-phase** `keydown`/`keyup` listeners on
//! the document:
//!
//! - the raw DOM closure runs the processor *synchronously* so a match
//!   can `preventDefault()` + `stopPropagation()` in the same tick
//!   (that's what lets app shortcuts win over browser natives) — and
//!   ONLY a match does either, so unbound keys are untouched;
//! - matched action ids cross back into the Dioxus runtime over a
//!   channel a spawned task drains (signal writes + navigation are not
//!   legal inside a raw `wasm_bindgen` closure);
//! - `keyup` feeds [`InputProcessor::notify_key_release`] so the
//!   held-key / sticky-prefix bookkeeping stays correct across repeated
//!   sequences.
//!
//! **Typing guard**: when `document.activeElement` is an
//! input/textarea/select or a contenteditable region (the vault
//! editor), plain-key shortcuts are skipped entirely — only chords
//! carrying Ctrl/Cmd pass, so `g h` typed into the palette input or the
//! editor inserts text instead of navigating. (`input-dioxus` exports a
//! `TEXT_INPUT_FOCUS_COUNT` global for this, but nothing in the tree
//! increments it yet — the activeElement check is authoritative and
//! needs no per-widget cooperation.)
//!
//! ## Browser-reserved combos (cannot be intercepted)
//!
//! Chromium (and Brave) handle these at the browser level **before**
//! the page sees the keydown — `preventDefault()` is powerless, so the
//! canonical binding gets a web alternate instead:
//!
//! - `Ctrl+N` (new window), `Ctrl+Shift+N` (incognito)
//! - `Ctrl+T` (new tab), `Ctrl+Shift+T` (reopen tab)
//! - `Ctrl+W` / `Ctrl+Shift+W` (close tab / window)
//! - `Ctrl+Shift+Q` (quit, some platforms), `F11` (fullscreen on some)
//!
//! Combos that *are* deliverable but shadow a browser default (`Ctrl+P`
//! print, `Ctrl+K` omnibox search, `Ctrl+Shift+B` bookmarks bar) are
//! flagged [`ShortcutDef::overrides_browser`]: they stay active while
//! the **"prioritize app shortcuts"** pref (default on) is set, and are
//! dropped from the compiled keymap when the user turns it off.

use std::cell::RefCell;
use std::rc::Rc;

use dioxus::prelude::*;
use input::{InputProcessor, KeymapConfig};

// ── binding table ───────────────────────────────────────────────────

/// One canonical shortcut: the single source of truth for the compiled
/// keymap AND the read-only table Settings → Keyboard renders.
#[derive(Debug, PartialEq, Eq)]
pub struct ShortcutDef {
    /// Action id dispatched on match (see the dispatcher in
    /// [`use_app_shortcuts`]).
    pub action: &'static str,
    /// Human description for the Settings table.
    pub description: &'static str,
    /// Canonical key sequence (`input::parse_key_sequence` syntax:
    /// chords space-separated, modifiers `ctrl+`/`alt+`/`shift+`/`cmd+`).
    pub canonical: &'static str,
    /// Web alternate when the canonical chord is browser-RESERVED
    /// (undeliverable — see the module docs). `None` = canonical works
    /// everywhere.
    pub web: Option<&'static str>,
    /// The (effective) chord shadows a browser default that
    /// `preventDefault()` CAN suppress. Active only while the
    /// "prioritize app shortcuts" pref is on.
    pub overrides_browser: bool,
}

/// The canonical app keymap. Order is the Settings display order.
pub const SHORTCUTS: &[ShortcutDef] = &[
    ShortcutDef {
        action: "palette.toggle",
        description: "Open the command palette",
        canonical: "ctrl+p",
        web: None,
        // Shadows the browser print dialog (interceptable).
        overrides_browser: true,
    },
    ShortcutDef {
        action: "palette.toggle",
        description: "Open the command palette (alternate)",
        canonical: "ctrl+k",
        web: None,
        // Shadows omnibox search focus (interceptable).
        overrides_browser: true,
    },
    ShortcutDef {
        action: "capture.fleeting",
        description: "Capture a fleeting note",
        // Ctrl+N is browser-RESERVED on web (new window, undeliverable)
        // → remapped to Ctrl+Alt+N there. Desktop/mobile keep Ctrl+N.
        canonical: "ctrl+n",
        web: Some("ctrl+alt+n"),
        overrides_browser: false,
    },
    ShortcutDef {
        action: "capture.fleeting",
        description: "Capture a fleeting note (alternate)",
        canonical: "ctrl+shift+f",
        web: None,
        overrides_browser: false,
    },
    ShortcutDef {
        action: "sidebar.toggle",
        description: "Toggle the vault sidebar",
        canonical: "ctrl+\\",
        web: None,
        overrides_browser: false,
    },
    ShortcutDef {
        action: "panel.toggle",
        description: "Toggle the right panel",
        canonical: "ctrl+shift+b",
        web: None,
        // Shadows the bookmarks-bar toggle. Chromium delivers the
        // keydown, but suppression of the bar toggle is best-effort.
        overrides_browser: true,
    },
    ShortcutDef {
        action: "nav.home",
        description: "Go to Home (dashboard)",
        canonical: "g h",
        web: None,
        overrides_browser: false,
    },
    ShortcutDef {
        action: "nav.inbox",
        description: "Go to Inbox",
        canonical: "g i",
        web: None,
        overrides_browser: false,
    },
    ShortcutDef {
        action: "nav.tasks",
        description: "Go to Tasks",
        canonical: "g t",
        web: None,
        overrides_browser: false,
    },
    ShortcutDef {
        action: "nav.settings",
        description: "Open Settings",
        canonical: "ctrl+,",
        web: None,
        overrides_browser: false,
    },
];

/// The sequence this build actually binds: the web alternate on wasm
/// (browser-reserved canonicals are undeliverable there), the
/// canonical everywhere else.
#[must_use]
pub fn effective_sequence(def: &ShortcutDef) -> &'static str {
    if cfg!(target_arch = "wasm32") {
        def.web.unwrap_or(def.canonical)
    } else {
        def.canonical
    }
}

/// Compile the table into the processor's config. `prioritize_app` off
/// drops every binding flagged [`ShortcutDef::overrides_browser`], so
/// only non-conflicting shortcuts stay live.
#[must_use]
pub fn keymap_config(prioritize_app: bool) -> KeymapConfig {
    let mut cfg = KeymapConfig::default();
    let normal = cfg.keymap.entry("normal".to_string()).or_default();
    for def in SHORTCUTS {
        if def.overrides_browser && !prioritize_app {
            continue;
        }
        normal.insert(
            effective_sequence(def).to_string(),
            def.action.to_string(),
        );
    }
    cfg
}

// ── engine hook ─────────────────────────────────────────────────────

/// Mount the app-wide shortcut engine. Call once from an app-long
/// component under `AppShell` (today: [`crate::palette::CommandPalette`]
/// — it has the palette/chrome contexts and the router in scope).
pub fn use_app_shortcuts() {
    let palette = use_context::<crate::palette::PaletteOpen>().0;
    let fleeting = use_context::<crate::chrome::FleetingOpen>().0;
    let explorer = use_context::<Signal<crate::chrome::ExplorerOpen>>();
    let right_panel = use_context::<Signal<crate::chrome::RightPanelOpen>>();
    let nav = use_navigator();
    let prefs = use_context::<crate::prefs::PrefsCtx>().prefs;

    // The processor is shared between the raw DOM closures (sync match
    // + preventDefault) and the Dioxus side (config reloads). Single
    // threaded on wasm, so Rc<RefCell> is sound.
    let engine: Rc<RefCell<InputProcessor>> = use_hook(|| {
        Rc::new(RefCell::new(
            InputProcessor::from_config(keymap_config(true)).unwrap_or_default(),
        ))
    });

    // Recompile the keymap whenever the "prioritize app shortcuts"
    // pref changes (boot load included — prefs stream in after mount).
    {
        let engine = engine.clone();
        use_effect(move || {
            let prioritize = prefs.read().shortcuts_priority;
            if engine
                .borrow_mut()
                .reload_config(keymap_config(prioritize))
                .is_err()
            {
                tracing::warn!("shortcut keymap reload failed");
            }
        });
    }

    #[cfg(target_arch = "wasm32")]
    install_dom_listeners(engine, palette, fleeting, explorer, right_panel, nav);
    #[cfg(not(target_arch = "wasm32"))]
    let _ = (engine, palette, fleeting, explorer, right_panel, nav);
}

// ── wasm wiring ─────────────────────────────────────────────────────

/// Capture-phase document listeners + the action dispatcher task.
/// Installed once; the palette lives as long as the shell, so the
/// listeners are `forget`-leaked like the old palette hotkey was.
#[cfg(target_arch = "wasm32")]
fn install_dom_listeners(
    engine: Rc<RefCell<InputProcessor>>,
    mut palette: Signal<bool>,
    mut fleeting: Signal<bool>,
    mut explorer: Signal<crate::chrome::ExplorerOpen>,
    mut right_panel: Signal<crate::chrome::RightPanelOpen>,
    nav: dioxus_router::Navigator,
) {
    use std::cell::Cell;

    use futures_util::StreamExt;
    use wasm_bindgen::JsCast;
    use wasm_bindgen::prelude::Closure;

    use crate::routes::Route;

    use_hook(move || {
        // Matched action ids cross from the raw closure back into the
        // Dioxus runtime here (signal writes / navigation are not legal
        // outside it).
        let (tx, mut rx) = futures_channel::mpsc::unbounded::<String>();
        spawn(async move {
            while let Some(action) = rx.next().await {
                match action.as_str() {
                    "palette.toggle" => {
                        let cur = *palette.peek();
                        palette.set(!cur);
                    }
                    "capture.fleeting" => fleeting.set(true),
                    "sidebar.toggle" => {
                        let cur = explorer.peek().0;
                        explorer.set(crate::chrome::ExplorerOpen(!cur));
                    }
                    "panel.toggle" => {
                        let cur = right_panel.peek().0;
                        right_panel.set(crate::chrome::RightPanelOpen(!cur));
                    }
                    "nav.home" => {
                        nav.push(Route::DashboardRoute {});
                    }
                    "nav.inbox" => {
                        nav.push(Route::InboxRoute {});
                    }
                    "nav.tasks" => {
                        nav.push(Route::TasksRoute {});
                    }
                    "nav.settings" => {
                        nav.push(Route::SettingsRoute {});
                    }
                    other => tracing::warn!(%other, "unmapped shortcut action"),
                }
            }
        });

        // Bumped on every processed keydown; pending-sequence timeouts
        // only fire if no further key arrived (same epoch dance as
        // input-dioxus's hook, but on gloo timers — wasm has no tokio
        // time).
        let epoch = Rc::new(Cell::new(0_u64));

        let keydown = {
            let engine = engine.clone();
            let epoch = epoch.clone();
            Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |e: web_sys::KeyboardEvent| {
                // Typing guard: plain keys belong to the focused text
                // control (palette input, editor contenteditable, …);
                // only Ctrl/Cmd chords are shortcuts there.
                if text_entry_focused() && !(e.ctrl_key() || e.meta_key()) {
                    return;
                }
                // OS auto-repeat never fires app actions (a held Ctrl+P
                // must not strobe the palette); the processor's own
                // held-key suppression only covers in-sequence repeats.
                if e.repeat() {
                    return;
                }
                let Some(event) = convert_key_event(&e) else {
                    return;
                };
                epoch.set(epoch.get().wrapping_add(1));
                let ctx = input::ActionContext::new();
                let commands = engine
                    .borrow_mut()
                    .process(input::InputEvent::Key(event), &ctx);
                let mut matched = false;
                for cmd in commands {
                    match cmd {
                        input::InputCommand::Action(id) => {
                            matched = true;
                            let _ = tx.unbounded_send(id.as_str().to_string());
                        }
                        input::InputCommand::ActionWithArgs { action, .. } => {
                            matched = true;
                            let _ = tx.unbounded_send(action.as_str().to_string());
                        }
                        input::InputCommand::Pending { .. } => {
                            schedule_sequence_timeout(engine.clone(), epoch.clone());
                        }
                        _ => {}
                    }
                }
                // Only a MATCH claims the event — pending prefixes and
                // misses leave the browser/native behavior untouched.
                if matched {
                    e.prevent_default();
                    e.stop_propagation();
                }
            })
        };

        // keyup keeps the processor's held-key / sticky-prefix state
        // honest — without it a repeated `g h` would be misread as OS
        // auto-repeat and swallowed.
        let keyup = {
            let engine = engine.clone();
            Closure::<dyn FnMut(web_sys::KeyboardEvent)>::new(move |e: web_sys::KeyboardEvent| {
                if let Some(k) = convert_key_event(&e) {
                    let _ = engine
                        .borrow_mut()
                        .notify_key_release(input::KeyChord::new(k.key, k.modifiers));
                }
            })
        };

        if let Some(doc) = web_sys::window().and_then(|w| w.document()) {
            // Capture phase (`true`): first crack at the event, ahead
            // of every bubbling in-page handler.
            let _ = doc.add_event_listener_with_callback_and_bool(
                "keydown",
                keydown.as_ref().unchecked_ref(),
                true,
            );
            let _ = doc.add_event_listener_with_callback_and_bool(
                "keyup",
                keyup.as_ref().unchecked_ref(),
                true,
            );
        }
        keydown.forget();
        keyup.forget();
    });
}

/// Expire a pending multi-key prefix (`g` waiting for `h`) after the
/// processor's timeout, unless another key arrived first.
#[cfg(target_arch = "wasm32")]
fn schedule_sequence_timeout(
    engine: Rc<RefCell<InputProcessor>>,
    epoch: Rc<std::cell::Cell<u64>>,
) {
    let want = epoch.get();
    let ms = u32::try_from(engine.borrow().timeout_duration().as_millis()).unwrap_or(1000);
    gloo_timers::callback::Timeout::new(ms, move || {
        if epoch.get() == want {
            let _ = engine.borrow_mut().timeout_expired();
        }
    })
    .forget();
}

/// Is the focused element a text-entry surface? Inputs, textareas,
/// selects, and contenteditable hosts (the vault editor) all qualify.
#[cfg(target_arch = "wasm32")]
fn text_entry_focused() -> bool {
    use wasm_bindgen::JsCast;
    let Some(el) = web_sys::window()
        .and_then(|w| w.document())
        .and_then(|d| d.active_element())
    else {
        return false;
    };
    let tag = el.tag_name().to_ascii_lowercase();
    if matches!(tag.as_str(), "input" | "textarea" | "select") {
        return true;
    }
    el.dyn_ref::<web_sys::HtmlElement>()
        .is_some_and(|h| h.is_content_editable())
}

/// `web_sys::KeyboardEvent` → `input::KeyEvent`. Mirrors
/// `input-dioxus`'s Dioxus-event converter (which we can't use here —
/// the raw capture listener sees DOM events, not Dioxus ones).
#[cfg(target_arch = "wasm32")]
fn convert_key_event(e: &web_sys::KeyboardEvent) -> Option<input::KeyEvent> {
    let key = e.key();
    let code = match key.as_str() {
        "ArrowUp" => input::KeyCode::ArrowUp,
        "ArrowDown" => input::KeyCode::ArrowDown,
        "ArrowLeft" => input::KeyCode::ArrowLeft,
        "ArrowRight" => input::KeyCode::ArrowRight,
        "Enter" => input::KeyCode::Enter,
        "Escape" => input::KeyCode::Escape,
        "Tab" => input::KeyCode::Tab,
        "Backspace" => input::KeyCode::Backspace,
        "Delete" => input::KeyCode::Delete,
        f if f.len() >= 2 && f.starts_with('F') => {
            input::KeyCode::F(f[1..].parse::<u8>().ok().filter(|n| (1..=12).contains(n))?)
        }
        c if c.chars().count() == 1 => input::KeyCode::Character(c.to_lowercase()),
        _ => return None,
    };
    Some(input::KeyEvent {
        key: code,
        modifiers: input::Modifiers {
            ctrl: e.ctrl_key(),
            alt: e.alt_key(),
            shift: e.shift_key(),
            meta: e.meta_key(),
        },
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_sequence_parses() {
        for def in SHORTCUTS {
            input::parse_key_sequence(def.canonical)
                .unwrap_or_else(|e| panic!("bad canonical {:?}: {e}", def.canonical));
            if let Some(web) = def.web {
                input::parse_key_sequence(web)
                    .unwrap_or_else(|e| panic!("bad web alt {:?}: {e}", web));
            }
        }
    }

    #[test]
    fn full_keymap_compiles_into_a_processor() {
        let cfg = keymap_config(true);
        assert!(InputProcessor::from_config(cfg).is_ok());
    }

    #[test]
    fn priority_off_drops_only_overriding_bindings() {
        let on = keymap_config(true);
        let off = keymap_config(false);
        let overriding = SHORTCUTS.iter().filter(|d| d.overrides_browser).count();
        assert!(overriding > 0, "table should flag some overrides");
        assert_eq!(
            on.keymap["normal"].len(),
            off.keymap["normal"].len() + overriding
        );
        // The g-sequences survive either way.
        assert!(off.keymap["normal"].contains_key("g h"));
    }

    #[test]
    fn browser_reserved_canonicals_have_web_alternates() {
        // Ctrl+N/T/W (and their shifted forms) are undeliverable on
        // web — any table entry using one MUST carry a web remap.
        let reserved = ["ctrl+n", "ctrl+t", "ctrl+w", "ctrl+shift+n", "ctrl+shift+t", "ctrl+shift+w"];
        for def in SHORTCUTS {
            if reserved.contains(&def.canonical) {
                assert!(
                    def.web.is_some(),
                    "{} is browser-reserved and needs a web alternate",
                    def.canonical
                );
            }
        }
    }
}
