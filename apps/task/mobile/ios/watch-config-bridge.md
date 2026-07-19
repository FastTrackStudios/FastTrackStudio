# iOS → Watch config bridge (WCSession) — implementation guide

**Status: watch-side DONE, iOS-side is open R&D that must be built + verified on
airlock (Xcode).** This is subtask **S6** of the federated-account epic
(Task issue `8821acac`).

## Goal

The Apple Watch has no independent way to know which Task server + account to
talk to. It should **inherit the paired iPhone's active config** — the
`{baseURL, orgSlug, token}` the phone is currently signed into (including
whatever the federated-locker flow selected) — instead of the user typing it
into the watch's Settings by hand.

Transport: **WatchConnectivity `updateApplicationContext(_:)`** — a small,
latest-value-wins key/value dictionary that iOS delivers to the watch even when
it's not running. Perfect for config (not a message stream).

## Wire contract (keys MUST match on both sides)

```
["baseURL": String,   // e.g. "https://task.starcommand.live"
 "orgSlug": String,   // e.g. "codywright"
 "token":   String]   // the phone's active session token (or device token)
```

## Watch side — DONE (this PR)

- `apps/task/watchos/TaskWatch/Model/PhoneSync.swift` — a `WCSessionDelegate`
  that activates the session and applies received context to `TaskStore`
  (`baseURL`/`orgSlug`/`token`), only overwriting non-empty values so a partial
  context never wipes a working manual config.
- Wired in `TaskWatchApp.swift` via `.onAppear { phoneSync.start(store: store) }`.

The watch will inherit config the moment an iOS sender exists. Manual Settings
stays as the fallback. (Verify the watch build on airlock — no watchOS SDK in
the Linux dev shell.)

## iOS side — TODO (needs airlock; no precedent in this repo)

The dx (Dioxus mobile) Task iOS app currently has **zero native code**, and
neither does the audio app (`apps/fasttrackstudio/ios` is deploy scripts +
icons only — its watch talks straight to the rig over BLE/HTTP, so it never
needed a phone shim). So injecting a native `WCSession` host into the
dx-generated iOS app is unsolved here and needs experimentation on the Mac.

### Two things the iOS shim must do

1. **Read the Rust app's active config.** The Rust UI already persists, in the
   iOS app container:
   - `$HOME/.local/share/task/servers.json` + `active-server`
     (`crates/task/ui/src/server_registry.rs`) — the active `ServerEntry`
     (`server_url`, `session_token`, …).
   - the FileTokenStore under `.../task/ui-tokens/`.
   Simplest robust option: have the Rust side write a dedicated
   `watch-config.json` (`{baseURL, orgSlug, token}`, http base already derived)
   into the container whenever the active server/session changes — a tiny,
   stable file the native shim reads without parsing the registry. (Add this
   write next to `set_active_server` / `sync_active_server_entry`.)

2. **Push it over WCSession** whenever it changes, via a native
   `WCSessionDelegate` that calls `updateApplicationContext`.

### Reference iOS shim (adapt when wiring into the dx build)

```swift
import Foundation
import WatchConnectivity

final class WatchConfigBridge: NSObject, WCSessionDelegate {
    static let shared = WatchConfigBridge()

    func start() {
        guard WCSession.isSupported() else { return }
        let s = WCSession.default
        s.delegate = self
        s.activate()
    }

    /// Call whenever the active server/session changes (or on a timer / file
    /// watch of watch-config.json). Idempotent: WCSession dedups identical
    /// contexts.
    func push(baseURL: String, orgSlug: String, token: String) {
        guard WCSession.default.activationState == .activated,
              WCSession.default.isWatchAppInstalled else { return }
        try? WCSession.default.updateApplicationContext([
            "baseURL": baseURL, "orgSlug": orgSlug, "token": token,
        ])
    }

    // iOS requires all three; no-ops are fine for a one-way sender.
    func session(_ s: WCSession, activationDidCompleteWith st: WCSessionActivationState, error: Error?) {}
    func sessionDidBecomeInactive(_ s: WCSession) {}
    func sessionDidDeactivate(_ s: WCSession) { s.activate() }
}
```

### The hard part: hosting native Swift in the dx iOS app

`dx build --platform ios` generates the Xcode project; there is no `AppDelegate`
to hook (the entry is the Rust `main`). Options to investigate on airlock, in
rough order of preference:

1. **dx bundle-time inclusion** — check whether Dioxus 0.7 supports adding
   native sources / a plist `UIApplicationDelegate` via `Dioxus.toml` or an
   `ios/` overlay that survives regeneration. If so, drop in
   `WatchConfigBridge.swift` + an `@objc` `AppDelegate` that calls `start()` +
   installs a file-watch of `watch-config.json`.
2. **FFI from Rust** — expose the bridge to Rust via a tiny Swift/ObjC static
   lib linked into the app, called from the Rust side when config changes
   (`swift-bridge` / a C ABI shim). Heaviest, but keeps the trigger in Rust
   where the config actually changes.
3. **Post-build injection** in `deploy-testflight.sh` — least clean; only if 1/2
   fail.

Whichever wins, the acceptance test is: sign in on the phone → within a few
seconds the watch's Settings auto-populate and `Test connection` succeeds,
with no manual entry. Then drop the manual Settings fields to read-only
(inherited) with a manual-override escape hatch.

## Related
- Server bridge that accepts the inherited token: `apps/task/server/src/watch_bridge.rs`
  (`/watch/v1`, accepts a real `current_session`-validated token OR the static
  `TASK_WATCH_TOKEN`).
- Federated-account plan: `apps/task/plans/multi-server-auth.md` item 6.
