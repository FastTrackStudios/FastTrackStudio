// Watch ← iPhone config inheritance (WCSession).
//
// The watch has no independent way to know which Task server + account it
// should talk to. Instead it INHERITS the paired iPhone's active config
// ({baseURL, orgSlug, token}) over WatchConnectivity: the phone pushes its
// current server/session via `updateApplicationContext(_:)`, and this
// delegate applies it to the shared `TaskStore` — so signing in on the phone
// (and the federated-locker flow that follows) auto-configures the watch with
// no manual Settings entry. Manual Settings still works as a fallback.
//
// Pairs with the iOS-side shim documented in
// `apps/task/mobile/ios/watch-config-bridge.md` (the phone sender). The keys
// here MUST match the keys the phone writes.

import Foundation
import WatchConnectivity

/// Activates a `WCSession` and mirrors the iPhone's application context into
/// the watch's `TaskStore`. Only overwrites a field when the phone actually
/// sent a non-empty value, so a partial/late context never wipes a working
/// manual config.
@MainActor
final class PhoneSync: NSObject, WCSessionDelegate {
    private weak var store: TaskStore?

    /// Wire to the store and activate the session. Call once, on app launch.
    func start(store: TaskStore) {
        self.store = store
        guard WCSession.isSupported() else { return }
        let session = WCSession.default
        session.delegate = self
        session.activate()
        // The phone may have delivered a context before we activated —
        // `receivedApplicationContext` holds the latest one, replayed here.
        apply(session.receivedApplicationContext)
    }

    /// Apply an application-context dictionary to the store (main-actor).
    private func apply(_ ctx: [String: Any]) {
        guard let store else { return }
        if let base = ctx["baseURL"] as? String, !base.isEmpty { store.baseURL = base }
        if let slug = ctx["orgSlug"] as? String, !slug.isEmpty { store.orgSlug = slug }
        if let token = ctx["token"] as? String, !token.isEmpty { store.token = token }
    }

    // ── WCSessionDelegate ──
    // On watchOS the only required callback is activation completion; the
    // inactive/deactivate pair is iOS-only. Delegate callbacks arrive off the
    // main actor, so hop back before touching the @MainActor store.

    nonisolated func session(
        _ session: WCSession,
        activationDidCompleteWith activationState: WCSessionActivationState,
        error: Error?
    ) {
        let ctx = session.receivedApplicationContext
        Task { @MainActor in self.apply(ctx) }
    }

    nonisolated func session(
        _ session: WCSession,
        didReceiveApplicationContext applicationContext: [String: Any]
    ) {
        Task { @MainActor in self.apply(applicationContext) }
    }
}
