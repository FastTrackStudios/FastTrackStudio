// Task watch app — entry point. A vertical-paging TabView: Timer (start /
// stop time tracking), Capture (dictate a fleeting note), Settings (server +
// token). The two main tabs are the whole point of the app: one-tap timer
// toggling and voice-captured notes for later processing.

import SwiftUI

@main
struct TaskWatchApp: App {
    @State private var store = TaskStore()
    @State private var tab: String =
        ProcessInfo.processInfo.environment["TASK_TAB"] ?? "timer"

    var body: some Scene {
        WindowGroup {
            TabView(selection: $tab) {
                TimerView()
                    .tag("timer")
                CaptureView()
                    .tag("capture")
                SettingsView()
                    .tag("settings")
            }
            .tabViewStyle(.verticalPage)
            .environment(store)
        }
    }
}
