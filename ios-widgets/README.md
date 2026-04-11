# TaskWidgets

iOS WidgetKit extension package for the Task app. Provides lock screen and home screen widgets powered by a shared App Group container.

## Integration with Xcode Project

1. Open your Xcode project.
2. Add a new **Widget Extension** target (File > New > Target > Widget Extension). Name it `TaskWidgetExtension`.
3. In the widget extension target's build phases, add `TaskWidgets` as a dependency by linking to this Swift package (File > Add Package Dependencies, point to this local package or embed it via the monorepo).
4. Replace the generated widget files with references to the types exported from this package, or delete the boilerplate and import `TaskWidgets`.
5. Set the `@main` entry point to `TaskWidgetBundle` from this package (or re-export it from the extension target).

## App Group Setup

Both the main app target and the widget extension target must belong to the same App Group so they can share files.

1. In Xcode, select the **main app target** > Signing & Capabilities > + Capability > App Groups.
2. Add the group identifier: `group.com.codyswright.task`
3. Repeat for the **widget extension target**.
4. Confirm the entitlement appears in both `.entitlements` files:
   ```xml
   <key>com.apple.security.application-groups</key>
   <array>
       <string>group.com.codyswright.task</string>
   </array>
   ```

The `SharedDataReader.appGroupID` constant in `SharedData.swift` is set to this identifier. If the group ID ever changes, update it there.

## iOS Version Requirements

| Minimum | Feature |
|---------|---------|
| iOS 16.0 | Lock screen widgets (`accessoryCircular`, `accessoryRectangular`, `accessoryInline`) via WidgetKit |
| iOS 16.0 | `AppIntents` framework for `AddTaskIntent` and lock screen interactive button |
| iOS 17.0 | Interactive home screen checkboxes (`Button(intent:)` inside `.systemMedium`/`.systemLarge` widgets) |

The package deployment target is set to iOS 16. The interactive `CompleteTaskIntent` button in `HomeScreenWidget` requires iOS 17 at runtime. Wrap any iOS 17-only surfaces in `#available(iOS 17, *)` guards in the consuming Xcode target if you need to support iOS 16 for the home screen widget.

## How the Main App Writes today_tasks.json

After every mutation through `VaultService` (create, update, complete, delete), the main app should:

1. Query tasks that are due or scheduled today, plus any overdue tasks.
2. Map them to `[TaskSnapshot]` — a lightweight, Codable struct with no internal Vault types.
3. JSON-encode the array and write it atomically to the shared container:

```swift
import Foundation

func writeWidgetSnapshot(_ tasks: [TaskSnapshot]) {
    guard let containerURL = FileManager.default
        .containerURL(forSecurityApplicationGroupIdentifier: SharedDataReader.appGroupID) else { return }
    let url = containerURL.appendingPathComponent("today_tasks.json")
    if let data = try? JSONEncoder().encode(tasks) {
        try? data.write(to: url, options: .atomic)
    }
    // Tell WidgetKit to reload all widget timelines immediately.
    WidgetCenter.shared.reloadAllTimelines()
}
```

Call `WidgetCenter.shared.reloadAllTimelines()` after writing so the widgets pick up changes without waiting for the 15-minute polling interval.

## Processing Pending Captures and Completions on App Open

The widget extension cannot write directly to the Vault. Instead it queues intent results to two JSON files in the shared container:

| File | Written by | Meaning |
|------|-----------|---------|
| `pending_captures.json` | `AddTaskIntent` (lock screen widget) | New task titles to create |
| `pending_completions.json` | `CompleteTaskIntent` (home screen widget) | Task titles to mark done |

On every app foreground event (`scenePhase == .active` or `applicationDidBecomeActive`), drain both queues:

```swift
func processPendingWidgetActions() {
    // 1. Captures
    let captures = SharedDataReader.loadPendingCaptures()
    for title in captures {
        vaultService.createTask(title: title)
    }
    if !captures.isEmpty {
        clearFile(named: SharedDataReader.pendingCaptureFileName)
    }

    // 2. Completions
    let completions = SharedDataReader.loadPendingCompletions()
    for title in completions {
        if let task = vaultService.findTask(byTitle: title) {
            vaultService.completeTask(id: task.id)
        }
    }
    if !completions.isEmpty {
        clearFile(named: SharedDataReader.pendingCompletionsFileName)
    }
}

private func clearFile(named name: String) {
    guard let url = FileManager.default
        .containerURL(forSecurityApplicationGroupIdentifier: SharedDataReader.appGroupID)?
        .appendingPathComponent(name) else { return }
    try? Data().write(to: url, options: .atomic)
}
```

After draining, call `writeWidgetSnapshot(...)` to refresh the widget display with the updated state.
