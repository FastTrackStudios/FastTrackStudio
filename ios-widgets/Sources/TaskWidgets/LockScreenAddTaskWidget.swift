import WidgetKit
import SwiftUI
import AppIntents

// r[impl ios.widget.lockscreen.add]
// r[impl ios.widget.lockscreen.add.authentication]

struct AddTaskIntent: AppIntent {
    static var title: LocalizedStringResource = "Add Task"
    static var description = IntentDescription("Quickly capture a task from the lock screen.")
    static var authenticationPolicy: IntentAuthenticationPolicy = .requiresLocalAuthentication

    @Parameter(title: "Task Title", requestValueDialog: IntentDialog("What's the task?"))
    var taskTitle: String

    func perform() async throws -> some IntentResult {
        SharedDataReader.appendPendingCapture(title: taskTitle)
        return .result()
    }
}

struct AddTaskEntry: TimelineEntry {
    let date: Date
}

struct AddTaskProvider: TimelineProvider {
    func placeholder(in context: Context) -> AddTaskEntry { AddTaskEntry(date: Date()) }
    func getSnapshot(in context: Context, completion: @escaping (AddTaskEntry) -> Void) {
        completion(AddTaskEntry(date: Date()))
    }
    func getTimeline(in context: Context, completion: @escaping (Timeline<AddTaskEntry>) -> Void) {
        completion(Timeline(entries: [AddTaskEntry(date: Date())], policy: .never))
    }
}

struct AddTaskWidgetView: View {
    var body: some View {
        Button(intent: AddTaskIntent()) {
            ZStack {
                AccessoryWidgetBackground()
                Image(systemName: "plus")
                    .font(.title3.weight(.semibold))
            }
        }
        .buttonStyle(.plain)
    }
}

struct LockScreenAddTaskWidget: Widget {
    let kind = "LockScreenAddTaskWidget"

    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: AddTaskProvider()) { _ in
            AddTaskWidgetView()
        }
        .configurationDisplayName("Add Task")
        .description("Capture a new task directly from the lock screen.")
        .supportedFamilies([.accessoryCircular])
    }
}
