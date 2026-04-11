import WidgetKit
import SwiftUI

// r[impl ios.widget.lockscreen.display]

struct LockScreenEntry: TimelineEntry {
    let date: Date
    let tasks: [TaskSnapshot]
}

struct LockScreenProvider: TimelineProvider {
    func placeholder(in context: Context) -> LockScreenEntry {
        LockScreenEntry(date: Date(), tasks: [])
    }

    func getSnapshot(in context: Context, completion: @escaping (LockScreenEntry) -> Void) {
        completion(LockScreenEntry(date: Date(), tasks: SharedDataReader.loadTodayTasks()))
    }

    // r[impl ios.widget.lockscreen.refresh]
    func getTimeline(in context: Context, completion: @escaping (Timeline<LockScreenEntry>) -> Void) {
        let tasks = SharedDataReader.loadTodayTasks()
        let entry = LockScreenEntry(date: Date(), tasks: tasks)
        let refresh = Calendar.current.date(byAdding: .minute, value: 15, to: Date())!
        completion(Timeline(entries: [entry], policy: .after(refresh)))
    }
}

// Circular: overdue count badge
struct CircularView: View {
    let entry: LockScreenEntry
    var overdueCount: Int { entry.tasks.filter { $0.isOverdue }.count }

    var body: some View {
        ZStack {
            AccessoryWidgetBackground()
            VStack(spacing: 1) {
                Text("\(overdueCount)")
                    .font(.system(size: 20, weight: .bold, design: .rounded))
                Text("overdue")
                    .font(.system(size: 7))
                    .foregroundStyle(.secondary)
            }
        }
    }
}

// r[impl ios.widget.lockscreen.display]
// Rectangular: top 2 tasks by urgency
struct RectangularView: View {
    let entry: LockScreenEntry
    var topTasks: [TaskSnapshot] {
        Array(entry.tasks.sorted { $0.urgencyScore > $1.urgencyScore }.prefix(2))
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 4) {
            if topTasks.isEmpty {
                Text("No tasks today")
                    .font(.caption)
                    .foregroundStyle(.secondary)
            } else {
                ForEach(topTasks) { task in
                    HStack(spacing: 4) {
                        Image(systemName: task.isOverdue ? "exclamationmark.circle.fill" : "circle")
                            .foregroundStyle(task.isOverdue ? .red : .primary)
                            .font(.caption2)
                        Text(task.title)
                            .font(.caption)
                            .lineLimit(1)
                        Spacer(minLength: 0)
                        if let due = task.due {
                            Text(due)
                                .font(.caption2)
                                .foregroundStyle(.secondary)
                        }
                    }
                }
            }
        }
    }
}

// Inline: highest urgency task title
struct InlineView: View {
    let entry: LockScreenEntry
    var topTask: TaskSnapshot? { entry.tasks.max(by: { $0.urgencyScore < $1.urgencyScore }) }

    var body: some View {
        if let task = topTask {
            Label(task.title, systemImage: task.isOverdue ? "exclamationmark.circle" : "circle")
        } else {
            Text("No tasks")
        }
    }
}

// r[impl ios.widget.lockscreen.display.tap]
struct LockScreenDisplayWidget: Widget {
    let kind = "LockScreenDisplayWidget"

    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: LockScreenProvider()) { entry in
            Group {
                switch entry.tasks.count {
                default:
                    RectangularView(entry: entry)
                }
            }
            .widgetURL(URL(string: "task://today"))
        }
        .configurationDisplayName("Today's Tasks")
        .description("See your top tasks for today on the lock screen.")
        .supportedFamilies([
            .accessoryCircular,
            .accessoryRectangular,
            .accessoryInline,
        ])
    }
}
