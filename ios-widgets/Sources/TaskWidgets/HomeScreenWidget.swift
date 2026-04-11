import WidgetKit
import SwiftUI
import AppIntents

// r[impl ios.widget.homescreen]

struct CompleteTaskIntent: AppIntent {
    static var title: LocalizedStringResource = "Complete Task"

    @Parameter(title: "Task Title")
    var taskTitle: String

    init() { self.taskTitle = "" }
    init(taskTitle: String) { self.taskTitle = taskTitle }

    func perform() async throws -> some IntentResult {
        SharedDataReader.appendPendingCompletion(taskTitle: taskTitle)
        return .result()
    }
}

struct HomeScreenEntry: TimelineEntry {
    let date: Date
    let tasks: [TaskSnapshot]
}

struct HomeScreenProvider: TimelineProvider {
    func placeholder(in context: Context) -> HomeScreenEntry {
        HomeScreenEntry(date: Date(), tasks: [])
    }
    func getSnapshot(in context: Context, completion: @escaping (HomeScreenEntry) -> Void) {
        completion(HomeScreenEntry(date: Date(), tasks: SharedDataReader.loadTodayTasks()))
    }
    func getTimeline(in context: Context, completion: @escaping (Timeline<HomeScreenEntry>) -> Void) {
        let tasks = SharedDataReader.loadTodayTasks()
        let entry = HomeScreenEntry(date: Date(), tasks: tasks)
        let refresh = Calendar.current.date(byAdding: .minute, value: 15, to: Date())!
        completion(Timeline(entries: [entry], policy: .after(refresh)))
    }
}

// r[impl ios.widget.homescreen]
struct HomeScreenWidgetView: View {
    let entry: HomeScreenEntry
    @Environment(\.widgetFamily) var family

    var maxItems: Int {
        switch family {
        case .systemLarge: return 7
        default: return 4
        }
    }

    var displayTasks: [TaskSnapshot] {
        Array(
            entry.tasks
                .filter { $0.status != "done" && $0.status != "cancelled" && $0.status != "archived" }
                .sorted { $0.urgencyScore > $1.urgencyScore }
                .prefix(maxItems)
        )
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack {
                Text("Today")
                    .font(.headline.weight(.semibold))
                Spacer()
                Text(formattedDate)
                    .font(.caption)
                    .foregroundStyle(.secondary)
            }
            .padding(.bottom, 2)

            if displayTasks.isEmpty {
                Spacer()
                Text("All caught up!")
                    .font(.subheadline)
                    .foregroundStyle(.secondary)
                    .frame(maxWidth: .infinity, alignment: .center)
                Spacer()
            } else {
                ForEach(displayTasks) { task in
                    HStack(spacing: 8) {
                        Button(intent: CompleteTaskIntent(taskTitle: task.title)) {
                            Image(systemName: "circle")
                                .foregroundStyle(priorityColor(task.priority))
                                .font(.body)
                        }
                        .buttonStyle(.plain)

                        VStack(alignment: .leading, spacing: 1) {
                            Text(task.title)
                                .font(.subheadline)
                                .lineLimit(1)
                            if let due = task.due, task.isOverdue {
                                Text(due)
                                    .font(.caption2)
                                    .foregroundStyle(.red)
                            } else if !task.projectNames.isEmpty {
                                Text(task.projectNames.first!)
                                    .font(.caption2)
                                    .foregroundStyle(.secondary)
                            }
                        }

                        Spacer(minLength: 0)
                    }
                }
            }
        }
        .padding()
        .containerBackground(.fill.tertiary, for: .widget)
    }

    var formattedDate: String {
        let fmt = DateFormatter()
        fmt.dateFormat = "MMM d"
        return fmt.string(from: entry.date)
    }

    func priorityColor(_ priority: String) -> Color {
        switch priority {
        case "urgent": return .red
        case "high": return .orange
        case "normal": return .accentColor
        default: return .secondary
        }
    }
}

struct HomeScreenWidget: Widget {
    let kind = "HomeScreenWidget"

    var body: some WidgetConfiguration {
        StaticConfiguration(kind: kind, provider: HomeScreenProvider()) { entry in
            HomeScreenWidgetView(entry: entry)
        }
        .configurationDisplayName("Task – Today")
        .description("See and complete today's top tasks from your home screen.")
        .supportedFamilies([.systemMedium, .systemLarge])
    }
}
