import Foundation

// r[impl ios.widget.lockscreen.refresh]

/// Lightweight task snapshot written by the main app to the App Group container.
/// The main app writes today_tasks.json after every VaultService mutation.
public struct TaskSnapshot: Codable, Identifiable {
    public let id: String
    public let title: String
    public let status: String
    public let priority: String
    public let due: String?
    public let scheduled: String?
    public let projectNames: [String]
    public let urgencyScore: Int
    public let isOverdue: Bool
}

/// Reads/writes shared data from the App Group container.
// r[impl ios.widget.lockscreen]
public enum SharedDataReader {
    public static let appGroupID = "group.com.codyswright.task"
    static let snapshotFileName = "today_tasks.json"
    static let pendingCaptureFileName = "pending_captures.json"
    static let pendingCompletionsFileName = "pending_completions.json"

    static var containerURL: URL? {
        FileManager.default.containerURL(forSecurityApplicationGroupIdentifier: appGroupID)
    }

    /// Load today's task snapshot from the App Group container.
    public static func loadTodayTasks() -> [TaskSnapshot] {
        guard let url = containerURL?.appendingPathComponent(snapshotFileName),
              let data = try? Data(contentsOf: url),
              let tasks = try? JSONDecoder().decode([TaskSnapshot].self, from: data) else {
            return []
        }
        return tasks
    }

    /// Append a pending capture (from lock screen widget) to the queue.
    // r[impl ios.widget.lockscreen.add]
    public static func appendPendingCapture(title: String) {
        guard let url = containerURL?.appendingPathComponent(pendingCaptureFileName) else { return }
        var pending = loadPendingCaptures()
        pending.append(title)
        if let data = try? JSONEncoder().encode(pending) {
            try? data.write(to: url, options: .atomic)
        }
    }

    public static func loadPendingCaptures() -> [String] {
        guard let url = containerURL?.appendingPathComponent(pendingCaptureFileName),
              let data = try? Data(contentsOf: url),
              let items = try? JSONDecoder().decode([String].self, from: data) else {
            return []
        }
        return items
    }

    /// Queue a task completion from the home screen interactive widget.
    public static func appendPendingCompletion(taskTitle: String) {
        guard let url = containerURL?.appendingPathComponent(pendingCompletionsFileName) else { return }
        var completions = loadPendingCompletions()
        completions.append(taskTitle)
        if let data = try? JSONEncoder().encode(completions) {
            try? data.write(to: url, options: .atomic)
        }
    }

    public static func loadPendingCompletions() -> [String] {
        guard let url = containerURL?.appendingPathComponent(pendingCompletionsFileName),
              let data = try? Data(contentsOf: url),
              let items = try? JSONDecoder().decode([String].self, from: data) else {
            return []
        }
        return items
    }
}
