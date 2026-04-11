import WidgetKit
import SwiftUI

// r[impl ios.widget.lockscreen]
// r[impl ios.widget.homescreen]

@main
struct TaskWidgetBundle: WidgetBundle {
    var body: some Widget {
        LockScreenDisplayWidget()
        LockScreenAddTaskWidget()
        HomeScreenWidget()
    }
}
