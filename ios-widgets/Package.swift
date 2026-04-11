// swift-tools-version: 5.9
import PackageDescription

let package = Package(
    name: "TaskWidgets",
    platforms: [
        .iOS(.v16),
    ],
    products: [
        .library(name: "TaskWidgets", targets: ["TaskWidgets"]),
    ],
    targets: [
        .target(
            name: "TaskWidgets",
            path: "Sources/TaskWidgets"
        ),
    ]
)
