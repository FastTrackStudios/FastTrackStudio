//! Shared action definitions for dynamic-template providers.

actions_proto::define_actions! {
    pub dynamic_template_actions {
        prefix: "fts.dynamic_template",
        title: "Dynamic Template",
        SORT_SELECTED = "sort_selected" {
            name: "Sort Selected",
            description: "Organize selected items into a hierarchical track template",
            category: General,
        }
        SORT_ALL = "sort_all" {
            name: "Sort All",
            description: "Organize all project items into a hierarchical track template",
            category: General,
        }
        IMPORT_AND_SORT = "import_and_sort" {
            name: "Import and Sort",
            description: "Import audio files and organize them into a hierarchical track template",
            category: General,
        }
        ORGANIZE_DEMO = "organize_demo" {
            name: "Organize Demo Inputs",
            description: "Run organizer on a built-in sample input set",
            category: Dev,
            group: "Dev",
        }
        LOG_STATUS = "log_status" {
            name: "Log Status",
            description: "Log dynamic-template runtime status",
            category: Dev,
            group: "Dev",
        }
        LOG_GROUPS = "log_groups" {
            name: "Log Groups",
            description: "Log configured dynamic-template group names",
            category: Dev,
            group: "Dev",
        }
        AUTO_COLOR_ALL = "auto_color_all" {
            name: "Auto Color All Tracks",
            description: "Classify all tracks by instrument group and apply colors",
            category: General,
        }
        AUTO_COLOR_SELECTED = "auto_color_selected" {
            name: "Auto Color Selected",
            description: "Classify selected tracks by instrument group and apply colors",
            category: General,
        }
        AUTO_COLOR_TOGGLE = "auto_color_toggle" {
            name: "Toggle Auto Color",
            description: "Toggle auto-color on/off (applies or clears all track colors)",
            category: General,
        }
        AUTO_COLOR_CLEAR_ALL = "auto_color_clear_all" {
            name: "Clear All Track Colors",
            description: "Reset colors on all tracks to default",
            category: General,
        }
        AUTO_COLOR_CLEAR_SELECTED = "auto_color_clear_selected" {
            name: "Clear Selected Track Colors",
            description: "Reset colors on selected tracks to default",
            category: General,
        }
    }
}
