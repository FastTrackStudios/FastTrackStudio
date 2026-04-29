//! Component showcase — importable sanity check for all fts-ui components.
//!
//! ```rust,ignore
//! use fts_ui::showcase::Showcase;
//! rsx! { Showcase {} }
//! ```

use crate::prelude::*;
use dioxus::prelude::*;

/// Full showcase of every fts-ui component with spacing rulers.
/// Import this into any app to visually verify the design system.
#[component]
pub fn Showcase() -> Element {
    rsx! {
        div { class: "min-h-screen bg-background text-foreground",
            div { class: "flex",
                // Sidebar nav
                nav { class: "w-56 h-screen sticky top-0 bg-sidebar border-r border-sidebar-border p-4 overflow-y-auto shrink-0",
                    p { class: "text-sm font-semibold mb-4", "Components" }
                    div { class: "flex flex-col gap-1 text-sm text-muted-foreground",
                        a { class: "hover:text-foreground", href: "#buttons", "Buttons" }
                        a { class: "hover:text-foreground", href: "#inputs", "Inputs" }
                        a { class: "hover:text-foreground", href: "#badges", "Badges" }
                        a { class: "hover:text-foreground", href: "#cards", "Cards" }
                        a { class: "hover:text-foreground", href: "#select", "Select" }
                        a { class: "hover:text-foreground", href: "#checkbox-switch", "Checkbox & Switch" }
                        a { class: "hover:text-foreground", href: "#progress", "Progress" }
                        a { class: "hover:text-foreground", href: "#alerts", "Alerts" }
                        a { class: "hover:text-foreground", href: "#tables", "Tables" }
                        a { class: "hover:text-foreground", href: "#layout", "Layout" }
                        a { class: "hover:text-foreground", href: "#typography", "Typography" }
                        a { class: "hover:text-foreground", href: "#tooltips", "Tooltips" }
                        a { class: "hover:text-foreground", href: "#accordion", "Accordion" }
                        a { class: "hover:text-foreground", href: "#tabs", "Tabs" }
                        a { class: "hover:text-foreground", href: "#icons", "Icons" }
                        a { class: "hover:text-foreground", href: "#spacing", "Spacing Test" }
                    }
                }

                // Main content
                main { class: "flex-1 p-8 overflow-y-auto",
                    div { class: "max-w-4xl mx-auto flex flex-col gap-12",

                        // ── Buttons ──────────────────────────────
                        ShowcaseSection { id: "buttons", title: "Buttons",
                            ShowcaseRow { label: "Variants",
                                Button { variant: ButtonVariant::Primary, "Primary" }
                                Button { variant: ButtonVariant::Secondary, "Secondary" }
                                Button { variant: ButtonVariant::Outline, "Outline" }
                                Button { variant: ButtonVariant::Ghost, "Ghost" }
                                Button { variant: ButtonVariant::Destructive, "Destructive" }
                                Button { variant: ButtonVariant::Link, "Link" }
                            }
                            ShowcaseRow { label: "Sizes",
                                Button { size: ButtonSize::Small, "Small" }
                                Button { size: ButtonSize::Medium, "Medium" }
                                Button { size: ButtonSize::Large, "Large" }
                            }
                            ShowcaseRow { label: "States",
                                Button { disabled: true, "Disabled" }
                                Button { loading: true, "Loading" }
                            }
                        }

                        // ── Inputs ───────────────────────────────
                        ShowcaseSection { id: "inputs", title: "Inputs",
                            ShowcaseRow { label: "Sizes",
                                Input { value: use_signal(|| "Small".to_string()), size: InputSize::Small }
                                Input { value: use_signal(|| "Medium".to_string()) }
                                Input { value: use_signal(|| "Large".to_string()), size: InputSize::Large }
                            }
                            ShowcaseRow { label: "States",
                                Input { value: use_signal(String::new), placeholder: "Placeholder...".to_string() }
                                Input { value: use_signal(|| "Disabled".to_string()), disabled: true }
                                Input { value: use_signal(|| "Error".to_string()), variant: InputVariant::Error }
                            }
                            ShowcaseRow { label: "Textarea",
                                Textarea { value: use_signal(|| "Multi-line content.\nSecond line.".to_string()) }
                            }
                        }

                        // ── Badges ───────────────────────────────
                        ShowcaseSection { id: "badges", title: "Badges",
                            ShowcaseRow { label: "Variants",
                                Badge { variant: BadgeVariant::Default, "Default" }
                                Badge { variant: BadgeVariant::Secondary, "Secondary" }
                                Badge { variant: BadgeVariant::Destructive, "Destructive" }
                                Badge { variant: BadgeVariant::Outline, "Outline" }
                            }
                            ShowcaseRow { label: "Status Badges",
                                StatusBadge { variant: StatusBadgeVariant::Success, label: "Connected".to_string() }
                                StatusBadge { variant: StatusBadgeVariant::Warning, label: "Degraded".to_string() }
                                StatusBadge { variant: StatusBadgeVariant::Danger, label: "Error".to_string() }
                                StatusBadge { variant: StatusBadgeVariant::Neutral, label: "Offline".to_string() }
                            }
                            ShowcaseRow { label: "Status Dots",
                                StatusDot { color: StatusDotColor::Success }
                                StatusDot { color: StatusDotColor::Warning }
                                StatusDot { color: StatusDotColor::Danger }
                                StatusDot { color: StatusDotColor::Neutral }
                            }
                            ShowcaseRow { label: "Kbd",
                                Kbd { "Ctrl" }
                                Kbd { "K" }
                                span { class: "text-sm text-muted-foreground", "Command palette" }
                            }
                        }

                        // ── Cards ────────────────────────────────
                        ShowcaseSection { id: "cards", title: "Cards",
                            div { class: "grid grid-cols-2 gap-4",
                                Card {
                                    CardHeader {
                                        CardTitle { "Project Alpha" }
                                        CardDescription { "A sample project card." }
                                    }
                                    CardContent {
                                        p { class: "text-sm text-muted-foreground", "Card content with px-6 padding." }
                                    }
                                    CardFooter {
                                        Button { variant: ButtonVariant::Primary, size: ButtonSize::Small, "Action" }
                                        Button { variant: ButtonVariant::Ghost, size: ButtonSize::Small, "Cancel" }
                                    }
                                }
                                Card {
                                    CardHeader { CardTitle { "Statistics" } }
                                    CardContent {
                                        div { class: "flex flex-col gap-3",
                                            KeyValueRow { label: "Tasks".to_string(), value: "42".to_string() }
                                            KeyValueRow { label: "Completed".to_string(), value: "38".to_string(), bold: true }
                                            KeyValueRow { label: "Overdue".to_string(), value: "2".to_string() }
                                        }
                                    }
                                }
                            }
                        }

                        // ── Select ───────────────────────────────
                        ShowcaseSection { id: "select", title: "Select",
                            ShowcaseRow { label: "Basic",
                                Select {
                                    value: use_signal(String::new),
                                    placeholder: "Choose a fruit...".to_string(),
                                    SelectContent {
                                        SelectLabel { "Fruits" }
                                        SelectItem { value: "apple".to_string(), "Apple" }
                                        SelectItem { value: "banana".to_string(), "Banana" }
                                        SelectItem { value: "cherry".to_string(), "Cherry" }
                                        SelectSeparator {}
                                        SelectLabel { "Vegetables" }
                                        SelectItem { value: "carrot".to_string(), "Carrot" }
                                        SelectItem { value: "potato".to_string(), "Potato" }
                                    }
                                }
                            }
                        }

                        // ── Checkbox & Switch ────────────────────
                        ShowcaseSection { id: "checkbox-switch", title: "Checkbox & Switch",
                            ShowcaseRow { label: "Checkbox",
                                div { class: "flex items-center gap-2",
                                    crate::components::Checkbox { checked: use_signal(|| false) }
                                    Label { "Unchecked" }
                                }
                                div { class: "flex items-center gap-2",
                                    crate::components::Checkbox { checked: use_signal(|| true) }
                                    Label { "Checked" }
                                }
                                div { class: "flex items-center gap-2",
                                    crate::components::Checkbox { checked: use_signal(|| false), disabled: true }
                                    Label { "Disabled" }
                                }
                            }
                            ShowcaseRow { label: "Switch",
                                div { class: "flex items-center gap-2",
                                    crate::components::Switch { checked: use_signal(|| false) }
                                    Label { "Off" }
                                }
                                div { class: "flex items-center gap-2",
                                    crate::components::Switch { checked: use_signal(|| true) }
                                    Label { "On" }
                                }
                            }
                            ShowcaseRow { label: "Radio Group",
                                RadioGroup { value: use_signal(|| "a".to_string()),
                                    div { class: "flex items-center gap-2",
                                        RadioGroupItem { value: "a".to_string() }
                                        Label { "Option A" }
                                    }
                                    div { class: "flex items-center gap-2",
                                        RadioGroupItem { value: "b".to_string() }
                                        Label { "Option B" }
                                    }
                                    div { class: "flex items-center gap-2",
                                        RadioGroupItem { value: "c".to_string() }
                                        Label { "Option C" }
                                    }
                                }
                            }
                        }

                        // ── Progress ─────────────────────────────
                        ShowcaseSection { id: "progress", title: "Progress",
                            ShowcaseRow { label: "Variants",
                                div { class: "flex flex-col gap-3 w-full",
                                    Progress { value: 75.0 }
                                    Progress { value: 45.0, variant: ProgressVariant::Success }
                                    Progress { value: 30.0, variant: ProgressVariant::Warning }
                                    Progress { value: 90.0, variant: ProgressVariant::Destructive }
                                }
                            }
                            ShowcaseRow { label: "Skeleton",
                                div { class: "flex flex-col gap-3 w-64",
                                    SkeletonText {}
                                    SkeletonText {}
                                    div { class: "flex items-center gap-3",
                                        SkeletonCircle { class: "size-8".to_string() }
                                        div { class: "flex-1 flex flex-col gap-2",
                                            SkeletonText {}
                                            Skeleton { class: "h-3 w-2/3".to_string() }
                                        }
                                    }
                                }
                            }
                            ShowcaseRow { label: "Spinner",
                                Spinner { size: SpinnerSize::Small }
                                Spinner {}
                                Spinner { size: SpinnerSize::Large }
                            }
                        }

                        // ── Alerts ───────────────────────────────
                        ShowcaseSection { id: "alerts", title: "Alerts",
                            Alert {
                                AlertTitle { "Heads up!" }
                                AlertDescription { "You can add components using the CLI." }
                            }
                            Alert { variant: AlertVariant::Destructive,
                                AlertTitle { "Error" }
                                AlertDescription { "Your session has expired." }
                            }
                        }

                        // ── Tables ───────────────────────────────
                        ShowcaseSection { id: "tables", title: "Tables",
                            TableContainer {
                                Table {
                                    TableHeader {
                                        TableRow {
                                            TableHead { "Name" }
                                            TableHead { "Status" }
                                            TableHead { "Priority" }
                                            TableHead { "Due" }
                                        }
                                    }
                                    TableBody {
                                        TableRow {
                                            TableCell { "Fix auth bug" }
                                            TableCell { Badge { variant: BadgeVariant::Default, "In Progress" } }
                                            TableCell { "High" }
                                            TableCell { "Apr 11" }
                                        }
                                        TableRow {
                                            TableCell { "Design dashboard" }
                                            TableCell { Badge { variant: BadgeVariant::Secondary, "Open" } }
                                            TableCell { "Normal" }
                                            TableCell { "Apr 15" }
                                        }
                                    }
                                }
                            }
                        }

                        // ── Layout ───────────────────────────────
                        ShowcaseSection { id: "layout", title: "Layout",
                            ShowcaseRow { label: "ListRow",
                                div { class: "flex flex-col gap-2 w-full",
                                    ListRow {
                                        label: "Signal REAPER".to_string(),
                                        detail: "Running — PID 1234".to_string(),
                                        leading: rsx! { StatusDot { color: StatusDotColor::Success } },
                                        trailing: rsx! { Button { variant: ButtonVariant::Secondary, size: ButtonSize::Small, "Launch" } },
                                    }
                                    ListRow {
                                        label: "Audio Interface".to_string(),
                                        detail: "Disconnected".to_string(),
                                        tag: "USB".to_string(),
                                        leading: rsx! { StatusDot { color: StatusDotColor::Danger } },
                                    }
                                }
                            }
                            ShowcaseRow { label: "Divider",
                                div { class: "flex flex-col gap-3 w-full",
                                    p { class: "text-sm", "Above" }
                                    Divider {}
                                    p { class: "text-sm", "Below" }
                                }
                            }
                            ShowcaseRow { label: "EmptyState",
                                EmptyState { message: "No items to display".to_string() }
                            }
                        }

                        // ── Typography ───────────────────────────
                        ShowcaseSection { id: "typography", title: "Typography",
                            div { class: "flex flex-col gap-3",
                                Heading { level: HeadingLevel::H1, "Heading 1" }
                                Heading { level: HeadingLevel::H2, "Heading 2" }
                                Heading { level: HeadingLevel::H3, "Heading 3" }
                                Heading { level: HeadingLevel::H4, "Heading 4" }
                                Text { "Body text (default)" }
                                Text { variant: TextVariant::Small, "Small text" }
                                Text { variant: TextVariant::Muted, "Muted text" }
                                Text { variant: TextVariant::Code, "Code text" }
                            }
                        }

                        // ── Tooltips ─────────────────────────────
                        ShowcaseSection { id: "tooltips", title: "Tooltips",
                            ShowcaseRow { label: "Hover me",
                                Tooltip {
                                    TooltipTrigger { Button { variant: ButtonVariant::Outline, "Top" } }
                                    TooltipContent { side: TooltipSide::Top, "Tooltip on top" }
                                }
                                Tooltip {
                                    TooltipTrigger { Button { variant: ButtonVariant::Outline, "Bottom" } }
                                    TooltipContent { side: TooltipSide::Bottom, "Tooltip on bottom" }
                                }
                                Tooltip {
                                    TooltipTrigger { Button { variant: ButtonVariant::Outline, "Left" } }
                                    TooltipContent { side: TooltipSide::Left, "Tooltip on left" }
                                }
                                Tooltip {
                                    TooltipTrigger { Button { variant: ButtonVariant::Outline, "Right" } }
                                    TooltipContent { side: TooltipSide::Right, "Tooltip on right" }
                                }
                            }
                        }

                        // ── Accordion ────────────────────────────
                        ShowcaseSection { id: "accordion", title: "Accordion",
                            Accordion {
                                AccordionItem { index: 0,
                                    AccordionTrigger { "Is it accessible?" }
                                    AccordionContent { "Yes. It follows WAI-ARIA design patterns." }
                                }
                                AccordionItem { index: 1,
                                    AccordionTrigger { "Is it styled?" }
                                    AccordionContent { "Yes. It matches shadcn v4 maia styling." }
                                }
                                AccordionItem { index: 2,
                                    AccordionTrigger { "Is it animated?" }
                                    AccordionContent { "Content transitions are CSS-based." }
                                }
                            }
                        }

                        // ── Tabs ─────────────────────────────────
                        ShowcaseSection { id: "tabs", title: "Tabs",
                            Tabs { default_value: "account".to_string(),
                                TabList {
                                    TabTrigger { value: "account".to_string(), index: 0, "Account" }
                                    TabTrigger { value: "password".to_string(), index: 1, "Password" }
                                    TabTrigger { value: "settings".to_string(), index: 2, "Settings" }
                                }
                                TabContent { value: "account".to_string(), index: 0,
                                    Card {
                                        CardHeader {
                                            CardTitle { "Account" }
                                            CardDescription { "Make changes to your account." }
                                        }
                                        CardContent {
                                            p { class: "text-sm text-muted-foreground", "Account settings content." }
                                        }
                                    }
                                }
                                TabContent { value: "password".to_string(), index: 1,
                                    Card {
                                        CardHeader {
                                            CardTitle { "Password" }
                                            CardDescription { "Change your password." }
                                        }
                                        CardContent {
                                            p { class: "text-sm text-muted-foreground", "Password settings content." }
                                        }
                                    }
                                }
                                TabContent { value: "settings".to_string(), index: 2,
                                    Card {
                                        CardHeader {
                                            CardTitle { "Settings" }
                                            CardDescription { "Manage preferences." }
                                        }
                                        CardContent {
                                            p { class: "text-sm text-muted-foreground", "General settings content." }
                                        }
                                    }
                                }
                            }
                        }

                        // ── Icons (lucide-dioxus) ─────────────────
                        //
                        // Real-world smoke test for SVG rendering via Blitz:
                        // each lucide icon emits `<svg stroke="currentColor">`
                        // and relies on the CSS `color` cascade + Blitz's
                        // currentColor substitution to pick up the right hue.
                        ShowcaseSection { id: "icons", title: "Icons (Lucide)",
                            p { class: "text-sm text-muted-foreground mb-4",
                                "Icons from lucide-dioxus. Strokes use `currentColor`, so they inherit the CSS `color` of their container. This exercises Blitz's SVG attribute-level currentColor substitution."
                            }

                            ShowcaseRow { label: "Default (foreground)",
                                div { class: "flex items-center gap-4",
                                    lucide_dioxus::Check { size: 24 }
                                    lucide_dioxus::X { size: 24 }
                                    lucide_dioxus::Search { size: 24 }
                                    lucide_dioxus::House { size: 24 }
                                    lucide_dioxus::Settings { size: 24 }
                                    lucide_dioxus::Bell { size: 24 }
                                    lucide_dioxus::Heart { size: 24 }
                                    lucide_dioxus::Star { size: 24 }
                                    lucide_dioxus::ChevronRight { size: 24 }
                                    lucide_dioxus::ChevronDown { size: 24 }
                                }
                            }

                            ShowcaseRow { label: "Coloured via Tailwind `text-*`",
                                div { class: "flex items-center gap-4",
                                    span { class: "text-destructive",  lucide_dioxus::CircleAlert  { size: 24 } }
                                    span { class: "text-primary",      lucide_dioxus::Info          { size: 24 } }
                                    span { class: "text-chart-2",      lucide_dioxus::CircleCheck  { size: 24 } }
                                    span { class: "text-chart-4",      lucide_dioxus::TriangleAlert { size: 24 } }
                                    span { class: "text-muted-foreground", lucide_dioxus::Circle   { size: 24 } }
                                }
                            }

                            ShowcaseRow { label: "Inline within text",
                                p { class: "flex items-center gap-2 text-sm",
                                    lucide_dioxus::Lightbulb { size: 16 }
                                    "Inline icons should align with surrounding text."
                                }
                            }

                            ShowcaseRow { label: "Sizes",
                                div { class: "flex items-end gap-4",
                                    lucide_dioxus::Star { size: 12 }
                                    lucide_dioxus::Star { size: 16 }
                                    lucide_dioxus::Star { size: 24 }
                                    lucide_dioxus::Star { size: 32 }
                                    lucide_dioxus::Star { size: 48 }
                                }
                            }

                            ShowcaseRow { label: "Inside a button",
                                div { class: "flex items-center gap-3",
                                    Button { variant: ButtonVariant::Primary,
                                        lucide_dioxus::Download { size: 16 }
                                        "Download"
                                    }
                                    Button { variant: ButtonVariant::Outline,
                                        lucide_dioxus::Pencil { size: 16 }
                                        "Edit"
                                    }
                                    Button { variant: ButtonVariant::Destructive,
                                        lucide_dioxus::Trash { size: 16 }
                                        "Delete"
                                    }
                                }
                            }
                        }

                        // ── Spacing Test ─────────────────────────
                        ShowcaseSection { id: "spacing", title: "Spacing Verification",
                            p { class: "text-sm text-muted-foreground",
                                "Three methods compared side-by-side: Tailwind gap class, VStack/HStack (inline style), and flex+gap class on a column."
                            }

                            // Side-by-side for each gap value
                            for gap_val in ["1", "2", "3", "4", "6", "8"] {
                                {
                                    let gap_val = gap_val.to_string();
                                    let gap_class_h = format!("flex gap-{gap_val}");
                                    let gap_class_v = format!("flex flex-col gap-{gap_val}");
                                    rsx! {
                                        div { class: "flex flex-col gap-2 mb-6",
                                            span { class: "text-xs font-semibold uppercase tracking-wider text-muted-foreground",
                                                "gap-{gap_val}"
                                            }
                                            div { class: "grid grid-cols-3 gap-6",
                                                // Column 1: Tailwind gap class (horizontal)
                                                div { class: "flex flex-col gap-1",
                                                    span { class: "text-[10px] text-muted-foreground", "Tailwind class (horiz)" }
                                                    div { class: gap_class_h,
                                                        div { class: "w-8 h-8 rounded bg-primary" }
                                                        div { class: "w-8 h-8 rounded bg-primary" }
                                                        div { class: "w-8 h-8 rounded bg-primary" }
                                                    }
                                                }
                                                // Column 2: HStack (inline style)
                                                div { class: "flex flex-col gap-1",
                                                    span { class: "text-[10px] text-muted-foreground", "HStack (inline style)" }
                                                    HStack { gap: gap_val.clone(),
                                                        div { class: "w-8 h-8 rounded bg-chart-2" }
                                                        div { class: "w-8 h-8 rounded bg-chart-2" }
                                                        div { class: "w-8 h-8 rounded bg-chart-2" }
                                                    }
                                                }
                                                // Column 3: Tailwind gap class (vertical)
                                                div { class: "flex flex-col gap-1",
                                                    span { class: "text-[10px] text-muted-foreground", "Tailwind class (vert)" }
                                                    div { class: gap_class_v,
                                                        div { class: "w-8 h-8 rounded bg-destructive" }
                                                        div { class: "w-8 h-8 rounded bg-destructive" }
                                                        div { class: "w-8 h-8 rounded bg-destructive" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            // VStack vertical comparison
                            div { class: "grid grid-cols-2 gap-6",
                                div { class: "flex flex-col gap-1",
                                    span { class: "text-[10px] text-muted-foreground", "VStack gap=4 (inline style)" }
                                    VStack { gap: "4".to_string(),
                                        div { class: "w-full h-8 rounded bg-destructive" }
                                        div { class: "w-full h-8 rounded bg-destructive" }
                                        div { class: "w-full h-8 rounded bg-destructive" }
                                    }
                                }
                                div { class: "flex flex-col gap-1",
                                    span { class: "text-[10px] text-muted-foreground", "flex flex-col gap-4 (class)" }
                                    div { class: "flex flex-col gap-4",
                                        div { class: "w-full h-8 rounded bg-primary" }
                                        div { class: "w-full h-8 rounded bg-primary" }
                                        div { class: "w-full h-8 rounded bg-primary" }
                                    }
                                }
                            }
                        }

                        div { class: "h-16" }
                    }
                }
            }
        }
    }
}

// ── Showcase Helpers (internal) ──────────────────────────────────────────────

#[component]
fn ShowcaseSection(id: String, title: String, children: Element) -> Element {
    rsx! {
        section { id: id,
            div { class: "flex flex-col gap-6",
                div { class: "flex items-center gap-3",
                    h2 { class: "text-xl font-semibold tracking-tight", "{title}" }
                    Divider { class: "flex-1".to_string() }
                }
                {children}
            }
        }
    }
}

#[component]
fn ShowcaseRow(label: String, children: Element) -> Element {
    rsx! {
        div { class: "flex flex-col gap-3",
            span { class: "text-xs font-medium uppercase tracking-wider text-muted-foreground", "{label}" }
            div { class: "flex items-start gap-3 flex-wrap",
                {children}
            }
        }
    }
}
