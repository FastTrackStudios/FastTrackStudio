//! Convenience re-exports — `use fts_ui::prelude::*`.

// Components
pub use crate::components::{
    // Wrapped
    Button, ButtonSize, ButtonVariant,
    Input, InputSize, InputVariant,
    // Migrated
    Dialog, DialogClose, DialogDescription, DialogFooter, DialogHeader, DialogTitle,
    Tabs, TabList, TabTrigger, TabContent,
    // New
    Badge, BadgeVariant,
    Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle,
    EmptyState,
    FormField,
    InlineEdit,
    KeyValueRow,
    ListRow,
    SearchableDropdown,
    SearchableList, SearchableListItem,
    SectionHeader, SectionHeaderSize,
    SegmentedControl, SegmentedControlSize,
    StatusBadge, StatusBadgeVariant,
    StatusDot, StatusDotColor, StatusDotSize,
};

// Re-exported component modules (used via fts_ui::prelude::checkbox::Checkbox etc.)
pub use crate::components::checkbox;
pub use crate::components::context_menu;
pub use crate::components::dropdown;
pub use crate::components::label;
pub use crate::components::progress;
pub use crate::components::side_sheet;
pub use crate::components::switch;
pub use crate::components::toast;

// Layout
pub use crate::layout::{Divider, DividerOrientation, HStack, Spacer, VStack};

// Typography
pub use crate::typography::{Heading, HeadingLevel, Text, TextVariant};

// Theme tokens
pub use crate::theme::tokens;
