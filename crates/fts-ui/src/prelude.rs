//! Convenience re-exports — `use fts_ui::prelude::*`.

// Components — core
pub use crate::components::{
    Button, ButtonRenderAs, ButtonSize, ButtonVariant,
    Input, InputSize, InputVariant,
    Dialog, DialogClose, DialogDescription, DialogFooter, DialogHeader, DialogTitle,
    Tabs, TabList, TabTrigger, TabContent,
    Badge, BadgeVariant,
    Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle,
    Checkbox,
    Collapsible, CollapsibleContent, CollapsibleTrigger,
    ContextMenu, ContextMenuContent, ContextMenuItem, ContextMenuLabel, ContextMenuSeparator, ContextMenuTrigger,
    Dropdown, DropdownContent, DropdownItem, DropdownLabel, DropdownSeparator, DropdownTrigger,
    EmptyState,
    FormField,
    HoverCard, HoverCardContent, HoverCardTrigger,
    InlineEdit,
    KeyValueRow,
    Label,
    ListRow,
    Progress, ProgressVariant,
    SearchableDropdown,
    SearchableList, SearchableListItem,
    SectionHeader, SectionHeaderSize,
    SegmentedControl, SegmentedControlSize,
    Sheet, SheetDescription, SheetFooter, SheetHeader, SheetSide, SheetTitle,
    StatusBadge, StatusBadgeVariant,
    StatusDot, StatusDotColor, StatusDotSize,
    Switch,
    NavTab,
    ProgressBar, ProgressBarOrientation,
    Accordion, AccordionContent, AccordionItem, AccordionTrigger,
};

// Components — shadcn v4 parity
pub use crate::components::{
    Alert, AlertDescription, AlertTitle, AlertVariant,
    AlertDialog, AlertDialogAction, AlertDialogCancel, AlertDialogDescription,
    AlertDialogFooter, AlertDialogHeader, AlertDialogTitle,
    Breadcrumb, BreadcrumbItem, BreadcrumbLink, BreadcrumbList, BreadcrumbPage, BreadcrumbSeparator,
    Drawer, DrawerDescription, DrawerFooter, DrawerHandle, DrawerHeader, DrawerSide, DrawerTitle,
    Field, FieldDescription, FieldLabel, FieldMessage,
    Kbd,
    Popover, PopoverContent, PopoverTrigger,
    RadioGroup, RadioGroupItem,
    ScrollArea,
    Select, SelectContent, SelectItem, SelectLabel, SelectSeparator, SelectTrigger,
    Skeleton, SkeletonCircle, SkeletonText,
    Slider,
    Spinner, SpinnerSize,
    Table, TableBody, TableCaption, TableCell, TableContainer, TableFooter, TableHead, TableHeader, TableRow,
    Textarea,
    Toggle, ToggleSize, ToggleVariant,
    ToggleGroup, ToggleGroupItem,
    Tooltip, TooltipContent, TooltipSide, TooltipTrigger,
    Avatar, AvatarFallback, AvatarImage, AvatarSize,
    InputGroup, InputGroupAddon, InputGroupAlign, InputGroupControl,
    Pagination, PaginationContent, PaginationEllipsis, PaginationItem, PaginationLink, PaginationNext, PaginationPrevious,
    Menubar, MenubarMenu, MenubarTrigger, MenubarContent, MenubarItem, MenubarSeparator, MenubarLabel, MenubarShortcut,
    CommandDialog, CommandInput, CommandList, CommandGroup, CommandItem, CommandEmpty, CommandSeparator, CommandShortcut,
    Combobox, ComboboxTrigger, ComboboxContent, ComboboxItem, ComboboxEmpty,
    AspectRatio,
    NavigationMenu, NavigationMenuList, NavigationMenuItem, NavigationMenuTrigger, NavigationMenuContent, NavigationMenuLink,
    Calendar,
    ResizablePanelGroup, ResizablePanel, ResizableHandle, ResizeDirection, ResizableContext,
    Sidebar, SidebarCollapsible, SidebarContent, SidebarContext, SidebarFooter, SidebarGroup,
    SidebarGroupContent, SidebarGroupLabel, SidebarHeader, SidebarInset, SidebarMenu,
    SidebarMenuBadge, SidebarMenuButton, SidebarMenuButtonSize, SidebarMenuButtonVariant,
    SidebarMenuItem, SidebarProvider, SidebarSeparator, SidebarSide, SidebarTrigger, SidebarVariant,
    Carousel, CarouselContent, CarouselItem, CarouselNext, CarouselPrevious,
    InputOtp, InputOtpGroup, InputOtpSeparator, InputOtpSlot,
};

// Toast (used via toast::use_toast(), toast::ToastProvider, etc.)
pub use crate::components::toast;

// Layout
pub use crate::layout::{Divider, DividerOrientation, HStack, Spacer, VStack};

// Typography
pub use crate::typography::{Heading, HeadingLevel, Text, TextVariant};

// Theme tokens
pub use crate::theme::tokens;

// Class merging utilities. The primary component API is the `fts_ui::cn!`
// macro; these helpers cover already-joined or pre-sliced class strings.
pub use crate::cn::{merge, merge_slice};
