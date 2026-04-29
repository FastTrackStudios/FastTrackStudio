//! Convenience re-exports — `use fts_ui::prelude::*`.

// Components — core
pub use crate::components::{
    Accordion, AccordionContent, AccordionItem, AccordionTrigger, Badge, BadgeVariant, Button,
    ButtonRenderAs, ButtonSize, ButtonVariant, Card, CardContent, CardDescription, CardFooter,
    CardHeader, CardTitle, Checkbox, Collapsible, CollapsibleContent, CollapsibleTrigger,
    ContextMenu, ContextMenuContent, ContextMenuItem, ContextMenuLabel, ContextMenuSeparator,
    ContextMenuTrigger, Dialog, DialogClose, DialogDescription, DialogFooter, DialogHeader,
    DialogTitle, Dropdown, DropdownContent, DropdownItem, DropdownLabel, DropdownSeparator,
    DropdownTrigger, EmptyState, FormField, HoverCard, HoverCardContent, HoverCardTrigger,
    InlineEdit, Input, InputSize, InputVariant, KeyValueRow, Label, ListRow, NavTab, Progress,
    ProgressBar, ProgressBarOrientation, ProgressVariant, SearchableDropdown, SearchableList,
    SearchableListItem, SectionHeader, SectionHeaderSize, SegmentedControl, SegmentedControlSize,
    Sheet, SheetDescription, SheetFooter, SheetHeader, SheetSide, SheetTitle, StatusBadge,
    StatusBadgeVariant, StatusDot, StatusDotColor, StatusDotSize, Switch, TabContent, TabList,
    TabTrigger, Tabs,
};

// Components — shadcn v4 parity
pub use crate::components::{
    Alert, AlertDescription, AlertDialog, AlertDialogAction, AlertDialogCancel,
    AlertDialogDescription, AlertDialogFooter, AlertDialogHeader, AlertDialogTitle, AlertTitle,
    AlertVariant, AspectRatio, Avatar, AvatarFallback, AvatarImage, AvatarSize, Breadcrumb,
    BreadcrumbItem, BreadcrumbLink, BreadcrumbList, BreadcrumbPage, BreadcrumbSeparator, Calendar,
    Carousel, CarouselContent, CarouselItem, CarouselNext, CarouselPrevious, Combobox,
    ComboboxContent, ComboboxEmpty, ComboboxItem, ComboboxTrigger, CommandDialog, CommandEmpty,
    CommandGroup, CommandInput, CommandItem, CommandList, CommandSeparator, CommandShortcut,
    Drawer, DrawerDescription, DrawerFooter, DrawerHandle, DrawerHeader, DrawerSide, DrawerTitle,
    Field, FieldDescription, FieldLabel, FieldMessage, InputGroup, InputGroupAddon,
    InputGroupAlign, InputGroupControl, InputOtp, InputOtpGroup, InputOtpSeparator, InputOtpSlot,
    Kbd, Menubar, MenubarContent, MenubarItem, MenubarLabel, MenubarMenu, MenubarSeparator,
    MenubarShortcut, MenubarTrigger, NavigationMenu, NavigationMenuContent, NavigationMenuItem,
    NavigationMenuLink, NavigationMenuList, NavigationMenuTrigger, Pagination, PaginationContent,
    PaginationEllipsis, PaginationItem, PaginationLink, PaginationNext, PaginationPrevious,
    Popover, PopoverContent, PopoverTrigger, RadioGroup, RadioGroupItem, ResizableContext,
    ResizableHandle, ResizablePanel, ResizablePanelGroup, ResizeDirection, ScrollArea, Select,
    SelectContent, SelectItem, SelectLabel, SelectSeparator, SelectTrigger, Sidebar,
    SidebarCollapsible, SidebarContent, SidebarContext, SidebarFooter, SidebarGroup,
    SidebarGroupContent, SidebarGroupLabel, SidebarHeader, SidebarInset, SidebarMenu,
    SidebarMenuBadge, SidebarMenuButton, SidebarMenuButtonSize, SidebarMenuButtonVariant,
    SidebarMenuItem, SidebarProvider, SidebarSeparator, SidebarSide, SidebarTrigger,
    SidebarVariant, Skeleton, SkeletonCircle, SkeletonText, Slider, Spinner, SpinnerSize, Table,
    TableBody, TableCaption, TableCell, TableContainer, TableFooter, TableHead, TableHeader,
    TableRow, Textarea, Toggle, ToggleGroup, ToggleGroupItem, ToggleSize, ToggleVariant, Tooltip,
    TooltipContent, TooltipSide, TooltipTrigger,
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
