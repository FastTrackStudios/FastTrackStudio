//! Table — shadcn v4 maia style.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct TableContainerProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-container
#[component]
pub fn TableContainer(props: TableContainerProps) -> Element {
    rsx! {
        div {
            class: format!("relative w-full overflow-x-auto {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table
#[component]
pub fn Table(props: TableProps) -> Element {
    rsx! {
        table {
            class: format!("w-full caption-bottom text-sm {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableHeaderProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-header
#[component]
pub fn TableHeader(props: TableHeaderProps) -> Element {
    rsx! {
        thead {
            class: format!("[&_tr]:border-b {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableBodyProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-body
#[component]
pub fn TableBody(props: TableBodyProps) -> Element {
    rsx! {
        tbody {
            class: format!("[&_tr:last-child]:border-0 {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableFooterProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-footer
#[component]
pub fn TableFooter(props: TableFooterProps) -> Element {
    rsx! {
        tfoot {
            class: format!(
                "bg-muted/50 border-t font-medium [&>tr]:last:border-b-0 {}",
                props.class
            ),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableRowProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-row
#[component]
pub fn TableRow(props: TableRowProps) -> Element {
    rsx! {
        tr {
            class: format!(
                "hover:bg-muted/50 data-[state=selected]:bg-muted border-b transition-colors {}",
                props.class
            ),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableHeadProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-head
#[component]
pub fn TableHead(props: TableHeadProps) -> Element {
    rsx! {
        th {
            class: format!(
                "text-foreground h-12 px-3 text-left align-middle font-medium whitespace-nowrap {}",
                props.class
            ),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableCellProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-cell
#[component]
pub fn TableCell(props: TableCellProps) -> Element {
    rsx! {
        td {
            class: format!("p-3 align-middle whitespace-nowrap {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TableCaptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-table-caption
#[component]
pub fn TableCaption(props: TableCaptionProps) -> Element {
    rsx! {
        caption {
            class: format!("text-muted-foreground mt-4 text-sm {}", props.class),
            {props.children}
        }
    }
}
