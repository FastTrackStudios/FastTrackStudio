//! Invoice Ninja-style invoicing UI — composable submodules.

pub mod bridge;
pub mod client_picker;
pub mod common;
pub mod dashboard;
pub mod editor;
pub mod list;
pub mod payment;
pub mod preview;
pub mod recurring;
pub mod status;

pub use bridge::{
    BillingProposal, ProposedLine, TimeEntriesToInvoiceDialog, TimeEntriesToInvoiceDialogProps,
    propose_lines_from_entries,
};
pub use client_picker::{
    ClientCreateDialog, ClientCreateDialogProps, ClientPicker, ClientPickerProps,
};
pub use common::{
    InvoiceTotals, compute_totals, format_money, format_qty_thousandths, invoice_status_variant,
    parse_money_input,
};
pub use dashboard::{InvoiceNinjaDashboard, InvoiceNinjaDashboardProps};
pub use editor::{InvoiceDraft, InvoiceEditor, InvoiceEditorProps};
pub use list::{InvoiceListFilter, InvoiceListView, InvoiceListViewProps, apply_invoice_filter};
pub use payment::{PaymentRecordDialog, PaymentRecordDialogProps};
pub use preview::{InvoicePreview, InvoicePreviewProps};
pub use recurring::{
    RecurringInvoiceDraft, RecurringInvoiceEditor, RecurringInvoiceEditorProps,
    RecurringInvoiceList, RecurringInvoiceListProps,
};
pub use status::{InvoiceStatusPipeline, InvoiceStatusPipelineProps};
