//! Native PDF rendering for an `Invoice`.
//!
//! **Platform split.** printpdf 0.7 transitively depends on a `time`
//! crate version that fails to compile for wasm32 (deranged / ranged
//! integer From-impl conflicts). For the web build the UI falls back
//! to a `window.print()` flow off the `InvoicePreview` component;
//! native (desktop / server) hits this module for a real PDF byte
//! stream.
//!
//! - Web:    print-via-browser.
//! - Native: real PDF via `printpdf`.

use chrono::{DateTime, Utc};
use invoice_proto::{Client, Invoice, InvoiceLine};
use printpdf::{BuiltinFont, IndirectFontRef, Mm, PdfDocument, PdfDocumentReference};

const PAGE_W_MM: f32 = 210.0; // A4
const PAGE_H_MM: f32 = 297.0;
const MARGIN_MM: f32 = 25.4; // ~72pt

/// Render an A4 invoice to a PDF byte stream. Simple but presentable:
/// header, bill-to block, dates, line-items table, totals, notes.
pub fn render_invoice_pdf(invoice: &Invoice, lines: &[InvoiceLine], client: &Client) -> Vec<u8> {
    let (doc, page1, layer1) = PdfDocument::new(
        format!("Invoice {}", invoice.number),
        Mm(PAGE_W_MM),
        Mm(PAGE_H_MM),
        "Layer 1",
    );
    let font_regular = doc
        .add_builtin_font(BuiltinFont::Helvetica)
        .expect("builtin font");
    let font_bold = doc
        .add_builtin_font(BuiltinFont::HelveticaBold)
        .expect("builtin font");

    let layer = doc.get_page(page1).get_layer(layer1);

    let from_name =
        std::env::var("INVOICE_FROM_NAME").unwrap_or_else(|_| "FastTrack Studio".into());

    // ── Header ───────────────────────────────────────────────────────
    let y_top = PAGE_H_MM - MARGIN_MM;
    layer.use_text(&from_name, 14.0, Mm(MARGIN_MM), Mm(y_top), &font_bold);

    layer.use_text(
        "INVOICE",
        24.0,
        Mm(PAGE_W_MM - MARGIN_MM - 50.0),
        Mm(y_top),
        &font_bold,
    );
    layer.use_text(
        format!("#{}", invoice.number),
        10.0,
        Mm(PAGE_W_MM - MARGIN_MM - 50.0),
        Mm(y_top - 8.0),
        &font_regular,
    );

    // ── Bill-to + Dates ──────────────────────────────────────────────
    let bill_top = y_top - 25.0;
    layer.use_text("BILL TO", 8.0, Mm(MARGIN_MM), Mm(bill_top), &font_bold);
    layer.use_text(
        &client.name,
        11.0,
        Mm(MARGIN_MM),
        Mm(bill_top - 5.0),
        &font_regular,
    );
    let mut yy = bill_top - 10.0;
    for line in [
        client.billing_address_line1.as_deref(),
        client.billing_address_line2.as_deref(),
        client.billing_city.as_deref(),
        client.email.as_deref(),
    ]
    .iter()
    .flatten()
    {
        layer.use_text(*line, 9.0, Mm(MARGIN_MM), Mm(yy), &font_regular);
        yy -= 4.5;
    }

    let date_x = PAGE_W_MM - MARGIN_MM - 60.0;
    layer.use_text("ISSUED", 8.0, Mm(date_x), Mm(bill_top), &font_bold);
    layer.use_text(
        fmt_date(invoice.issue_date),
        9.0,
        Mm(date_x),
        Mm(bill_top - 4.5),
        &font_regular,
    );
    if let Some(due) = invoice.due_date {
        layer.use_text("DUE", 8.0, Mm(date_x), Mm(bill_top - 12.0), &font_bold);
        layer.use_text(
            fmt_date(due),
            9.0,
            Mm(date_x),
            Mm(bill_top - 16.5),
            &font_regular,
        );
    }

    // ── Line items ───────────────────────────────────────────────────
    let table_top = bill_top - 40.0;
    layer.use_text("DESCRIPTION", 8.0, Mm(MARGIN_MM), Mm(table_top), &font_bold);
    layer.use_text(
        "QTY",
        8.0,
        Mm(PAGE_W_MM - MARGIN_MM - 60.0),
        Mm(table_top),
        &font_bold,
    );
    layer.use_text(
        "PRICE",
        8.0,
        Mm(PAGE_W_MM - MARGIN_MM - 40.0),
        Mm(table_top),
        &font_bold,
    );
    layer.use_text(
        "AMOUNT",
        8.0,
        Mm(PAGE_W_MM - MARGIN_MM - 18.0),
        Mm(table_top),
        &font_bold,
    );

    let mut y = table_top - 6.0;
    let row_h = 5.5;
    let mut subtotal: i64 = 0;
    for line in lines {
        if y < MARGIN_MM + 60.0 {
            // FUTURE: real pagination. v1 truncates with an indicator.
            layer.use_text(
                "(additional lines omitted — see screen)",
                8.0,
                Mm(MARGIN_MM),
                Mm(y),
                &font_regular,
            );
            break;
        }
        layer.use_text(
            truncate(&line.description, 60),
            9.0,
            Mm(MARGIN_MM),
            Mm(y),
            &font_regular,
        );
        layer.use_text(
            fmt_qty(line.quantity_thousandths),
            9.0,
            Mm(PAGE_W_MM - MARGIN_MM - 60.0),
            Mm(y),
            &font_regular,
        );
        layer.use_text(
            fmt_money(line.unit_price_cents, &invoice.currency),
            9.0,
            Mm(PAGE_W_MM - MARGIN_MM - 40.0),
            Mm(y),
            &font_regular,
        );
        layer.use_text(
            fmt_money(line.amount_cents, &invoice.currency),
            9.0,
            Mm(PAGE_W_MM - MARGIN_MM - 18.0),
            Mm(y),
            &font_regular,
        );
        subtotal += line.amount_cents;
        y -= row_h;
    }

    // ── Totals ───────────────────────────────────────────────────────
    let totals_x = PAGE_W_MM - MARGIN_MM - 60.0;
    let total_label_x = PAGE_W_MM - MARGIN_MM - 18.0;
    y -= 8.0;
    draw_total_row(
        &layer,
        &font_regular,
        "Subtotal",
        subtotal,
        invoice,
        totals_x,
        total_label_x,
        y,
    );
    y -= row_h;
    if invoice.discount_cents != 0 {
        draw_total_row(
            &layer,
            &font_regular,
            "Discount",
            -invoice.discount_cents,
            invoice,
            totals_x,
            total_label_x,
            y,
        );
        y -= row_h;
    }
    if invoice.tax_rate_bps > 0 {
        draw_total_row(
            &layer,
            &font_regular,
            "Tax",
            invoice.tax_cents,
            invoice,
            totals_x,
            total_label_x,
            y,
        );
        y -= row_h;
    }
    draw_total_row_bold(
        &layer,
        &font_bold,
        "Total",
        invoice.total_cents,
        invoice,
        totals_x,
        total_label_x,
        y,
    );
    y -= row_h;
    draw_total_row(
        &layer,
        &font_regular,
        "Balance due",
        invoice.balance_cents,
        invoice,
        totals_x,
        total_label_x,
        y,
    );

    // ── Notes ────────────────────────────────────────────────────────
    if let Some(notes) = invoice.notes.as_deref() {
        layer.use_text(
            "NOTES",
            8.0,
            Mm(MARGIN_MM),
            Mm(MARGIN_MM + 20.0),
            &font_bold,
        );
        for (i, chunk) in wrap(notes, 90).iter().take(6).enumerate() {
            layer.use_text(
                chunk,
                9.0,
                Mm(MARGIN_MM),
                Mm(MARGIN_MM + 15.0 - i as f32 * 4.5),
                &font_regular,
            );
        }
    }

    finalize(doc)
}

fn draw_total_row(
    layer: &printpdf::PdfLayerReference,
    font: &IndirectFontRef,
    label: &str,
    amount: i64,
    inv: &Invoice,
    label_x: f32,
    amount_x: f32,
    y: f32,
) {
    layer.use_text(label, 9.0, Mm(label_x), Mm(y), font);
    layer.use_text(
        fmt_money(amount, &inv.currency),
        9.0,
        Mm(amount_x),
        Mm(y),
        font,
    );
}

fn draw_total_row_bold(
    layer: &printpdf::PdfLayerReference,
    font: &IndirectFontRef,
    label: &str,
    amount: i64,
    inv: &Invoice,
    label_x: f32,
    amount_x: f32,
    y: f32,
) {
    layer.use_text(label, 10.0, Mm(label_x), Mm(y), font);
    layer.use_text(
        fmt_money(amount, &inv.currency),
        10.0,
        Mm(amount_x),
        Mm(y),
        font,
    );
}

fn finalize(doc: PdfDocumentReference) -> Vec<u8> {
    let mut buf = Vec::new();
    {
        let mut writer = std::io::BufWriter::new(&mut buf);
        doc.save(&mut writer).expect("pdf save");
    }
    buf
}

fn fmt_money(cents: i64, currency: &str) -> String {
    let sign = if cents < 0 { "-" } else { "" };
    let abs = cents.unsigned_abs() as i128;
    format!("{} {}{:.2}", currency, sign, abs as f32 / 100.0)
}

fn fmt_qty(thou: i64) -> String {
    if thou % 1000 == 0 {
        (thou / 1000).to_string()
    } else {
        format!("{:.3}", thou as f32 / 1000.0)
    }
}

fn fmt_date(d: DateTime<Utc>) -> String {
    d.format("%b %-d, %Y").to_string()
}

fn truncate(s: &str, n: usize) -> String {
    if s.chars().count() <= n {
        s.to_string()
    } else {
        let mut out: String = s.chars().take(n.saturating_sub(1)).collect();
        out.push('…');
        out
    }
}

fn wrap(s: &str, n: usize) -> Vec<String> {
    let mut out = Vec::new();
    let mut cur = String::new();
    for word in s.split_whitespace() {
        if cur.len() + 1 + word.len() > n {
            out.push(std::mem::take(&mut cur));
        }
        if !cur.is_empty() {
            cur.push(' ');
        }
        cur.push_str(word);
    }
    if !cur.is_empty() {
        out.push(cur);
    }
    out
}
