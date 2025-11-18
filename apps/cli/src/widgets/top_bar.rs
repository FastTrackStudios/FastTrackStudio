use ratatui::{
    buffer::Buffer,
    layout::Rect,
    text::Line,
    widgets::{Block, Paragraph, Widget},
};

pub struct TopBar;

impl Widget for TopBar {
    fn render(self, area: Rect, buf: &mut Buffer) {
        let width = area.width as usize;
        let spaces = " ".repeat(width.saturating_sub(2));
        let top_bar = Line::from(vec![
            ratatui::text::Span::raw("🔒"),
            ratatui::text::Span::raw(spaces),
            ratatui::text::Span::raw("✕"),
        ]);
        let widget = Paragraph::new(top_bar).block(Block::default());
        widget.render(area, buf);
    }
}

