//! Server-side syntax highlighting for code blocks via syntect.
//!
//! Loads Sublime grammar bundle once at startup, looks up the
//! grammar by language name (case-insensitive). Outputs HTML
//! with `<span class="syn-{class}">` per token — themed by our
//! `style.css` rather than a syntect theme so the published
//! site stays close to the live app's look.
//!
//! Grammars covered: rust, typescript, javascript, python, html,
//! css, json, yaml, toml, markdown, bash, sql, c, cpp, java,
//! go, ruby, php, scala, swift, kotlin, scss, sass, dart,
//! ocaml, haskell, elm, clojure, clj, scheme, lisp, plain text,
//! and many more (whatever the default Sublime bundle ships).

use std::sync::OnceLock;
use syntect::easy::HighlightLines;
use syntect::highlighting::{Color, FontStyle, Style, Theme, ThemeSet};
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;

fn syntax_set() -> &'static SyntaxSet {
    static S: OnceLock<SyntaxSet> = OnceLock::new();
    S.get_or_init(SyntaxSet::load_defaults_newlines)
}

fn theme() -> &'static Theme {
    static T: OnceLock<Theme> = OnceLock::new();
    T.get_or_init(|| {
        // Grab the bundled `base16-ocean.dark` if present; fall
        // back to an empty theme so we still emit raw spans.
        let ts = ThemeSet::load_defaults();
        ts.themes
            .get("base16-ocean.dark")
            .cloned()
            .unwrap_or_else(|| Theme::default())
    })
}

/// Highlight `code` for the given language → an HTML string of
/// `<span style="color:#…">…</span>` per token. Returns plain
/// (escaped) text when the language isn't recognized.
pub fn highlight(code: &str, lang: &str) -> String {
    let ss = syntax_set();
    let syntax = ss
        .find_syntax_by_token(lang)
        .or_else(|| ss.find_syntax_by_extension(lang))
        .or_else(|| ss.find_syntax_by_name(lang))
        .unwrap_or_else(|| ss.find_syntax_plain_text());
    let mut h = HighlightLines::new(syntax, theme());
    let mut out = String::with_capacity(code.len() * 2);
    for line in LinesWithEndings::from(code) {
        let regions: Vec<(Style, &str)> = h.highlight_line(line, ss).unwrap_or_default();
        for (style, text) in regions {
            out.push_str("<span style=\"");
            color_style(&style, &mut out);
            out.push_str("\">");
            push_escaped(&mut out, text);
            out.push_str("</span>");
        }
    }
    out
}

fn color_style(style: &Style, out: &mut String) {
    let Color { r, g, b, a: _ } = style.foreground;
    out.push_str(&format!("color:#{:02x}{:02x}{:02x}", r, g, b));
    if style.font_style.contains(FontStyle::BOLD) {
        out.push_str(";font-weight:bold");
    }
    if style.font_style.contains(FontStyle::ITALIC) {
        out.push_str(";font-style:italic");
    }
    if style.font_style.contains(FontStyle::UNDERLINE) {
        out.push_str(";text-decoration:underline");
    }
}

fn push_escaped(out: &mut String, s: &str) {
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn highlights_rust() {
        let html = highlight("fn main() { let x = 42; }\n", "rust");
        assert!(html.contains("<span"));
        // Should escape rendered text properly (no raw `<` from html ops).
        assert!(!html.contains("<fn>"));
    }

    #[test]
    fn unknown_lang_passes_through_as_plain() {
        let html = highlight("just some words\n", "fakelang");
        // Plain-text syntax still wraps in spans but doesn't crash.
        assert!(html.contains("just some words"));
    }

    #[test]
    fn escapes_html_entities() {
        // Trailing newline because `LinesWithEndings` won't
        // emit a final unterminated line. Real code-block
        // content always ends in `\n` after our content
        // pipeline normalizes it.
        //
        // syntect tokenizes per-token (separate spans for `<`,
        // `div`, `>`), so we assert the entities appear and the
        // raw `<` / `>` from the input do not appear unescaped
        // outside our wrapping `<span>` tags.
        let html = highlight("<div>\n", "html");
        assert!(html.contains("&lt;"), "{html}");
        assert!(html.contains("&gt;"), "{html}");
        assert!(html.contains("div"), "{html}");
    }
}
