//! Sitemap.xml + RSS feed emission.
//!
//! Sitemap is for search-engine discoverability; RSS is for
//! readers (NetNewsWire, Inoreader, etc.) — useful when the
//! published vault is a journal-style site.
//!
//! Both formats are tiny + standardized; we hand-write them
//! rather than pull a templating dep.

use crate::site::slugify;
use chrono::{DateTime, Utc};
use knowledge_proto::{Block, Page};
use std::collections::HashMap;
use uuid::Uuid;

/// Emit a sitemap.xml string for the given pages.
/// `base_url` should be the site's absolute origin (e.g.
/// `https://docs.example.com`) — without trailing slash.
pub fn sitemap(base_url: &str, pages: &[Page]) -> String {
    let mut out = String::with_capacity(pages.len() * 200 + 256);
    out.push_str(r#"<?xml version="1.0" encoding="UTF-8"?>"#);
    out.push('\n');
    out.push_str(r#"<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">"#);
    out.push('\n');
    let base = base_url.trim_end_matches('/');
    // Landing page.
    out.push_str("  <url><loc>");
    out.push_str(base);
    out.push_str("/</loc></url>\n");
    for p in pages {
        out.push_str("  <url><loc>");
        out.push_str(base);
        out.push('/');
        out.push_str(&slugify(&p.basename));
        out.push_str("/</loc>");
        out.push_str(&format!(
            "<lastmod>{}</lastmod>",
            p.updated_at.format("%Y-%m-%d")
        ));
        out.push_str("</url>\n");
    }
    out.push_str("</urlset>\n");
    out
}

/// Emit an RSS 2.0 feed of the most-recently-modified pages.
/// Includes a brief snippet per item from the page's first
/// few blocks. Bounded to `limit` items so the feed stays a
/// reasonable size.
pub fn rss(
    base_url: &str,
    site_title: &str,
    pages: &[Page],
    blocks_by_page: &HashMap<Uuid, Vec<Block>>,
    limit: usize,
) -> String {
    let base = base_url.trim_end_matches('/');
    let mut sorted: Vec<&Page> = pages.iter().collect();
    sorted.sort_by(|a, b| b.updated_at.cmp(&a.updated_at));
    let now: DateTime<Utc> = Utc::now();
    let mut out = String::with_capacity(2048 + sorted.len() * 400);
    out.push_str(r#"<?xml version="1.0" encoding="UTF-8"?>"#);
    out.push('\n');
    out.push_str(r#"<rss version="2.0"><channel>"#);
    out.push('\n');
    out.push_str(&format!("  <title>{}</title>\n", xml_escape(site_title)));
    out.push_str(&format!("  <link>{}/</link>\n", base));
    out.push_str(&format!(
        "  <description>{}</description>\n",
        xml_escape(site_title)
    ));
    out.push_str(&format!(
        "  <lastBuildDate>{}</lastBuildDate>\n",
        now.to_rfc2822()
    ));
    for p in sorted.into_iter().take(limit) {
        let slug = slugify(&p.basename);
        let snippet = first_blocks_snippet(p.id, blocks_by_page, 240);
        out.push_str("  <item>\n");
        out.push_str(&format!("    <title>{}</title>\n", xml_escape(&p.basename)));
        out.push_str(&format!("    <link>{}/{}/</link>\n", base, slug));
        out.push_str(&format!(
            "    <guid isPermaLink=\"true\">{}/{}/</guid>\n",
            base, slug
        ));
        out.push_str(&format!(
            "    <pubDate>{}</pubDate>\n",
            p.updated_at.to_rfc2822()
        ));
        out.push_str(&format!(
            "    <description>{}</description>\n",
            xml_escape(&snippet)
        ));
        out.push_str("  </item>\n");
    }
    out.push_str("</channel></rss>\n");
    out
}

fn first_blocks_snippet(
    page_id: Uuid,
    blocks_by_page: &HashMap<Uuid, Vec<Block>>,
    max_chars: usize,
) -> String {
    let Some(blocks) = blocks_by_page.get(&page_id) else {
        return String::new();
    };
    let mut sorted = blocks.clone();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let mut joined = String::new();
    for b in sorted {
        if !joined.is_empty() {
            joined.push(' ');
        }
        joined.push_str(&b.content);
        if joined.len() > max_chars * 2 {
            break;
        }
    }
    let collapsed: String = joined.split_whitespace().collect::<Vec<_>>().join(" ");
    if collapsed.chars().count() > max_chars {
        let mut s: String = collapsed.chars().take(max_chars).collect();
        s.push('…');
        s
    } else {
        collapsed
    }
}

fn xml_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&apos;"),
            _ => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn page(name: &str) -> Page {
        Page {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            folder_id: None,
            path: format!("{}.md", name),
            basename: name.into(),
            ext: "md".into(),
            aliases: Vec::new(),
            frontmatter_json: "{}".into(),
            stat_ctime: Utc::now(),
            stat_mtime: Utc::now(),
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    #[test]
    fn sitemap_includes_root_and_pages() {
        let pages = vec![page("Hello"), page("World")];
        let xml = sitemap("https://example.com", &pages);
        assert!(xml.contains("<loc>https://example.com/</loc>"));
        assert!(xml.contains("<loc>https://example.com/hello/</loc>"));
    }

    #[test]
    fn sitemap_strips_trailing_slash() {
        let xml = sitemap("https://x/", &[]);
        assert!(xml.contains("<loc>https://x/</loc>"));
        assert!(!xml.contains("<loc>https://x//"));
    }

    #[test]
    fn rss_orders_by_updated_at_desc() {
        let mut a = page("Older");
        a.updated_at = Utc::now() - chrono::Duration::days(2);
        let b = page("Newer");
        let xml = rss("https://x", "Site", &[a, b], &HashMap::new(), 10);
        let pos_newer = xml.find("Newer").unwrap();
        let pos_older = xml.find("Older").unwrap();
        assert!(pos_newer < pos_older);
    }

    #[test]
    fn xml_entities_escaped_in_titles() {
        let xml = rss(
            "https://x",
            "Site & Co",
            &[page("Hello <world>")],
            &HashMap::new(),
            10,
        );
        assert!(xml.contains("Site &amp; Co"));
        assert!(xml.contains("Hello &lt;world&gt;"));
    }
}
