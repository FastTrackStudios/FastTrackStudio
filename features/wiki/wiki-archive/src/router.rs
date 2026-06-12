//! Content-type router + canonical-URL normalization.
//!
//! `classify` decides which extractor handles a URL;
//! `canonicalize` produces the dedup key. Both are pure
//! string→value functions so they unit-test without network.
//!
//! ## Canonicalization rules
//!
//! Provider-specific first (these collapse the many spellings
//! of the same resource):
//!
//! - YouTube (`watch?v=`, `youtu.be/`, `/shorts/`, `/embed/`,
//!   `/live/`) → `https://www.youtube.com/watch?v=<id>`
//! - Google Docs → `https://docs.google.com/document/d/<id>`
//!
//! Generic fallback: lowercase host, strip a leading `www.`,
//! drop default ports + fragments + tracking params
//! (`utm_*`, click ids, …), sort surviving query params,
//! trim the trailing slash. Same article shared from Pocket
//! (`?utm_source=pocket`) and Readwise (clean URL) → one key.

use url::Url;

use crate::ArchiveError;

/// Which extractor a URL routes to.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Route {
    /// Default: readability article extraction.
    Article,
    /// Google Docs — fetched via the `/export?format=md`
    /// endpoint instead of scraping the JS app shell.
    GoogleDoc { doc_id: String },
    /// YouTube (incl. Shorts / youtu.be / embeds). Carries
    /// the video id so the canonical URL and the
    /// SourceViewer iframe need no re-parse.
    YouTube { video_id: String },
    /// Other yt-dlp-able hosts (Vimeo, TikTok, X video) —
    /// same transcript path as YouTube, no id extraction.
    Video,
}

/// Parse + classify in one step. The common entry point for
/// the CLI verb and the importers.
pub fn classify(input: &str) -> Result<(Url, Route), ArchiveError> {
    let url = Url::parse(input)
        .map_err(|e| ArchiveError::InvalidUrl(input.to_string(), e.to_string()))?;
    if !matches!(url.scheme(), "http" | "https") {
        return Err(ArchiveError::InvalidUrl(
            input.to_string(),
            format!("unsupported scheme `{}`", url.scheme()),
        ));
    }
    let route = route_for(&url);
    Ok((url, route))
}

/// `content_type:` frontmatter value for a route.
#[must_use]
pub fn content_type_for(route: &Route) -> &'static str {
    match route {
        Route::Article => "article",
        Route::GoogleDoc { .. } => "document",
        Route::YouTube { .. } | Route::Video => "video",
    }
}

fn route_for(url: &Url) -> Route {
    let host = host_no_www(url);
    let path = url.path();

    // ── YouTube family ──────────────────────────────────
    if host == "youtu.be" {
        let id = path.trim_start_matches('/');
        if let Some(id) = clean_video_id(id) {
            return Route::YouTube { video_id: id };
        }
    }
    if host == "youtube.com" || host.ends_with(".youtube.com") {
        if path == "/watch" {
            if let Some(id) = url
                .query_pairs()
                .find(|(k, _)| k == "v")
                .and_then(|(_, v)| clean_video_id(&v))
            {
                return Route::YouTube { video_id: id };
            }
        }
        for prefix in ["/shorts/", "/embed/", "/live/", "/v/"] {
            if let Some(rest) = path.strip_prefix(prefix) {
                if let Some(id) = clean_video_id(rest.split('/').next().unwrap_or("")) {
                    return Route::YouTube { video_id: id };
                }
            }
        }
    }

    // ── Google Docs ─────────────────────────────────────
    if host == "docs.google.com" {
        if let Some(rest) = path.strip_prefix("/document/d/") {
            let id = rest.split('/').next().unwrap_or("");
            if !id.is_empty() {
                return Route::GoogleDoc {
                    doc_id: id.to_string(),
                };
            }
        }
    }

    // ── Other yt-dlp hosts (same transcript path) ───────
    if host == "vimeo.com" || host.ends_with(".vimeo.com") {
        return Route::Video;
    }
    if host == "tiktok.com" || host.ends_with(".tiktok.com") {
        return Route::Video;
    }
    // X/Twitter: only `/status/` permalinks can carry video;
    // profile/search pages stay articles (they'd fail in
    // yt-dlp anyway). Social-post text extraction proper is
    // phase 3.
    if (host == "x.com" || host == "twitter.com") && path.contains("/status/") {
        return Route::Video;
    }

    Route::Article
}

/// Canonical dedup key for a URL. Provider-specific
/// rewrites first; generic normalization otherwise.
#[must_use]
pub fn canonicalize(url: &Url, route: &Route) -> String {
    match route {
        Route::YouTube { video_id } => {
            format!("https://www.youtube.com/watch?v={video_id}")
        }
        Route::GoogleDoc { doc_id } => {
            format!("https://docs.google.com/document/d/{doc_id}")
        }
        Route::Article | Route::Video => generic_canonical(url),
    }
}

fn generic_canonical(url: &Url) -> String {
    let host = host_no_www(url);
    let scheme = url.scheme();
    let port = match (url.port(), scheme) {
        (Some(443), "https") | (Some(80), "http") | (None, _) => String::new(),
        (Some(p), _) => format!(":{p}"),
    };
    let path = if url.path() == "/" {
        String::new()
    } else {
        url.path().trim_end_matches('/').to_string()
    };
    let mut params: Vec<(String, String)> = url
        .query_pairs()
        .filter(|(k, _)| !is_tracking_param(k))
        .map(|(k, v)| (k.into_owned(), v.into_owned()))
        .collect();
    params.sort();
    let query = if params.is_empty() {
        String::new()
    } else {
        let joined = params
            .iter()
            .map(|(k, v)| {
                if v.is_empty() {
                    k.clone()
                } else {
                    format!("{k}={v}")
                }
            })
            .collect::<Vec<_>>()
            .join("&");
        format!("?{joined}")
    };
    format!("{scheme}://{host}{port}{path}{query}")
}

/// Tracking / share-attribution params that never change the
/// resource: stripping them is what lets the same article
/// from two importers collapse to one canonical key.
fn is_tracking_param(key: &str) -> bool {
    if key.starts_with("utm_") || key.starts_with("_hs") || key.starts_with("hsa_") {
        return true;
    }
    matches!(
        key,
        "fbclid"
            | "gclid"
            | "gbraid"
            | "wbraid"
            | "msclkid"
            | "twclid"
            | "ttclid"
            | "yclid"
            | "dclid"
            | "mc_cid"
            | "mc_eid"
            | "igshid"
            | "igsh"
            | "si" // youtube/spotify share token
            | "ref"
            | "ref_src"
            | "ref_url"
            | "cmpid"
            | "s_kwcid"
            | "sscid"
            | "vero_id"
            | "oly_anon_id"
            | "oly_enc_id"
            | "guccounter"
            | "share_id"
    )
}

fn host_no_www(url: &Url) -> String {
    let host = url.host_str().unwrap_or_default().to_ascii_lowercase();
    host.strip_prefix("www.").unwrap_or(&host).to_string()
}

/// YouTube ids are `[A-Za-z0-9_-]{6,}`; share links append
/// junk (`?si=`, trailing slashes) that `Url` already split
/// off — this guards against empty/odd leftovers.
fn clean_video_id(raw: &str) -> Option<String> {
    let id: String = raw
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || matches!(c, '-' | '_'))
        .collect();
    if id.len() >= 6 { Some(id) } else { None }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn route(s: &str) -> Route {
        classify(s).unwrap().1
    }

    fn canon(s: &str) -> String {
        let (url, r) = classify(s).unwrap();
        canonicalize(&url, &r)
    }

    #[test]
    fn youtube_spellings_collapse() {
        let forms = [
            "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
            "https://youtube.com/watch?v=dQw4w9WgXcQ&t=42s",
            "https://youtu.be/dQw4w9WgXcQ?si=AbCdEfGh123",
            "https://www.youtube.com/shorts/dQw4w9WgXcQ",
            "https://www.youtube.com/embed/dQw4w9WgXcQ",
            "https://m.youtube.com/watch?v=dQw4w9WgXcQ",
            "https://www.youtube.com/live/dQw4w9WgXcQ",
        ];
        for f in forms {
            assert_eq!(
                route(f),
                Route::YouTube {
                    video_id: "dQw4w9WgXcQ".into()
                },
                "route for {f}"
            );
            assert_eq!(
                canon(f),
                "https://www.youtube.com/watch?v=dQw4w9WgXcQ",
                "canon for {f}"
            );
        }
    }

    #[test]
    fn google_doc_routes_to_export() {
        let u = "https://docs.google.com/document/d/1AbC_dEf-123/edit?tab=t.0#heading=h.x";
        assert_eq!(
            route(u),
            Route::GoogleDoc {
                doc_id: "1AbC_dEf-123".into()
            }
        );
        assert_eq!(canon(u), "https://docs.google.com/document/d/1AbC_dEf-123");
    }

    #[test]
    fn pocket_vs_readwise_spelling_dedups() {
        let pocket = "https://Example.com/blog/post/?utm_source=pocket&utm_medium=email&b=2&a=1";
        let readwise = "https://www.example.com/blog/post?a=1&b=2#section";
        assert_eq!(canon(pocket), canon(readwise));
        assert_eq!(canon(pocket), "https://example.com/blog/post?a=1&b=2");
    }

    #[test]
    fn meaningful_query_params_survive() {
        assert_eq!(
            canon("https://example.com/search?q=rust&page=2"),
            "https://example.com/search?page=2&q=rust"
        );
    }

    #[test]
    fn root_and_ports_normalize() {
        assert_eq!(canon("https://example.com:443/"), "https://example.com");
        assert_eq!(
            canon("http://example.com:8080/x/"),
            "http://example.com:8080/x"
        );
    }

    #[test]
    fn video_hosts_route_to_video() {
        assert_eq!(route("https://vimeo.com/12345"), Route::Video);
        assert_eq!(route("https://www.tiktok.com/@user/video/7123"), Route::Video);
        assert_eq!(route("https://x.com/user/status/17890"), Route::Video);
        assert_eq!(route("https://x.com/user"), Route::Article);
    }

    #[test]
    fn non_http_rejected() {
        assert!(classify("ftp://example.com/f").is_err());
        assert!(classify("not a url").is_err());
    }
}
