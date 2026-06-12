//! Reddit thread extraction — accept-fragility tier.
//!
//! Live-verified 2026-06: the cookieless `<permalink>.json`
//! endpoint is DEAD (403 for anonymous clients). The working
//! anonymous path is a two-step dance:
//!
//! 1. `GET https://old.reddit.com/` once — the response sets
//!    an anonymous `loid` cookie (plus friends).
//! 2. `GET https://old.reddit.com<permalink>.json` WITH those
//!    cookies — returns the classic two-`Listing` payload
//!    (`[post, comments]`).
//!
//! Reddit blocks aggressively: keep ≤ ~10 requests/min
//! ([`MIN_REQUEST_INTERVAL`]), always content-type-check
//! before parsing (block pages come back as HTML with 200s
//! sometimes), and treat 403/429 as "unarchived, retry
//! later" — the CLI stores an unarchived stub and the retry
//! verb picks it up. Standing maintenance is expected here;
//! when this path dies the health surface will show it.

use std::time::Duration;

use serde_json::Value;

use crate::ArchiveError;

/// Stay under ~10 requests/minute. Batch flows (importers,
/// `archive retry`) must sleep this long between Reddit hits.
pub const MIN_REQUEST_INTERVAL: Duration = Duration::from_secs(6);

/// Reddit 403s anything that doesn't look like a stock
/// browser — INCLUDING UA strings with tool suffixes
/// (live-verified: the crate's usual self-identifying UA
/// gets no `loid` cookie and a 403; a plain Firefox UA
/// works). Anonymity is the price of admission here.
const REDDIT_USER_AGENT: &str =
    "Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0";

/// Dedicated client for the Reddit dance.
pub fn client() -> Result<reqwest::Client, ArchiveError> {
    reqwest::Client::builder()
        .user_agent(REDDIT_USER_AGENT)
        .timeout(Duration::from_secs(45))
        .build()
        .map_err(|e| ArchiveError::Fetch {
            url: String::new(),
            message: format!("client build: {e}"),
        })
}

/// Depth cap for the rendered comment tree.
const MAX_DEPTH: usize = 4;
/// Total comment cap — archives are digests, not mirrors.
const MAX_COMMENTS: usize = 50;

/// The post + flattened comment tree.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct RedditThread {
    pub title: String,
    pub subreddit: String,
    pub author: String,
    pub score: i64,
    pub upvote_ratio: Option<f64>,
    pub created_utc: Option<i64>,
    /// Self-post body (already markdown — Reddit speaks md).
    pub selftext: String,
    /// Link posts: the outbound URL.
    pub link_url: Option<String>,
    pub num_comments: Option<i64>,
    pub comments: Vec<RedditComment>,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct RedditComment {
    pub author: String,
    pub score: i64,
    pub body: String,
    /// 0 = top-level reply.
    pub depth: usize,
}

/// Fetch one thread anonymously (loid-cookie dance + .json).
pub async fn fetch_thread(
    client: &reqwest::Client,
    permalink: &str,
) -> Result<RedditThread, ArchiveError> {
    let cookies = fetch_anon_cookies(client).await?;
    let url = format!(
        "https://old.reddit.com{}.json?limit=100&raw_json=1",
        permalink.trim_end_matches('/')
    );
    let resp = client
        .get(&url)
        .header("cookie", cookies)
        .header("accept", "application/json")
        .send()
        .await
        .map_err(|e| ArchiveError::Fetch {
            url: url.clone(),
            message: e.to_string(),
        })?;
    let status = resp.status();
    if !status.is_success() {
        return Err(ArchiveError::BadResponse {
            url,
            message: format!(
                "HTTP {status} — Reddit is rate-limiting or blocking; retry later \
                 (`task wiki archive retry`)"
            ),
        });
    }
    // Content-type check BEFORE parsing: blocked/interstitial
    // responses come back as HTML, sometimes with a 200.
    let ct = resp
        .headers()
        .get("content-type")
        .and_then(|v| v.to_str().ok())
        .unwrap_or_default()
        .to_ascii_lowercase();
    if !ct.contains("application/json") {
        return Err(ArchiveError::BadResponse {
            url,
            message: format!(
                "expected application/json, got `{ct}` — likely a block page; retry later"
            ),
        });
    }
    let body = resp.text().await.map_err(|e| ArchiveError::Fetch {
        url: url.clone(),
        message: e.to_string(),
    })?;
    parse_thread(&body)
}

/// One GET against old.reddit.com to collect the anonymous
/// cookie set (`loid` et al), returned as a `Cookie:` header
/// value.
async fn fetch_anon_cookies(client: &reqwest::Client) -> Result<String, ArchiveError> {
    let url = "https://old.reddit.com/";
    let resp = client
        .get(url)
        .send()
        .await
        .map_err(|e| ArchiveError::Fetch {
            url: url.to_string(),
            message: e.to_string(),
        })?;
    let cookies: Vec<String> = resp
        .headers()
        .get_all("set-cookie")
        .iter()
        .filter_map(|v| v.to_str().ok())
        .filter_map(|c| c.split(';').next())
        .map(ToString::to_string)
        .collect();
    if cookies.is_empty() {
        return Err(ArchiveError::BadResponse {
            url: url.to_string(),
            message: "old.reddit.com set no cookies — anonymous session unavailable".into(),
        });
    }
    Ok(cookies.join("; "))
}

/// Parse the two-`Listing` payload. Pure — fixture-tested.
pub fn parse_thread(json: &str) -> Result<RedditThread, ArchiveError> {
    let v: Value = serde_json::from_str(json)
        .map_err(|e| ArchiveError::ImportParse(format!("reddit json: {e}")))?;
    let listings = v
        .as_array()
        .ok_or_else(|| ArchiveError::ImportParse("reddit: expected [post, comments] array".into()))?;
    let post = listings
        .first()
        .and_then(|l| l.pointer("/data/children/0/data"))
        .ok_or_else(|| ArchiveError::ImportParse("reddit: no post in first Listing".into()))?;

    let str_of = |v: &Value, k: &str| {
        v.get(k)
            .and_then(Value::as_str)
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .map(ToString::to_string)
    };

    let title = str_of(post, "title")
        .ok_or_else(|| ArchiveError::ImportParse("reddit: post has no title".into()))?;
    let selftext = str_of(post, "selftext").unwrap_or_default();
    // Link posts carry the outbound URL in `url`; self posts
    // point it back at the permalink — drop those.
    let link_url = str_of(post, "url").filter(|u| {
        post.get("is_self").and_then(Value::as_bool) != Some(true)
            && !u.contains("/comments/")
    });

    let mut comments = Vec::new();
    if let Some(children) = listings
        .get(1)
        .and_then(|l| l.pointer("/data/children"))
        .and_then(Value::as_array)
    {
        for child in children {
            collect_comment(child, 0, &mut comments);
            if comments.len() >= MAX_COMMENTS {
                break;
            }
        }
    }
    comments.truncate(MAX_COMMENTS);

    Ok(RedditThread {
        title,
        subreddit: str_of(post, "subreddit").unwrap_or_default(),
        author: str_of(post, "author").unwrap_or_else(|| "[deleted]".into()),
        score: post.get("score").and_then(Value::as_i64).unwrap_or(0),
        upvote_ratio: post.get("upvote_ratio").and_then(Value::as_f64),
        created_utc: post
            .get("created_utc")
            .and_then(Value::as_f64)
            .map(|f| f as i64),
        selftext,
        link_url,
        num_comments: post.get("num_comments").and_then(Value::as_i64),
        comments,
    })
}

fn collect_comment(child: &Value, depth: usize, out: &mut Vec<RedditComment>) {
    if out.len() >= MAX_COMMENTS || depth > MAX_DEPTH {
        return;
    }
    // `kind: "more"` children are pagination stubs.
    if child.get("kind").and_then(Value::as_str) != Some("t1") {
        return;
    }
    let Some(data) = child.get("data") else { return };
    let body = data
        .get("body")
        .and_then(Value::as_str)
        .map(str::trim)
        .unwrap_or_default();
    if body.is_empty() || body == "[removed]" || body == "[deleted]" {
        return;
    }
    out.push(RedditComment {
        author: data
            .get("author")
            .and_then(Value::as_str)
            .unwrap_or("[deleted]")
            .to_string(),
        score: data.get("score").and_then(Value::as_i64).unwrap_or(0),
        body: body.to_string(),
        depth,
    });
    // `replies` is `""` (string!) when empty — only recurse
    // into object form.
    if let Some(replies) = data
        .get("replies")
        .and_then(|r| r.pointer("/data/children"))
        .and_then(Value::as_array)
    {
        for reply in replies {
            collect_comment(reply, depth + 1, out);
        }
    }
}

/// Render the raw-source body: post lede + selftext (already
/// markdown), then the comment digest as quote blocks with
/// `↳`-prefixed nesting (the SourceViewer's quote renderer
/// handles these with zero changes).
#[must_use]
pub fn render_reddit_markdown(thread: &RedditThread) -> String {
    let mut out = String::new();

    let mut lede: Vec<String> = Vec::new();
    if !thread.subreddit.is_empty() {
        lede.push(format!("r/{}", thread.subreddit));
    }
    lede.push(format!("u/{}", thread.author));
    if let Some(ts) = thread.created_utc {
        if let Some(dt) = chrono::DateTime::from_timestamp(ts, 0) {
            lede.push(dt.format("%Y-%m-%d").to_string());
        }
    }
    let ratio = thread
        .upvote_ratio
        .map(|r| format!(" ({}% upvoted)", (r * 100.0).round() as i64))
        .unwrap_or_default();
    lede.push(format!("score {}{ratio}", thread.score));
    out.push_str(&format!("_{}_\n\n", lede.join(" · ")));

    if let Some(link) = &thread.link_url {
        out.push_str(&format!("Link post → <{link}>\n\n"));
    }
    if !thread.selftext.is_empty() {
        out.push_str(thread.selftext.trim());
        out.push_str("\n\n");
    }

    if !thread.comments.is_empty() {
        let total = thread
            .num_comments
            .map(|n| format!(" (showing {} of {n})", thread.comments.len()))
            .unwrap_or_default();
        out.push_str(&format!("## Comments{total}\n\n"));
        for c in &thread.comments {
            let indent = "↳ ".repeat(c.depth);
            out.push_str(&format!("> {indent}**u/{}** · {:+}\n", c.author, c.score));
            for line in c.body.lines() {
                out.push_str("> ");
                out.push_str(line);
                out.push('\n');
            }
            out.push('\n');
        }
    }
    out.trim_end().to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> String {
        r#"[
          {"kind":"Listing","data":{"children":[{"kind":"t3","data":{
            "title":"How do you archive the web?",
            "subreddit":"datahoarder",
            "author":"alice",
            "score":420,
            "upvote_ratio":0.97,
            "created_utc":1765432100.0,
            "selftext":"I want **everything** saved.\n\nWhat do you use?",
            "is_self":true,
            "url":"https://www.reddit.com/r/datahoarder/comments/abc123/how/",
            "num_comments":3
          }}]}},
          {"kind":"Listing","data":{"children":[
            {"kind":"t1","data":{"author":"bob","score":99,"body":"ArchiveBox works.","replies":{"kind":"Listing","data":{"children":[
              {"kind":"t1","data":{"author":"alice","score":12,"body":"Trying it, thanks!","replies":""}}
            ]}}}},
            {"kind":"t1","data":{"author":"spam","score":-5,"body":"[removed]","replies":""}},
            {"kind":"more","data":{"count":1,"children":["zzz"]}}
          ]}}
        ]"#
        .to_string()
    }

    #[test]
    fn parses_two_listing_payload() {
        let t = parse_thread(&fixture()).unwrap();
        assert_eq!(t.title, "How do you archive the web?");
        assert_eq!(t.subreddit, "datahoarder");
        assert_eq!(t.score, 420);
        assert_eq!(t.link_url, None, "self post keeps no link_url");
        assert!(t.selftext.contains("**everything**"));
        // removed comment + `more` stub skipped; nested reply kept.
        assert_eq!(t.comments.len(), 2);
        assert_eq!(t.comments[0].author, "bob");
        assert_eq!(t.comments[0].depth, 0);
        assert_eq!(t.comments[1].author, "alice");
        assert_eq!(t.comments[1].depth, 1);
    }

    #[test]
    fn link_posts_keep_outbound_url() {
        let json = fixture()
            .replace("\"is_self\":true", "\"is_self\":false")
            .replace(
                "https://www.reddit.com/r/datahoarder/comments/abc123/how/",
                "https://example.com/article",
            );
        let t = parse_thread(&json).unwrap();
        assert_eq!(t.link_url.as_deref(), Some("https://example.com/article"));
    }

    #[test]
    fn renders_quote_digest() {
        let t = parse_thread(&fixture()).unwrap();
        let md = render_reddit_markdown(&t);
        assert!(md.contains("_r/datahoarder · u/alice · 2025-12-11 · score 420 (97% upvoted)_"), "{md}");
        assert!(md.contains("## Comments (showing 2 of 3)"), "{md}");
        assert!(md.contains("> **u/bob** · +99"), "{md}");
        assert!(md.contains("> ↳ **u/alice** · +12"), "{md}");
    }

    #[test]
    fn non_json_and_wrong_shapes_error() {
        assert!(parse_thread("<html>blocked</html>").is_err());
        assert!(parse_thread("{}").is_err());
        assert!(parse_thread("[]").is_err());
    }
}
