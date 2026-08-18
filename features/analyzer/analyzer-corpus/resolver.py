#!/usr/bin/env python3
"""Resolve song titles to YouTube Music tracks, one request per song.

Why this exists
---------------
Driving the search through yt-dlp costs roughly six requests per song: a
search page, then a full metadata extraction per candidate. Acquisition
is limited by a per-IP *request quota*, not by bandwidth, so that
overhead — spent only to pick which upload to take — is what forces the
whole corpus into rate-limit cooldowns.

YouTube Music's own search endpoint answers with twenty fully-described
results in a single request: title, artists, album, duration, videoId.
One request instead of six, and more candidates to choose from rather
than fewer.

Protocol
--------
A long-running process so the interpreter and the API handshake are paid
once, not per song. Read one JSON object per line on stdin:

    {"id": 123, "query": "artist title"}

Write one JSON object per line on stdout, in the same order:

    {"id": 123, "candidates": [...]}          on success
    {"id": 123, "error": "..."}               on failure

A failure for one song is reported and the loop continues — the caller
records it and moves on, rather than the whole run dying on one bad
lookup.
"""

import json
import sys


def duration_seconds(track):
    """Seconds for a track, from whichever field is populated.

    `duration_seconds` is usually present; `duration` is a "m:ss" or
    "h:mm:ss" string. Returning None rather than guessing matters: the
    caller disqualifies candidates with no duration, because length is
    how a clip or a compilation is told from a song.
    """
    secs = track.get("duration_seconds")
    if isinstance(secs, int) and secs > 0:
        return secs
    text = track.get("duration")
    if not isinstance(text, str):
        return None
    parts = text.strip().split(":")
    if not all(p.isdigit() for p in parts) or not 2 <= len(parts) <= 3:
        return None
    total = 0
    for p in parts:
        total = total * 60 + int(p)
    return total or None


def to_candidate(track):
    """Flatten one search result into the shape the scorer expects."""
    artists = [a["name"] for a in (track.get("artists") or []) if a.get("name")]
    album = (track.get("album") or {}).get("name")
    return {
        "id": track.get("videoId"),
        # The scorer reads `track` when present and falls back to
        # `title`; both are the same here since this is structured
        # metadata rather than an uploader's free-text video title.
        "title": track.get("title") or "",
        "track": track.get("title"),
        "artist": ", ".join(artists) if artists else None,
        "channel": artists[0] if artists else None,
        # Album carries the version that the title sometimes omits —
        # a live record's album says "Live" even when its track does not.
        "album": album,
        "duration": duration_seconds(track),
        # YouTube Music search does not return a release year. Left None
        # rather than invented; the scorer treats it as optional and
        # leans on title/album version markers instead.
        "release_year": None,
    }


def main():
    from ytmusicapi import YTMusic

    yt = YTMusic()
    limit = int(sys.argv[1]) if len(sys.argv) > 1 else 20

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except json.JSONDecodeError as e:
            print(json.dumps({"id": None, "error": f"bad request: {e}"}), flush=True)
            continue

        out = {"id": req.get("id")}
        try:
            results = yt.search(req["query"], filter="songs", limit=limit)
            cands = [to_candidate(t) for t in results]
            # A candidate with no videoId cannot be downloaded.
            out["candidates"] = [c for c in cands if c["id"]]
        except Exception as e:  # noqa: BLE001 - report, never die
            out["error"] = f"{type(e).__name__}: {e}"
        print(json.dumps(out), flush=True)


if __name__ == "__main__":
    main()
