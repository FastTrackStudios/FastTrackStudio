---
name: vocal-corpus
description: "Build and extend the Billboard hit-vocal corpus — chart ingest, audio acquisition, and demucs stem separation via features/analyzer/analyzer-corpus. Use when adding songs, widening the corpus to genre charts, re-running acquisition or separation, resuming an interrupted run, or when any stage starts failing wholesale (rate limits, bot checks, CUDA, demucs producing nothing)."
---

# The vocal corpus

A corpus of charting songs, their audio, and their separated vocal and
instrumental stems, for measuring what hit vocals actually do — how
compressed they are, what EQ curve they follow, and where the vocal sits
against the track.

Everything lives in `features/analyzer/analyzer-corpus`, driven by one
binary (`corpus`). Data is on AudioHaven, never in the repo:

```
/run/media/AudioHaven/fts-corpus/
  corpus.sqlite        chart entries, songs, renditions, stems
  audio/<song_id>/     downloaded source (Opus in WebM)
  stems/<song_id>/     vocals.opus + instrumental.opus
  tmp-stems/           demucs scratch, deleted per batch
```

## The stages

```
charts ──► songs ──► audio ──► stems ──► metrics
 (1)        (dedupe)   (2)       (3)      (later)
```

```bash
# 1. charts. Hot 100 has a public JSON archive; genre charts are scraped.
corpus ingest-hot100 --db $DB --from 1990 --to 2025
corpus ingest-genre  --db $DB            # ~11k page fetches, resumable

# 2. audio. Best-charting first, so a partial run still covers the hits.
corpus acquire --db $DB --scope hot100 --concurrency 5 --candidates 20

# 3. stems. NOT inside `nix develop` — see CUDA below.
UV=$(command -v uv) FFMPEG=$(nix develop --command command -v ffmpeg) \
corpus separate --db $DB --scope hot100 --batch 8 --bitrate-k 128 --device cuda

corpus status --db $DB
corpus export --db $DB --out songs.csv
```

Every stage is resumable: a song with a recorded outcome is skipped. All
of them will be interrupted, so this matters more than it sounds.

`--scope` picks the slice — `hot100`, `top40`, `top10`,
`top40-plus-genre-leaders`, `all`. Scope is the main lever on when
results arrive; `all` is ~43k songs against the Hot 100's ~15k.

## Scale, measured

| | |
|---|---|
| Hot 100, 1990-2025 | 15,165 songs (43,481 with genre charts) |
| Downloaded | 93.5% — the rest are genuine no-matches |
| Source audio | ~4 MB/song, ~56 GB |
| Stems, Opus 128k | ~8.2 MB/song, ~114 GB |
| Acquisition | ~30 songs/min at `--concurrency 5` |
| Separation | ~5 s/song on a 4080, ~21 h for the corpus |

## Traps

Every one of these fails **silently** — no crash, no error, just wrong
or missing data that looks fine until it is averaged into a result.

### A transient refusal is not a verdict

This cost the corpus twice, in two different stages, and is the single
most expensive mistake available here.

Acquisition skips any song with a recorded outcome. So recording a
temporary refusal as a permanent one means that song is **never retried**:

- A search returning zero candidates was filed as `no_match` ("we looked,
  this is not it"). During one rate-limited stretch that marked ~2,800
  songs as permanent gaps.
- A download hitting YouTube's bot check was filed as `failed`. That was
  6,942 songs — 46% of the Hot 100 — silently written off.

Both are recoverable and both were recovered. The rule: refusals get a
retryable status (`blocked`), which every pass requeues; only real
verdicts (unavailable, private, no confident match) are terminal. See
`is_retryable_download_error`.

**Diagnose by shape, not by error count.** 98% of those "misses" had
*zero* candidates, and the success rate over time went 96% → 37% → 0%
for 2,800 songs → 90%. A genuine miss rate does not do that.

### The limit is a per-IP request quota

Not a concurrency cap. It trips after roughly 500-800 songs whether 8 or
16 run at once, and clears on its own after **about 2.2 hours**
(measured). So the shape that works is: work until refused, wait it out,
resume — `--cooldown-secs`, plus a circuit breaker that stops after 25
consecutive refusals rather than marching through the corpus recording
them.

Bandwidth is never the constraint. The job moves well under 1 MB/s.
Sharding across other machines only helps with **independent egress**;
voyager and airlock share this machine's public IP, so they would put 3x
the requests behind one address and make it worse.

The real lever is requests *per song*, not machines. See below.

### Plain YouTube search cannot find the record

Search ranks by engagement. For a real 1995 chart-topper, twelve results
held five reaction videos, karaoke, a play-along, a movie clip, a cover,
a live cut and a 2022 re-recording — and **no original master**.

Use YouTube Music, which returns the label's own art tracks with
structured `artist` / `track` / `album` metadata. Resolution goes through
`acquire::search` (a long-running `resolver.py` on `ytmusicapi`), which
answers with 20 fully-described candidates in **one** request instead of
about six. That is a 6x throughput gain and 4x more candidates at once —
and it is what keeps the quota from tripping.

### Title and artist must be necessary, not additive

A scorer that adds up signals will accept the artist's *other song*:
right artist, right era, label metadata, zero title overlap scored 8.0
and passed. A faithful cover passes on title alone. Both put a completely
wrong vocal into the corpus under a real song's name — the failure mode
that survives averaging and is invisible afterwards.

Gate them: `TITLE_FLOOR` 0.6, `ARTIST_FLOOR` 0.5, checked before scoring.
Version markers (live, remix, karaoke, "classic version", re-record) are
large penalties, not small ones — they mean a different performance.

**A gap in the corpus beats a confident wrong number.**

### Billboard serves the nearest chart for a date that has none

Asking for a genre chart before it launched does **not** 404. Billboard
answers 200, no redirect, and renders its earliest available week.
Dance/Electronic (launched 2013) returns the same January 2013 chart for
1990, 1999 and 2009 alike.

Parse the page's own `Week of ...` date, file entries under **that**, and
treat a rendered date more than 7 days from the request as "this chart
had no edition then". A page that parses to zero rows is an error, never
an empty week.

### Downloads need a specific player client

Resolving metadata is not the same as being allowed to fetch audio: the
default client resolves fine, then answers **HTTP 403**. Only
`web_embedded` served it — `web`, `web_safari`, `ios`, `mweb` and
`android` all called the format unavailable, `tv` demanded a reload.

Kept as an ordered fallback list (`PLAYER_CLIENTS`), because which
clients work is a property of YouTube's defences this week, not of this
code. If downloads start failing wholesale, re-test and reorder.

Always capture yt-dlp's stderr. Swallowing it turns a plain "HTTP 403"
into an opaque "exited with status 1".

### demucs exits 0 when it fails

Handed audio it cannot decode, demucs prints a `LoadAudioError` and
**still exits successfully**. A batch run trusting the exit code produces
nothing for thousands of songs and reports success.

Confirm every song by the presence of its output files. Never by status.

And it needs **ffmpeg on its own PATH** — it shells out to decode
anything its built-in loader cannot read, which is every file in this
corpus (Opus in WebM). Knowing where ffmpeg is in the parent process is
not enough; put it on the child's `PATH`.

### CUDA: check nvidia-smi first

If separation is slow or `torch.cuda.is_available()` is False, run
`nvidia-smi` **before** touching library paths. A NixOS upgrade installs
a new driver while the running kernel keeps the old module, and
everything then fails with:

```
Error 803: system has unsupported display driver / cuda driver combination
```

`nvidia-smi` says `Driver/library version mismatch`; compare
`/proc/driver/nvidia/version` against `/run/opengl-driver/lib/libcuda.so.*`.
**A reboot fixes it.** Hours were lost blaming library paths for this.

Separately, and genuinely: do **not** run separation inside
`nix develop`. The dev shell's toolchain libraries break the PyPI torch
wheel. Run the binary outside it and pass `UV=` and `FFMPEG=` in, which
is also why the commands above look the way they do.

Torch needs `/run/opengl-driver/lib` on `LD_LIBRARY_PATH` (handled by
`separate::Tools`), and PyPI numpy wheels need `libz.so.1` from the store.

### Nix can garbage-collect a pinned store path

Passing tool paths as `/nix/store/...` env vars works until a GC removes
them, and then 38 songs "fail" for reasons that look like unavailability.
Re-resolve through `nix develop --command command -v <tool>` at launch
rather than hardcoding a path that outlives its derivation.

### Never compare FFT bins across sample rates

An early codec comparison reported a 2.6 dB error for *every* Opus
bitrate, including one that was audibly transparent — purely because Opus
resamples to 48 kHz while demucs writes 44.1 kHz, so the bins were
different frequencies.

Resample to a common rate before comparing spectra, or work in
logarithmic **hertz** bands. `metrics::band_spectrum` does the latter and
is still not fully rate-invariant for tonal signals — its test is
`#[ignore]`d with the details. **Resolve that before publishing any
spectral aggregate.**

## Measurement notes

Crest factor is peak minus RMS **over voiced material only**. Including
the gaps between phrases drags RMS down and inflates the number; that is
the most common way this measurement is reported wrong. `metrics::crest_db`
gates at -40 dB from the loudest 50 ms frame.

Stems are archived as Opus 128k (~114 GB vs ~443 GB for FLAC). Measured
over twelve stems that shifts crest factor 0.12 dB on average — but
**0.81 dB worst case**, which is the size of the genre differences being
studied. Anything needing exactness should be measured from demucs's
lossless output before encoding.

`htdemucs`, not `htdemucs_ft`: the fine-tuned variant is a bag of four
models and ~4x slower, a one-day run against four. Cross-check a subset
against `htdemucs_ft` before drawing conclusions.

## Early result, for calibration

Twelve Hot 100 #1s, crest factor on separated vocals:

| era | mean | in the 14-18 dB window |
|---|---|---|
| 2021-2025 | 16.2 dB | 6/6 |
| 1990 | 18.6 dB | 2/6 |

The modern figure matches the published ~16 dB, which is what validates
the chain end to end. The 1990 figure suggests that "16 dB" is an average
over an era trend rather than a constant — worth resolving properly with
the full corpus, and a reason to keep songs that charted poorly as well
as the hits.

Twelve songs is a smoke test, not evidence.
