# Track Template Engine

A DAW-agnostic engine for matching track names to templates, creating tracks, and managing track hierarchies with sends/receives.

This crate is separate from the `naming-convention` crate, which is responsible only for parsing strings into `TrackName` structs. This crate uses that parsing to match and organize tracks into templates.

## Features

- **Track Matching**: Match parsed track names to existing tracks in a template
- **Template Management**: Create templates from scratch or match against existing ones
- **Track Hierarchy**: Support for parent/child relationships (folders)
- **Takes (Item Lanes)**: Support for multiple takes on a single track
- **Send/Receive**: Support for track routing relationships

## Basic Usage

```rust
use track_template::{TrackMatcher, Track, Template};

// Create a matcher
let mut matcher = TrackMatcher::new();

// Parse a track name
let track_name = matcher.parse("D Kick In");

// Find or create a track
let track = matcher.find_or_create_track(&track_name, Some("Kick In"));

// Create a template
let mut template = Template::new("My Template");
template.add_track(track);
```

## Architecture

- **`Track`**: Represents a single track with name, type, parent, takes, and sends/receives
- **`TrackList`**: A collection of tracks that can be matched against
- **`TrackMatcher`**: Matches parsed `TrackName` objects to tracks in a template
- **`Template`**: A complete template structure with a track list

## Integration with naming-convention

This crate uses the `naming-convention` crate to parse track names. The naming convention crate is responsible for:
- Parsing strings into `TrackName` structs
- Extracting components (group prefix, sub-type, multi-mic, etc.)

This crate is responsible for:
- Matching parsed names to tracks
- Creating and organizing tracks
- Managing template structures

