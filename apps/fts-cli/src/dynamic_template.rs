use std::path::{Path, PathBuf};

use clap::Subcommand;
use dynamic_template::{auto_color, default_config, OrganizeIntoTracks};
use eyre::{eyre, Result};
use serde::Serialize;

#[derive(Subcommand)]
pub enum DynamicTemplateCommand {
    /// Organize track/item names into the default Dynamic Template hierarchy
    Organize(NameInputArgs),
    /// Classify names and print their auto-color assignments
    Colors(NameInputArgs),
    /// List the default Dynamic Template groups
    Groups,
}

#[derive(clap::Args)]
pub struct NameInputArgs {
    /// Read one name per line from a file
    #[arg(long, short)]
    file: Option<PathBuf>,
    /// Track or item names to classify
    names: Vec<String>,
}

#[derive(Serialize)]
struct TrackHierarchyOutput {
    input_count: usize,
    track_count: usize,
    tracks: Vec<TrackNodeOutput>,
}

#[derive(Serialize)]
struct TrackNodeOutput {
    name: String,
    is_folder: bool,
    folder_depth_change: String,
    folder_depth_raw: i32,
    items: Vec<String>,
    color: Option<u32>,
    metadata: Option<String>,
}

#[derive(Serialize)]
struct ColorOutput {
    name: String,
    color: u32,
    color_hex: String,
}

pub fn run(cmd: DynamicTemplateCommand, as_json: bool) -> Result<()> {
    match cmd {
        DynamicTemplateCommand::Organize(args) => organize(args, as_json),
        DynamicTemplateCommand::Colors(args) => colors(args, as_json),
        DynamicTemplateCommand::Groups => groups(as_json),
    }
}

fn organize(args: NameInputArgs, as_json: bool) -> Result<()> {
    let names = read_names(args)?;
    let input_count = names.len();
    let hierarchy = names.organize_into_tracks(&default_config(), None)?;
    let output = TrackHierarchyOutput {
        input_count,
        track_count: hierarchy.tracks.len(),
        tracks: hierarchy
            .tracks
            .iter()
            .map(|node| TrackNodeOutput {
                name: node.name.clone(),
                is_folder: node.is_folder,
                folder_depth_change: format!("{:?}", node.folder_depth_change),
                folder_depth_raw: node.folder_depth_change.to_raw_value(),
                items: node.items.clone(),
                color: node.color,
                metadata: node.metadata.clone(),
            })
            .collect(),
    };

    if as_json {
        println!("{}", serde_json::to_string_pretty(&output)?);
    } else {
        println!(
            "Organized {} names into {} tracks",
            output.input_count, output.track_count
        );
        for track in &output.tracks {
            let marker = if track.is_folder { "[F] " } else { "" };
            println!(
                "{}{}  depth={} items={}",
                marker,
                track.name,
                track.folder_depth_raw,
                track.items.len()
            );
        }
    }

    Ok(())
}

fn colors(args: NameInputArgs, as_json: bool) -> Result<()> {
    let names = read_names(args)?;
    let color_map = auto_color::classify_and_color(names);
    let mut output: Vec<_> = color_map
        .into_iter()
        .map(|(name, color)| {
            let color = color.to_hex();
            ColorOutput {
                name,
                color,
                color_hex: format!("#{color:06X}"),
            }
        })
        .collect();
    output.sort_by(|a, b| a.name.cmp(&b.name));

    if as_json {
        println!("{}", serde_json::to_string_pretty(&output)?);
    } else if output.is_empty() {
        println!("No color assignments found");
    } else {
        for item in &output {
            println!("{} {}", item.color_hex, item.name);
        }
    }

    Ok(())
}

fn groups(as_json: bool) -> Result<()> {
    let config = default_config();
    let names: Vec<_> = config
        .groups
        .iter()
        .map(|group| group.name.clone())
        .collect();

    if as_json {
        println!("{}", serde_json::to_string_pretty(&names)?);
    } else {
        for name in names {
            println!("{name}");
        }
    }

    Ok(())
}

fn read_names(args: NameInputArgs) -> Result<Vec<String>> {
    let mut names = args.names;

    if let Some(path) = args.file {
        names.extend(read_names_file(&path)?);
    }

    names.retain(|name| !name.trim().is_empty());
    if names.is_empty() {
        return Err(eyre!("provide at least one name or --file"));
    }

    Ok(names)
}

fn read_names_file(path: &Path) -> Result<Vec<String>> {
    let content = std::fs::read_to_string(path)?;
    Ok(content
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToOwned::to_owned)
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_names_rejects_empty_input() {
        let err = read_names(NameInputArgs {
            file: None,
            names: Vec::new(),
        })
        .unwrap_err();

        assert!(err.to_string().contains("provide at least one name"));
    }

    #[test]
    fn colors_assigns_known_drum_name() {
        let colors = auto_color::classify_and_color(vec!["Kick In".to_string()]);

        assert!(colors.contains_key("Kick In"));
    }
}
