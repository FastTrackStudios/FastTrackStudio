use std::path::{Path, PathBuf};

/// A plugin found on disk.
#[derive(Debug, Clone, PartialEq)]
pub struct ScannedPlugin {
    pub name: String,
    pub vendor: String,
    pub path: PathBuf,
    pub format: PluginFormat,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PluginFormat {
    Clap,
    Vst3,
}

impl PluginFormat {
    pub fn label(&self) -> &'static str {
        match self {
            PluginFormat::Clap => "CLAP",
            PluginFormat::Vst3 => "VST3",
        }
    }
}

/// Return the default search directories for CLAP and VST3 plugins on Linux.
pub fn default_search_dirs() -> Vec<(PathBuf, PluginFormat)> {
    let mut dirs = Vec::new();

    if let Some(home) = std::env::var_os("HOME").map(PathBuf::from) {
        dirs.push((home.join(".clap"), PluginFormat::Clap));
        dirs.push((home.join(".vst3"), PluginFormat::Vst3));
    }

    dirs.push((PathBuf::from("/usr/lib/clap"), PluginFormat::Clap));
    dirs.push((PathBuf::from("/usr/lib/vst3"), PluginFormat::Vst3));
    dirs.push((PathBuf::from("/usr/local/lib/clap"), PluginFormat::Clap));
    dirs.push((PathBuf::from("/usr/local/lib/vst3"), PluginFormat::Vst3));

    dirs
}

/// Scan all default directories and return discovered plugins.
pub fn scan_plugins() -> Vec<ScannedPlugin> {
    let mut plugins = Vec::new();
    let search_dirs = default_search_dirs();
    let roots: Vec<&Path> = search_dirs.iter().map(|(p, _)| p.as_path()).collect();

    for (dir, format) in &search_dirs {
        scan_dir(dir, *format, &roots, &mut plugins);
    }

    plugins.sort_by(|a, b| {
        a.vendor
            .to_lowercase()
            .cmp(&b.vendor.to_lowercase())
            .then_with(|| a.name.to_lowercase().cmp(&b.name.to_lowercase()))
    });
    plugins
}

fn scan_dir(dir: &Path, format: PluginFormat, roots: &[&Path], out: &mut Vec<ScannedPlugin>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };

    let ext = match format {
        PluginFormat::Clap => "clap",
        PluginFormat::Vst3 => "vst3",
    };

    for entry in entries.flatten() {
        let path = entry.path();

        if path.extension().is_some_and(|e| e == ext) {
            let name = path
                .file_stem()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();

            // Derive vendor from parent directory.
            // If parent is a search root, vendor is empty (top-level plugin).
            let vendor = path
                .parent()
                .filter(|parent| !roots.contains(parent))
                .and_then(|p| p.file_name())
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_default();

            out.push(ScannedPlugin {
                name,
                vendor,
                path,
                format,
            });
        } else if path.is_dir() {
            scan_dir(&path, format, roots, out);
        }
    }
}
