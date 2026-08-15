//! FTS plugin-bundle install/uninstall/list (issue #31).
//!
//! The plugin bundle is the `fts-plugins-v*-<platform>.<ext>` release
//! asset — a `.tar.gz` of `target/bundled/` on Linux (`just
//! plugins-package`), a Developer-ID-signed + notarized `.zip` on macOS
//! (`apps/fasttrackstudio/ios/deploy-macos-plugins.sh` — notarization
//! only accepts zip/pkg/dmg submissions, not tar.gz): `FTS <Name>.clap`
//! files and `FTS <Name>.vst3` bundle directories either way.
//!
//! Install targets are the per-user plugin dirs:
//!
//! ```text
//! Linux:  <prefix>/.clap/FTS <Name>.clap
//!         <prefix>/.vst3/FTS <Name>.vst3/
//! macOS:  <prefix>/Library/Audio/Plug-Ins/CLAP/FTS <Name>.clap
//!         <prefix>/Library/Audio/Plug-Ins/VST3/FTS <Name>.vst3/
//! ```
//!
//! No AU (Audio Unit) build exists yet — CLAP/VST3 only, both platforms.
//! The macOS zip is signed but NOT stapled (stapling only works on
//! .app/.pkg/.dmg); Gatekeeper falls back to an online notarization-ticket
//! check on first load, which needs network access at that point.
//!
//! An install manifest (`<prefix>/.local/share/fts/plugins/MANIFEST` +
//! `VERSION`) records what we put there, so `uninstall` removes exactly
//! that set and `update`-style checks can compare versions. This same
//! module is the engine the desktop app's plugin manager drives; the CLI
//! works with no other FTS software installed.

use std::fs;
use std::path::{Path, PathBuf};

use eyre::{Context, eyre};

use crate::{codeberg, fetch};

pub struct PluginDirs {
    pub clap: PathBuf,
    pub vst3: PathBuf,
    /// Manifest dir: `<prefix>/.local/share/fts/plugins`.
    pub state: PathBuf,
}

impl PluginDirs {
    pub fn new(prefix: Option<PathBuf>) -> eyre::Result<Self> {
        let home = match prefix {
            Some(p) => p,
            None => dirs_home()?,
        };
        if cfg!(target_os = "macos") {
            return Ok(Self {
                clap: home.join("Library/Audio/Plug-Ins/CLAP"),
                vst3: home.join("Library/Audio/Plug-Ins/VST3"),
                state: home.join("Library/Application Support/fts/plugins"),
            });
        }
        Ok(Self {
            clap: home.join(".clap"),
            vst3: home.join(".vst3"),
            state: home.join(".local/share/fts/plugins"),
        })
    }

    fn manifest(&self) -> PathBuf {
        self.state.join("MANIFEST")
    }

    fn version_file(&self) -> PathBuf {
        self.state.join("VERSION")
    }
}

/// Install (or reinstall) the plugin bundle.
///
/// `from`: a local `.tar.gz` or an already-extracted directory — skips the
/// release API entirely. Otherwise the bundle asset is resolved from the
/// release feed (latest, or `version` tag), downloaded, and SHA256-verified
/// when the release carries a SHA256SUMS.
pub async fn install(
    version: Option<String>,
    from: Option<PathBuf>,
    prefix: Option<PathBuf>,
) -> eyre::Result<()> {
    let dirs = PluginDirs::new(prefix)?;

    let stage = StageDir::new()?;
    let (bundle_root, tag): (PathBuf, String) = match from {
        Some(path) if path.is_dir() => (path, "local".to_string()),
        Some(path) => {
            let extract = stage.path().join("bundle");
            fetch::extract_archive(&path, &extract)?;
            (extract, "local".to_string())
        }
        None => {
            let client = fetch::http_client()?;
            let release = if cfg!(target_os = "macos") {
                codeberg::resolve_macos_plugins_zip(&client, version.as_deref()).await?
            } else {
                codeberg::resolve_with_prefix(&client, version.as_deref(), "fts-plugins-").await?
            };
            let tarball_path = stage.path().join(&release.tarball.name);
            fetch::download(
                &client,
                &release.tarball.url,
                &tarball_path,
                &release.tarball.name,
            )
            .await?;
            if let Some(sums) = &release.sums {
                let sums_text = fetch::fetch_text(&client, &sums.url).await?;
                fetch::verify_sha256(&tarball_path, &release.tarball.name, &sums_text)?;
            }
            let extract = stage.path().join("bundle");
            fetch::extract_archive(&tarball_path, &extract)?;
            (extract, release.tag)
        }
    };

    // Collect the bundles: .clap files, .vst3 directories.
    let mut installed: Vec<String> = Vec::new();
    fs::create_dir_all(&dirs.clap).wrap_err("creating ~/.clap")?;
    fs::create_dir_all(&dirs.vst3).wrap_err("creating ~/.vst3")?;
    for entry in fs::read_dir(&bundle_root).wrap_err("reading bundle contents")? {
        let entry = entry?;
        let name = entry.file_name().to_string_lossy().into_owned();
        let src = entry.path();
        if name.ends_with(".clap") {
            let dest = dirs.clap.join(&name);
            copy_any(&src, &dest)?;
            installed.push(format!("clap/{name}"));
        } else if name.ends_with(".vst3") {
            let dest = dirs.vst3.join(&name);
            copy_any(&src, &dest)?;
            installed.push(format!("vst3/{name}"));
        }
    }
    if installed.is_empty() {
        return Err(eyre!(
            "no .clap/.vst3 bundles found in {}",
            bundle_root.display()
        ));
    }
    installed.sort();

    // The macOS zip is signed + notarized but not stapled (only
    // .app/.pkg/.dmg support stapling); a lingering quarantine xattr on the
    // extracted bundles would otherwise force every host's Gatekeeper
    // check to hit the network on first load. Best-effort, non-fatal.
    if cfg!(target_os = "macos") {
        for name in &installed {
            let path = match name.split_once('/') {
                Some(("clap", n)) => dirs.clap.join(n),
                Some(("vst3", n)) => dirs.vst3.join(n),
                _ => continue,
            };
            let _ = std::process::Command::new("xattr")
                .args(["-dr", "com.apple.quarantine"])
                .arg(&path)
                .status();
        }
    }

    fs::create_dir_all(&dirs.state)?;
    fs::write(dirs.manifest(), installed.join("\n") + "\n")?;
    fs::write(dirs.version_file(), format!("{tag}\n"))?;

    println!("installed {} plugin bundles ({tag}):", installed.len());
    for name in &installed {
        println!("  {name}");
    }
    Ok(())
}

/// Remove everything the manifest says we installed.
pub fn uninstall(prefix: Option<PathBuf>) -> eyre::Result<()> {
    let dirs = PluginDirs::new(prefix)?;
    let manifest = dirs.manifest();
    let listing = fs::read_to_string(&manifest)
        .wrap_err_with(|| format!("no plugin install manifest at {}", manifest.display()))?;
    let mut removed = 0usize;
    for line in listing.lines().filter(|l| !l.is_empty()) {
        let path = match line.split_once('/') {
            Some(("clap", name)) => dirs.clap.join(name),
            Some(("vst3", name)) => dirs.vst3.join(name),
            _ => continue,
        };
        if path.is_dir() {
            fs::remove_dir_all(&path).wrap_err_with(|| format!("removing {}", path.display()))?;
            removed += 1;
        } else if path.exists() {
            fs::remove_file(&path).wrap_err_with(|| format!("removing {}", path.display()))?;
            removed += 1;
        }
    }
    let _ = fs::remove_file(&manifest);
    let _ = fs::remove_file(dirs.version_file());
    println!("removed {removed} plugin bundles");
    Ok(())
}

/// Print what is installed (per the manifest) and the recorded version.
pub fn list(prefix: Option<PathBuf>) -> eyre::Result<()> {
    let dirs = PluginDirs::new(prefix)?;
    match fs::read_to_string(dirs.version_file()) {
        Ok(v) => println!("version: {}", v.trim()),
        Err(_) => println!("version: (not installed)"),
    }
    if let Ok(listing) = fs::read_to_string(dirs.manifest()) {
        for line in listing.lines().filter(|l| !l.is_empty()) {
            println!("  {line}");
        }
    }
    Ok(())
}

fn dirs_home() -> eyre::Result<PathBuf> {
    std::env::var_os("HOME")
        .map(PathBuf::from)
        .ok_or_else(|| eyre!("$HOME is not set"))
}

/// Copy a file or directory tree, replacing any existing destination.
fn copy_any(src: &Path, dest: &Path) -> eyre::Result<()> {
    if dest.is_dir() {
        fs::remove_dir_all(dest)?;
    } else if dest.exists() {
        fs::remove_file(dest)?;
    }
    if src.is_dir() {
        copy_tree(src, dest)?;
    } else {
        fs::copy(src, dest).wrap_err_with(|| format!("copying {}", src.display()))?;
    }
    Ok(())
}

fn copy_tree(src: &Path, dest: &Path) -> eyre::Result<()> {
    fs::create_dir_all(dest)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let to = dest.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&entry.path(), &to)?;
        } else {
            fs::copy(entry.path(), &to)?;
        }
    }
    Ok(())
}

/// Unique staging dir under the system temp root, removed on drop
/// (std-only stand-in for `tempfile::tempdir`).
struct StageDir(PathBuf);

impl StageDir {
    fn new() -> eyre::Result<Self> {
        let path = std::env::temp_dir().join(format!("fts-plugins-install-{}", std::process::id()));
        if path.exists() {
            fs::remove_dir_all(&path).ok();
        }
        fs::create_dir_all(&path).wrap_err("creating staging dir")?;
        Ok(Self(path))
    }

    fn path(&self) -> &Path {
        &self.0
    }
}

impl Drop for StageDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}
