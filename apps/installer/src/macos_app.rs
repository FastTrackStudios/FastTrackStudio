//! macOS app install/update/uninstall — the counterpart of `layout.rs` for
//! the notarized `.dmg` release built by `apps/fasttrackstudio/ios/deploy-macos.sh`
//! (see the `macos-desktop` job in `.github/workflows/release-binaries.yml`).
//!
//! Unlike the Linux tarball, this is a real distributable: Developer-ID
//! signed, notarized, and stapled, so no SHA256SUMS check is needed — the
//! codesign/notarization chain IS the integrity + authenticity check.
//!
//! Installed layout (prefix defaults to `$HOME`):
//!
//! ```text
//! <prefix>/Applications/<Name>.app          the app bundle (whatever dx bundled it as)
//! <prefix>/.local/bin/fasttrackstudio        symlink -> .app/Contents/MacOS/<executable>
//! <prefix>/.local/share/fts/macos-app-version
//! ```
//!
//! Known gaps (see README): the release doesn't ship a separate `fts` CLI
//! for macOS yet (only the `fasttrackstudio` GUI/`--engine` binary), and
//! there's no `reaper_fts_extensions.dylib` build for macOS yet, so unlike
//! the Linux install this never touches the REAPER rig dirs.

use std::fs;
use std::path::{Path, PathBuf};

use eyre::{Context, eyre};

use crate::{fetch, github};

pub struct MacApp {
    prefix: PathBuf,
}

impl MacApp {
    pub fn new(prefix: Option<PathBuf>) -> eyre::Result<Self> {
        let prefix = match prefix {
            Some(p) => p,
            None => std::env::var_os("HOME").map(PathBuf::from).ok_or_else(|| eyre!("$HOME is not set"))?,
        };
        Ok(Self { prefix })
    }

    fn applications_dir(&self) -> PathBuf {
        self.prefix.join("Applications")
    }

    fn bin_dir(&self) -> PathBuf {
        self.prefix.join(".local/bin")
    }

    fn state_dir(&self) -> PathBuf {
        self.prefix.join(".local/share/fts")
    }

    fn version_file(&self) -> PathBuf {
        self.state_dir().join("macos-app-version")
    }

    /// Path of the currently installed .app, if any (recorded alongside the
    /// version so uninstall/update know what to remove — the bundle name
    /// isn't fixed, it's whatever `dx build` produced).
    fn app_name_file(&self) -> PathBuf {
        self.state_dir().join("macos-app-name")
    }

    pub fn installed_version(&self) -> Option<String> {
        let v = fs::read_to_string(self.version_file()).ok()?;
        let v = v.trim().to_string();
        (!v.is_empty()).then_some(v)
    }

    fn installed_app_path(&self) -> Option<PathBuf> {
        let name = fs::read_to_string(self.app_name_file()).ok()?;
        let name = name.trim();
        (!name.is_empty()).then(|| self.applications_dir().join(name))
    }

    /// Install (or reinstall) from a downloaded/mounted .dmg.
    pub fn install(&self, dmg: &Path, tag: &str) -> eyre::Result<()> {
        let mount = MountedVolume::attach(dmg)?;
        let app = find_app_bundle(mount.path())
            .ok_or_else(|| eyre!("no .app bundle found in {}", dmg.display()))?;
        let app_name = app
            .file_name()
            .ok_or_else(|| eyre!("app bundle has no filename"))?
            .to_owned();

        let apps_dir = self.applications_dir();
        fs::create_dir_all(&apps_dir).wrap_err_with(|| format!("creating {}", apps_dir.display()))?;
        let dest = apps_dir.join(&app_name);
        if dest.exists() {
            fs::remove_dir_all(&dest).wrap_err_with(|| format!("removing old {}", dest.display()))?;
        }
        copy_tree(&app, &dest).wrap_err_with(|| format!("copying {} to {}", app.display(), dest.display()))?;
        drop(mount);

        // Clear the quarantine attribute macOS stamped on the downloaded
        // .dmg's contents — the app is notarized/stapled, so this is safe
        // (Gatekeeper would otherwise still re-flag the copy on first launch
        // in some macOS versions since xattrs aren't guaranteed to be
        // stripped by a plain copy).
        let _ = std::process::Command::new("xattr")
            .args(["-dr", "com.apple.quarantine"])
            .arg(&dest)
            .status();

        let exe = find_executable(&dest)
            .ok_or_else(|| eyre!("no executable found in {}/Contents/MacOS", dest.display()))?;
        let bin_dir = self.bin_dir();
        fs::create_dir_all(&bin_dir).wrap_err_with(|| format!("creating {}", bin_dir.display()))?;
        let link_name = exe.file_name().unwrap();
        force_symlink(&exe, &bin_dir.join(link_name))?;

        let state = self.state_dir();
        fs::create_dir_all(&state).wrap_err_with(|| format!("creating {}", state.display()))?;
        fs::write(self.version_file(), format!("{tag}\n"))?;
        fs::write(self.app_name_file(), format!("{}\n", app_name.to_string_lossy()))?;

        println!("  {} -> {}", app_name.to_string_lossy(), dest.display());
        println!(
            "  symlink -> {}",
            bin_dir.join(link_name).display()
        );
        Ok(())
    }

    pub fn uninstall(&self) -> eyre::Result<()> {
        if let Some(app_path) = self.installed_app_path()
            && let Some(exe) = find_executable(&app_path)
            && let Some(name) = exe.file_name()
        {
            let _ = fs::remove_file(self.bin_dir().join(name));
        }
        if let Some(app_path) = self.installed_app_path()
            && app_path.exists()
        {
            fs::remove_dir_all(&app_path).wrap_err_with(|| format!("removing {}", app_path.display()))?;
        }
        let _ = fs::remove_file(self.version_file());
        let _ = fs::remove_file(self.app_name_file());
        println!(
            "uninstalled FastTrackStudio.app from {} (user data in ~/.config/fts and ~/.config/signal kept)",
            self.applications_dir().display()
        );
        Ok(())
    }
}

/// Resolve + download the macOS app .dmg, then install it. `url` bypasses
/// the release feed entirely (a direct dmg URL), mirroring `install_tarball`
/// on Linux.
pub async fn install(prefix: Option<PathBuf>, version: Option<String>, url: Option<String>) -> eyre::Result<()> {
    let app = MacApp::new(prefix)?;
    let client = fetch::http_client()?;
    let work = tempfile_dir()?;

    let (dmg_url, dmg_name, tag) = match url {
        Some(url) => {
            let name = url
                .rsplit('/')
                .next()
                .filter(|n| !n.is_empty())
                .ok_or_else(|| eyre!("--url has no filename component: {url}"))?
                .to_string();
            (url, name, None)
        }
        None => {
            let release = github::resolve_macos_dmg(&client, version.as_deref()).await?;
            println!("release: {} ({})", release.tag, release.tarball.name);
            (release.tarball.url, release.tarball.name, Some(release.tag))
        }
    };

    let dmg_path = work.join(&dmg_name);
    fetch::download(&client, &dmg_url, &dmg_path, &dmg_name).await?;
    app.install(&dmg_path, tag.as_deref().unwrap_or("local"))?;
    let _ = fs::remove_dir_all(&work);

    match tag {
        Some(tag) => println!("installed FastTrackStudio {tag}"),
        None => println!("installed FastTrackStudio from {dmg_url}"),
    }
    Ok(())
}

pub async fn update(prefix: Option<PathBuf>) -> eyre::Result<()> {
    let app = MacApp::new(prefix.clone())?;
    let client = fetch::http_client()?;
    let release = github::resolve_macos_dmg(&client, None).await?;
    let latest = release.tag.trim_start_matches('v').to_string();

    match app.installed_version() {
        Some(installed) if installed.trim_start_matches('v') == latest => {
            println!("FastTrackStudio {installed} is already the latest release — nothing to do");
            return Ok(());
        }
        Some(installed) => println!("updating {installed} -> {} ...", release.tag),
        None => println!("no installed version found — installing {} ...", release.tag),
    }

    let work = tempfile_dir()?;
    let dmg_path = work.join(&release.tarball.name);
    fetch::download(&client, &release.tarball.url, &dmg_path, &release.tarball.name).await?;
    app.install(&dmg_path, &release.tag)?;
    let _ = fs::remove_dir_all(&work);
    println!("installed FastTrackStudio {}", release.tag);
    Ok(())
}

pub fn uninstall(prefix: Option<PathBuf>) -> eyre::Result<()> {
    MacApp::new(prefix)?.uninstall()
}

fn tempfile_dir() -> eyre::Result<PathBuf> {
    let dir = std::env::temp_dir().join(format!("fts-installer-macos-{}", std::process::id()));
    fs::create_dir_all(&dir).wrap_err_with(|| format!("creating {}", dir.display()))?;
    Ok(dir)
}

/// A `hdiutil`-mounted .dmg, detached automatically on drop.
struct MountedVolume {
    mount_point: PathBuf,
}

impl MountedVolume {
    fn attach(dmg: &Path) -> eyre::Result<Self> {
        let mount_point = std::env::temp_dir().join(format!("fts-installer-app-dmg-{}", std::process::id()));
        fs::create_dir_all(&mount_point).wrap_err_with(|| format!("creating {}", mount_point.display()))?;
        let status = std::process::Command::new("hdiutil")
            .args(["attach", "-nobrowse", "-quiet", "-mountpoint"])
            .arg(&mount_point)
            .arg(dmg)
            .status()
            .wrap_err("running hdiutil attach")?;
        if !status.success() {
            eyre::bail!("hdiutil attach failed for {}", dmg.display());
        }
        Ok(Self { mount_point })
    }

    fn path(&self) -> &Path {
        &self.mount_point
    }
}

impl Drop for MountedVolume {
    fn drop(&mut self) {
        let _ = std::process::Command::new("hdiutil")
            .args(["detach", "-quiet"])
            .arg(&self.mount_point)
            .status();
    }
}

fn find_app_bundle(dir: &Path) -> Option<PathBuf> {
    fs::read_dir(dir).ok()?.flatten().map(|e| e.path()).find(|p| {
        p.is_dir() && p.extension().and_then(|e| e.to_str()) == Some("app")
    })
}

/// The single executable dx puts at `<app>/Contents/MacOS/<name>`.
fn find_executable(app: &Path) -> Option<PathBuf> {
    let macos_dir = app.join("Contents/MacOS");
    fs::read_dir(macos_dir).ok()?.flatten().map(|e| e.path()).find(|p| p.is_file())
}

fn force_symlink(target: &Path, link: &Path) -> eyre::Result<()> {
    match fs::remove_file(link) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => return Err(e).wrap_err_with(|| format!("removing {}", link.display())),
    }
    #[cfg(unix)]
    std::os::unix::fs::symlink(target, link)
        .wrap_err_with(|| format!("symlinking {} -> {}", link.display(), target.display()))?;
    #[cfg(not(unix))]
    let _ = (target, link);
    Ok(())
}

fn copy_tree(src: &Path, dest: &Path) -> eyre::Result<()> {
    fs::create_dir_all(dest)?;
    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let to = dest.join(entry.file_name());
        let file_type = entry.file_type()?;
        if file_type.is_symlink() {
            let target = fs::read_link(entry.path())?;
            #[cfg(unix)]
            std::os::unix::fs::symlink(&target, &to)?;
        } else if file_type.is_dir() {
            copy_tree(&entry.path(), &to)?;
        } else {
            fs::copy(entry.path(), &to)?;
            if let Ok(meta) = fs::metadata(entry.path()) {
                let _ = fs::set_permissions(&to, meta.permissions());
            }
        }
    }
    Ok(())
}
