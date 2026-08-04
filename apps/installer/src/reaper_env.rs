//! `fts-installer reaper` — set up the complete REAPER environment on a
//! fresh machine: REAPER itself, SWS, ReaPack, and the THREE FTS rig
//! resource dirs, so a user never downloads anything by hand.
//!
//! Installed layout (prefix defaults to `$HOME`):
//!
//! ```text
//! <prefix>/.local/lib/fts/reaper/           portable REAPER (binary at REAPER/reaper)
//! <prefix>/fasttrackstudio/                 fts-reaper — main DAW (recording)
//! <prefix>/fts-tracks/                      fts-tracks — live tracks playback
//! <prefix>/fts-dev/                         fts-dev — dev/testing copy
//! <prefix>/fasttrackstudio/FastTrackStudio/launch.json   (fts-reaper + fts-tracks)
//! <prefix>/fts-dev/launch.json                            (fts-dev)
//! <prefix>/.local/share/applications/fts-{reaper,tracks,dev}.desktop
//! ```
//!
//! Every rig dir gets `UserPlugins/` with SWS + ReaPack and a minimal
//! `reaper.ini` (only written when missing — re-runs never clobber a
//! configured rig). REAPER/SWS/ReaPack are downloaded from their official
//! sources at install time (REAPER's license forbids redistributing it in
//! our artifacts).

use std::io::Read;
use std::path::{Path, PathBuf};

use eyre::{Context, eyre};

use crate::fetch;

/// Used when the cockos.com latest-version endpoint is unreachable.
const FALLBACK_REAPER_VERSION: &str = "7.75";
/// SWS official pre-release build (the same one the nix rig pins).
const SWS_VERSION: &str = "2.14.0.7";
const SWS_COMMIT: &str = "9daba634";
/// ReaPack release on cfillion's GitHub.
const REAPACK_VERSION: &str = "1.2.5.1";

struct Rig {
    id: &'static str,
    name: &'static str,
    comment: &'static str,
    role: &'static str,
    rig_type: &'static str,
    /// Resource dir, relative to the prefix.
    dir: &'static str,
}

/// THE three REAPER options — same set as `daw_profiles()` and the
/// fts-reaper-flake launchers. Instrument rigs come from the signal
/// engine, not REAPER.
const RIGS: &[Rig] = &[
    Rig {
        id: "fts-reaper",
        name: "FTS REAPER",
        comment: "FTS REAPER — main DAW instance for recording",
        role: "session",
        rig_type: "reaper",
        dir: "fasttrackstudio",
    },
    Rig {
        id: "fts-tracks",
        name: "FTS Tracks",
        comment: "FTS Tracks — live tracks playback instance",
        role: "tracks",
        rig_type: "session",
        dir: "fts-tracks",
    },
    Rig {
        id: "fts-dev",
        name: "FTS Dev",
        comment: "FTS Dev REAPER — isolated development/testing instance",
        role: "dev",
        rig_type: "dev",
        dir: "fts-dev",
    },
];

/// The rig resource dirs under `prefix` (`~/fasttrackstudio`,
/// `~/fts-tracks`, `~/fts-dev`) — where `reaper_fts_extensions.so` from
/// the app tarball belongs, in each rig's `UserPlugins/`. Used by
/// `layout::install` to lay the extension down after `reaper` has run;
/// a rig dir that doesn't exist yet (REAPER env not set up) is skipped.
pub fn rig_dirs(prefix: &Path) -> Vec<PathBuf> {
    RIGS.iter().map(|r| prefix.join(r.dir)).collect()
}

pub async fn setup(prefix: Option<PathBuf>) -> eyre::Result<()> {
    if !cfg!(target_os = "linux") && !cfg!(target_os = "macos") {
        eyre::bail!("`fts-installer reaper` currently supports Linux and macOS only");
    }

    let home = std::env::var_os("HOME")
        .map(PathBuf::from)
        .ok_or_else(|| eyre!("HOME is not set"))?;
    let is_real_home = prefix.is_none();
    let prefix = prefix.unwrap_or(home);

    let client = fetch::http_client()?;
    let work = work_dir()?;

    // 1. REAPER itself (portable install under .local/lib/fts/reaper).
    let reaper_root = prefix.join(".local/lib/fts/reaper");
    let version = latest_reaper_version(&client)
        .await
        .unwrap_or_else(|| FALLBACK_REAPER_VERSION.to_string());
    let reaper_exe = install_reaper(&client, &work, &reaper_root, &version).await?;

    // 2. The three rig resource dirs + extensions.
    let sws = download_sws(&client, &work).await?;
    let reapack = download_reapack(&client, &work).await?;
    for rig in RIGS {
        let dir = prefix.join(rig.dir);
        setup_rig_dir(&dir, &sws, &reapack)?;
        println!("  {} → {}", rig.id, dir.display());
    }

    // 3. launch.json files (same shape the fts-reaper-flake launchers use).
    write_launch_jsons(&prefix, &reaper_exe)?;

    // 4. Desktop entries (real-home installs only).
    if is_real_home {
        write_desktop_entries(&prefix, &reaper_exe)?;
    }

    let _ = std::fs::remove_dir_all(&work);
    println!("REAPER environment ready (REAPER {version})");
    println!("  launch: fts-reaper / fts-tracks / fts-dev desktop entries,");
    println!(
        "  or {} -cfgfile <rig>/reaper.ini -newinst",
        reaper_exe.display()
    );
    Ok(())
}

/// Latest stable REAPER version from cockos.com (first line of the
/// latestversion endpoint), or None on any failure.
async fn latest_reaper_version(client: &reqwest::Client) -> Option<String> {
    let text = fetch::fetch_text(client, "https://www.cockos.com/reaper/latestversion/")
        .await
        .ok()?;
    let version = text.lines().next()?.trim().to_string();
    (version.starts_with(|c: char| c.is_ascii_digit())
        && version.contains('.')
        && version.len() <= 10)
        .then_some(version)
}

/// Download and unpack REAPER into `reaper_root`; returns the binary path.
/// Skips the download when this exact version is already installed.
async fn install_reaper(
    client: &reqwest::Client,
    work: &Path,
    reaper_root: &Path,
    version: &str,
) -> eyre::Result<PathBuf> {
    let exe = if cfg!(target_os = "macos") {
        reaper_root.join("REAPER.app/Contents/MacOS/REAPER")
    } else {
        reaper_root.join("REAPER/reaper")
    };
    let version_file = reaper_root.join("VERSION");
    if exe.exists()
        && std::fs::read_to_string(&version_file)
            .map(|v| v.trim() == version)
            .unwrap_or(false)
    {
        println!("REAPER {version} already installed at {}", reaper_root.display());
        return Ok(exe);
    }

    let major = version.split('.').next().unwrap_or("7");
    let slug = version.replace('.', "");

    if reaper_root.exists() {
        std::fs::remove_dir_all(reaper_root)
            .wrap_err_with(|| format!("removing old {}", reaper_root.display()))?;
    }
    std::fs::create_dir_all(reaper_root)?;

    if cfg!(target_os = "macos") {
        // The macOS build is a single universal (arm64+x86_64) binary
        // inside REAPER.app, distributed as a .dmg.
        let url = format!("https://www.reaper.fm/files/{major}.x/reaper{slug}_universal.dmg");
        let dmg = work.join(format!("reaper{slug}.dmg"));
        fetch::download(client, &url, &dmg, &format!("REAPER {version}")).await?;
        let mount = MountedDmg::attach(&dmg)?;
        let app = find_dir_named(mount.path(), "REAPER.app")
            .ok_or_else(|| eyre!("REAPER.app not found in {url}"))?;
        copy_dir_recursive(&app, &reaper_root.join("REAPER.app"))?;
        drop(mount);
    } else {
        let arch = if cfg!(target_arch = "aarch64") { "aarch64" } else { "x86_64" };
        let url = format!("https://www.reaper.fm/files/{major}.x/reaper{slug}_linux_{arch}.tar.xz");
        let tarball = work.join(format!("reaper{slug}.tar.xz"));
        fetch::download(client, &url, &tarball, &format!("REAPER {version}")).await?;

        // The tarball unpacks to reaper_linux_<arch>/REAPER/…
        let stage = work.join("reaper-extracted");
        extract_tar_xz(&tarball, &stage)?;
        let reaper_dir = find_dir_named(&stage, "REAPER")
            .ok_or_else(|| eyre!("REAPER directory not found in {url}"))?;
        move_dir(&reaper_dir, &reaper_root.join("REAPER"))?;
    }

    std::fs::write(&version_file, format!("{version}\n"))?;
    println!("REAPER {version} installed at {}", reaper_root.display());
    Ok(exe)
}

/// Download SWS and return (extension .so/.dylib, python scripts) staged
/// in `work`.
async fn download_sws(
    client: &reqwest::Client,
    work: &Path,
) -> eyre::Result<(PathBuf, Vec<PathBuf>)> {
    if cfg!(target_os = "macos") {
        // SWS ships a universal (arm64+x86_64) .dmg on macOS, same
        // version/commit pin as the Linux pre-release build.
        let url = format!(
            "https://www.sws-extension.org/download/pre-release/sws-{SWS_VERSION}-macOS-{SWS_COMMIT}.dmg"
        );
        let dmg = work.join("sws.dmg");
        fetch::download(client, &url, &dmg, "SWS extension").await?;
        let mount = MountedDmg::attach(&dmg)?;
        let so = find_file_matching(mount.path(), |n| n.starts_with("reaper_sws") && n.ends_with(".dylib"))
            .ok_or_else(|| eyre!("reaper_sws .dylib not found in SWS dmg"))?;
        let scripts_found =
            find_files_matching(mount.path(), |n| n.starts_with("sws_python") && n.ends_with(".py"));
        // Copy out of the mount before it's detached — the paths above
        // become invalid once the disk image is gone.
        let stage = work.join("sws-staged");
        std::fs::create_dir_all(&stage)?;
        let so_dest = stage.join(so.file_name().unwrap());
        std::fs::copy(&so, &so_dest)?;
        let mut scripts = Vec::new();
        for script in &scripts_found {
            let dest = stage.join(script.file_name().unwrap());
            std::fs::copy(script, &dest)?;
            scripts.push(dest);
        }
        drop(mount);
        Ok((so_dest, scripts))
    } else {
        let arch = if cfg!(target_arch = "aarch64") { "aarch64" } else { "x86_64" };
        let url = format!(
            "https://www.sws-extension.org/download/pre-release/sws-{SWS_VERSION}-Linux-{arch}-{SWS_COMMIT}.tar.xz"
        );
        let tarball = work.join("sws.tar.xz");
        fetch::download(client, &url, &tarball, "SWS extension").await?;

        let stage = work.join("sws-extracted");
        extract_tar_xz(&tarball, &stage)?;

        let so = find_file_matching(&stage, |n| n.starts_with("reaper_sws") && n.ends_with(".so"))
            .ok_or_else(|| eyre!("reaper_sws .so not found in SWS archive"))?;
        let scripts =
            find_files_matching(&stage, |n| n.starts_with("sws_python") && n.ends_with(".py"));
        Ok((so, scripts))
    }
}

/// Download the ReaPack .so/.dylib into `work` and return its path.
async fn download_reapack(client: &reqwest::Client, work: &Path) -> eyre::Result<PathBuf> {
    let arch = if cfg!(target_arch = "aarch64") { "aarch64" } else { "x86_64" };
    let ext = if cfg!(target_os = "macos") { "dylib" } else { "so" };
    let filename = format!("reaper_reapack-{arch}.{ext}");
    let url = format!(
        "https://github.com/cfillion/reapack/releases/download/v{REAPACK_VERSION}/{filename}"
    );
    let dest = work.join(&filename);
    fetch::download(client, &url, &dest, "ReaPack").await?;
    Ok(dest)
}

/// Create one rig resource dir: UserPlugins (SWS + ReaPack), Scripts
/// (SWS python), and a minimal reaper.ini when none exists.
fn setup_rig_dir(dir: &Path, sws: &(PathBuf, Vec<PathBuf>), reapack: &Path) -> eyre::Result<()> {
    let user_plugins = dir.join("UserPlugins");
    let scripts = dir.join("Scripts");
    std::fs::create_dir_all(&user_plugins)?;
    std::fs::create_dir_all(&scripts)?;

    let (sws_so, sws_scripts) = sws;
    copy_into(sws_so, &user_plugins)?;
    copy_into(reapack, &user_plugins)?;
    for script in sws_scripts {
        copy_into(script, &scripts)?;
    }

    let ini = dir.join("reaper.ini");
    if !ini.exists() {
        std::fs::write(&ini, "[REAPER]\nautosaveint=0\n")
            .wrap_err_with(|| format!("writing {}", ini.display()))?;
    }
    Ok(())
}

/// launch.json files matching the fts-reaper-flake launchers: the main
/// one (fts-reaper + fts-tracks) at fasttrackstudio/FastTrackStudio/,
/// and fts-dev's own at fts-dev/launch.json.
fn write_launch_jsons(prefix: &Path, reaper_exe: &Path) -> eyre::Result<()> {
    let entry = |rig: &Rig| {
        let dir = prefix.join(rig.dir);
        serde_json::json!({
            "role": rig.role,
            "rig_type": rig.rig_type,
            "reaper_executable": reaper_exe.to_string_lossy(),
            "resources_dir": dir.to_string_lossy(),
            "ini_path": dir.join("reaper.ini").to_string_lossy(),
            "ini_overrides": { "undo_max_mem": 0 },
            "restore_ini_after_launch": false,
            "reaper_args": ["-newinst", "-nosplash", "-ignoreerrors"],
        })
    };

    let mut main = serde_json::Map::new();
    for rig in RIGS.iter().filter(|r| r.id != "fts-dev") {
        main.insert(rig.id.to_string(), entry(rig));
    }
    let main_dir = prefix.join("fasttrackstudio/FastTrackStudio");
    std::fs::create_dir_all(&main_dir)?;
    let main_path = main_dir.join("launch.json");
    std::fs::write(&main_path, serde_json::to_string_pretty(&main)?)?;
    println!("  launch.json → {}", main_path.display());

    let dev = RIGS.iter().find(|r| r.id == "fts-dev").expect("fts-dev rig");
    let dev_path = prefix.join("fts-dev/launch.json");
    std::fs::write(&dev_path, serde_json::to_string_pretty(&entry(dev))?)?;
    println!("  launch.json → {}", dev_path.display());
    Ok(())
}

/// Desktop entries launching REAPER directly with each rig's cfgfile
/// (no reaper-launcher dependency on stock distros). Linux (XDG) only —
/// macOS doesn't have an equivalent of a raw .desktop file; a real Launch
/// Services entry needs a wrapper .app bundle with its own Info.plist,
/// which isn't built yet. On macOS the rigs are still fully usable from a
/// terminal (run the REAPER binary directly with `-cfgfile`).
fn write_desktop_entries(prefix: &Path, reaper_exe: &Path) -> eyre::Result<()> {
    if cfg!(target_os = "macos") {
        println!(
            "  (macOS — no dock entries yet; launch with: {} -cfgfile <rig>/reaper.ini -newinst)",
            reaper_exe.display()
        );
        return Ok(());
    }

    let apps = prefix.join(".local/share/applications");
    std::fs::create_dir_all(&apps)?;
    for rig in RIGS {
        let ini = prefix.join(rig.dir).join("reaper.ini");
        let entry = format!(
            "[Desktop Entry]\n\
             Type=Application\n\
             Name={name}\n\
             Comment={comment}\n\
             Exec={exe} -cfgfile {ini} -newinst -nosplash -ignoreerrors\n\
             Terminal=false\n\
             Categories=AudioVideo;Audio;\n\
             StartupWMClass={id}\n\
             Keywords=reaper;daw;{rig_type};fasttrackstudio;\n",
            name = rig.name,
            comment = rig.comment,
            exe = reaper_exe.display(),
            ini = ini.display(),
            id = rig.id,
            rig_type = rig.rig_type,
        );
        std::fs::write(apps.join(format!("{}.desktop", rig.id)), entry)?;
    }
    // Refresh the desktop database when the tool is available.
    let _ = std::process::Command::new("update-desktop-database")
        .arg(&apps)
        .status();
    println!("  desktop entries → {}", apps.display());
    Ok(())
}

// ── archive / fs helpers ─────────────────────────────────────────────

/// A `hdiutil`-mounted .dmg, detached automatically on drop. Used for
/// REAPER's and SWS's macOS releases (both ship as .dmg, not .tar.xz).
/// Anything needed from the volume must be copied out before the guard
/// drops — the mount point stops existing once `hdiutil detach` runs.
struct MountedDmg {
    mount_point: PathBuf,
}

impl MountedDmg {
    fn attach(dmg: &Path) -> eyre::Result<Self> {
        let mount_point =
            std::env::temp_dir().join(format!("fts-installer-dmg-{}-{}", std::process::id(), fastrand_suffix()));
        std::fs::create_dir_all(&mount_point)
            .wrap_err_with(|| format!("creating mount point {}", mount_point.display()))?;
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

impl Drop for MountedDmg {
    fn drop(&mut self) {
        let _ = std::process::Command::new("hdiutil")
            .args(["detach", "-quiet"])
            .arg(&self.mount_point)
            .status();
    }
}

/// A cheap unique-enough suffix for temp dir names (no extra crate dep).
fn fastrand_suffix() -> u128 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0)
}

/// Extract a .tar.xz to `dest` (pure Rust: xz2 stream + tar).
fn extract_tar_xz(tarball: &Path, dest: &Path) -> eyre::Result<()> {
    std::fs::create_dir_all(dest)?;
    let file = std::fs::File::open(tarball)
        .wrap_err_with(|| format!("opening {}", tarball.display()))?;
    let mut decompressed = Vec::new();
    xz2::read::XzDecoder::new(file)
        .read_to_end(&mut decompressed)
        .wrap_err_with(|| format!("decompressing {}", tarball.display()))?;
    tar::Archive::new(decompressed.as_slice())
        .unpack(dest)
        .wrap_err_with(|| format!("unpacking {}", tarball.display()))?;
    Ok(())
}

fn find_dir_named(root: &Path, name: &str) -> Option<PathBuf> {
    walk(root, &mut |p| {
        (p.is_dir() && p.file_name().is_some_and(|n| n == name)).then(|| p.to_path_buf())
    })
}

fn find_file_matching(root: &Path, pred: impl Fn(&str) -> bool + Copy) -> Option<PathBuf> {
    walk(root, &mut |p| {
        (p.is_file()
            && p.file_name()
                .and_then(|n| n.to_str())
                .is_some_and(pred))
        .then(|| p.to_path_buf())
    })
}

fn find_files_matching(root: &Path, pred: impl Fn(&str) -> bool + Copy) -> Vec<PathBuf> {
    let mut found = Vec::new();
    let _ = walk(root, &mut |p| {
        if p.is_file() && p.file_name().and_then(|n| n.to_str()).is_some_and(pred) {
            found.push(p.to_path_buf());
        }
        None::<()>
    });
    found
}

/// Depth-first walk; returns the first Some produced by `f`.
fn walk<T>(dir: &Path, f: &mut impl FnMut(&Path) -> Option<T>) -> Option<T> {
    let entries = std::fs::read_dir(dir).ok()?;
    for entry in entries.flatten() {
        let path = entry.path();
        if let Some(hit) = f(&path) {
            return Some(hit);
        }
        if path.is_dir()
            && let Some(hit) = walk(&path, f) {
                return Some(hit);
            }
    }
    None
}

fn copy_into(src: &Path, dir: &Path) -> eyre::Result<()> {
    let name = src
        .file_name()
        .ok_or_else(|| eyre!("no filename in {}", src.display()))?;
    std::fs::copy(src, dir.join(name))
        .wrap_err_with(|| format!("copying {} into {}", src.display(), dir.display()))?;
    Ok(())
}

/// Move a directory, falling back to copy+delete across filesystems.
fn move_dir(src: &Path, dest: &Path) -> eyre::Result<()> {
    if std::fs::rename(src, dest).is_ok() {
        return Ok(());
    }
    copy_dir_recursive(src, dest)?;
    std::fs::remove_dir_all(src)?;
    Ok(())
}

fn copy_dir_recursive(src: &Path, dest: &Path) -> eyre::Result<()> {
    std::fs::create_dir_all(dest)?;
    for entry in std::fs::read_dir(src)?.flatten() {
        let from = entry.path();
        let to = dest.join(entry.file_name());
        if from.is_dir() {
            copy_dir_recursive(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
            // Preserve permissions (the reaper binary's executable bit).
            if let Ok(meta) = std::fs::metadata(&from) {
                let _ = std::fs::set_permissions(&to, meta.permissions());
            }
        }
    }
    Ok(())
}

fn work_dir() -> eyre::Result<PathBuf> {
    let dir = std::env::temp_dir().join(format!("fts-installer-reaper-{}", std::process::id()));
    std::fs::create_dir_all(&dir).wrap_err_with(|| format!("creating {}", dir.display()))?;
    Ok(dir)
}
