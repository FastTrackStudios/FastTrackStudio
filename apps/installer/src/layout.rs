//! The installed layout — the Rust twin of the tarball's install.sh /
//! uninstall.sh (which remain the no-Rust fallback), and of the repo's
//! `just install` / `just uninstall` recipes.

use std::path::{Path, PathBuf};
use std::process::Command;

use eyre::{Context, eyre};

pub struct Layout {
    prefix: PathBuf,
    /// True when installing into the real $HOME: run the systemd /
    /// desktop-database refreshes. With a custom --prefix these are
    /// skipped (test installs).
    system_integration: bool,
}

impl Layout {
    pub fn new(prefix: Option<PathBuf>) -> eyre::Result<Self> {
        let home = std::env::var_os("HOME")
            .map(PathBuf::from)
            .ok_or_else(|| eyre!("$HOME is not set"))?;
        let (prefix, system_integration) = match prefix {
            Some(p) => {
                let is_home = p == home;
                (p, is_home)
            }
            None => (home, true),
        };
        Ok(Self { prefix, system_integration })
    }

    fn lib_dir(&self) -> PathBuf {
        self.prefix.join(".local/lib/fts")
    }
    fn bin_dir(&self) -> PathBuf {
        self.prefix.join(".local/bin")
    }
    fn unit_path(&self) -> PathBuf {
        self.prefix.join(".config/systemd/user/signal-engine.service")
    }
    fn desktop_path(&self) -> PathBuf {
        self.prefix.join(".local/share/applications/fasttrackstudio.desktop")
    }
    fn icon_path(&self) -> PathBuf {
        self.prefix
            .join(".local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg")
    }

    /// Version recorded by a previous install (contents of lib/fts/VERSION).
    pub fn installed_version(&self) -> Option<String> {
        let v = std::fs::read_to_string(self.lib_dir().join("VERSION")).ok()?;
        let v = v.trim().to_string();
        (!v.is_empty()).then_some(v)
    }

    /// Apply the layout from an extracted release tarball at `stage`.
    pub fn install(&self, stage: &Path) -> eyre::Result<()> {
        let lib = self.lib_dir();
        let bin = self.bin_dir();
        std::fs::create_dir_all(&lib)?;
        std::fs::create_dir_all(&bin)?;

        // Binaries: copy to .new then rename over — a running binary is
        // replaced atomically (same trick as `just install`).
        for name in ["fasttrackstudio", "fts"] {
            let src = stage.join(name);
            if !src.exists() {
                eyre::bail!("tarball is missing the {name} binary (looked at {})", src.display());
            }
            let tmp = lib.join(format!("{name}.new"));
            std::fs::copy(&src, &tmp).wrap_err_with(|| format!("copying {name}"))?;
            set_executable(&tmp)?;
            std::fs::rename(&tmp, lib.join(name))?;
            force_symlink(&lib.join(name), &bin.join(name))?;
        }

        // VERSION marker (used by `fts-installer update`).
        let version_src = stage.join("VERSION");
        if version_src.exists() {
            std::fs::copy(&version_src, lib.join("VERSION"))?;
        }

        // systemd user unit — installed but NOT enabled; the app (or an
        // explicit `systemctl --user start signal-engine`) is the on/off
        // switch.
        let unit = self.unit_path();
        std::fs::create_dir_all(unit.parent().unwrap())?;
        std::fs::copy(stage.join("signal-engine.service"), &unit)
            .wrap_err("installing signal-engine.service")?;

        // Icon.
        let icon = self.icon_path();
        std::fs::create_dir_all(icon.parent().unwrap())?;
        std::fs::copy(stage.join("icon.svg"), &icon).wrap_err("installing icon")?;

        // Desktop entry: substitute @BIN@ in the template.
        let template = std::fs::read_to_string(stage.join("fasttrackstudio.desktop"))
            .wrap_err("reading fasttrackstudio.desktop template")?;
        let desktop = self.desktop_path();
        std::fs::create_dir_all(desktop.parent().unwrap())?;
        std::fs::write(
            &desktop,
            template.replace("@BIN@", &lib.join("fasttrackstudio").to_string_lossy()),
        )?;

        if self.system_integration {
            // All best-effort: a missing systemctl / desktop tooling is
            // not an install failure.
            best_effort("systemctl", &["--user", "daemon-reload"]);
            best_effort("systemctl", &["--user", "try-restart", "signal-engine"]);
            best_effort(
                "update-desktop-database",
                &[&desktop.parent().unwrap().to_string_lossy()],
            );
            best_effort(
                "gtk-update-icon-cache",
                &[&self.prefix.join(".local/share/icons/hicolor").to_string_lossy()],
            );
        } else {
            println!(
                "  (custom prefix {} — systemd/desktop-database refresh skipped)",
                self.prefix.display()
            );
        }

        println!("  layout applied under {}", self.prefix.display());
        Ok(())
    }

    /// Reverse the install. User data (~/.config/fts, ~/.config/signal)
    /// is untouched.
    pub fn uninstall(&self) -> eyre::Result<()> {
        if self.system_integration {
            best_effort("systemctl", &["--user", "stop", "signal-engine"]);
            best_effort("systemctl", &["--user", "disable", "signal-engine"]);
        }
        remove_file_if_exists(&self.unit_path())?;
        if self.system_integration {
            best_effort("systemctl", &["--user", "daemon-reload"]);
        }

        for name in ["fasttrackstudio", "fts"] {
            remove_file_if_exists(&self.bin_dir().join(name))?;
        }
        let lib = self.lib_dir();
        if lib.exists() {
            std::fs::remove_dir_all(&lib).wrap_err_with(|| format!("removing {}", lib.display()))?;
        }
        remove_file_if_exists(&self.desktop_path())?;
        remove_file_if_exists(&self.icon_path())?;

        if self.system_integration {
            best_effort(
                "update-desktop-database",
                &[&self.desktop_path().parent().unwrap().to_string_lossy()],
            );
            best_effort(
                "gtk-update-icon-cache",
                &[&self.prefix.join(".local/share/icons/hicolor").to_string_lossy()],
            );
        }

        println!(
            "uninstalled from {} (user data in ~/.config/fts and ~/.config/signal kept)",
            self.prefix.display()
        );
        Ok(())
    }
}

fn set_executable(path: &Path) -> eyre::Result<()> {
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o755))?;
    }
    Ok(())
}

/// `ln -sf target link`.
fn force_symlink(target: &Path, link: &Path) -> eyre::Result<()> {
    remove_file_if_exists(link)?;
    #[cfg(unix)]
    std::os::unix::fs::symlink(target, link)
        .wrap_err_with(|| format!("symlinking {} -> {}", link.display(), target.display()))?;
    #[cfg(not(unix))]
    let _ = (target, link);
    Ok(())
}

fn remove_file_if_exists(path: &Path) -> eyre::Result<()> {
    match std::fs::remove_file(path) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e).wrap_err_with(|| format!("removing {}", path.display())),
    }
}

/// Run a command, ignoring absence and failure (mirrors the `|| true`
/// calls in the shell recipes).
fn best_effort(cmd: &str, args: &[&str]) {
    let _ = Command::new(cmd)
        .args(args)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status();
}
