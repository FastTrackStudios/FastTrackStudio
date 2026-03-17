//! Progress event types for communicating install status to the UI.

#[derive(Debug, Clone)]
pub enum InstallEvent {
    StepStarted { step: InstallStep, label: String },
    StepProgress { step: InstallStep, fraction: f32, message: String },
    StepCompleted(InstallStep),
    StepFailed { step: InstallStep, error: String },
    AllCompleted,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InstallStep {
    DownloadReaper,
    ExtractDmg,
    CopyExtension,
    DownloadLibrary,
    WriteReaperIni,
    InstallFtsControl,
    SetupShell,
}

impl InstallStep {
    pub fn label(&self) -> &'static str {
        match self {
            Self::DownloadReaper => "Download REAPER",
            Self::ExtractDmg => "Extract REAPER",
            Self::CopyExtension => "Install extension",
            Self::DownloadLibrary => "Download library",
            Self::WriteReaperIni => "Configure REAPER",
            Self::InstallFtsControl => "Install FTS Control",
            Self::SetupShell => "Set up PATH",
        }
    }

    pub fn all() -> &'static [InstallStep] {
        &[
            Self::DownloadReaper,
            Self::ExtractDmg,
            Self::CopyExtension,
            Self::DownloadLibrary,
            Self::WriteReaperIni,
            Self::InstallFtsControl,
            Self::SetupShell,
        ]
    }
}

pub type EventSender = tokio::sync::mpsc::Sender<InstallEvent>;
pub type EventReceiver = tokio::sync::mpsc::Receiver<InstallEvent>;
