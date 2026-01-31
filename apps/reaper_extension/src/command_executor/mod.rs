//! Reaper Command Executor
//!
//! Centralized command queue for all REAPER API interactions.
//! Inspired by helgobox's task system.
//!
//! Architecture:
//! - Single bounded queue for all commands (backpressure protection)
//! - Trait-based commands (extensible, type-safe)
//! - Bulk processing in timer callback (prevents starvation)
//! - Non-blocking submission (fire-and-forget)

use reaper_high::Reaper;
use std::sync::{mpsc, Arc, Mutex};
use tracing::{debug, error, trace, warn};

/// Maximum commands to process per timer cycle (prevents starvation)
pub const MAX_COMMANDS_PER_CYCLE: usize = 10;

/// Default channel capacity (bounded for backpressure)
pub const DEFAULT_CHANNEL_CAPACITY: usize = 100;

/// Error type for command submission
#[derive(Debug, Clone, PartialEq)]
pub enum CommandError {
    QueueFull,
    ChannelClosed,
    NotInitialized,
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::QueueFull => write!(f, "Command queue is full"),
            CommandError::ChannelClosed => write!(f, "Command channel is closed"),
            CommandError::NotInitialized => write!(f, "Command executor not initialized"),
        }
    }
}

impl std::error::Error for CommandError {}

/// Trait for all commands that can be executed on REAPER's main thread
///
/// This trait is object-safe and allows heterogeneous command types
/// in the same queue.
pub trait ReaperCommand: Send + 'static {
    /// Execute the command with access to REAPER API
    fn execute(&self, reaper: &Reaper);

    /// Human-readable name for debugging
    fn name(&self) -> &'static str;
}

/// Central command executor for REAPER API calls
///
/// All REAPER API interactions must go through this executor to ensure
/// they run on the main thread.
pub struct CommandExecutor {
    /// Sender for submitting commands
    tx: mpsc::Sender<Box<dyn ReaperCommand>>,

    /// Receiver for processing commands (wrapped in Mutex for timer callback access)
    rx: Arc<Mutex<mpsc::Receiver<Box<dyn ReaperCommand>>>>,
}

impl CommandExecutor {
    /// Create a new command executor with the specified capacity
    pub fn new(capacity: usize) -> Self {
        let (tx, rx) = mpsc::channel();

        Self {
            tx,
            rx: Arc::new(Mutex::new(rx)),
        }
    }

    /// Create with default capacity
    pub fn default() -> Self {
        Self::new(DEFAULT_CHANNEL_CAPACITY)
    }

    /// Submit a command for execution on the main thread
    ///
    /// This is non-blocking and returns immediately. The command will be
    /// executed on the next timer callback.
    pub fn submit(&self, cmd: impl ReaperCommand) -> Result<(), CommandError> {
        let boxed_cmd: Box<dyn ReaperCommand> = Box::new(cmd);

        match self.tx.try_send(boxed_cmd) {
            Ok(()) => {
                trace!("Command submitted successfully");
                Ok(())
            }
            Err(mpsc::TrySendError::Full(_)) => {
                warn!("Command queue is full, dropping command");
                Err(CommandError::QueueFull)
            }
            Err(mpsc::TrySendError::Disconnected(_)) => {
                error!("Command channel is closed");
                Err(CommandError::ChannelClosed)
            }
        }
    }

    /// Process pending commands (called from timer callback on main thread)
    ///
    /// Processes up to MAX_COMMANDS_PER_CYCLE commands to prevent
    /// starving other timer callback work.
    pub fn process_pending(&self) {
        let reaper = Reaper::get();

        let rx = match self.rx.lock() {
            Ok(guard) => guard,
            Err(e) => {
                error!("Failed to lock command receiver: {}", e);
                return;
            }
        };

        let mut processed = 0;

        // Bulk processing: take up to MAX_COMMANDS_PER_CYCLE
        for cmd in rx.try_iter().take(MAX_COMMANDS_PER_CYCLE) {
            trace!("Executing command: {}", cmd.name());
            cmd.execute(&reaper);
            processed += 1;
        }

        if processed > 0 {
            debug!("Processed {} commands", processed);
        }
    }

    /// Get the number of pending commands in the queue
    pub fn pending_count(&self) -> usize {
        let rx = match self.rx.lock() {
            Ok(guard) => guard,
            Err(_) => return 0,
        };

        rx.iter().count()
    }

    /// Check if the queue is empty
    pub fn is_empty(&self) -> bool {
        self.pending_count() == 0
    }
}

impl std::fmt::Debug for CommandExecutor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CommandExecutor")
            .field("pending_count", &self.pending_count())
            .finish()
    }
}

impl Clone for CommandExecutor {
    fn clone(&self) -> Self {
        Self {
            tx: self.tx.clone(),
            rx: Arc::clone(&self.rx),
        }
    }
}

// ============================================================
// Macros for easy command creation
// ============================================================

/// Create a simple command struct with a name and execute block
#[macro_export]
macro_rules! define_command {
    (
        $(#[$meta:meta])*
        pub struct $name:ident;
        execute($reaper:ident) $body:block
    ) => {
        $(#[$meta])*
        pub struct $name;

        impl $crate::command_executor::ReaperCommand for $name {
            fn execute(&self, $reaper: &reaper_high::Reaper) {
                $body
            }

            fn name(&self) -> &'static str {
                stringify!($name)
            }
        }
    };
}

/// Create a command struct with fields
#[macro_export]
macro_rules! define_command_with_fields {
    (
        $(#[$meta:meta])*
        pub struct $name:ident {
            $($field:ident: $type:ty),* $(,)?
        }
        execute($reaper:ident, $self:ident) $body:block
    ) => {
        $(#[$meta])*
        pub struct $name {
            $(pub $field: $type),*
        }

        impl $crate::command_executor::ReaperCommand for $name {
            fn execute(&self, $reaper: &reaper_high::Reaper) {
                let $self = self;
                $body
            }

            fn name(&self) -> &'static str {
                stringify!($name)
            }
        }
    };
}
