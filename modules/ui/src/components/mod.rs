pub mod layout;
pub mod transport;
pub mod progress;
pub mod song;
pub mod sidebar_items;
pub mod mode_toggle;
pub mod lyrics;
pub mod text_fit;
pub mod piano;
pub mod midi_editor;
pub mod syllable_editor;
pub mod arrangement;
pub mod slider;
pub mod ruler;
pub mod tcp;
pub mod chords;

// Re-export commonly used components
pub use layout::{TopBar, Sidebar, MainContent, EditViewMode};
pub use transport::{TransportControlBar, ConnectionStatus};
pub use progress::{SongProgressBar, SectionProgressBar, SegmentedProgressBar, CompactProgressBar, ProgressSection, TempoMarker};
pub use song::{SongTitle, FadedSongTitle};
pub use mode_toggle::ModeToggle;
pub use lyrics::{LyricsView, LyricsEditView, PerformancePreview};
pub use text_fit::{TextFit, TextFitMode, TextFitProps};
pub use piano::{Piano, PianoProps, MidiNote};
pub use midi_editor::MidiEditor;
pub use syllable_editor::{SyllableEditor, SyllableEditorProps, SyllableKey};
pub use arrangement::ArrangementView;
pub use slider::Slider;
pub use ruler::{Ruler, MeasureInfo};
pub use tcp::TrackControlPanel;
pub use chords::{ChordsView, ChordsViewProps};

