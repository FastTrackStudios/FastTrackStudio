pub mod arrangement;
pub mod chord_tab;
pub mod chords;
pub mod layout;
pub mod lyrics;
pub mod midi_editor;
pub mod mode_toggle;
pub mod piano;
pub mod progress;
pub mod ruler;
pub mod section_progress;
pub mod sidebar_items;
pub mod slider;
pub mod song;
pub mod syllable_editor;
pub mod tcp;
pub mod text_fit;
pub mod transport;

// Re-export commonly used components
pub use arrangement::ArrangementView;
pub use chord_tab::{ChordTabView, ChordTabViewProps};
pub use chords::{ChordsView, ChordsViewProps};
pub use layout::{EditViewMode, MainContent, Sidebar, TopBar};
pub use lyrics::{LyricsEditView, LyricsView, PerformancePreview};
pub use midi_editor::MidiEditor;
pub use mode_toggle::ModeToggle;
pub use piano::{MidiNote, Piano, PianoProps};
pub use progress::{
    CompactProgressBar, MeasureIndicator, ProgressSection, SegmentedProgressBar, SongProgressBar,
    TempoCard, TempoMarker, TimeSignatureCard,
};
pub use ruler::{MeasureInfo, Ruler};
pub use section_progress::SectionProgressBar;
pub use slider::Slider;
pub use song::{FadedSongTitle, SongTitle};
pub use syllable_editor::{SyllableEditor, SyllableEditorProps, SyllableKey};
pub use tcp::TrackControlPanel;
pub use text_fit::{TextFit, TextFitMode, TextFitProps};
pub use transport::{ConnectionStatus, TransportControlBar};
