//! Envelope data structures for REAPER

use serde::{Deserialize, Serialize};
use std::fmt;

/// Envelope point shapes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EnvelopePointShape {
    Linear = 0,
    Square = 1,
    SlowStartEnd = 2,
    FastStart = 3,
    FastEnd = 4,
    Bezier = 5,
    Default = -1, // Use envelope default
}

impl fmt::Display for EnvelopePointShape {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnvelopePointShape::Linear => write!(f, "Linear"),
            EnvelopePointShape::Square => write!(f, "Square"),
            EnvelopePointShape::SlowStartEnd => write!(f, "Slow Start/End"),
            EnvelopePointShape::FastStart => write!(f, "Fast Start"),
            EnvelopePointShape::FastEnd => write!(f, "Fast End"),
            EnvelopePointShape::Bezier => write!(f, "Bezier"),
            EnvelopePointShape::Default => write!(f, "Default"),
        }
    }
}

impl From<i32> for EnvelopePointShape {
    fn from(value: i32) -> Self {
        match value {
            0 => EnvelopePointShape::Linear,
            1 => EnvelopePointShape::Square,
            2 => EnvelopePointShape::SlowStartEnd,
            3 => EnvelopePointShape::FastStart,
            4 => EnvelopePointShape::FastEnd,
            5 => EnvelopePointShape::Bezier,
            -1 => EnvelopePointShape::Default,
            _ => EnvelopePointShape::Default,
        }
    }
}

impl From<i64> for EnvelopePointShape {
    fn from(value: i64) -> Self {
        Self::from(value as i32)
    }
}

/// Automation Item (AI) properties within an envelope
/// 
/// Automation items are reusable automation patterns that can be pooled and shared
/// across multiple envelope instances. They contain their own envelope points and
/// can be looped, have different play rates, and baseline/amplitude adjustments.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AutomationItem {
    /// Pool index - pooled instances have the same index
    /// (greater by 1 than the one displayed by default in the Name field of AI Properties window)
    pub pool_index: i32,
    
    /// Position in seconds
    pub position: f64,
    
    /// Length in seconds
    pub length: f64,
    
    /// Start offset in seconds
    pub start_offset: f64,
    
    /// Play rate (1.0 = normal speed)
    pub play_rate: f64,
    
    /// Selected state (bool)
    pub selected: bool,
    
    /// Baseline value (0 = -100%, 0.5 = 0%, 1 = 100%)
    pub baseline: f64,
    
    /// Amplitude (-2 = -200%, 1 = 100%, 2 = 200%)
    /// Baseline/amplitude affects pooled copies
    pub amplitude: f64,
    
    /// Loop enabled (bool)
    pub loop_enabled: bool,
    
    /// Position in quarter notes (only used in certain contexts)
    pub position_qn: f64,
    
    /// Length in quarter notes (only used in certain contexts)
    pub length_qn: f64,
    
    /// 1-based index of AI since starting the project, incremented even if
    /// older AIs were deleted and regardless of the AI being pooled
    pub instance_index: i32,
    
    /// Muted state (bool)
    pub muted: bool,
    
    /// Start offset in quarter notes (only used in certain contexts)
    pub start_offset_qn: f64,
    
    /// Transition time in seconds
    pub transition_time: f64,
    
    /// Volume envelope maximum when this instance was created
    /// (matches the 1|4 bits of the "volenvrange" config variable:
    /// 0=+6dB, 1=+0dB, 4=+12dB, 5=+24dB)
    pub volume_envelope_max: i32,
}

impl fmt::Display for AutomationItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "POOLEDENVINST {} {:.6} {:.6} {:.6} {:.6} {} {:.6} {:.6} {} {:.6} {:.6} {} {} {:.6} {:.6} {}",
            self.pool_index,
            self.position,
            self.length,
            self.start_offset,
            self.play_rate,
            if self.selected { 1 } else { 0 },
            self.baseline,
            self.amplitude,
            if self.loop_enabled { 1 } else { 0 },
            self.position_qn,
            self.length_qn,
            self.instance_index,
            if self.muted { 1 } else { 0 },
            self.start_offset_qn,
            self.transition_time,
            self.volume_envelope_max
        )
    }
}

/// Extension-specific persistent data block within an envelope
/// 
/// These blocks contain arbitrary data that can be added by extensions
/// using the GetSetEnvelopeInfo_String() function with 'P_EXT' parameter.
/// The structure is unknown and can vary, so we preserve the raw content.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtensionData {
    /// The parameter name (field 1)
    pub parmname: String,
    
    /// The string data (field 2)
    pub string_data: String,
    
    /// Raw content of the entire EXT block for preservation
    pub raw_content: String,
}

impl fmt::Display for ExtensionData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw_content)
    }
}

/// An envelope point - the lowest level component in RPP files
/// 
/// PT 3.000000 -0.2 5 0 0 0 -0.7
/// field 1, float, position (seconds)
/// field 2, float, value
/// field 3, int, shape (-1 = envelope default?)
/// field 4, int, optional, ?? (TEMPOENVEX time sig = 65536 * 
/// time signature denominator + time signature numerator)
/// field 5, int (bool), selected (optional)
/// field 6, int, ?? (optional)
/// field 7, float, bezier tension (optional)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnvelopePoint {
    pub position: f64,           // field 1: position in seconds
    pub value: f64,              // field 2: envelope value
    pub shape: EnvelopePointShape, // field 3: point shape
    pub time_sig: Option<i32>,   // field 4: time signature (TEMPOENVEX only)
    pub selected: Option<bool>,  // field 5: selected state
    pub unknown_field_6: Option<i32>, // field 6: unknown field
    pub bezier_tension: Option<f64>, // field 7: bezier tension
}

impl fmt::Display for EnvelopePoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PT {:.6} {:.6} {} ", 
               self.position, 
               self.value, 
               self.shape as i32)?;
        
        if let Some(ts) = self.time_sig {
            write!(f, "{} ", ts)?;
        }
        if let Some(sel) = self.selected {
            write!(f, "{} ", if sel { 1 } else { 0 })?;
        }
        if let Some(unk) = self.unknown_field_6 {
            write!(f, "{} ", unk)?;
        }
        if let Some(tension) = self.bezier_tension {
            write!(f, "{:.6}", tension)?;
        }
        
        Ok(())
    }
}

/// A REAPER envelope
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Envelope {
    pub envelope_type: String,
    pub guid: String,
    pub active: bool,
    pub visible: bool,
    pub show_in_lane: bool,
    pub lane_height: i32,
    pub armed: bool,
    pub default_shape: i32,
    pub points: Vec<EnvelopePoint>,
    pub automation_items: Vec<AutomationItem>,
    pub extension_data: Vec<ExtensionData>,
}

impl fmt::Display for Envelope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Envelope({}) - {} points, {} automation items, {} extension blocks", 
               self.envelope_type, self.points.len(), self.automation_items.len(), self.extension_data.len())
    }
}

