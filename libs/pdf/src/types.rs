use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum RotationAngle {
    None = 0,      // 0 degrees
    Rotate90 = 1,  // 90 degrees (clockwise)
    Rotate180 = 2, // 180 degrees
    Rotate270 = 3, // 270 degrees (clockwise)
}

impl RotationAngle {
    pub fn next(self) -> Self {
        match self {
            RotationAngle::None => RotationAngle::Rotate90,
            RotationAngle::Rotate90 => RotationAngle::Rotate180,
            RotationAngle::Rotate180 => RotationAngle::Rotate270,
            RotationAngle::Rotate270 => RotationAngle::None,
        }
    }
    
    pub fn to_degrees(self) -> f32 {
        match self {
            RotationAngle::None => 0.0,
            RotationAngle::Rotate90 => 90.0,
            RotationAngle::Rotate180 => 180.0,
            RotationAngle::Rotate270 => 270.0,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PdfPageData {
    pub image_data: String,
    pub text_elements: Vec<TextElement>,
    pub page_width: f32,
    pub page_height: f32,
    pub page_index: usize, // For contamination checking
    pub rotation: RotationAngle, // Page rotation state
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextElement {
    pub text: String,
    pub bounds: TextBounds,
    pub font_size: f32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TextBounds {
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}

