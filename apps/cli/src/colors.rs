use ratatui::style::Color;

// Color palette based on Tailwind-inspired colors
// Each color group has 5 shades: [brightest, bright, medium, dark, darkest]
const COLOR_PALETTE: &[&[Color]] = &[
    // Red (#b91c1c)
    &[
        Color::Rgb(255, 148, 166), // #ff94a6
        Color::Rgb(255, 54, 54),   // #ff3636
        Color::Rgb(226, 103, 90),  // #e2675a
        Color::Rgb(198, 146, 139), // #c6928b
        Color::Rgb(175, 51, 51),   // #af3333
    ],
    // Orange (#c2410c)
    &[
        Color::Rgb(255, 82, 41),  // #ff5229
        Color::Rgb(246, 108, 3),  // #f66c03
        Color::Rgb(255, 163, 116), // #ffa374
        Color::Rgb(183, 130, 86), // #b78256
        Color::Rgb(169, 81, 49),  // #a95131
    ],
    // Amber (#b45309)
    &[
        Color::Rgb(204, 153, 39), // #cc9927
        Color::Rgb(153, 114, 75), // #99724b
        Color::Rgb(211, 173, 113), // #d3ad71
        Color::Rgb(153, 131, 106), // #99836a
        Color::Rgb(114, 79, 65),  // #724f41
    ],
    // Yellow (#a16207)
    &[
        Color::Rgb(247, 244, 124), // #f7f47c
        Color::Rgb(255, 240, 52),  // #fff034
        Color::Rgb(237, 255, 174), // #edffae
        Color::Rgb(191, 186, 105), // #bfba69
        Color::Rgb(219, 195, 0),   // #dbc300
    ],
    // Lime (#4d7c0f)
    &[
        Color::Rgb(191, 251, 0),  // #bffb00
        Color::Rgb(135, 255, 103), // #87ff67
        Color::Rgb(210, 228, 152), // #d2e498
        Color::Rgb(166, 190, 0),  // #a6be00
        Color::Rgb(133, 150, 31), // #85961f
    ],
    // Green (#15803d)
    &[
        Color::Rgb(26, 255, 47),  // #1aff2f
        Color::Rgb(61, 195, 0),   // #3dc300
        Color::Rgb(186, 208, 116), // #bad074
        Color::Rgb(125, 176, 77), // #7db04d
        Color::Rgb(83, 159, 49),  // #539f31
    ],
    // Emerald (#047857)
    &[
        Color::Rgb(37, 255, 168), // #25ffa8
        Color::Rgb(0, 191, 175),  // #00bfaf
        Color::Rgb(155, 196, 141), // #9bc48d
        Color::Rgb(136, 194, 186), // #88c2ba
        Color::Rgb(10, 156, 142), // #0a9c8e
    ],
    // Teal (#0f766e)
    &[
        Color::Rgb(92, 255, 232), // #5cffe8
        Color::Rgb(25, 233, 255),  // #19e9ff
        Color::Rgb(212, 253, 225), // #d4fde1
        Color::Rgb(155, 179, 196), // #9bb3c4
        Color::Rgb(35, 99, 132),  // #236384
    ],
    // Sky (#0369a1)
    &[
        Color::Rgb(139, 197, 255), // #8bc5ff
        Color::Rgb(16, 164, 238),  // #10a4ee
        Color::Rgb(205, 241, 248), // #cdf1f8
        Color::Rgb(133, 165, 194), // #85a5c2
        Color::Rgb(26, 47, 150),  // #1a2f96
    ],
    // Blue (#1d4ed8)
    &[
        Color::Rgb(84, 128, 228), // #5480e4
        Color::Rgb(0, 125, 192),  // #007dc0
        Color::Rgb(185, 193, 227), // #b9c1e3
        Color::Rgb(131, 147, 204), // #8393cc
        Color::Rgb(47, 82, 162),  // #2f52a2
    ],
    // Violet (#6d28d9)
    &[
        Color::Rgb(146, 167, 255), // #92a7ff
        Color::Rgb(136, 108, 228), // #886ce4
        Color::Rgb(205, 187, 228), // #cdbbe4
        Color::Rgb(165, 149, 181), // #a595b5
        Color::Rgb(98, 75, 173),  // #624bad
    ],
    // Purple (#7e22ce)
    &[
        Color::Rgb(216, 108, 228), // #d86ce4
        Color::Rgb(182, 119, 198), // #b677c6
        Color::Rgb(174, 152, 229), // #ae98e5
        Color::Rgb(191, 159, 190), // #bf9fbe
        Color::Rgb(163, 75, 173),  // #a34bad
    ],
    // Pink (#be185d)
    &[
        Color::Rgb(229, 83, 160), // #e553a0
        Color::Rgb(255, 57, 212), // #ff39d4
        Color::Rgb(229, 220, 225), // #e5dce1
        Color::Rgb(188, 113, 150), // #bc7196
        Color::Rgb(204, 46, 110), // #cc2e6e
    ],
    // Gray (#374151)
    &[
        Color::Rgb(255, 255, 255), // #ffffff
        Color::Rgb(208, 208, 208), // #d0d0d0
        Color::Rgb(169, 169, 169), // #a9a9a9
        Color::Rgb(123, 123, 123), // #7b7b7b
        Color::Rgb(60, 60, 60),   // #3c3c3c
    ],
];

// Darkening factor for main colors (adjust this to make all colors darker/lighter)
const MAIN_COLOR_DARKEN_FACTOR: f32 = 0.85; // 85% brightness - adjust this value

// Brightness factor for muted colors (higher = brighter)
const MUTED_COLOR_BRIGHTNESS: f32 = 0.50; // 50% brightness - adjust this value

pub fn get_song_color(index: usize) -> Color {
    // Use the brightest shade (index 0) for song colors, then darken slightly
    let palette_idx = index % (COLOR_PALETTE.len() - 1); // Exclude gray
    let base_color = COLOR_PALETTE[palette_idx][0];
    darken_color(base_color, MAIN_COLOR_DARKEN_FACTOR)
}

pub fn get_song_color_muted(index: usize) -> Color {
    // Use a darker shade for muted song colors
    let palette_idx = index % (COLOR_PALETTE.len() - 1);
    COLOR_PALETTE[palette_idx][4] // Darkest shade
}

pub fn get_song_color_bright(index: usize) -> Color {
    // Use the bright shade (index 1) for progress bars, then darken slightly
    let palette_idx = index % (COLOR_PALETTE.len() - 1);
    let base_color = COLOR_PALETTE[palette_idx][1];
    darken_color(base_color, MAIN_COLOR_DARKEN_FACTOR)
}

pub fn get_section_color(song_idx: usize, section_idx: usize) -> Color {
    // Combine song and section indices for unique colors, then darken slightly
    let palette_idx = (song_idx * 10 + section_idx) % (COLOR_PALETTE.len() - 1);
    let base_color = COLOR_PALETTE[palette_idx][0]; // Brightest shade
    darken_color(base_color, MAIN_COLOR_DARKEN_FACTOR)
}

pub fn get_section_color_muted(song_idx: usize, section_idx: usize) -> Color {
    // Use the darkest shade and brighten it
    let palette_idx = (song_idx * 10 + section_idx) % (COLOR_PALETTE.len() - 1);
    let darkest = COLOR_PALETTE[palette_idx][4];
    darken_color(darkest, MUTED_COLOR_BRIGHTNESS) // Brighten muted colors
}

pub fn get_section_color_bright(song_idx: usize, section_idx: usize) -> Color {
    // Use the bright shade (index 1) for progress bars, then darken slightly
    let palette_idx = (song_idx * 10 + section_idx) % (COLOR_PALETTE.len() - 1);
    let base_color = COLOR_PALETTE[palette_idx][1];
    darken_color(base_color, MAIN_COLOR_DARKEN_FACTOR)
}

pub fn darken_color(color: Color, factor: f32) -> Color {
    // Extract RGB values from color and darken them
    let (r, g, b) = match color {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Black => (0, 0, 0),
        Color::Red => (187, 0, 0),
        Color::Green => (0, 187, 0),
        Color::Yellow => (187, 187, 0),
        Color::Blue => (0, 0, 187),
        Color::Magenta => (187, 0, 187),
        Color::Cyan => (0, 187, 187),
        Color::Gray => (136, 136, 136),
        Color::DarkGray => (85, 85, 85),
        Color::LightRed => (255, 85, 85),
        Color::LightGreen => (85, 255, 85),
        Color::LightYellow => (255, 255, 85),
        Color::LightBlue => (85, 85, 255),
        Color::LightMagenta => (255, 85, 255),
        Color::LightCyan => (85, 255, 255),
        Color::White => (238, 238, 238),
        Color::Indexed(_) => (128, 128, 128), // Fallback for indexed colors
        Color::Reset => (0, 0, 0),
    };
    
    // Darken the RGB values
    let r_dark = (r as f32 * factor) as u8;
    let g_dark = (g as f32 * factor) as u8;
    let b_dark = (b as f32 * factor) as u8;
    
    Color::Rgb(r_dark, g_dark, b_dark)
}

fn ansi_to_rgb(color: Color) -> Color {
    // Convert ANSI colors to RGB values for consistent rendering
    match color {
        Color::Black => Color::Rgb(0, 0, 0),
        Color::Red => Color::Rgb(187, 0, 0),
        Color::Green => Color::Rgb(0, 187, 0),
        Color::Yellow => Color::Rgb(187, 187, 0),
        Color::Blue => Color::Rgb(0, 0, 187),
        Color::Magenta => Color::Rgb(187, 0, 187),
        Color::Cyan => Color::Rgb(0, 187, 187),
        Color::Gray => Color::Rgb(136, 136, 136),
        Color::DarkGray => Color::Rgb(85, 85, 85),
        Color::LightRed => Color::Rgb(255, 85, 85),
        Color::LightGreen => Color::Rgb(85, 255, 85),
        Color::LightYellow => Color::Rgb(255, 255, 85),
        Color::LightBlue => Color::Rgb(85, 85, 255),
        Color::LightMagenta => Color::Rgb(255, 85, 255),
        Color::LightCyan => Color::Rgb(85, 255, 255),
        Color::White => Color::Rgb(238, 238, 238),
        Color::Rgb(..) => color, // Already RGB
        Color::Indexed(_) => Color::Rgb(128, 128, 128), // Fallback
        Color::Reset => Color::Rgb(0, 0, 0),
    }
}

pub fn brighten_color(color: Color) -> Color {
    // Return a brighter version of the color for progress bars
    match color {
        Color::DarkGray => Color::Gray,
        Color::Gray => Color::White,
        Color::Red => Color::LightRed,
        Color::Green => Color::LightGreen,
        Color::Yellow => Color::LightYellow,
        Color::Blue => Color::LightBlue,
        Color::Magenta => Color::LightMagenta,
        Color::Cyan => Color::LightCyan,
        _ => color,
    }
}
