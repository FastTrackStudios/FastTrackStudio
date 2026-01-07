// Meter shader - renders stereo level meters as vertical bars

struct Uniforms {
    resolution: vec2<f32>,
    left_level: f32,
    right_level: f32,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

// Fullscreen triangle vertex shader
@vertex
fn vs_main(@builtin(vertex_index) vertex_index: u32) -> VertexOutput {
    var out: VertexOutput;
    
    // Generate fullscreen triangle vertices
    let x = f32(i32(vertex_index & 1u) * 4 - 1);
    let y = f32(i32(vertex_index >> 1u) * 4 - 1);
    
    out.position = vec4<f32>(x, y, 0.0, 1.0);
    out.uv = vec2<f32>((x + 1.0) * 0.5, (1.0 - y) * 0.5);
    
    return out;
}

// Fragment shader - draws two vertical meter bars
@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let uv = in.uv;
    
    // Meter layout constants
    let meter_width = 0.35;      // Width of each meter bar (normalized)
    let gap = 0.1;               // Gap between meters (normalized)
    let margin = 0.1;            // Margin on sides (normalized)
    let corner_radius = 0.02;    // Rounded corner radius
    
    // Calculate meter positions
    let total_meter_width = meter_width * 2.0 + gap;
    let start_x = (1.0 - total_meter_width) * 0.5;
    
    let left_x_min = start_x;
    let left_x_max = start_x + meter_width;
    let right_x_min = start_x + meter_width + gap;
    let right_x_max = start_x + meter_width * 2.0 + gap;
    
    // Vertical margins
    let y_min = margin;
    let y_max = 1.0 - margin;
    
    // Check if we're in the left meter
    let in_left = uv.x >= left_x_min && uv.x <= left_x_max && uv.y >= y_min && uv.y <= y_max;
    
    // Check if we're in the right meter
    let in_right = uv.x >= right_x_min && uv.x <= right_x_max && uv.y >= y_min && uv.y <= y_max;
    
    if !in_left && !in_right {
        discard;
    }
    
    // Calculate the level for this meter
    var level: f32;
    var local_x: f32;
    
    if in_left {
        level = uniforms.left_level;
        local_x = (uv.x - left_x_min) / meter_width;
    } else {
        level = uniforms.right_level;
        local_x = (uv.x - right_x_min) / meter_width;
    }
    
    // Calculate local Y position (0 at bottom, 1 at top)
    let local_y = 1.0 - (uv.y - y_min) / (y_max - y_min);
    
    // Background color (dark)
    let bg_color = vec3<f32>(0.1, 0.1, 0.12);
    
    // Meter colors based on level
    // Green for low levels, yellow for medium, red for high
    var meter_color: vec3<f32>;
    if local_y < 0.6 {
        // Green zone (0-60%)
        meter_color = vec3<f32>(0.2, 0.8, 0.3);
    } else if local_y < 0.85 {
        // Yellow zone (60-85%)
        meter_color = vec3<f32>(0.9, 0.8, 0.2);
    } else {
        // Red zone (85-100%)
        meter_color = vec3<f32>(0.9, 0.2, 0.2);
    }
    
    // Determine if this pixel should be lit
    let is_lit = local_y <= level;
    
    // Choose color
    var final_color: vec3<f32>;
    if is_lit {
        final_color = meter_color;
    } else {
        // Dim version of the meter color for unlit segments
        final_color = meter_color * 0.15;
    }
    
    // Add subtle segmentation (horizontal lines)
    let segment_count = 30.0;
    let segment_y = fract(local_y * segment_count);
    let segment_gap = 0.1;
    if segment_y < segment_gap {
        final_color = final_color * 0.7;
    }
    
    return vec4<f32>(final_color, 1.0);
}
