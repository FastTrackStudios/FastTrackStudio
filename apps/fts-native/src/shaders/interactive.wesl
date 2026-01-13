// Interactive particle shader
// Uses WESL imports for shared utilities

import package::common::{screen_to_clip, quad_vertex, soft_circle_alpha};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
};

struct Particle {
    position: vec2<f32>,
    size: f32,
    _pad: f32,
    color: vec4<f32>,
};

@group(0) @binding(0)
var<uniform> screen_size: vec2<f32>;

@group(0) @binding(1)
var<storage, read> particles: array<Particle>;

@vertex
fn vs_main(
    @builtin(vertex_index) vertex_idx: u32,
    @builtin(instance_index) instance_idx: u32
) -> VertexOutput {
    let particle = particles[instance_idx];

    // Generate quad vertices using shared utility
    let quad_pos = quad_vertex(vertex_idx);
    let local_pos = (quad_pos - 0.5) * particle.size * 2.0;

    // Convert to clip space using shared utility
    let world_pos = particle.position + local_pos;
    let clip_pos = screen_to_clip(world_pos, screen_size);

    var output: VertexOutput;
    output.position = vec4<f32>(clip_pos, 0.0, 1.0);
    output.uv = quad_pos * 2.0 - 1.0; // -1 to 1 for circle calculation
    output.color = particle.color;
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Draw a soft circle using shared utility
    let alpha = soft_circle_alpha(input.uv, 0.8, 1.0);

    // Conditional debug mode - show debug visualization
    @if(DEBUG) {
        // In debug mode, show UV coordinates as color
        return vec4<f32>(input.uv * 0.5 + 0.5, 1.0, 1.0);
    }

    if alpha <= 0.0 {
        discard;
    }

    return vec4<f32>(input.color.rgb, input.color.a * alpha);
}
