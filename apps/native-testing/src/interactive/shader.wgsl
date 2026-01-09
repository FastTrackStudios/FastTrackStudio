// Simple shader for rendering colored circles/particles

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

    // Generate quad vertices (triangle strip: 0,1,2,3 -> positions)
    let x = f32(vertex_idx & 1u);
    let y = f32((vertex_idx >> 1u) & 1u);
    let local_pos = vec2<f32>(x - 0.5, y - 0.5) * particle.size * 2.0;

    // Convert to clip space
    let world_pos = particle.position + local_pos;
    let clip_pos = vec2<f32>(
        (world_pos.x / screen_size.x) * 2.0 - 1.0,
        1.0 - (world_pos.y / screen_size.y) * 2.0
    );

    var output: VertexOutput;
    output.position = vec4<f32>(clip_pos, 0.0, 1.0);
    output.uv = vec2<f32>(x, y) * 2.0 - 1.0; // -1 to 1 for circle calculation
    output.color = particle.color;
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Draw a circle
    let dist = length(input.uv);
    if dist > 1.0 {
        discard;
    }

    // Soft edge
    let alpha = 1.0 - smoothstep(0.8, 1.0, dist);
    return vec4<f32>(input.color.rgb, input.color.a * alpha);
}
