// Common shader utilities shared across all shaders
// Using WESL for modular shader development

// Convert screen coordinates to clip space (-1 to 1)
fn screen_to_clip(pos: vec2<f32>, screen_size: vec2<f32>) -> vec2<f32> {
    return vec2<f32>(
        (pos.x / screen_size.x) * 2.0 - 1.0,
        1.0 - (pos.y / screen_size.y) * 2.0
    );
}

// Generate quad vertex position from vertex index (triangle strip)
fn quad_vertex(vertex_idx: u32) -> vec2<f32> {
    let x = f32(vertex_idx & 1u);
    let y = f32((vertex_idx >> 1u) & 1u);
    return vec2<f32>(x, y);
}

// Soft circle alpha (for smooth particle edges)
fn soft_circle_alpha(uv: vec2<f32>, edge_start: f32, edge_end: f32) -> f32 {
    let dist = length(uv);
    if dist > edge_end {
        return 0.0;
    }
    return 1.0 - smoothstep(edge_start, edge_end, dist);
}

// Unpack RGBA color from u32
fn unpack_color(packed: u32) -> vec4<f32> {
    return vec4<f32>(
        (vec4<u32>(packed) >> vec4<u32>(0u, 8u, 16u, 24u)) & vec4<u32>(255u)
    ) / 255.0;
}
