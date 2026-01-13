// Multi-pass bloom post-processing shader
// Based on the physically-based bloom from learnopengl.com and Call of Duty presentations
// Uses progressive downsample/upsample with proper filtering

// Vertex output structure for full-screen quad
struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

// Bloom parameters
struct BloomParams {
    threshold: f32,      // Brightness threshold
    knee: f32,           // Soft knee for threshold
    intensity: f32,      // Final bloom intensity
    enabled: f32,        // 1.0 = enabled, 0.0 = pass-through
}

// Prefilter pass bindings
@group(0) @binding(0) var input_texture: texture_2d<f32>;
@group(0) @binding(1) var input_sampler: sampler;
@group(0) @binding(2) var<uniform> params: BloomParams;

const EPSILON: f32 = 0.0001;

// Quadratic threshold with soft knee
fn quadratic_threshold(color: vec3<f32>, threshold: f32, knee: f32) -> vec3<f32> {
    let brightness = max(max(color.r, color.g), color.b);

    // Calculate soft threshold curve
    let soft = brightness - threshold + knee;
    let soft_clamped = clamp(soft, 0.0, 2.0 * knee);
    let soft_weight = soft_clamped * soft_clamped / (4.0 * knee + EPSILON);

    // Blend between soft and hard threshold
    let contribution = max(soft_weight, brightness - threshold) / max(brightness, EPSILON);

    return color * max(contribution, 0.0);
}

// 13-tap box downsample filter (reduces fireflies)
// Sample pattern from Call of Duty presentation
fn downsample_box13(tex: texture_2d<f32>, samp: sampler, uv: vec2<f32>, texel_size: vec2<f32>) -> vec3<f32> {
    // Center
    let A = textureSample(tex, samp, uv).rgb;

    // Inner samples (weight 0.5 total)
    let B = textureSample(tex, samp, uv + texel_size * vec2<f32>(-1.0, -1.0)).rgb;
    let C = textureSample(tex, samp, uv + texel_size * vec2<f32>(-1.0, 1.0)).rgb;
    let D = textureSample(tex, samp, uv + texel_size * vec2<f32>(1.0, -1.0)).rgb;
    let E = textureSample(tex, samp, uv + texel_size * vec2<f32>(1.0, 1.0)).rgb;

    // Outer samples (weight 0.5 total, distributed)
    let F = textureSample(tex, samp, uv + texel_size * vec2<f32>(-2.0, -2.0)).rgb;
    let G = textureSample(tex, samp, uv + texel_size * vec2<f32>(-2.0, 0.0)).rgb;
    let H = textureSample(tex, samp, uv + texel_size * vec2<f32>(-2.0, 2.0)).rgb;
    let I = textureSample(tex, samp, uv + texel_size * vec2<f32>(0.0, -2.0)).rgb;
    let J = textureSample(tex, samp, uv + texel_size * vec2<f32>(0.0, 2.0)).rgb;
    let K = textureSample(tex, samp, uv + texel_size * vec2<f32>(2.0, -2.0)).rgb;
    let L = textureSample(tex, samp, uv + texel_size * vec2<f32>(2.0, 0.0)).rgb;
    let M = textureSample(tex, samp, uv + texel_size * vec2<f32>(2.0, 2.0)).rgb;

    // Weighted average
    var result = (B + C + D + E) * 0.125;  // Inner = 0.5 / 4
    result += (F + H + K + M) * 0.03125;   // Corners = 0.125 / 4
    result += (G + I + J + L) * 0.0625;    // Edges = 0.25 / 4
    result += A * 0.125;                    // Center

    return result;
}

// Simpler 4-tap bilinear downsample (for lower mips)
fn downsample_box4(tex: texture_2d<f32>, samp: sampler, uv: vec2<f32>, texel_size: vec2<f32>) -> vec3<f32> {
    let offset = texel_size * 0.5;

    let a = textureSample(tex, samp, uv + vec2<f32>(-offset.x, -offset.y)).rgb;
    let b = textureSample(tex, samp, uv + vec2<f32>(offset.x, -offset.y)).rgb;
    let c = textureSample(tex, samp, uv + vec2<f32>(-offset.x, offset.y)).rgb;
    let d = textureSample(tex, samp, uv + vec2<f32>(offset.x, offset.y)).rgb;

    return (a + b + c + d) * 0.25;
}

// 9-tap tent upsample filter
fn upsample_tent9(tex: texture_2d<f32>, samp: sampler, uv: vec2<f32>, texel_size: vec2<f32>) -> vec3<f32> {
    let d = texel_size;

    // 3x3 tent kernel
    var result = textureSample(tex, samp, uv + vec2<f32>(-d.x, -d.y)).rgb * 1.0;
    result += textureSample(tex, samp, uv + vec2<f32>(0.0, -d.y)).rgb * 2.0;
    result += textureSample(tex, samp, uv + vec2<f32>(d.x, -d.y)).rgb * 1.0;

    result += textureSample(tex, samp, uv + vec2<f32>(-d.x, 0.0)).rgb * 2.0;
    result += textureSample(tex, samp, uv).rgb * 4.0;
    result += textureSample(tex, samp, uv + vec2<f32>(d.x, 0.0)).rgb * 2.0;

    result += textureSample(tex, samp, uv + vec2<f32>(-d.x, d.y)).rgb * 1.0;
    result += textureSample(tex, samp, uv + vec2<f32>(0.0, d.y)).rgb * 2.0;
    result += textureSample(tex, samp, uv + vec2<f32>(d.x, d.y)).rgb * 1.0;

    return result / 16.0;
}

// ============================================
// PREFILTER PASS - Extract bright areas
// ============================================
@fragment
fn fs_prefilter(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_size = vec2<f32>(textureDimensions(input_texture));
    let texel_size = 1.0 / tex_size;

    // Downsample with 13-tap filter
    let color = downsample_box13(input_texture, input_sampler, in.uv, texel_size);

    // Apply threshold
    let bright = quadratic_threshold(color, params.threshold, params.knee);

    return vec4<f32>(bright, 1.0);
}

// ============================================
// DOWNSAMPLE PASS - Progressive downsample
// ============================================
@fragment
fn fs_downsample(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_size = vec2<f32>(textureDimensions(input_texture));
    let texel_size = 1.0 / tex_size;

    let color = downsample_box4(input_texture, input_sampler, in.uv, texel_size);

    return vec4<f32>(color, 1.0);
}

// ============================================
// UPSAMPLE PASS - Upsample and combine
// Reads from smaller mip, combines with current level
// ============================================
// For upsample, we need two textures:
// - input_texture: the smaller mip to upsample
// - bloom_texture: the current level to combine with (bound at binding 3)
@group(0) @binding(3) var bloom_texture: texture_2d<f32>;

@fragment
fn fs_upsample(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_size = vec2<f32>(textureDimensions(input_texture));
    let texel_size = 1.0 / tex_size;

    // Upsample the smaller texture
    let upsampled = upsample_tent9(input_texture, input_sampler, in.uv, texel_size);

    // Get existing color at this level
    let existing = textureSample(bloom_texture, input_sampler, in.uv).rgb;

    // Combine with lerp (intensity acts as combine factor)
    let combined = mix(existing, upsampled, 0.6);

    return vec4<f32>(combined, 1.0);
}

// ============================================
// COMPOSITE PASS - Blend bloom with original
// ============================================
@fragment
fn fs_composite(in: VertexOutput) -> @location(0) vec4<f32> {
    let original = textureSample(input_texture, input_sampler, in.uv);

    // Get bloom result from upsample chain
    let bloom = textureSample(bloom_texture, input_sampler, in.uv).rgb;

    // Add bloom to original with intensity
    let result = original.rgb + bloom * params.intensity;

    return vec4<f32>(result, original.a);
}

// ============================================
// PASSTHROUGH - Simple copy for disabled state
// ============================================
@fragment
fn fs_passthrough(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(input_texture, input_sampler, in.uv);
}
