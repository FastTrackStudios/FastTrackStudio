// Glow post-processing shader using WESL
// Applies a dramatic bloom/glow effect with color shifting

import package::common;

// Vertex output structure for full-screen quad
struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
}

// Uniforms for glow parameters
struct GlowParams {
    threshold: f32,      // Brightness threshold for glow (0.0 - 1.0)
    intensity: f32,      // Glow intensity multiplier
    radius: f32,         // Blur radius in pixels
    enabled: f32,        // 1.0 = enabled, 0.0 = pass-through
}

@group(0) @binding(0) var input_texture: texture_2d<f32>;
@group(0) @binding(1) var input_sampler: sampler;
@group(0) @binding(2) var<uniform> params: GlowParams;

// Calculate luminance of a color
fn luminance(color: vec3<f32>) -> f32 {
    return dot(color, vec3<f32>(0.299, 0.587, 0.114));
}

// Extract bright areas above threshold with boosted contribution
fn extract_bright(color: vec3<f32>, threshold: f32) -> vec3<f32> {
    let brightness = luminance(color);
    // More aggressive extraction - boost contribution
    let contribution = pow(max(brightness - threshold, 0.0) / max(1.0 - threshold, 0.001), 0.5);
    return color * contribution * 1.5;
}

// Shift color toward vibrant hues (cyan/magenta tint)
fn shift_color(color: vec3<f32>, amount: f32) -> vec3<f32> {
    // Create a more vibrant version by boosting saturation
    let lum = luminance(color);
    let saturated = mix(vec3<f32>(lum), color, 1.0 + amount * 0.5);

    // Add color tint - shift toward cyan/magenta
    let tint = vec3<f32>(
        color.r * 0.9 + color.b * 0.2,
        color.g * 1.1 + color.b * 0.1,
        color.b * 1.3 + color.r * 0.2
    );

    return mix(saturated, tint, amount * 0.3);
}

// 9-tap gaussian blur kernel weights (boosted)
const BLUR_WEIGHTS: array<f32, 5> = array<f32, 5>(
    0.30, 0.25, 0.18, 0.10, 0.05
);

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let tex_size = vec2<f32>(textureDimensions(input_texture));
    let original = textureSample(input_texture, input_sampler, in.uv);

    // Pass through if disabled
    if (params.enabled < 0.5) {
        return original;
    }

    let pixel_size = 1.0 / tex_size;

    // Multi-layer bloom for more dramatic effect
    var bloom_small = vec3<f32>(0.0);
    var bloom_medium = vec3<f32>(0.0);
    var bloom_large = vec3<f32>(0.0);

    // Small blur radius (sharp glow core)
    let small_radius = params.radius * 0.5;
    // Medium blur radius
    let medium_radius = params.radius * 1.5;
    // Large blur radius (soft outer glow)
    let large_radius = params.radius * 3.0;

    // Center sample
    let center_bright = extract_bright(original.rgb, params.threshold);
    bloom_small += center_bright * BLUR_WEIGHTS[0];
    bloom_medium += center_bright * BLUR_WEIGHTS[0];
    bloom_large += center_bright * BLUR_WEIGHTS[0];

    // Sample in a star pattern for each layer
    for (var i = 1; i < 5; i++) {
        let weight = BLUR_WEIGHTS[i];
        let fi = f32(i);

        // Small blur samples
        let small_offset = fi * small_radius;
        let small_h = vec2<f32>(small_offset * pixel_size.x, 0.0);
        let small_v = vec2<f32>(0.0, small_offset * pixel_size.y);

        bloom_small += extract_bright(textureSample(input_texture, input_sampler, in.uv + small_h).rgb, params.threshold) * weight;
        bloom_small += extract_bright(textureSample(input_texture, input_sampler, in.uv - small_h).rgb, params.threshold) * weight;
        bloom_small += extract_bright(textureSample(input_texture, input_sampler, in.uv + small_v).rgb, params.threshold) * weight;
        bloom_small += extract_bright(textureSample(input_texture, input_sampler, in.uv - small_v).rgb, params.threshold) * weight;

        // Medium blur samples
        let medium_offset = fi * medium_radius;
        let medium_h = vec2<f32>(medium_offset * pixel_size.x, 0.0);
        let medium_v = vec2<f32>(0.0, medium_offset * pixel_size.y);
        let medium_d = vec2<f32>(medium_offset * pixel_size.x * 0.707, medium_offset * pixel_size.y * 0.707);

        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv + medium_h).rgb, params.threshold) * weight;
        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv - medium_h).rgb, params.threshold) * weight;
        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv + medium_v).rgb, params.threshold) * weight;
        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv - medium_v).rgb, params.threshold) * weight;
        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv + medium_d).rgb, params.threshold) * weight * 0.5;
        bloom_medium += extract_bright(textureSample(input_texture, input_sampler, in.uv - medium_d).rgb, params.threshold) * weight * 0.5;

        // Large blur samples (softest, most spread out)
        let large_offset = fi * large_radius;
        let large_h = vec2<f32>(large_offset * pixel_size.x, 0.0);
        let large_v = vec2<f32>(0.0, large_offset * pixel_size.y);
        let large_d1 = vec2<f32>(large_offset * pixel_size.x * 0.707, large_offset * pixel_size.y * 0.707);
        let large_d2 = vec2<f32>(large_offset * pixel_size.x * 0.707, -large_offset * pixel_size.y * 0.707);

        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv + large_h).rgb, params.threshold) * weight;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv - large_h).rgb, params.threshold) * weight;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv + large_v).rgb, params.threshold) * weight;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv - large_v).rgb, params.threshold) * weight;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv + large_d1).rgb, params.threshold) * weight * 0.5;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv - large_d1).rgb, params.threshold) * weight * 0.5;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv + large_d2).rgb, params.threshold) * weight * 0.5;
        bloom_large += extract_bright(textureSample(input_texture, input_sampler, in.uv - large_d2).rgb, params.threshold) * weight * 0.5;
    }

    // Normalize the blooms
    bloom_small /= 4.0;
    bloom_medium /= 5.0;
    bloom_large /= 6.0;

    // Apply color shifting to each layer (more shift on outer glow)
    bloom_small = shift_color(bloom_small, 0.3);
    bloom_medium = shift_color(bloom_medium, 0.6);
    bloom_large = shift_color(bloom_large, 1.0);

    // Combine all bloom layers with different weights
    let combined_bloom = bloom_small * 0.6 + bloom_medium * 0.8 + bloom_large * 0.4;

    // Apply intensity and add to original
    let glow = combined_bloom * params.intensity;
    var result = original.rgb + glow;

    // Soft tone mapping to prevent harsh clipping
    result = result / (result + vec3<f32>(1.0));
    result = pow(result, vec3<f32>(1.0 / 1.1)); // Slight gamma correction

    return vec4<f32>(result, original.a);
}
