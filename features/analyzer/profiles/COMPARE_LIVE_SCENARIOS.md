# Compare Compressor Live: Scenario Profiles

The `compare-compressor-live` command supports JSON scenario profiles for consistent, repeatable testing of Pro-C 3 against FTS-Comp.

## Quick Start

1. **Create a scenario profile** (see examples below)
2. **Run with overrides** to test different parameters without editing the file:

```bash
# Use profile defaults
fts-analyzer-cli compare-compressor-live \
  --profile profiles/compare-compressor-live-example.json

# Override threshold on-the-fly
fts-analyzer-cli compare-compressor-live \
  --profile profiles/compare-compressor-live-example.json \
  --threshold-db -24

# Test different attack/release
fts-analyzer-cli compare-compressor-live \
  --profile profiles/compare-compressor-live-example.json \
  --attack-ms 5 \
  --release-ms 200
```

## Profile JSON Format

All fields are optional. CLI arguments override profile values.

```json
{
  "reference": "/path/to/Pro-C3.clap",
  "plugin": "../FTS-Plugins/target/bundled/comp-plugin.clap",
  "out": "./comparison-results",
  "attack_ms": 10,
  "release_ms": 50,
  "threshold_db": -18,
  "ratio": 4.0,
  "knee_db": 2.0,
  "tolerance_db": 1.0,
  "sample_rate": 48000,
  "block_size": 512,
  "param_remaps": {
    "7": "Attack",
    "8": "Release",
    "0": "Threshold",
    "1": "Ratio",
    "2": "Knee"
  }
}
```

### Fields Reference

| Field | Type | Default | Notes |
|-------|------|---------|-------|
| `reference` | string | N/A (required) | Path to reference plugin (.clap/.vst2/.vst3) |
| `plugin` | string | N/A (required) | Path to test plugin |
| `out` | string | N/A (required) | Output directory for results |
| `attack_ms` | number | N/A (required) | Attack time in milliseconds |
| `release_ms` | number | N/A (required) | Release time in milliseconds |
| `threshold_db` | number | (none) | Threshold in dB (optional) |
| `ratio` | number | (none) | Compression ratio (optional) |
| `knee_db` | number | (none) | Knee width in dB (optional) |
| `tolerance_db` | number | 1.0 | Pass/fail tolerance in dB |
| `sample_rate` | number | 48000 | Sample rate in Hz |
| `block_size` | number | 512 | Block size in frames |
| `param_remaps` | object | (none) | Map reference param IDs to test plugin param names |

### Paths

- Paths can be **absolute** or **relative** (relative to the profile file's directory)
- Use `../` to reference directories outside the profile directory
- Windows paths work with forward slashes: `C:/path/to/plugin.clap`

### Parameter Remapping

The `param_remaps` object maps reference plugin parameter IDs to test plugin parameter names.

Common Pro-C 3 parameter IDs:
- `"7"`: Attack (ms)
- `"8"`: Release (ms)
- `"0"`: Threshold (dB)
- `"1"`: Ratio
- `"2"`: Knee (dB)

When remapping is configured, the test plugin's parameter will be set to the value parsed from the scenario name (e.g., "atk-10ms" → 10.0 ms).

## Example Scenarios

### Basic Clean Compression
```json
{
  "reference": "/Volumes/Pro-C3.clap",
  "plugin": "comp-plugin.clap",
  "out": "./results/clean",
  "attack_ms": 10,
  "release_ms": 50,
  "threshold_db": -18,
  "ratio": 4.0,
  "knee_db": 2.0,
  "tolerance_db": 1.0,
  "param_remaps": {
    "7": "Attack",
    "8": "Release"
  }
}
```

### Fast Attack (Transients)
```json
{
  "reference": "/Volumes/Pro-C3.clap",
  "plugin": "comp-plugin.clap",
  "out": "./results/fast-attack",
  "attack_ms": 1,
  "release_ms": 100,
  "threshold_db": -12,
  "ratio": 6.0,
  "knee_db": 3.0,
  "tolerance_db": 1.0
}
```

### Slow Release (Smooth)
```json
{
  "reference": "/Volumes/Pro-C3.clap",
  "plugin": "comp-plugin.clap",
  "out": "./results/smooth",
  "attack_ms": 50,
  "release_ms": 500,
  "threshold_db": -20,
  "ratio": 3.0,
  "knee_db": 4.0,
  "tolerance_db": 1.0
}
```

### Mastering Levels
```json
{
  "reference": "/Volumes/Pro-C3.clap",
  "plugin": "comp-plugin.clap",
  "out": "./results/mastering",
  "attack_ms": 100,
  "release_ms": 200,
  "threshold_db": -30,
  "ratio": 2.0,
  "knee_db": 6.0,
  "tolerance_db": 0.5
}
```

## Workflow: Testing Different Parameters

### Setup
1. Create a base profile with your reference paths and defaults
2. Keep one profile file as your "golden" setup

### Testing
```bash
# Test different thresholds without editing the file
for thresh in -30 -24 -18 -12 -6; do
  fts-analyzer-cli compare-compressor-live \
    --profile profiles/my-setup.json \
    --threshold-db $thresh \
    --out results/thresh-$thresh
done

# Test different attack/release combinations
for atk in 1 10 50 100; do
  for rel in 50 100 200 500; do
    fts-analyzer-cli compare-compressor-live \
      --profile profiles/my-setup.json \
      --attack-ms $atk \
      --release-ms $rel \
      --out results/atk-${atk}ms_rel-${rel}ms
  done
done
```

## Output

Each comparison generates:
- **metadata.json** — Test parameters, frequencies, results summary, worst RMS difference
- **{scenario}_reference.bin** — Reference plugin's GR curves (quantized u8)
- **{scenario}_test.bin** — Test plugin's GR curves (same format)

Example metadata:
```json
{
  "reference_plugin": "/Volumes/Pro-C3.clap",
  "test_plugin": "comp-plugin.clap",
  "scenario": "atk-10ms_rel-50ms",
  "attack_ms": 10,
  "release_ms": 50,
  "threshold_db": -18,
  "ratio": 4.0,
  "knee_db": 2.0,
  "tolerance_db": 1.0,
  "frequencies": [20, 30, 40, ...],
  "passed": 35,
  "total": 35,
  "worst_rms_diff_db": 0.234
}
```

## Tips

- **Start conservative**: Use 1.0 dB tolerance initially; tighten to 0.5 dB if parity is high
- **Test musically**: Use common DAW comp settings (10ms/50ms, 100ms/200ms) first
- **Monitor worst RMS**: Check the summary line to see where differences are largest
- **Freq filtering**: Use `--freq-filter` to test specific problem frequencies
- **Batch testing**: Combine profiles with shell loops for systematic parameter sweeps

## Troubleshooting

**"Failed to load reference plugin"**
- Check the absolute path or profile-relative path is correct
- Ensure the file exists and is readable

**"parameter remapped to test plugin but value not parsed"**
- The scenario name doesn't contain the expected format (e.g., "atk-10ms")
- CLI args like `--attack-ms` will be used directly instead

**"High RMS difference at low frequencies"**
- Low frequencies may be more sensitive to coefficient precision
- Test higher frequencies first with `--freq-filter 100 1000 10000`

**"Worst RMS diff is stable but specific frequencies fail"**
- Use `--freq-filter` to test just that frequency
- Check if it's a boundary case (very high/low frequency)
