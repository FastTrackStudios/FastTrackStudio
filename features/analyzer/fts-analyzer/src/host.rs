use std::ffi::CString;
use std::path::Path;

use anyhow::{Context, Result};
use clack_extensions::latency::*;
use clack_extensions::params::*;
use clack_host::events::event_types::ParamValueEvent;
use clack_host::prelude::*;
use clack_host::utils::Cookie;

// ---------------------------------------------------------------------------
// Host handler types — minimal, headless-only
// ---------------------------------------------------------------------------

struct AnalyzerHostShared;

impl<'a> SharedHandler<'a> for AnalyzerHostShared {
    fn request_restart(&self) {}
    fn request_process(&self) {}
    fn request_callback(&self) {}
}

struct AnalyzerHostMainThread;

impl<'a> MainThreadHandler<'a> for AnalyzerHostMainThread {}

struct AnalyzerHostAudioProcessor;

impl<'a> AudioProcessorHandler<'a> for AnalyzerHostAudioProcessor {}

struct AnalyzerHost;

impl HostHandlers for AnalyzerHost {
    type Shared<'a> = AnalyzerHostShared;
    type MainThread<'a> = AnalyzerHostMainThread;
    type AudioProcessor<'a> = AnalyzerHostAudioProcessor;
}

// ---------------------------------------------------------------------------
// Plugin parameter descriptor
// ---------------------------------------------------------------------------

/// A snapshot of a plugin parameter's metadata.
#[derive(Debug, Clone)]
pub struct ParamInfo {
    pub id: u32,
    pub name: String,
    pub min: f64,
    pub max: f64,
    pub default: f64,
}

// ---------------------------------------------------------------------------
// Loaded plugin wrapper
// ---------------------------------------------------------------------------

/// A CLAP plugin loaded and ready for offline processing.
pub struct LoadedPlugin {
    instance: PluginInstance<AnalyzerHost>,
    sample_rate: f64,
    block_size: u32,
}

impl LoadedPlugin {
    /// Load a CLAP plugin from a `.clap` bundle path.
    ///
    /// `plugin_index` selects which plugin inside the bundle (usually 0).
    pub fn load(
        clap_path: &Path,
        plugin_index: usize,
        sample_rate: f64,
        block_size: u32,
    ) -> Result<Self> {
        let entry =
            unsafe { PluginEntry::load(clap_path) }.context("failed to load .clap bundle")?;

        let factory = entry
            .get_plugin_factory()
            .context("plugin has no factory")?;

        let descriptor = factory
            .plugin_descriptors()
            .nth(plugin_index)
            .context("plugin index out of range")?;

        let plugin_id = descriptor.id().context("plugin has no id")?;

        let host_info = HostInfo::new(
            "fts-analyzer",
            "com.fasttrackstudio.analyzer",
            "",
            env!("CARGO_PKG_VERSION"),
        )
        .context("failed to create host info")?;

        let instance = PluginInstance::<AnalyzerHost>::new(
            |_| AnalyzerHostShared,
            |_| AnalyzerHostMainThread,
            &entry,
            plugin_id,
            &host_info,
        )
        .context("failed to instantiate plugin")?;

        Ok(Self {
            instance,
            sample_rate,
            block_size,
        })
    }

    /// List all plugins in a `.clap` bundle.
    pub fn list_plugins(clap_path: &Path) -> Result<Vec<String>> {
        let entry =
            unsafe { PluginEntry::load(clap_path) }.context("failed to load .clap bundle")?;

        let factory = entry
            .get_plugin_factory()
            .context("plugin has no factory")?;

        let names: Vec<String> = factory
            .plugin_descriptors()
            .filter_map(|d| d.name().map(|n| n.to_string_lossy().into_owned()))
            .collect();

        Ok(names)
    }

    /// Enumerate all parameters exposed by the plugin.
    pub fn params(&mut self) -> Vec<ParamInfo> {
        let mut handle = self.instance.plugin_handle();
        let Some(params_ext) = handle.get_extension::<PluginParams>() else {
            return Vec::new();
        };

        let count = params_ext.count(&mut handle);
        let mut out = Vec::with_capacity(count as usize);
        let mut info_buf = ParamInfoBuffer::new();

        for i in 0..count {
            if let Some(info) = params_ext.get_info(&mut handle, i, &mut info_buf) {
                out.push(ParamInfo {
                    id: info.id.get(),
                    name: String::from_utf8_lossy(info.name).into_owned(),
                    min: info.min_value,
                    max: info.max_value,
                    default: info.default_value,
                });
            }
        }

        out
    }

    /// Process a mono audio signal through the plugin and return the output.
    ///
    /// `param_overrides` is a list of `(param_id, value)` pairs applied at
    /// sample offset 0 of the first block.
    pub fn process(&mut self, input: &[f32], param_overrides: &[(u32, f64)]) -> Result<Vec<f32>> {
        let config = PluginAudioConfiguration {
            sample_rate: self.sample_rate,
            min_frames_count: 1,
            max_frames_count: self.block_size,
        };

        let audio_processor = self
            .instance
            .activate(|_, _| AnalyzerHostAudioProcessor, config)
            .context("failed to activate plugin")?;

        let mut processor = audio_processor
            .start_processing()
            .map_err(|_| anyhow::anyhow!("failed to start processing"))?;

        let total_samples = input.len();
        let block = self.block_size as usize;
        let mut output = vec![0.0f32; total_samples];
        let mut offset = 0;

        while offset < total_samples {
            let end = (offset + block).min(total_samples);

            // Send param overrides on every block to ensure they're applied
            let mut input_events = EventBuffer::new();
            for &(param_id, value) in param_overrides {
                let clap_id = ClapId::from(param_id);
                input_events.push(&ParamValueEvent::new(
                    0,
                    clap_id,
                    Pckn::match_all(),
                    value,
                    Cookie::empty(),
                ));
            }
            let mut output_events = EventBuffer::new();

            // Prepare stereo audio buffers (most plugins expect stereo).
            // We duplicate the mono input to both L and R channels,
            // then read back the L channel as the output.
            let mut input_l = input[offset..end].to_vec();
            let mut input_r = input[offset..end].to_vec();
            let mut output_l = vec![0.0f32; end - offset];
            let mut output_r = vec![0.0f32; end - offset];

            let mut input_ports = AudioPorts::with_capacity(2, 1);
            let mut output_ports = AudioPorts::with_capacity(2, 1);

            let inputs = input_ports.with_input_buffers([AudioPortBuffer {
                latency: 0,
                channels: AudioPortBufferType::f32_input_only([
                    InputChannel {
                        buffer: input_l.as_mut_slice(),
                        is_constant: false,
                    },
                    InputChannel {
                        buffer: input_r.as_mut_slice(),
                        is_constant: false,
                    },
                ]),
            }]);
            let mut outputs = output_ports.with_output_buffers([AudioPortBuffer {
                latency: 0,
                channels: AudioPortBufferType::f32_output_only([
                    output_l.as_mut_slice(),
                    output_r.as_mut_slice(),
                ]),
            }]);

            let in_events = input_events.as_input();
            let mut out_events = output_events.as_output();

            processor
                .process(
                    &inputs,
                    &mut outputs,
                    &in_events,
                    &mut out_events,
                    None,
                    None,
                )
                .context("process() failed")?;

            // Copy L channel output back
            output[offset..end].copy_from_slice(&output_l);

            offset = end;
        }

        let stopped = processor.stop_processing();
        self.instance.deactivate(stopped);

        Ok(output)
    }

    /// Process a mono audio signal through the plugin and return stereo output (L, R).
    pub fn process_stereo(
        &mut self,
        input: &[f32],
        param_overrides: &[(u32, f64)],
    ) -> Result<(Vec<f32>, Vec<f32>)> {
        let config = PluginAudioConfiguration {
            sample_rate: self.sample_rate,
            min_frames_count: 1,
            max_frames_count: self.block_size,
        };

        let audio_processor = self
            .instance
            .activate(|_, _| AnalyzerHostAudioProcessor, config)
            .context("failed to activate plugin")?;

        let mut processor = audio_processor
            .start_processing()
            .map_err(|_| anyhow::anyhow!("failed to start processing"))?;

        let total_samples = input.len();
        let block = self.block_size as usize;
        let mut output_l_full = vec![0.0f32; total_samples];
        let mut output_r_full = vec![0.0f32; total_samples];
        let mut offset = 0;

        while offset < total_samples {
            let end = (offset + block).min(total_samples);

            let mut input_events = EventBuffer::new();
            for &(param_id, value) in param_overrides {
                let clap_id = ClapId::from(param_id);
                input_events.push(&ParamValueEvent::new(
                    0,
                    clap_id,
                    Pckn::match_all(),
                    value,
                    Cookie::empty(),
                ));
            }
            let mut output_events = EventBuffer::new();

            let mut input_l = input[offset..end].to_vec();
            let mut input_r = input[offset..end].to_vec();
            let mut output_l = vec![0.0f32; end - offset];
            let mut output_r = vec![0.0f32; end - offset];

            let mut input_ports = AudioPorts::with_capacity(2, 1);
            let mut output_ports = AudioPorts::with_capacity(2, 1);

            let inputs = input_ports.with_input_buffers([AudioPortBuffer {
                latency: 0,
                channels: AudioPortBufferType::f32_input_only([
                    InputChannel {
                        buffer: input_l.as_mut_slice(),
                        is_constant: false,
                    },
                    InputChannel {
                        buffer: input_r.as_mut_slice(),
                        is_constant: false,
                    },
                ]),
            }]);
            let mut outputs = output_ports.with_output_buffers([AudioPortBuffer {
                latency: 0,
                channels: AudioPortBufferType::f32_output_only([
                    output_l.as_mut_slice(),
                    output_r.as_mut_slice(),
                ]),
            }]);

            let in_events = input_events.as_input();
            let mut out_events = output_events.as_output();

            processor
                .process(
                    &inputs,
                    &mut outputs,
                    &in_events,
                    &mut out_events,
                    None,
                    None,
                )
                .context("process() failed")?;

            output_l_full[offset..end].copy_from_slice(&output_l);
            output_r_full[offset..end].copy_from_slice(&output_r);

            offset = end;
        }

        let stopped = processor.stop_processing();
        self.instance.deactivate(stopped);

        Ok((output_l_full, output_r_full))
    }

    /// Convert a display string (e.g. "1000 Hz") to a plugin-native parameter value
    /// using the CLAP `text_to_value` extension.
    pub fn text_to_value(&mut self, param_id: u32, text: &str) -> Option<f64> {
        let mut handle = self.instance.plugin_handle();
        let params_ext = handle.get_extension::<PluginParams>()?;
        let clap_id = ClapId::from(param_id);
        let cstr = CString::new(text).ok()?;
        params_ext.text_to_value(&mut handle, clap_id, &cstr)
    }

    /// Convert a plugin-native parameter value back to its display string using the
    /// CLAP `value_to_text` extension. Returns None if the extension is missing or
    /// the plugin doesn't provide a formatter for this param.
    pub fn value_to_text(&mut self, param_id: u32, value: f64) -> Option<String> {
        let mut handle = self.instance.plugin_handle();
        let params_ext = handle.get_extension::<PluginParams>()?;
        let clap_id = ClapId::from(param_id);
        let mut buf = [0u8; 256];
        params_ext
            .value_to_text(&mut handle, clap_id, value, &mut buf)
            .ok()?;
        // Null-terminated C string
        let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
        std::str::from_utf8(&buf[..end]).ok().map(|s| s.to_string())
    }

    /// Query reported latency in samples (0 if the extension is missing).
    pub fn latency(&mut self) -> u32 {
        let mut handle = self.instance.plugin_handle();
        handle
            .get_extension::<PluginLatency>()
            .map(|ext| ext.get(&mut handle))
            .unwrap_or(0)
    }
}
