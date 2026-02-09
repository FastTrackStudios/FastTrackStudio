//! VST Parameter Bridge — abstraction for syncing internal parameters with VST plugins.
//!
//! The bridge connects the internal parameter system (string IDs, [`ParamSpec`],
//! [`NormalizedF64`]) to a plugin's raw parameter interface (numeric indices,
//! float values).
//!
//! # Architecture
//!
//! ```text
//! Internal (ParamSpec, ParameterValue)
//!     ↕  ParameterSyncManager (bidirectional mapping)
//! VST Plugin (numeric indices, normalized floats)
//! ```
//!
//! The [`ParameterSyncManager`] discovers plugin parameters via the
//! [`VstParameterBridge`] trait and builds bidirectional maps between internal
//! `param_id` strings and VST indices. It uses [`ParamSpec::plugin_index`] as
//! the primary mapping key, with name matching as a fallback.
//!
//! # Example
//!
//! ```ignore
//! let mut bridge = MockVstBridge::new(3);
//! let specs = vec![
//!     ParamSpec { id: "drive".into(), plugin_index: Some(0), .. },
//!     ParamSpec { id: "tone".into(), plugin_index: Some(1), .. },
//! ];
//!
//! let mut sync = ParameterSyncManager::new();
//! sync.discover(&bridge, &specs);
//!
//! // Push internal value to VST
//! sync.sync_param_to_vst(&mut bridge, "drive", 0.75);
//! assert_eq!(bridge.get_param(0), Some(0.75));
//! ```

use std::collections::HashMap;

use signal_proto::parameter::{ParamSpec, ParameterValue, VstParamInfo};

// ─────────────────────────────────────────────────────────────────────────────
// VstParameterBridge trait
// ─────────────────────────────────────────────────────────────────────────────

/// Trait abstracting VST plugin parameter access for a single plugin instance.
///
/// Implementations bridge the internal parameter system (string IDs, ParamSpec,
/// NormalizedF64) to the plugin's raw parameter interface (numeric indices).
pub trait VstParameterBridge: Send + Sync {
    /// Total number of parameters exposed by the plugin.
    fn get_param_count(&self) -> u32;

    /// Get the current normalized [0.0, 1.0] value by VST index.
    /// Returns `None` if the index is out of range.
    fn get_param(&self, index: u32) -> Option<f64>;

    /// Set a parameter by VST index to a normalized [0.0, 1.0] value.
    /// Values are clamped to [0.0, 1.0]. Out-of-range indices are ignored.
    fn set_param(&mut self, index: u32, value: f64);

    /// Get the name of a parameter by VST index.
    /// Returns `None` if the index is out of range.
    fn get_param_name(&self, index: u32) -> Option<String>;

    /// Discover all parameter metadata from the plugin.
    /// Called once after plugin load; results are cached by [`ParameterSyncManager`].
    fn discover_params(&self) -> Vec<VstParamInfo>;
}

// ─────────────────────────────────────────────────────────────────────────────
// Supporting types
// ─────────────────────────────────────────────────────────────────────────────

/// Direction of a parameter sync operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyncDirection {
    /// Push internal parameter values to the VST plugin.
    InternalToVst,
    /// Pull current VST plugin values into the internal state.
    VstToInternal,
}

/// A parameter change event from DAW automation.
#[derive(Debug, Clone, PartialEq)]
pub struct AutomationEvent {
    /// VST parameter index that changed.
    pub vst_index: u32,
    /// New normalized value [0.0, 1.0].
    pub value: f64,
}

// ─────────────────────────────────────────────────────────────────────────────
// ParameterSyncManager
// ─────────────────────────────────────────────────────────────────────────────

/// Manages bidirectional parameter mapping between internal IDs and VST indices.
///
/// After calling [`discover`](Self::discover), the manager caches plugin
/// parameter metadata and maintains fast lookup tables in both directions.
pub struct ParameterSyncManager {
    /// Cached parameter metadata from discovery.
    param_cache: Vec<VstParamInfo>,
    /// Internal param_id → VST index.
    id_to_vst: HashMap<String, u32>,
    /// VST index → internal param_id.
    vst_to_id: HashMap<u32, String>,
    /// Whether discovery has been performed.
    discovered: bool,
}

impl ParameterSyncManager {
    /// Create an empty sync manager. Call [`discover`](Self::discover) to populate.
    pub fn new() -> Self {
        Self {
            param_cache: Vec::new(),
            id_to_vst: HashMap::new(),
            vst_to_id: HashMap::new(),
            discovered: false,
        }
    }

    /// Discover plugin parameters and build bidirectional mappings.
    ///
    /// Mapping priority:
    /// 1. `ParamSpec::plugin_index` — explicit mapping (preferred)
    /// 2. Case-insensitive name matching between `ParamSpec::name` and `VstParamInfo::name`
    /// 3. Sequential index assignment (for specs without explicit index or name match)
    pub fn discover(&mut self, bridge: &dyn VstParameterBridge, param_specs: &[ParamSpec]) {
        let mut discovered = bridge.discover_params();
        self.id_to_vst.clear();
        self.vst_to_id.clear();

        // Track which VST indices have been claimed
        let mut claimed: std::collections::HashSet<u32> = std::collections::HashSet::new();

        // Pass 1: Explicit plugin_index mapping
        for spec in param_specs {
            if let Some(vst_idx) = spec.plugin_index {
                if (vst_idx as usize) < discovered.len() {
                    self.id_to_vst.insert(spec.id.clone(), vst_idx);
                    self.vst_to_id.insert(vst_idx, spec.id.clone());
                    discovered[vst_idx as usize].mapped_param_id = Some(spec.id.clone());
                    claimed.insert(vst_idx);
                }
            }
        }

        // Pass 2: Case-insensitive name matching for unmapped specs
        for spec in param_specs {
            if self.id_to_vst.contains_key(&spec.id) {
                continue; // Already mapped in pass 1
            }
            let spec_name_lower = spec.name.to_lowercase();
            if let Some(info) = discovered.iter_mut().find(|info| {
                !claimed.contains(&info.vst_index) && info.name.to_lowercase() == spec_name_lower
            }) {
                self.id_to_vst.insert(spec.id.clone(), info.vst_index);
                self.vst_to_id.insert(info.vst_index, spec.id.clone());
                info.mapped_param_id = Some(spec.id.clone());
                claimed.insert(info.vst_index);
            }
        }

        self.param_cache = discovered;
        self.discovered = true;
    }

    /// Whether discovery has been performed.
    pub fn is_discovered(&self) -> bool {
        self.discovered
    }

    /// Access cached parameter metadata.
    pub fn cached_info(&self) -> &[VstParamInfo] {
        &self.param_cache
    }

    /// Look up the VST index for an internal param_id.
    pub fn vst_index_for_param(&self, param_id: &str) -> Option<u32> {
        self.id_to_vst.get(param_id).copied()
    }

    /// Look up the internal param_id for a VST index.
    pub fn param_id_for_vst_index(&self, index: u32) -> Option<&str> {
        self.vst_to_id.get(&index).map(|s| s.as_str())
    }

    /// Sync all mapped parameters between internal state and VST plugin.
    ///
    /// - `InternalToVst`: pushes `block_params` values to the VST plugin
    /// - `VstToInternal`: pulls VST values into the returned update list
    ///
    /// `param_specs` is needed to resolve `ParameterValue::index` to `ParamSpec::id`.
    pub fn sync(
        &self,
        bridge: &mut dyn VstParameterBridge,
        block_params: &[ParameterValue],
        param_specs: &[ParamSpec],
        direction: SyncDirection,
    ) -> Vec<(String, f64)> {
        let mut updates = Vec::new();

        match direction {
            SyncDirection::InternalToVst => {
                for pv in block_params {
                    if let Some(spec) = param_specs.get(pv.index as usize) {
                        if let Some(vst_idx) = self.id_to_vst.get(&spec.id) {
                            bridge.set_param(*vst_idx, pv.value.value());
                        }
                    }
                }
            }
            SyncDirection::VstToInternal => {
                for (&vst_idx, param_id) in &self.vst_to_id {
                    if let Some(value) = bridge.get_param(vst_idx) {
                        updates.push((param_id.clone(), value));
                    }
                }
            }
        }

        updates
    }

    /// Push a single internal parameter value to the VST plugin.
    pub fn sync_param_to_vst(
        &self,
        bridge: &mut dyn VstParameterBridge,
        param_id: &str,
        value: f64,
    ) -> bool {
        if let Some(&vst_idx) = self.id_to_vst.get(param_id) {
            bridge.set_param(vst_idx, value);
            true
        } else {
            false
        }
    }

    /// Handle a DAW automation event, mapping it to an internal parameter.
    ///
    /// Returns `Some((param_id, value))` if the VST index is mapped,
    /// or `None` if the index has no internal mapping.
    pub fn handle_automation(&self, event: &AutomationEvent) -> Option<(String, f64)> {
        self.vst_to_id
            .get(&event.vst_index)
            .map(|id| (id.clone(), event.value))
    }
}

impl Default for ParameterSyncManager {
    fn default() -> Self {
        Self::new()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MockVstBridge
// ─────────────────────────────────────────────────────────────────────────────

/// In-memory parameter state for a single mock VST parameter.
struct MockVstParam {
    name: String,
    value: f64,
    info: VstParamInfo,
}

/// Mock implementation of [`VstParameterBridge`] for testing.
///
/// Stores parameter values in a `Vec<f64>` with configurable names.
pub struct MockVstBridge {
    params: Vec<MockVstParam>,
}

impl MockVstBridge {
    /// Create a mock bridge with `count` parameters named "Param 0", "Param 1", etc.
    pub fn new(count: u32) -> Self {
        let params = (0..count)
            .map(|i| {
                let name = format!("Param {i}");
                MockVstParam {
                    name: name.clone(),
                    value: 0.0,
                    info: VstParamInfo {
                        vst_index: i,
                        name: name.clone(),
                        short_name: format!("P{i}"),
                        default_normalized: 0.0,
                        step_count: 0,
                        mapped_param_id: None,
                    },
                }
            })
            .collect();
        Self { params }
    }

    /// Create a mock bridge from parameter specs.
    ///
    /// Uses `ParamSpec::plugin_index` for VST index assignment when available,
    /// otherwise assigns indices sequentially.
    pub fn from_param_specs(specs: &[ParamSpec]) -> Self {
        let mut max_index = 0u32;
        // First pass: find the max explicit plugin_index
        for spec in specs {
            if let Some(idx) = spec.plugin_index {
                max_index = max_index.max(idx + 1);
            }
        }
        max_index = max_index.max(specs.len() as u32);

        let mut params: Vec<MockVstParam> = (0..max_index)
            .map(|i| MockVstParam {
                name: format!("Param {i}"),
                value: 0.0,
                info: VstParamInfo {
                    vst_index: i,
                    name: format!("Param {i}"),
                    short_name: format!("P{i}"),
                    default_normalized: 0.0,
                    step_count: 0,
                    mapped_param_id: None,
                },
            })
            .collect();

        // Populate named params from specs
        let mut next_sequential = 0u32;
        for spec in specs {
            let idx = spec.plugin_index.unwrap_or_else(|| {
                // Find next unclaimed sequential index
                while params
                    .get(next_sequential as usize)
                    .map_or(false, |p| p.info.mapped_param_id.is_some())
                {
                    next_sequential += 1;
                }
                let idx = next_sequential;
                next_sequential += 1;
                idx
            });

            if let Some(param) = params.get_mut(idx as usize) {
                param.name = spec.name.clone();
                param.value = spec.default;
                param.info.name = spec.name.clone();
                param.info.short_name = spec.short_name.clone();
                param.info.default_normalized = spec.default;
                param.info.mapped_param_id = Some(spec.id.clone());
                let step_count = match &spec.format {
                    signal_proto::parameter::ParamFormat::Toggle => 2,
                    signal_proto::parameter::ParamFormat::Enum { options } => options.len() as u32,
                    signal_proto::parameter::ParamFormat::Integer { min, max } => {
                        (max - min + 1) as u32
                    }
                    _ => 0,
                };
                param.info.step_count = step_count;
            }
        }

        Self { params }
    }

    /// Get all current parameter values (for test assertions).
    pub fn values(&self) -> Vec<f64> {
        self.params.iter().map(|p| p.value).collect()
    }
}

impl VstParameterBridge for MockVstBridge {
    fn get_param_count(&self) -> u32 {
        self.params.len() as u32
    }

    fn get_param(&self, index: u32) -> Option<f64> {
        self.params.get(index as usize).map(|p| p.value)
    }

    fn set_param(&mut self, index: u32, value: f64) {
        if let Some(param) = self.params.get_mut(index as usize) {
            param.value = value.clamp(0.0, 1.0);
        }
    }

    fn get_param_name(&self, index: u32) -> Option<String> {
        self.params.get(index as usize).map(|p| p.name.clone())
    }

    fn discover_params(&self) -> Vec<VstParamInfo> {
        self.params.iter().map(|p| p.info.clone()).collect()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use signal_proto::parameter::ParamFormat;

    fn make_spec(id: &str, name: &str, plugin_index: Option<u32>) -> ParamSpec {
        ParamSpec {
            id: id.to_string(),
            name: name.to_string(),
            short_name: id[..std::cmp::min(3, id.len())].to_string(),
            format: ParamFormat::Percent,
            default: 0.5,
            priority: 0,
            plugin_index,
            group: None,
        }
    }

    // ── MockVstBridge basics ────────────────────────────────────────────

    #[test]
    fn mock_bridge_param_count() {
        let bridge = MockVstBridge::new(5);
        assert_eq!(bridge.get_param_count(), 5);
    }

    #[test]
    fn mock_bridge_get_set_param() {
        let mut bridge = MockVstBridge::new(3);
        assert_eq!(bridge.get_param(0), Some(0.0));

        bridge.set_param(1, 0.75);
        assert_eq!(bridge.get_param(1), Some(0.75));
    }

    #[test]
    fn mock_bridge_get_param_name() {
        let bridge = MockVstBridge::new(3);
        assert_eq!(bridge.get_param_name(0), Some("Param 0".to_string()));
        assert_eq!(bridge.get_param_name(2), Some("Param 2".to_string()));
    }

    #[test]
    fn mock_bridge_out_of_range_returns_none() {
        let bridge = MockVstBridge::new(2);
        assert_eq!(bridge.get_param(5), None);
        assert_eq!(bridge.get_param_name(5), None);
    }

    #[test]
    fn mock_bridge_clamps_values() {
        let mut bridge = MockVstBridge::new(2);
        bridge.set_param(0, 1.5);
        assert_eq!(bridge.get_param(0), Some(1.0));

        bridge.set_param(0, -0.5);
        assert_eq!(bridge.get_param(0), Some(0.0));
    }

    #[test]
    fn mock_bridge_values_snapshot() {
        let mut bridge = MockVstBridge::new(3);
        bridge.set_param(0, 0.1);
        bridge.set_param(1, 0.2);
        bridge.set_param(2, 0.3);
        assert_eq!(bridge.values(), vec![0.1, 0.2, 0.3]);
    }

    // ── Discovery ───────────────────────────────────────────────────────

    #[test]
    fn discover_populates_cache() {
        let bridge = MockVstBridge::new(4);
        let specs = vec![make_spec("drive", "Drive", Some(0))];

        let mut sync = ParameterSyncManager::new();
        assert!(!sync.is_discovered());

        sync.discover(&bridge, &specs);
        assert!(sync.is_discovered());
        assert_eq!(sync.cached_info().len(), 4);
    }

    #[test]
    fn discover_uses_plugin_index() {
        let bridge = MockVstBridge::new(4);
        let specs = vec![
            make_spec("drive", "Drive", Some(2)),
            make_spec("tone", "Tone", Some(0)),
        ];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        assert_eq!(sync.vst_index_for_param("drive"), Some(2));
        assert_eq!(sync.vst_index_for_param("tone"), Some(0));
        assert_eq!(sync.param_id_for_vst_index(2), Some("drive"));
        assert_eq!(sync.param_id_for_vst_index(0), Some("tone"));
    }

    #[test]
    fn discover_name_matching_fallback() {
        // Create a bridge with named params that match spec names
        let specs = vec![
            make_spec("vol", "Volume", None),
            make_spec("pan", "Pan", None),
        ];
        let bridge = MockVstBridge::from_param_specs(&specs);

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        // Should match by name (case-insensitive)
        assert!(sync.vst_index_for_param("vol").is_some());
        assert!(sync.vst_index_for_param("pan").is_some());
    }

    // ── Sync internal → VST ────────────────────────────────────────────

    #[test]
    fn sync_internal_to_vst_pushes_values() {
        let mut bridge = MockVstBridge::new(3);
        let specs = vec![
            make_spec("drive", "Drive", Some(0)),
            make_spec("tone", "Tone", Some(1)),
        ];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        let block_params = vec![
            ParameterValue::new(0, 0.8), // index 0 → spec "drive" → VST 0
            ParameterValue::new(1, 0.3), // index 1 → spec "tone" → VST 1
        ];

        sync.sync(
            &mut bridge,
            &block_params,
            &specs,
            SyncDirection::InternalToVst,
        );

        assert!((bridge.get_param(0).unwrap() - 0.8).abs() < 1e-10);
        assert!((bridge.get_param(1).unwrap() - 0.3).abs() < 1e-10);
        assert!((bridge.get_param(2).unwrap() - 0.0).abs() < 1e-10); // untouched
    }

    #[test]
    fn sync_internal_to_vst_skips_unmapped() {
        let mut bridge = MockVstBridge::new(3);
        let specs = vec![make_spec("drive", "Drive", Some(0))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        // ParameterValue index 5 is out of specs range — should be skipped
        let block_params = vec![ParameterValue::new(5, 0.9)];

        sync.sync(
            &mut bridge,
            &block_params,
            &specs,
            SyncDirection::InternalToVst,
        );

        // Nothing should have changed
        assert_eq!(bridge.values(), vec![0.0, 0.0, 0.0]);
    }

    // ── Sync VST → internal ────────────────────────────────────────────

    #[test]
    fn sync_vst_to_internal_pulls_values() {
        let mut bridge = MockVstBridge::new(3);
        bridge.set_param(0, 0.6);
        bridge.set_param(1, 0.4);

        let specs = vec![
            make_spec("drive", "Drive", Some(0)),
            make_spec("tone", "Tone", Some(1)),
        ];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        let updates = sync.sync(&mut bridge, &[], &specs, SyncDirection::VstToInternal);

        assert_eq!(updates.len(), 2);
        assert!(updates
            .iter()
            .any(|(id, v)| id == "drive" && (*v - 0.6).abs() < 1e-10));
        assert!(updates
            .iter()
            .any(|(id, v)| id == "tone" && (*v - 0.4).abs() < 1e-10));
    }

    // ── Single param sync ──────────────────────────────────────────────

    #[test]
    fn sync_single_param_to_vst() {
        let mut bridge = MockVstBridge::new(3);
        let specs = vec![make_spec("drive", "Drive", Some(1))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        assert!(sync.sync_param_to_vst(&mut bridge, "drive", 0.9));
        assert!((bridge.get_param(1).unwrap() - 0.9).abs() < 1e-10);

        // Unknown param returns false
        assert!(!sync.sync_param_to_vst(&mut bridge, "unknown", 0.5));
    }

    // ── Automation ─────────────────────────────────────────────────────

    #[test]
    fn handle_automation_maps_to_param_id() {
        let bridge = MockVstBridge::new(3);
        let specs = vec![make_spec("drive", "Drive", Some(1))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        let event = AutomationEvent {
            vst_index: 1,
            value: 0.65,
        };
        let result = sync.handle_automation(&event);
        assert_eq!(result, Some(("drive".to_string(), 0.65)));
    }

    #[test]
    fn handle_automation_unknown_index_returns_none() {
        let bridge = MockVstBridge::new(3);
        let specs = vec![make_spec("drive", "Drive", Some(0))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        let event = AutomationEvent {
            vst_index: 99,
            value: 0.5,
        };
        assert_eq!(sync.handle_automation(&event), None);
    }

    // ── Lookup ──────────────────────────────────────────────────────────

    #[test]
    fn vst_index_for_param_id() {
        let bridge = MockVstBridge::new(4);
        let specs = vec![make_spec("a", "A", Some(3)), make_spec("b", "B", Some(1))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        assert_eq!(sync.vst_index_for_param("a"), Some(3));
        assert_eq!(sync.vst_index_for_param("b"), Some(1));
        assert_eq!(sync.vst_index_for_param("missing"), None);
    }

    #[test]
    fn param_id_for_vst_index() {
        let bridge = MockVstBridge::new(4);
        let specs = vec![make_spec("x", "X", Some(0)), make_spec("y", "Y", Some(2))];

        let mut sync = ParameterSyncManager::new();
        sync.discover(&bridge, &specs);

        assert_eq!(sync.param_id_for_vst_index(0), Some("x"));
        assert_eq!(sync.param_id_for_vst_index(2), Some("y"));
        assert_eq!(sync.param_id_for_vst_index(1), None); // unmapped
    }

    // ── Integration ────────────────────────────────────────────────────

    #[test]
    fn from_param_specs_uses_plugin_index() {
        let specs = vec![
            make_spec("drive", "Drive", Some(2)),
            make_spec("tone", "Tone", Some(0)),
        ];
        let bridge = MockVstBridge::from_param_specs(&specs);

        assert_eq!(bridge.get_param_count(), 3); // indices 0, 1, 2
        assert_eq!(bridge.get_param_name(0), Some("Tone".to_string()));
        assert_eq!(bridge.get_param_name(2), Some("Drive".to_string()));
        // Default values from specs
        assert!((bridge.get_param(0).unwrap() - 0.5).abs() < 1e-10);
        assert!((bridge.get_param(2).unwrap() - 0.5).abs() < 1e-10);
    }

    #[test]
    fn default_sync_manager_is_not_discovered() {
        let sync = ParameterSyncManager::default();
        assert!(!sync.is_discovered());
        assert!(sync.cached_info().is_empty());
    }

    #[test]
    fn set_out_of_range_index_is_noop() {
        let mut bridge = MockVstBridge::new(2);
        bridge.set_param(99, 0.5); // should not panic
        assert_eq!(bridge.get_param(99), None);
    }
}
