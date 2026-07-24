//! Discovery — proxied from the gateway's REST surface
//! (`/v1/models`, `/v1/skills`, `/v1/capabilities`).
//!
//! The trait is sync (Facet RPC); calls run on the dispatcher's
//! blocking threads, so the async HTTP hops are driven via the
//! runtime handle captured at construction. Responses are parsed
//! defensively — the gateway's exact JSON evolves between Hermes
//! releases, and a missing field must degrade to an empty label,
//! not an error.

use agent_proto::error::AgentError;
use agent_proto::service::discovery::{CapabilityFlag, Discovery, ModelInfo, SkillInfo};
use serde_json::Value;

use crate::{BACKEND_ID, HermesBackend};

impl HermesBackend {
    /// Blocking GET against the gateway, JSON-decoded.
    fn gateway_get(&self, path: &str) -> Result<Value, AgentError> {
        let url = format!("{}{path}", self.inner.config.base_url);
        let http = self.inner.http.clone();
        let key = self.inner.config.api_key.clone();
        self.inner
            .runtime
            .block_on(async move {
                let mut req = http.get(&url);
                if !key.is_empty() {
                    req = req.header("Authorization", format!("Bearer {key}"));
                }
                let resp = req.send().await.map_err(|e| e.to_string())?;
                let status = resp.status();
                if !status.is_success() {
                    return Err(format!("HTTP {status}"));
                }
                resp.json::<Value>().await.map_err(|e| e.to_string())
            })
            .map_err(|e| AgentError::Io(format!("hermes {path}: {e}")))
    }
}

/// The gateway wraps lists differently per endpoint — accept
/// `{"data": [...]}`, `{"skills": [...]}`, or a bare array.
fn rows<'a>(v: &'a Value, keys: &[&str]) -> Vec<&'a Value> {
    if let Some(arr) = v.as_array() {
        return arr.iter().collect();
    }
    for k in keys {
        if let Some(arr) = v.get(*k).and_then(Value::as_array) {
            return arr.iter().collect();
        }
    }
    Vec::new()
}

fn s(v: &Value, key: &str) -> String {
    v.get(key).and_then(Value::as_str).unwrap_or_default().to_string()
}

/// models.dev provider ids surfaced in the picker when
/// `TASK_HERMES_PROVIDERS` doesn't override — the set the gateway
/// deployment can plausibly route to (its `/model` chat command
/// switches providers per session).
const DEFAULT_PROVIDERS: &str = "openai,anthropic,google,github-copilot,deepseek,x-ai,qwen,nousresearch";

/// In-process models.dev catalog cache (1h TTL) — the catalog is
/// ~2MB of JSON and changes rarely.
static CATALOG: std::sync::Mutex<Option<(std::time::Instant, Value)>> =
    std::sync::Mutex::new(None);

impl HermesBackend {
    /// The models.dev catalog (fetched + cached). The same source
    /// hermes-agent's own CLI uses for its `/model` picker; the
    /// gateway exposes no catalog endpoint, so we go straight to it.
    fn models_dev_catalog(&self) -> Option<Value> {
        {
            let cache = CATALOG.lock().ok()?;
            if let Some((at, v)) = cache.as_ref() {
                if at.elapsed() < std::time::Duration::from_secs(3600) {
                    return Some(v.clone());
                }
            }
        }
        let http = self.inner.http.clone();
        let fetched = self
            .inner
            .runtime
            .block_on(async move {
                let resp = http
                    .get("https://models.dev/api.json")
                    .send()
                    .await
                    .map_err(|e| e.to_string())?;
                if !resp.status().is_success() {
                    return Err(format!("HTTP {}", resp.status()));
                }
                resp.json::<Value>().await.map_err(|e| e.to_string())
            })
            .ok()?;
        if let Ok(mut cache) = CATALOG.lock() {
            *cache = Some((std::time::Instant::now(), fetched.clone()));
        }
        Some(fetched)
    }
}

impl Discovery for HermesBackend {
    fn list_models(&self, _backend_id: &str) -> Result<Vec<ModelInfo>, AgentError> {
        let default = self.inner.config.model.clone();
        let fallback_ctx = std::env::var("TASK_HERMES_CONTEXT_LENGTH")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(1_050_000);

        // The gateway's own façade model first — "the agent as
        // configured", always present and the default.
        let mut out = vec![ModelInfo {
            backend_id: BACKEND_ID.to_string(),
            id: default.clone(),
            label: "Hermes (configured default)".to_string(),
            is_default: true,
            context_length: fallback_ctx,
            provider_id: "hermes".to_string(),
            provider_name: "Hermes Gateway".to_string(),
            reasoning: true,
            cost_in_per_mtok: 0.0,
            cost_out_per_mtok: 0.0,
        }];

        // Provider-grouped catalog from models.dev, scoped to the
        // providers the deployment can route to. Selecting one of
        // these switches the session via the `/model` chat command.
        if let Some(catalog) = self.models_dev_catalog() {
            let scoped: Vec<String> = std::env::var("TASK_HERMES_PROVIDERS")
                .unwrap_or_else(|_| DEFAULT_PROVIDERS.to_string())
                .split(',')
                .map(|p| p.trim().to_string())
                .filter(|p| !p.is_empty())
                .collect();
            for pid in &scoped {
                let Some(provider) = catalog.get(pid) else {
                    continue;
                };
                let pname = {
                    let n = s(provider, "name");
                    if n.is_empty() { pid.clone() } else { n }
                };
                let Some(models) = provider.get("models").and_then(Value::as_object) else {
                    continue;
                };
                for (mid, m) in models {
                    // Chat-capable only: the agent needs tool calling.
                    if !m.get("tool_call").and_then(Value::as_bool).unwrap_or(false) {
                        continue;
                    }
                    out.push(ModelInfo {
                        backend_id: BACKEND_ID.to_string(),
                        id: format!("{pid}/{mid}"),
                        label: s(m, "name"),
                        is_default: false,
                        context_length: m
                            .pointer("/limit/context")
                            .and_then(Value::as_u64)
                            .unwrap_or(0),
                        provider_id: pid.clone(),
                        provider_name: pname.clone(),
                        reasoning: m
                            .get("reasoning")
                            .and_then(Value::as_bool)
                            .unwrap_or(false),
                        cost_in_per_mtok: m
                            .pointer("/cost/input")
                            .and_then(Value::as_f64)
                            .unwrap_or(0.0),
                        cost_out_per_mtok: m
                            .pointer("/cost/output")
                            .and_then(Value::as_f64)
                            .unwrap_or(0.0),
                    });
                }
            }
        }
        Ok(out)
    }

    fn list_skills(&self, _backend_id: &str) -> Result<Vec<SkillInfo>, AgentError> {
        let v = self.gateway_get("/skills")?;
        Ok(rows(&v, &["data", "skills"])
            .into_iter()
            .filter_map(|sk| {
                let name = if sk.is_string() {
                    sk.as_str().unwrap_or_default().to_string()
                } else {
                    let n = s(sk, "name");
                    if n.is_empty() { s(sk, "id") } else { n }
                };
                if name.is_empty() {
                    return None;
                }
                Some(SkillInfo {
                    backend_id: BACKEND_ID.to_string(),
                    description: s(sk, "description"),
                    enabled: sk
                        .get("enabled")
                        .and_then(Value::as_bool)
                        .unwrap_or(true),
                    name,
                })
            })
            .collect())
    }

    fn list_capabilities(&self, _backend_id: &str) -> Result<Vec<CapabilityFlag>, AgentError> {
        let v = self.gateway_get("/capabilities")?;
        // Flatten one level of {group: {flag: bool}} plus top-level bools.
        let mut out = Vec::new();
        let obj = v
            .get("capabilities")
            .and_then(Value::as_object)
            .or_else(|| v.as_object());
        if let Some(map) = obj {
            for (k, val) in map {
                match val {
                    Value::Bool(b) => out.push(CapabilityFlag {
                        backend_id: BACKEND_ID.to_string(),
                        name: k.clone(),
                        enabled: *b,
                    }),
                    Value::Object(inner) => {
                        for (ik, iv) in inner {
                            if let Some(b) = iv.as_bool() {
                                out.push(CapabilityFlag {
                                    backend_id: BACKEND_ID.to_string(),
                                    name: format!("{k}.{ik}"),
                                    enabled: b,
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(out)
    }
}
