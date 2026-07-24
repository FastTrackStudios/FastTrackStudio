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

impl Discovery for HermesBackend {
    fn list_models(&self, _backend_id: &str) -> Result<Vec<ModelInfo>, AgentError> {
        let v = self.gateway_get("/models")?;
        let default = self.inner.config.model.clone();
        // The gateway's model rows rarely carry a window size; fall
        // back to the deployment's configured context (the cluster
        // config pins model.context_length) via env.
        let fallback_ctx = std::env::var("TASK_HERMES_CONTEXT_LENGTH")
            .ok()
            .and_then(|v| v.parse().ok())
            .unwrap_or(1_050_000);
        Ok(rows(&v, &["data", "models"])
            .into_iter()
            .filter_map(|m| {
                let id = if m.is_string() {
                    m.as_str().unwrap_or_default().to_string()
                } else {
                    s(m, "id")
                };
                if id.is_empty() {
                    return None;
                }
                let ctx = m
                    .get("context_length")
                    .or_else(|| m.get("context_window"))
                    .and_then(Value::as_u64)
                    .unwrap_or(fallback_ctx);
                Some(ModelInfo {
                    backend_id: BACKEND_ID.to_string(),
                    is_default: id == default,
                    label: s(m, "name"),
                    context_length: ctx,
                    id,
                })
            })
            .collect())
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
