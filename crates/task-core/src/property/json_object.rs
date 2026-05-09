//! `JsonObject` — a Facet-friendly newtype around `serde_json::Value`.
//!
//! `serde_json::Value` does not implement `facet::Facet`, which prevents
//! it from being embedded in models that derive Facet (e.g. crudcrate's
//! generated `Api`/`Create`/`Update` structs when `generate_vox_service`
//! is enabled). This wrapper exposes a manual, opaque Facet `Shape`
//! while preserving full `serde` round-tripping, `Default`, and SeaORM
//! `ValueType`/`TryGetable` behaviour for storage as a JSON column.

use std::ops::{Deref, DerefMut};

use facet::{Facet, ProxyDef, PtrConst, PtrMut, PtrUninit, Shape, TypeOpsDirect, VarianceDesc};
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};

/// A `serde_json::Value` payload that is also a `facet::Facet` value.
///
/// Reflection treats the contents as opaque; serde sees the inner JSON
/// directly via `#[serde(transparent)]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct JsonObject(pub serde_json::Value);

impl utoipa::PartialSchema for JsonObject {
    fn schema() -> utoipa::openapi::RefOr<utoipa::openapi::schema::Schema> {
        utoipa::openapi::schema::ObjectBuilder::new()
            .schema_type(utoipa::openapi::schema::SchemaType::AnyValue)
            .description(Some(
                "Free-form JSON value used for the Obsidian-style property sidecar.",
            ))
            .build()
            .into()
    }
}

impl utoipa::ToSchema for JsonObject {
    fn name() -> std::borrow::Cow<'static, str> {
        std::borrow::Cow::Borrowed("JsonObject")
    }
}

impl Default for JsonObject {
    fn default() -> Self {
        Self(serde_json::Value::Object(serde_json::Map::new()))
    }
}

impl JsonObject {
    /// Create a new empty JSON object.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Wrap an arbitrary `serde_json::Value`.
    #[must_use]
    pub const fn from_value(value: serde_json::Value) -> Self {
        Self(value)
    }

    /// Take the wrapped JSON value.
    #[must_use]
    pub fn into_inner(self) -> serde_json::Value {
        self.0
    }

    /// Borrow the wrapped JSON value.
    #[must_use]
    pub const fn as_value(&self) -> &serde_json::Value {
        &self.0
    }
}

impl Deref for JsonObject {
    type Target = serde_json::Value;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for JsonObject {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<serde_json::Value> for JsonObject {
    fn from(value: serde_json::Value) -> Self {
        Self(value)
    }
}

impl From<JsonObject> for serde_json::Value {
    fn from(value: JsonObject) -> Self {
        value.0
    }
}

// ── Facet ──────────────────────────────────────────────────────────────────

// Reflection-opaque: callers cannot inspect the inner JSON value structurally
// through Facet, but the wrapper itself carries a valid `Shape` so it can be
// embedded inside other Facet types (e.g. crudcrate-generated API structs).
//
// `TypeOpsDirect` is wired up so deserializers can construct an empty JSON
// object via `Default` when the field is missing.
unsafe fn json_object_drop_in_place(ptr: *mut ()) {
    // SAFETY: facet guarantees the pointer points to a live `JsonObject`.
    unsafe { core::ptr::drop_in_place(ptr.cast::<JsonObject>()) }
}

unsafe fn json_object_default_in_place(ptr: *mut ()) {
    // SAFETY: facet guarantees the destination is uninitialized memory of the
    // correct size/alignment for `JsonObject`.
    unsafe { core::ptr::write(ptr.cast::<JsonObject>(), JsonObject::default()) }
}

unsafe fn json_object_clone_into(src: *const (), dst: *mut ()) {
    // SAFETY: facet guarantees `src` points to a live `JsonObject` and `dst`
    // points to writable, correctly-aligned memory of the right size.
    unsafe {
        let value = (*src.cast::<JsonObject>()).clone();
        core::ptr::write(dst.cast::<JsonObject>(), value);
    }
}

static JSON_OBJECT_TYPE_OPS: TypeOpsDirect = TypeOpsDirect {
    drop_in_place: json_object_drop_in_place,
    default_in_place: Some(json_object_default_in_place),
    clone_into: Some(json_object_clone_into),
    is_truthy: None,
};

// Proxy contract: vox-postcard / facet-json / etc. round-trip `JsonObject`
// through a `String` containing the JSON text. This avoids needing
// reflection-format-specific support for `serde_json::Value`.
unsafe fn json_object_convert_out(
    target_ptr: PtrConst,
    proxy_ptr: PtrUninit,
) -> Result<PtrMut, String> {
    // SAFETY: caller guarantees `target_ptr` points to an initialized `JsonObject`
    // and `proxy_ptr` points to writable memory sized for `String`.
    let target: &JsonObject = unsafe { target_ptr.get::<JsonObject>() };
    let text =
        serde_json::to_string(&target.0).map_err(|e| format!("JsonObject -> JSON text: {e}"))?;
    Ok(unsafe { proxy_ptr.put::<String>(text) })
}

unsafe fn json_object_convert_in(
    proxy_ptr: PtrConst,
    target_ptr: PtrUninit,
) -> Result<PtrMut, String> {
    // SAFETY: caller guarantees `proxy_ptr` points to an initialized `String`
    // and `target_ptr` points to writable memory sized for `JsonObject`.
    let text: String = unsafe { proxy_ptr.read::<String>() };
    let value: serde_json::Value =
        serde_json::from_str(&text).map_err(|e| format!("JSON text -> JsonObject: {e}"))?;
    Ok(unsafe { target_ptr.put::<JsonObject>(JsonObject(value)) })
}

static JSON_OBJECT_PROXY: ProxyDef = ProxyDef {
    shape: <String as Facet>::SHAPE,
    convert_in: json_object_convert_in,
    convert_out: json_object_convert_out,
};

unsafe impl<'facet> Facet<'facet> for JsonObject {
    const SHAPE: &'static Shape = &const {
        Shape::builder_for_sized::<JsonObject>("JsonObject")
            .variance(VarianceDesc::INVARIANT)
            .type_ops_direct(&JSON_OBJECT_TYPE_OPS)
            .proxy(&JSON_OBJECT_PROXY)
            .build()
    };
}

// ── SeaORM glue: store as JSON column ──────────────────────────────────────

impl From<JsonObject> for Value {
    fn from(value: JsonObject) -> Self {
        Value::Json(Some(Box::new(value.0)))
    }
}

impl Nullable for JsonObject {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for JsonObject {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        Ok(Self(value))
    }
}

impl ValueType for JsonObject {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(inner)) => Ok(Self(*inner)),
            Value::Json(None) => Ok(Self::default()),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        "JsonObject".to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}
