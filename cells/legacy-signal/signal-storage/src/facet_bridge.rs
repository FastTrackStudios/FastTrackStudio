//! Bridge between Facet serialization and SeaORM's `Json` column type.
//!
//! SeaORM stores JSON columns as `serde_json::Value`, but our domain types
//! use `facet::Facet` (not `serde::Serialize`). This module provides helpers
//! to convert between the two without pulling serde derives into domain types.

use crate::error::{StorageError, StorageResult};

/// Serialize a Facet type to a `serde_json::Value` for storage in a SeaORM `Json` column.
///
/// Two-step conversion: `facet_json::to_string` → `serde_json::from_str`.
pub fn to_json_value<T: for<'a> facet::Facet<'a>>(value: &T) -> StorageResult<serde_json::Value> {
    let json_str = facet_json::to_string(value).map_err(|e| {
        StorageError::Serialization(serde_json::Error::io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("facet serialization failed: {e}"),
        )))
    })?;
    serde_json::from_str(&json_str).map_err(StorageError::Serialization)
}

/// Deserialize a `serde_json::Value` (from a SeaORM `Json` column) into a Facet type.
///
/// Two-step conversion: `serde_json::to_string` → `facet_json::from_str`.
pub fn from_json_value<T: for<'a> facet::Facet<'a>>(value: &serde_json::Value) -> StorageResult<T> {
    let json_str = serde_json::to_string(value).map_err(StorageError::Serialization)?;
    facet_json::from_str(&json_str).map_err(|e| {
        StorageError::Serialization(serde_json::Error::io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("facet deserialization failed: {e}"),
        )))
    })
}

/// Deserialize a JSON string (from a SQLite TEXT column) into a Facet type.
///
/// One-step conversion: `facet_json::from_str`. Use this when the raw
/// string is available (e.g., from `sqlx::Row::get::<String>`).
pub fn from_json_str<T: for<'a> facet::Facet<'a>>(json_str: &str) -> StorageResult<T> {
    facet_json::from_str(json_str).map_err(|e| {
        StorageError::Serialization(serde_json::Error::io(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("facet deserialization failed: {e}"),
        )))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use signal_proto::category::{BaseTone, Genre, PresetCategory};

    #[test]
    fn preset_category_round_trip_generic() {
        let cat = PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        };
        let json = to_json_value(&cat).unwrap();
        let parsed: PresetCategory = from_json_value(&json).unwrap();
        assert_eq!(cat, parsed);
    }

    #[test]
    fn preset_category_round_trip_genre() {
        let cat = PresetCategory::Genre {
            base_tone: BaseTone::Lead,
            genre: Genre::Blues,
        };
        let json = to_json_value(&cat).unwrap();
        let parsed: PresetCategory = from_json_value(&json).unwrap();
        assert_eq!(cat, parsed);
    }

    #[test]
    fn preset_category_round_trip_from_json_str() {
        let cat = PresetCategory::Genre {
            base_tone: BaseTone::Drive,
            genre: Genre::Rock,
        };
        let json = to_json_value(&cat).unwrap();
        let json_str = serde_json::to_string(&json).unwrap();
        let parsed: PresetCategory = from_json_str(&json_str).unwrap();
        assert_eq!(cat, parsed);
    }

    #[test]
    fn preset_category_default_on_invalid_json() {
        let bad_json = serde_json::json!("not a valid category");
        let result: Result<PresetCategory, _> = from_json_value(&bad_json);
        assert!(result.is_err());
    }
}
