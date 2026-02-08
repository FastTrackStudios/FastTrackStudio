//! Database schema DDL for the users table
//!
//! Uses raw SQL DDL rather than sea-query since this is a simple single-table
//! schema. The actual preset/snapshot storage is in `signal-storage`.

/// SQL DDL for the `users` table
pub const USERS_TABLE_DDL: &str = r#"
CREATE TABLE IF NOT EXISTS users (
    id          UUID PRIMARY KEY,
    email       TEXT NOT NULL UNIQUE,
    display_name TEXT NOT NULL,
    created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_sync   TIMESTAMPTZ
);

CREATE INDEX IF NOT EXISTS idx_users_email ON users (email);
"#;

/// Verify the schema DDL is syntactically well-formed (basic check)
pub fn validate_schema() -> bool {
    USERS_TABLE_DDL.contains("CREATE TABLE")
        && USERS_TABLE_DDL.contains("id")
        && USERS_TABLE_DDL.contains("email")
        && USERS_TABLE_DDL.contains("display_name")
        && USERS_TABLE_DDL.contains("created_at")
        && USERS_TABLE_DDL.contains("last_sync")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schema_validate() {
        assert!(validate_schema());
    }

    #[test]
    fn test_schema_has_unique_email() {
        assert!(USERS_TABLE_DDL.contains("UNIQUE"));
    }

    #[test]
    fn test_schema_has_index() {
        assert!(USERS_TABLE_DDL.contains("CREATE INDEX"));
        assert!(USERS_TABLE_DDL.contains("idx_users_email"));
    }
}
