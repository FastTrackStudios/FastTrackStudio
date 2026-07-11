use facet::Facet;

/// Logical bucket for keys. Backends treat this as a directory
/// (json-on-disk) or a column / table prefix (sqlite). Convention:
/// `"<feature>.<purpose>"` — e.g. `"scheduling.sync"`,
/// `"scheduling.cache"`, `"email.flags"`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
pub struct Namespace(pub String);

impl<S: Into<String>> From<S> for Namespace {
    fn from(s: S) -> Self {
        Self(s.into())
    }
}

/// One entry in an append-only log. `seq` is monotonic per
/// channel; consumers can resume by passing the last `seq` they
/// saw. `data` is opaque bytes — the producer decides the
/// encoding (typically JSON).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct LogEntry {
    pub seq: u64,
    pub appended_utc: String,
    pub data: Vec<u8>,
}
