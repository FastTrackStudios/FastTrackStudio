/// Folder state for TCP (Track Control Panel)
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TcpFolderState {
    /// Normal folder state
    Normal,
    /// Small folder state
    Small,
    /// Collapsed folder state
    Collapsed,
}

/// Folder state for MCP (Mixer Control Panel)
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum McpFolderState {
    /// Normal folder state
    Normal,
    /// Collapsed folder state
    Collapsed,
}

