//! Loro-backed `VaultRepo`. Emitted by `entity_crdt!`.

use crdt::entity_crdt;
use knowledge_proto::{Vault, VaultCreate, VaultList, VaultRepo, VaultUpdate};

entity_crdt! {
    pub Vault,
    root = "knowledge_vaults",
    fields {
        id: uuid (pk),
        name: str (sortable),
        root_path: opt_str,
        use_markdown_links: bool,
        new_link_format: str,
        attachment_folder_path: str,
        default_view_mode: str,
        config_json: str,
    },
    audit_timestamps,
}
