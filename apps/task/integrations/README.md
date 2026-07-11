# Integrations

This directory is the home for service-specific adapters that connect the core
Task server to external systems.

Core rules:

- Keep canonical task, time, calendar, client, and invoice behavior in
  `crates/task-core`.
- Put provider-specific protocols, mapping quirks, credentials, sync jobs, and
  webhook handlers here.
- Route application behavior through the Vox services first. Integrations should
  call `TaskService`, `TimeService`, `CalendarService`, `ClientService`,
  `InvoiceService`, and related core services rather than bypassing them.
- Do not add UI code here.

Planned integration areas:

- `nextcloud/` for CalDAV, Deck, WebDAV, file sync, and account discovery.
- `invoice-ninja/` for invoice/client/payment synchronization.
- `obsidian/` for plugin packaging and vault-specific affordances.
