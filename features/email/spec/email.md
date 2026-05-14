+++
title = "Email contract"
description = "Tracey-tracked rules the EmailRepo + EmailService implementations must hold."
weight = 100
+++

The email feature is a **full email client + sync bridge** living
alongside Nextcloud Mail. Anything you can do in Nextcloud Mail
(read, write, search, label, move, attach, draft, send) must work in
Task — and both sides stay in sync via IMAP so a label set in Task
shows up in NC Mail and vice versa.

The driving use case: an agent user (`curator`) reads the inbox,
decides which task or project each message belongs to, sets a Proton
label that propagates through every IMAP client, and writes the
authoritative task↔email link into the Task DB. The link is what
makes "show me every email about Project X" a one-query lookup
across the whole graph.

This spec covers the **feature contract** for the email repo +
service. For the deployment pipeline (Proton → ProtonMail Bridge →
Nextcloud Mail → Task), see [`docs/spec/email.md`](../../../docs/spec/email.md) — the
higher-level architecture doc that this feature implements.

## Reference repos

| Repo | URL | What to copy |
|---|---|---|
| **Nextcloud Mail** | <https://github.com/nextcloud/mail#readme> | The reference implementation. Storage layout (`oc_mail_accounts`, `oc_mail_mailboxes`, `oc_mail_messages`, `oc_mail_tags`), threading rules, IMAP-sync state machine, label propagation, attachment cache, sieve rules. Our schema must round-trip cleanly with theirs. |

Feature-parity goal: any message visible in NC Mail's web UI is
visible in Task with the same subject/from/labels/folder/state; any
edit in Task (read/unread, star, label, move, delete, send) reflects
in NC Mail within one IMAP sync cycle.

Rules are linked to source via `r[impl <id>]` and `r[verify <id>]`
annotations. Run `cargo xtask tracey-validate` to confirm coverage.

## Accounts

r[email.account.shape]
An `EmailAccount` row pairs Task with one Nextcloud Mail account.
Fields: `id`, `nc_account_id` (the `oc_mail_accounts` row id on the
NC side), `nc_user` (NC user that owns the account row),
`email_address`, `display_name`, `imap_host`, `imap_port`,
`imap_security` enum (`tls`, `starttls`, `none`), `smtp_host`,
`smtp_port`, `smtp_security` enum, `auth_kind` enum (`password`,
`oauth2`, `app-password`), `auth_ref` (opaque ref into secrets,
never the credential itself).

r[email.account.multi-tenant]
Multiple `EmailAccount` rows may coexist — `codywright→cody@fasttrackaudio.com`
(personal), `curator→agent@fasttrackaudio.com` (curator's own
inbox), `curator→cody@fasttrackaudio.com` (curator triages cody's
inbox). All three live in the same Task DB; `nc_user` distinguishes
which NC identity owns each.

r[email.account.discovery]
`EmailService.discover_accounts()` reads the NC server's
`/index.php/apps/mail/api/accounts` and reconciles into Task. New
accounts are created locally; deleted-on-NC accounts are marked
`archived_at` in Task (never hard-deleted — historical message
references must survive account removal).

## Mailboxes (folders)

r[email.mailbox.shape]
A `Mailbox` row mirrors an IMAP folder: `id`, `account_id`, `path`
(IMAP path with the delimiter from the server: `INBOX`,
`INBOX.Sent`, `INBOX.Archive.2026`), `display_name`, `attributes`
(`Vec<String>` — IMAP `LIST` attributes like `\Drafts`, `\Sent`,
`\Trash`, `\Junk`, `\All`, `\Important`, `\Flagged`),
`unread_count` (denormalized u32 maintained by sync),
`total_count` (u32), `subscribed` (bool — only show subscribed
mailboxes in the UI by default).

r[email.mailbox.special-use]
Special-use mailboxes are identified by their IMAP attributes, not
by name. The UI maps `\Sent` → "Sent", `\Drafts` → "Drafts",
`\Trash` → "Trash", `\Junk` → "Spam", `\Archive` → "Archive". A
mailbox with no special-use attribute renders by `display_name`.

r[email.mailbox.delimiter]
The IMAP path delimiter is account-specific (`.`, `/`, etc.). The
repo stores the raw IMAP path and the delimiter; the UI splits on
the delimiter for the tree view. Delimiter changes after creation
are not supported.

## Messages

r[email.message.shape]
An `Email` (the message entity) carries `id` (Task UUID), `account_id`,
`mailbox_id`, `imap_uid` (u32 — IMAP unique identifier within the
mailbox), `message_id` (RFC822 Message-ID, globally unique),
`thread_id` (UUID — see threading below), `subject`, `from`
(structured `{name, addr}`), `to` / `cc` / `bcc` (each
`Vec<{name, addr}>`), `reply_to`, `in_reply_to` (Message-ID),
`references` (`Vec<Message-ID>`), `date` (envelope date,
`DateTime<Utc>`), `received_at` (when our IMAP sync first saw it).

r[email.message.body-parts]
A message body splits into parts persisted as separate rows in
`EmailBodyPart`: `id`, `email_id`, `part_path` (IMAP part path like
`1.2`), `content_type` (`text/plain`, `text/html`, `multipart/*`),
`content_id` (Option — for inline cid: refs), `charset`,
`size_bytes`, `body_text` (Option — populated for text parts),
`is_attachment` (bool), `filename` (Option for attachments),
`disposition` enum (`inline`, `attachment`).

r[email.message.preview]
`Email.snippet` is a 200-char plain-text preview synthesized from
the first text/plain or text/html body part on import. Used in
list views to avoid loading every body. Recomputed on body update.

r[email.message.flags]
`Email.flags` is a `Vec<String>` mirroring IMAP message flags:
`\Seen`, `\Answered`, `\Flagged`, `\Deleted`, `\Draft`, plus
arbitrary keywords like `$Junk`, `$Phishing`. The repo merges flag
changes via set semantics (concurrent add/remove resolve like
tags). Outbound sync to IMAP applies the diff.

r[email.message.read-state]
`Email.is_read` is a denormalized boolean derived from `\Seen` in
flags. Updates to `is_read` propagate to flags and queue an IMAP
`STORE +FLAGS \Seen` on the next sync. The denormalization exists
so filtering "unread inbox" doesn't require parsing the flags
field on every row.

## Threading

r[email.thread.entity]
An `EmailThread` row groups messages: `id`, `account_id`, `subject`
(latest in thread), `first_message_at`, `last_message_at`,
`message_count` (u32), `unread_count` (u32), `has_attachments`
(bool), `participants` (`Vec<{name, addr}>` deduped across the
thread), `mailbox_ids` (`Vec<Uuid>` — threads can span mailboxes
when a reply lands in a different folder).

r[email.thread.algorithm]
Threading follows JWZ ("In-Reply-To and References, then Subject")
matching NC Mail's behaviour. New messages join an existing thread
when (a) their `in_reply_to` or any `references` Message-ID maps to
an existing message in the thread, or (b) the normalized subject
matches and the from/to overlap and the date is within 30 days.
Re-threading on backfill is idempotent.

r[email.thread.cross-mailbox]
A thread aggregates across mailboxes — replies in Sent + originals
in Inbox are the same thread. The list UI groups by thread when in
"conversation view" and by message in "message view". The
mailbox-filtered list shows threads where any message is in that
mailbox.

## Labels and tags

r[email.tag.shape]
An `EmailTag` row is the IMAP-keyword analog of a Proton label:
`id`, `account_id`, `name` (the keyword as it appears on IMAP —
`Project/Acme`, `awaiting-reply`), `display_name`, `color` (hex),
`is_proton_label` (bool — when true, edits propagate to Proton's
label API in addition to IMAP keywords).

r[email.tag.message-link]
`EmailMessageTag` is a many-to-many link: `id`, `email_id`,
`tag_id`, `added_at`. Adding a tag in Task writes an IMAP `STORE
+FLAGS` for that keyword on the next sync; removing does
`STORE -FLAGS`. On the IMAP→Task direction, a flag diff in IMAP
syncs back as an `EmailMessageTag` insert/delete.

r[email.tag.label-propagation]
For Proton accounts, labels are first-class entities on the Proton
side. The service mirrors Proton labels into `EmailTag` on every
sync and writes label changes back via Proton's REST API in
addition to the IMAP keyword. Other providers use IMAP keywords
only.

## Attachments

r[email.attachment.storage]
Attachments live in `EmailBodyPart` with `is_attachment=true`. The
binary content is NOT stored inline in the row — large parts are
cached on disk under `data/email-attachments/<email_id>/<part_path>`
and the row holds the path. `size_bytes` is recorded inline.

r[email.attachment.fetch-on-demand]
The initial IMAP sync fetches headers + body structure only, not
attachment bytes. The first time a user opens an email with
attachments, the service requests the missing parts via IMAP
`FETCH BODY[<part>]` and persists them. `EmailBodyPart.cached_at`
records when the bytes arrived.

r[email.attachment.inline-images]
Body parts with `disposition=inline` and `content_id` set are
referenced by HTML body parts via `cid:<content_id>`. The HTML
renderer rewrites `cid:` refs to local attachment URLs on display.

## Compose, drafts, send

r[email.draft.shape]
A `Draft` row tracks an in-progress message: `id`, `account_id`,
`from_address`, `to` / `cc` / `bcc` (`Vec<{name, addr}>`),
`subject`, `body_text` (LoroText for multi-peer editing),
`body_html` (Option), `in_reply_to` (Message-ID, Option),
`thread_id` (Option), `attachment_paths` (`Vec<String>` — local
paths to upload),
`scheduled_send_at` (Option — for scheduled send), `sent_at`
(Option — set on successful send).

r[email.draft.autosave]
The UI calls `EmailService.save_draft(draft)` on every text change
(debounced 1s); the service persists locally AND uploads to the
account's `\Drafts` mailbox via IMAP APPEND. Cross-device drafts:
saving on desktop shows up on mobile via standard IMAP sync.

r[email.draft.send]
`EmailService.send(draft_id)` validates required fields (at least
one recipient, non-empty body OR attachments), submits via SMTP,
APPENDs the sent message to `\Sent`, marks the draft `sent_at` and
deletes the IMAP draft entry. On failure leaves the draft intact
with the error attached.

r[email.draft.scheduled-send]
When `scheduled_send_at` is set in the future, the draft stays in
`\Drafts` and the service schedules a job. At the scheduled time
the same send path runs. Cancellation by user clears
`scheduled_send_at`.

r[email.compose.signature]
A `Signature` row per account: `id`, `account_id`, `is_default`,
`name`, `body_text`, `body_html`, `prepend` (bool — for replies,
add above quoted text vs below). Compose UI inserts the default
signature unless overridden.

## Search

r[email.search.fulltext]
The repo provides fulltext search across `subject`, `from.name`,
`from.addr`, `to.name`, `to.addr`, `body_text` (text parts only,
stripped of HTML for HTML parts). A single query like "neumann"
matches a message whose subject mentions Neumann or whose from
address contains it.

r[email.search.filter-grammar]
Structured filters compose as AND: `account_id`, `mailbox_id`,
`thread_id`, `has_attachment`, `is_read`, `is_starred` (flagged),
`from_addr`, `to_addr`, `tag_id`, `date_range`. Matches NC Mail's
filter chip UX.

## Sync state machine

r[email.sync.imap-state]
Per-mailbox sync state lives in `EmailMailboxSyncState`: `id`,
`mailbox_id`, `uidvalidity` (u32), `highest_uid_seen` (u32),
`modseq` (u64 — CONDSTORE), `last_sync_at`, `last_error`. UID
validity changes trigger a full re-sync.

r[email.sync.incremental]
Normal sync fetches new UIDs > `highest_uid_seen` (new messages)
plus `CHANGEDSINCE modseq` for flag/label changes on existing
messages. CONDSTORE (RFC 4551) and QRESYNC (RFC 7162) are
preferred; falls back to full UID flag fetch on servers without
them.

r[email.sync.idle-push]
For servers supporting IMAP IDLE, the service maintains a long-
lived connection per mailbox-of-interest (INBOX + user-pinned
mailboxes) and processes EXISTS / FETCH responses as they arrive
for near-real-time updates. Connection lifetimes are 29 minutes
(RFC limit), reconnect-on-disconnect with exponential backoff.

r[email.sync.bidirectional-writes]
Local changes (flag add/remove, mailbox move, delete, append)
queue an `EmailSyncOp` row that the syncer drains in order. The
op records `account_id`, `kind` (`set-flag`, `unset-flag`,
`move`, `delete`, `append-draft`, …), `args_json`,
`attempted_at`, `succeeded_at`, `error`. Retries on transient
failures; surfaces persistent errors to the UI.

## Task / project linkage

r[email.link.task]
`EmailTaskLink` is the authoritative association of a message
(or thread) with a `Task`: `id`, `email_id` (Option), `thread_id`
(Option — link the whole thread vs one message), `task_id`,
`kind` enum (`relates`, `blocks`, `replied-to`, `attachment-of`),
`created_at`, `created_by_person_id`.

r[email.link.project]
`EmailProjectLink` is the same for projects: `id`, `email_id`
(Option), `thread_id` (Option), `project_id`, `created_at`. A
message may link to many tasks and many projects simultaneously
(common when one email triggers multiple work items).

r[email.link.curator-policy]
The `curator` agent (`agent` user account) reads incoming mail,
applies the project/task tagging policy, and creates
`EmailProjectLink` / `EmailTaskLink` rows. Tag changes propagate
back as Proton labels so the visibility shows up in every IMAP
client. The policy itself is encoded in agent prompts, not in
this spec — only the *link surface* is normative.

## Filters / Sieve rules

r[email.filter.shape]
An `EmailFilter` is a user-defined rule: `id`, `account_id`,
`name`, `priority` (i32 — lower runs first), `enabled` (bool),
`conditions_json` (parsed predicate tree: from/to/subject/body
match, all/any combinator), `actions_json` (move, flag, tag,
delete, forward, mark-read), `last_matched_at`,
`match_count` (u32 — denormalized counter).

r[email.filter.execution]
Filters run on inbound IMAP delivery before the message lands in
the user's inbox. Matching actions apply atomically. NC Mail
exports filter rules as Sieve scripts when the server supports
ManageSieve; otherwise filters run client-side per inbound
message.

## CRDT semantics

r[email.crdt.email-fields-lww]
`Email` scalar fields are LWW. Concurrent edits from two peers
resolve to the most recent commit. The exception is `flags` and
`tag_ids`, which merge as sets (per-element add/remove).

r[email.crdt.draft-body-loro-text]
`Draft.body_text` is a `LoroText` container so two peers editing
the same draft simultaneously merge at character granularity (the
same pattern as knowledge blocks). HTML drafts are stored as
plain `String` LWW — character-level CRDT on HTML breaks more
than it helps (mismatched tags after merge); HTML drafts are
edited by one peer at a time in practice.

r[email.crdt.threads-server-assigned]
Thread membership is server-mediated — the syncer assigns
messages to threads and writes the result. Concurrent peer
edits to thread membership don't happen because peers don't
manipulate threads directly. UI surfaces thread membership as
read-only.

## What this spec does NOT cover (yet)

- **End-to-end encryption** (PGP, S/MIME, ProtonMail's E2EE
  inside the Bridge): bodies arrive plaintext from the Bridge;
  Task treats them as it would any IMAP source. Future spec.
- **Calendar invites in messages** (`text/calendar`): parsed
  attachments only — accepting/declining writes through the
  `calendar` feature, separate spec for the handoff.
- **Templates / canned responses**: future. Drafts cover this
  manually for now.
- **Push notifications across devices** for new mail: handled
  by the `notifications` feature; this spec only emits the
  event.
- **Calendar / contact auto-import from message headers**
  (`X-Original-To`, vCard attachments, schema.org markup):
  future, separate spec.
- **Spam classification beyond IMAP `$Junk` keyword**: ML
  classifiers, Bayesian filtering — out of scope for v1.
- **Newsletter inbox**: dedicated category for bulk senders.
  Future enhancement, not v1.
