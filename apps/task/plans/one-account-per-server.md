# One account per server

**Status:** designed, not started (2026-08-13)

One principal per *server*, with real org memberships — so signing in
once shows you every org you belong to on that server.

Distinct from `federated-task-platform.md` phase 3, which is one account
across *servers* (the identity locker, cross-server links). This is the
inside-one-server half, and phase 3 sits on top of it: a locker that
links servers is much simpler when each server has exactly one principal
per human.

## The problem, concretely

`AppState` opens one `AuthState` per org (`lib.rs:562`, from
`org_root.auth_db()`). Six orgs on production, and
`acodywright@gmail.com` has an account in all six — six *distinct* user
ids that share a login and nothing else.

Nothing joins them, so:

- `.well-known/task-server.json` answers `member` by "does this token
  validate here", which is true for exactly the org that issued it
  (`lib.rs:2411` says so outright: "no cross-org membership table to
  consult").
- The client's `my_orgs_with_links` keeps orgs where `member || linked`,
  so **"All organizations" collapses to the home org**.
- Every multi-org view — projects, tasks, sessions, invoices — funnels
  through `orgs::selected_slugs`, so they are all one-org views in
  practice. The fan-out code is correct and is simply handed a
  one-element list.

The permission gate compounds it: `RoleEngine::with_default_user_role("member")`
means *any* user validated by an org's own store is a member of that
org. Membership is currently a side effect of which database answered.

## Target

```
<data_root>/identity/auth.sqlite     ONE auth store per server
                                     (users, sessions, accounts, …)
<data_root>/identity/memberships     (user_id, org_slug, role, created_at)
<data_root>/orgs/<slug>/…            unchanged: vault, content, feature stores
```

- **Sign-in** issues a server session, not an org session.
- **Org lane** resolves the bearer against the server store → `user_id`,
  then looks up `(user_id, org_slug)` for the role. No membership row =
  not a member = the gate refuses. Membership becomes an explicit fact
  instead of a database-routing accident.
- **Discovery** answers `member` from the membership table.
- **Client**: no change at all. `member: true` for six orgs makes
  `selected_slugs` return six, and every existing fan-out fills in.

## Staging

Each stage is separately deployable, and the user-visible win lands at
S3 — before the riskiest work.

**S1 — server store, dual read.** Create the server auth store and the
membership table. Sign-in issues server sessions; the org lane resolves
against the server store *first* and falls back to the org's own store
for tokens issued before the cutover. No data is moved. Nothing changes
for a self-hoster with one org.

**S2 — `admin merge-principals`, dry-run first.** Report per email:
which orgs hold an account, which user id is canonical (the home org's),
and every row that would be rewritten. Read-only, run against prod, read
the output, *then* run for real. Writes users + memberships into the
server store and persists `user_id_map(org_slug, old_user_id) → user_id`.
That map is permanent — it is how pre-merge references keep resolving.

**S3 — flip discovery + gate.** `member` comes from memberships; roles
come from the membership row rather than `default_user_role`. **"All
organizations" starts working here.** Sessions do not survive the
cutover (different store) — everyone signs in once more, and that must
be said out loud rather than discovered.

**S4 — rewrite references, drop the fallback.** Rewrite `user_id` in the
per-org feature stores through the map — the blast radius is smaller
than it sounds: timer (`timer/src/store.rs`), prefs, identity links,
workflows/agent, presence. Vault content is markdown and holds no user
ids. Then remove the per-org auth fallback and archive the old DBs
(never delete: they are the rollback).

## Risks, named

- **One store, one blast radius.** A corrupt server auth DB locks every
  org out, where today it would lock out one. Backups become
  load-bearing; the existing `task-git-backup` CronJob must cover
  `identity/`.
- **Org portability regresses.** `federated-task-platform.md` sells
  "rsync the org directory to another machine". With identity outside
  the org, moving an org now has to export its membership + user rows.
  That is the price of the merge and phase 3's locker is where it gets
  paid back. Open decision #1 in that doc leaned toward identity living
  *inside* the home org; this design puts it at `<data_root>/identity/`
  instead, because a server hosting six orgs has no single "home".
- **Merging by email is a judgement call.** Two humans sharing an email
  across orgs would merge into one principal. Acceptable here (the
  operator provisioned every account by hand, self-registration is
  closed) but the dry-run must print every merge for a human to read
  before it happens.
- **`architect-auth` assumes users are local to a store.** S1 needs a
  public way to issue a session for a known user
  (`flows.rs:2104 issue_session` is `pub(crate)`; `impersonate_user` is
  public but stamps `impersonated_by`, which would be a lie here).
  architect is in-tree, so this is an ordinary refactor.

## Not doing

- Cross-*server* identity (that is phase 3).
- Self-registration. Accounts stay admin-provisioned.
- Merging accounts with *different* emails. If one human has two emails,
  they get two principals until they say otherwise.
