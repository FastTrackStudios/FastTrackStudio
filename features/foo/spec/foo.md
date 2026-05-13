+++
title = "foo contract"
description = "CRUD lifecycle invariants the foo repository must hold."
weight = 100
+++

The `foo` feature focuses on **per-row lifecycle** invariants that
every `FooRepo` implementation must satisfy. Rules link to source via
`r[impl <id>]` and `r[verify <id>]` annotations.

Run `cargo xtask tracey-validate` or `tracey query status` to check
coverage. CI fails when any rule is uncovered.

## Identity

r[foo.create.id-generated]
A `create` call MUST allocate a fresh UUID for the row's primary key.
Two `create` calls from the same client in the same millisecond MUST
yield distinct ids. (Realistic implementations use `Uuid::new_v4()`;
any non-colliding generator is acceptable.)

r[foo.create.timestamps]
A `create` call MUST populate `created_at` and `updated_at` from the
server clock. Clients never set these fields — the architect derive
excludes them from the `FooCreate` payload.

## Read

r[foo.get.missing]
A `get` call against an unknown UUID MUST return `RepoError::NotFound`.
Implementations MUST NOT return a partial/empty row or surface a
generic `Internal` error in this case.

## Update

r[foo.update.partial]
An `update` call MUST patch only the fields whose corresponding
`Option<T>` in `FooUpdate` is `Some`. Fields with `None` MUST be left
exactly as they were before the call.

r[foo.update.touches-updated-at]
Every successful `update` MUST set `updated_at` to the current server
clock, regardless of whether any field actually changed. This is the
contract callers rely on to invalidate caches by timestamp.

## Delete

r[foo.delete.missing]
A `delete` call against an unknown UUID MUST return `RepoError::NotFound`.
Implementations MUST NOT silently succeed for missing rows — that
hides bugs in calling code.
