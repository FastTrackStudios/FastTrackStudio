+++
title = "bar contract"
description = "Listing, pagination, and sort invariants the bar repository must hold."
weight = 100
+++

The `bar` feature focuses on **read-side** invariants — the way
`BarRepo::list` paginates, sorts, and reports totals. Rules link to
source via `r[impl <id>]` and `r[verify <id>]` annotations.

Run `cargo xtask tracey-validate` or `tracey query status` to check
coverage. CI fails when any rule is uncovered.

## Empty state

r[bar.list.empty]
A `list` call against an empty repository MUST return `BarList { items:
[], total: 0, page }`. Implementations MUST NOT short-circuit with an
error and MUST report the page argument back verbatim so the caller
can chain.

## Pagination

r[bar.list.pagination.size]
A `list` call with `page.size = N` MUST return at most `N` items.
Implementations MAY return fewer (if past the end of the data) but
MUST NOT exceed `N`.

r[bar.list.pagination.size-clamped]
A `list` call with `page.size = 0` MUST be treated as `size = 1`.
Pagination math depends on a positive divisor; silently clamping is
preferable to returning an error for an edge-case caller input.

r[bar.list.pagination.offset]
A `list` call with `page.index = K` MUST skip the first `K * size`
rows of the underlying ordered sequence. Past-the-end indices MUST
return an empty `items` slice with the same `total`.

## Sort

r[bar.list.sort.name-asc]
A `list` call with `Sort { field: "name", order: Asc }` MUST return
rows ordered by `name` ascending using Rust's default `Ord` for
`String` (UTF-8 byte order).

r[bar.list.sort.name-desc]
A `list` call with `Sort { field: "name", order: Desc }` MUST return
the same rows as `Asc` but in reverse order.

r[bar.list.sort.unknown]
A `list` call with `Sort { field: <unrecognized> }` MUST return
`RepoError::InvalidInput`. This is the architect derive's contract
for fields not annotated `#[architect(sortable)]`.
