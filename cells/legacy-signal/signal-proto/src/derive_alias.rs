derive_aliases::define! {
    // Typestate marker — zero-sized sentinel for PhantomData.
    // Used by: Draft, Committed, AutoUpdate, Pinned, Original, Forked,
    //          Validated, Unvalidated, Resolved, Unresolved,
    //          NeedsModules, ModulesSet, NeedsBlocks, BlocksSet, etc.
    Marker = ::core::fmt::Debug, ::core::clone::Clone, ::core::marker::Copy,
             ::core::cmp::PartialEq, ::core::cmp::Eq, ::core::hash::Hash,
             ::facet::Facet;

    // Domain struct — standard derive set for named domain types.
    // Used by: LayerScene, EngineScene, BlockSnapshot, ModuleSnapshot, etc.
    Domain = ::core::fmt::Debug, ::core::clone::Clone, ::core::cmp::PartialEq,
             ::facet::Facet;
}
