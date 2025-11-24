# Visibility Domain Module

## Setup Status

✅ **Crate Structure**: Set up as a proper Rust crate with `Cargo.toml` and `src/lib.rs`  
✅ **Workspace Integration**: Added to workspace `Cargo.toml`  
⚠️ **Naming Convention Dependency**: Currently using placeholder - needs to be set up

## Naming Convention Integration

The visibility module depends on the `naming-convention` crate for `FullGroup`, but that crate is currently just a placeholder. 

**To complete setup:**
1. Copy the naming convention domain code from `FTS-Extensions/main/domain/src/naming_convention/` 
2. Set up `naming-convention` as a library crate (add `lib.rs`)
3. Uncomment the dependency in `Cargo.toml`
4. Remove the placeholder `FullGroup` struct in `group.rs`

## About Naming Parsing

**The visibility module does NOT parse track names from strings.**

- **Visibility module**: Uses already-parsed `TrackName` structs from naming-convention domain
- **Naming-convention domain**: Defines `TrackName` struct and `NamingConventionSource` trait
- **fts-naming-parser crate**: The actual parser that implements `NamingConventionSource` and converts strings → `TrackName`

**Flow:**
```
String → fts-naming-parser → TrackName → visibility module (uses for matching)
```

The visibility module only:
- Uses `FullGroup` from naming-convention to create visibility groups
- Uses parsed `TrackName` structs for matching (doesn't parse them itself)
- Organizes tracks into hierarchical groups
