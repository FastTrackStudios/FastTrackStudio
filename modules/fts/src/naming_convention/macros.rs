//! Macros for declaratively defining groups

/// Macro to define a group declaratively
/// 
/// Reduces boilerplate by defining groups with all their configuration in one place.
/// All fields except `name` and `prefix` are optional.
/// 
/// Example:
/// ```rust
/// define_group! {
///     name = "Kick",
///     prefix = "K",
///     patterns = ["kick", "kik", "bd", "bassdrum"],
///     negative_patterns = ["keys", "guitar", "gtr"],
///     parent_track = "D KICK (Sum)",
///     arrangement_patterns = ["Thump", "Click", "Sub"],
///     layers_patterns = ["Click", "Thump", "Attack", "Body"],
///     children = [
///         define_group! {
///             name = "In",
///             prefix = "IN",
///             patterns = ["in"],
///             arrangement_patterns = ["Inside", "Internal", "Beater", "Attack"],
///         },
///         define_group! {
///             name = "Out",
///             prefix = "OUT",
///             patterns = ["out"],
///             arrangement_patterns = ["Outside", "External", "Shell", "Body"],
///         },
///     ],
/// }
/// ```
/// 
/// Supported optional fields:
/// - `patterns` - Array of string literals for matching track names
/// - `negative_patterns` - Array of string literals to exclude
/// - `priority` - Integer priority (as string literal, e.g., "95")
/// - `parent_track` - Parent track name
/// - `arrangement_patterns` - Arrangement component patterns
/// - `section_patterns` - Section component patterns
/// - `layers_patterns` - Layers component patterns
/// - `multi_mic_patterns` - Multi-mic component patterns
/// - `track_type_patterns` - Track type component patterns
/// - `rec_tag_patterns` - Recording tag component patterns
/// - `performer_patterns` - Performer component patterns
/// - `playlist_patterns` - Playlist component patterns
/// - `group_type` - Group type identifier (e.g., `Increment`, `Static`, `DynamicHierarchy`)
/// - `children` - Array of nested `FullGroup` instances (can use nested `define_group!` calls)
#[macro_export]
macro_rules! define_group {
    {
        name = $name:literal,
        prefix = $prefix:literal
        $(,patterns = [$($pattern:literal),* $(,)?])?
        $(,negative_patterns = [$($neg_pattern:literal),* $(,)?])?
        $(,priority = $priority:literal)?
        $(,parent_track = $parent_track:literal)?
        $(,arrangement_patterns = [$($arr:literal),* $(,)?])?
        $(,section_patterns = [$($sec:literal),* $(,)?])?
        $(,layers_patterns = [$($lay:literal),* $(,)?])?
        $(,multi_mic_patterns = [$($mic:literal),* $(,)?])?
        $(,effect_patterns = [$($eff:literal),* $(,)?])?
        $(,track_type_patterns = [$($tt:literal),* $(,)?])?
        $(,rec_tag_patterns = [$($rt:literal),* $(,)?])?
        $(,performer_patterns = [$($perf:literal),* $(,)?])?
        $(,playlist_patterns = [$($pl:literal),* $(,)?])?
        $(,group_type = $group_type:ident)?
        $(,children = [$($child:expr),* $(,)?])?
        $(,)?
    } => {
        {
            let mut group = $crate::FullGroup::new($name, $prefix);
            
            // Patterns
            $(group.patterns = vec![$($pattern.to_string()),*];)?
            $(group.negative_patterns = vec![$($neg_pattern.to_string()),*];)?
            $(group.priority = $priority.parse().unwrap_or(0);)?
            $(group.parent_track = Some($parent_track.to_string());)?
            
            // Component patterns (using trait-based system)
            $(group.set_arrangement_patterns(vec![$($arr.to_string()),*]);)?
            $(group.set_section_patterns(vec![$($sec.to_string()),*]);)?
            $(group.set_layers_patterns(vec![$($lay.to_string()),*]);)?
            $(group.set_multi_mic_patterns(vec![$($mic.to_string()),*]);)?
            $(group.set_effect_patterns(vec![$($eff.to_string()),*]);)?
            $(group.set_track_type_patterns(vec![$($tt.to_string()),*]);)?
            $(group.set_rec_tag_patterns(vec![$($rt.to_string()),*]);)?
            $(group.set_performer_patterns(vec![$($perf.to_string()),*]);)?
            $(group.set_playlist_patterns(vec![$($pl.to_string()),*]);)?
            
            // Group type
            $(group.group_type = Some($crate::GroupType::$group_type);)?
            
            // Children
            $(
                $(
                    group.add_child($child);
                )*
            )?
            
            group
        }
    };
}

