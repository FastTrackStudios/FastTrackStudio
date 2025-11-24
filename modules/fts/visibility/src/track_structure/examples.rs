//! Example track structures for different groups
//!
//! These examples show how to define track structures for various group types.

use super::{
    TrackNode, TrackRole, TrackCategory,
    TrackStructure, EntryType, GroupingStrategy,
    FilterRule, FilterType,
};
use crate::track_scope::TrackIdentifier;

/// Example: Kick group structure
/// 
/// Structure:
/// - KICK BUS (root)
///   - Kick SUM
///     - In
///     - Out
///     - Trig
///   - Ambient
///   - Sub
pub fn kick_structure() -> TrackStructure {
    // Root: KICK BUS
    let mut root = TrackNode::new(
        TrackIdentifier::name("KICK BUS"),
        "KICK BUS".to_string(),
        TrackRole::Bus,
        TrackCategory::Bus,
    );
    
    // Kick SUM (child of root)
    let mut kick_sum = TrackNode::new(
        TrackIdentifier::name("Kick SUM"),
        "Kick SUM".to_string(),
        TrackRole::Sum,
        TrackCategory::Bus,
    );
    
    // In, Out, Trig (children of Kick SUM)
    kick_sum.add_child(TrackNode::new(
        TrackIdentifier::name("In"),
        "In".to_string(),
        TrackRole::Input,
        TrackCategory::Audio,
    ));
    kick_sum.add_child(TrackNode::new(
        TrackIdentifier::name("Out"),
        "Out".to_string(),
        TrackRole::Output,
        TrackCategory::Audio,
    ));
    kick_sum.add_child(TrackNode::new(
        TrackIdentifier::name("Trig"),
        "Trig".to_string(),
        TrackRole::Trigger,
        TrackCategory::Midi,
    ));
    
    root.add_child(kick_sum);
    
    // Ambient (child of root)
    root.add_child(TrackNode::new(
        TrackIdentifier::name("Ambient"),
        "Ambient".to_string(),
        TrackRole::Ambient,
        TrackCategory::Audio,
    ));
    
    // Sub (child of root)
    root.add_child(TrackNode::new(
        TrackIdentifier::name("Sub"),
        "Sub".to_string(),
        TrackRole::Sub,
        TrackCategory::Audio,
    ));
    
    TrackStructure {
        root,
        entry_types: vec![EntryType::Mono], // Kick is typically mono
        grouping_strategy: GroupingStrategy::Flat, // Single kick per project
        filter_rules: vec![
            FilterRule {
                filter_type: FilterType::AudioOnly,
                include: true,
            },
        ],
    }
}

/// Example: Electric Guitar entry types
/// 
/// Can be organized by:
/// - Performer (Blaine, Cody, Paul, Hector)
/// - Arrangement (Clean, Drive, Ambient)
/// 
/// Entry types:
/// - Stereo track
/// - Stereo + DI
/// - Doubled + DI + No FX
pub fn guitar_entry_types() -> Vec<EntryType> {
    vec![
        EntryType::Stereo,
        EntryType::StereoWithDi,
        EntryType::Doubled,
        EntryType::DoubledWithDi,
        EntryType::DoubledWithDiAndNoFx,
    ]
}

