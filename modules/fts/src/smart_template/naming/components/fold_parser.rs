//! Fold-based parser using component system
//!
//! This parser uses the fold pattern to parse track names in two phases:
//! 1. Group Matching Phase: Fold over groups to find the best match
//! 2. Component Parsing Phase: Fold over components in ComponentOrder to build ItemProperties
//!
//! This follows the fold pattern properly:
//! - Each phase transforms the input through a series of operations
//! - The result is a new ItemProperties struct (immutable transformation)
//! - Each folder can be composed and tested independently

use super::{
    ComponentParser, ComponentParseResult, ParseContext,
    RecTagParser, PerformerParser, ArrangementParser, SectionParser,
    LayersParser, MultiMicParser, EffectParser, IncrementParser,
    ChannelParser, PlaylistParser, TrackTypeParser,
};
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::group_config::GroupConfig;
use crate::smart_template::naming::component_order::{ComponentOrder, ComponentOrderType};

/// Fold-based parser that uses component parsers in the correct order
pub struct ComponentFoldParser {
    rec_tag: RecTagParser,
    performer: PerformerParser,
    arrangement: ArrangementParser,
    section: SectionParser,
    layers: LayersParser,
    multi_mic: MultiMicParser,
    effect: EffectParser,
    increment: IncrementParser,
    channel: ChannelParser,
    playlist: PlaylistParser,
    track_type: TrackTypeParser,
    component_order: ComponentOrder,
}

impl ComponentFoldParser {
    pub fn new() -> Self {
        Self {
            rec_tag: RecTagParser::new(),
            performer: PerformerParser::new(),
            arrangement: ArrangementParser::new(),
            section: SectionParser::new(),
            layers: LayersParser::new(),
            multi_mic: MultiMicParser::new(),
            effect: EffectParser::new(),
            increment: IncrementParser::new(),
            channel: ChannelParser::new(),
            playlist: PlaylistParser::new(),
            track_type: TrackTypeParser::new(),
            component_order: ComponentOrder::default(),
        }
    }
    
    /// Create with custom component order
    pub fn with_order(order: ComponentOrder) -> Self {
        Self {
            rec_tag: RecTagParser::new(),
            performer: PerformerParser::new(),
            arrangement: ArrangementParser::new(),
            section: SectionParser::new(),
            layers: LayersParser::new(),
            multi_mic: MultiMicParser::new(),
            effect: EffectParser::new(),
            increment: IncrementParser::new(),
            channel: ChannelParser::new(),
            playlist: PlaylistParser::new(),
            track_type: TrackTypeParser::new(),
            component_order: order,
        }
    }
    
    /// Parse a track name into ItemProperties using the fold pattern
    /// 
    /// This follows a two-phase fold:
    /// 1. Group matching phase (handled by caller - determines group_config)
    /// 2. Component parsing phase (this method) - folds over components in ComponentOrder
    /// 
    /// The fold pattern here means we transform the ParseContext through each
    /// component parser, building up ItemProperties incrementally.
    /// 
    /// # Flow
    /// 
    /// 1. Initialize ItemProperties with group info (from Phase 1)
    /// 2. Create ParseContext with matched words from group matching
    /// 3. Fold over components in ComponentOrder:
    ///    - For each component type, call its parser
    ///    - Parser updates context (marks matched words)
    ///    - Parser result updates ItemProperties
    /// 4. Collect unparsed words
    /// 5. Return complete ItemProperties
    /// 
    /// # Example
    /// 
    /// For "D Kick In .02" with Kick group config:
    /// - RecTag: NotFound
    /// - MultiMic: Found("In") → props.multi_mic = Some(vec!["In"])
    /// - Playlist: Found(".02") → props.playlist = Some(".02")
    /// - Result: ItemProperties with all components
    pub fn parse(&self, name: &str, group_config: Option<&GroupConfig>) -> ItemProperties {
        let mut context = ParseContext::new(name);
        if let Some(config) = group_config {
            context = context.with_group_config(config);
        }
        
        // Start with empty ItemProperties - this is our accumulator
        let mut props = ItemProperties::new();
        props.original_name = Some(name.to_string());
        
        // If we have a group config, extract group_prefix and sub_type
        if let Some(config) = group_config {
            // Set group_prefix from the matched group
            props.group_prefix = Some(config.prefix.clone());
            
            // Try to extract sub_type from the name using group patterns
            // Check if any pattern categories match (these are sub-types)
            if let Some(pattern_categories) = &config.pattern_categories {
                let name_lower = name.to_lowercase();
                for (category_name, category) in pattern_categories {
                    // Skip multi-mic descriptors (they're handled by MultiMic parser)
                    if category_name == "In" || category_name == "Out" || category_name == "Top" 
                        || category_name == "Bottom" || category_name == "DI" || category_name == "Amp"
                        || category_name == "Short" || category_name == "Far" || category_name == "Mono" {
                        continue;
                    }
                    
                    // Check if any pattern in this category matches
                    let matches = category.patterns.iter().any(|pattern| {
                        let pattern_lower = pattern.to_lowercase();
                        name_lower.contains(&pattern_lower)
                    });
                    
                    if matches {
                        props.sub_type = Some(vec![category_name.clone()]);
                        // Mark the matched pattern words
                        for pattern in &category.patterns {
                            context.mark_matched(pattern);
                        }
                        break; // Take first match
                    }
                }
            }
        }
        
        // Fold over components in the order defined by ComponentOrder
        // This is the core of the fold pattern: transform context through each component
        for component_type in &self.component_order.order {
            match component_type {
                ComponentOrderType::RecTag => {
                    if let ComponentParseResult::Found(value) = self.rec_tag.parse(&mut context) {
                        props.rec_tag = Some(value);
                    }
                }
                ComponentOrderType::GroupPrefix => {
                    // Group prefix is already set during group matching phase
                    // This is just a placeholder in the order
                }
                ComponentOrderType::SubType => {
                    // SubType is extracted during group matching
                    // This is just a placeholder in the order
                }
                ComponentOrderType::Performer => {
                    if let ComponentParseResult::Found(value) = self.performer.parse(&mut context) {
                        props.performer = Some(value);
                    }
                }
                ComponentOrderType::Arrangement => {
                    if let ComponentParseResult::Found(value) = self.arrangement.parse(&mut context) {
                        props.arrangement = Some(value);
                    }
                }
                ComponentOrderType::Section => {
                    if let ComponentParseResult::Found(value) = self.section.parse(&mut context) {
                        props.section = Some(value);
                    }
                }
                ComponentOrderType::Layers => {
                    if let ComponentParseResult::Found(value) = self.layers.parse(&mut context) {
                        props.layers = Some(value);
                    }
                }
                ComponentOrderType::MultiMic => {
                    // MultiMic can return multiple values, so we collect them
                    let mut multi_mic_values = Vec::new();
                    loop {
                        match self.multi_mic.parse(&mut context) {
                            ComponentParseResult::Found(value) => {
                                multi_mic_values.push(value);
                            }
                            ComponentParseResult::NotFound | ComponentParseResult::Skipped => {
                                break;
                            }
                        }
                    }
                    if !multi_mic_values.is_empty() {
                        props.multi_mic = Some(multi_mic_values);
                    }
                }
                ComponentOrderType::Effect => {
                    // Effect can return multiple values
                    let mut effect_values = Vec::new();
                    loop {
                        match self.effect.parse(&mut context) {
                            ComponentParseResult::Found(value) => {
                                effect_values.push(value);
                            }
                            ComponentParseResult::NotFound | ComponentParseResult::Skipped => {
                                break;
                            }
                        }
                    }
                    if !effect_values.is_empty() {
                        props.effect = Some(effect_values);
                    }
                }
                ComponentOrderType::Increment => {
                    if let ComponentParseResult::Found(value) = self.increment.parse(&mut context) {
                        props.increment = Some(value);
                    }
                }
                ComponentOrderType::Channel => {
                    if let ComponentParseResult::Found(value) = self.channel.parse(&mut context) {
                        props.channel = Some(value);
                    }
                }
                ComponentOrderType::Playlist => {
                    if let ComponentParseResult::Found(value) = self.playlist.parse(&mut context) {
                        props.playlist = Some(value);
                    }
                }
                ComponentOrderType::TrackType => {
                    if let ComponentParseResult::Found(value) = self.track_type.parse(&mut context) {
                        props.track_type = Some(value);
                    }
                }
            }
        }
        
        // Collect unparsed words (words not matched by any component)
        let words: Vec<String> = context.original_name
            .split_whitespace()
            .map(|w| w.trim_matches(|c: char| !c.is_alphanumeric() && c != '.' && c != '-'))
            .filter(|w| !w.is_empty())
            .map(|w| w.to_string())
            .collect();
        
        let unparsed: Vec<String> = words.iter()
            .filter(|w| !context.is_matched(w))
            .cloned()
            .collect();
        
        if !unparsed.is_empty() {
            props.unparsed_words = Some(unparsed);
        }
        
        props
    }
}

impl Default for ComponentFoldParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for group matching using the fold pattern
/// 
/// This allows different group matching strategies to be implemented
/// while following the fold pattern: fold over groups to find the best match.
/// 
/// # Flow
/// 
/// 1. Fold over all groups (in priority order)
/// 2. For each group:
///    - Check if name matches group patterns
///    - Check negative patterns (reject if match)
///    - Calculate match score (priority + pattern matches)
///    - Track best match
/// 3. Return GroupConfig of best match
pub trait GroupFolder: Send + Sync {
    /// Fold over groups to find the best match for the given name
    /// 
    /// Returns the GroupConfig of the best matching group, or None if no match.
    /// This is the first phase of the two-phase fold pattern.
    /// 
    /// # Example
    /// 
    /// For name "D Kick In .02":
    /// - Fold over [Drums, Kick, Snare, ...]
    /// - Drums matches "D" → score 5
    /// - Kick matches "Kick" → score 6 (better)
    /// - Return Kick GroupConfig
    fn fold_groups<'a>(&self, name: &str, groups: &'a [GroupConfig]) 
        -> Option<&'a GroupConfig>;
}

/// Default group folder implementation
/// 
/// Folds over groups, checking patterns and priority to find the best match.
/// 
/// # Algorithm
/// 
/// ```rust
/// let mut best_match = None;
/// let mut best_score = 0;
/// 
/// for group in groups.sorted_by_priority() {
///     if matches_patterns(name, group) && !matches_negative_patterns(name, group) {
///         let score = group.priority + pattern_match_count;
///         if score > best_score {
///             best_match = Some(group);
///             best_score = score;
///         }
///     }
/// }
/// 
/// best_match.map(|g| g.to_group_config())
/// ```
pub struct DefaultGroupFolder {
    /// Enable debug logging
    pub debug: bool,
}

impl DefaultGroupFolder {
    pub fn new() -> Self {
        Self { debug: false }
    }
    
    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }
}

impl GroupFolder for DefaultGroupFolder {
    fn fold_groups<'a>(&self, name: &str, groups: &'a [GroupConfig]) 
        -> Option<&'a GroupConfig> 
    {
        if self.debug {
            eprintln!("[GROUP_FOLDER] Starting group matching for: {}", name);
        }
        
        let name_lower = name.to_lowercase();
        let mut best_match: Option<&GroupConfig> = None;
        let mut best_score = i32::MIN;
        
        // Fold over groups, checking patterns and priority
        for group in groups {
            // Check negative patterns first (reject if match)
            let matches_negative = group.negative_patterns.iter()
                .any(|neg| {
                    let neg_lower = neg.to_lowercase();
                    name_lower == neg_lower
                        || name_lower.starts_with(&format!("{} ", neg_lower))
                        || name_lower.ends_with(&format!(" {}", neg_lower))
                        || name_lower.contains(&format!(" {} ", neg_lower))
                });
            
            if matches_negative {
                if self.debug {
                    eprintln!("[GROUP_FOLDER] Group {} rejected due to negative pattern", group.name);
                }
                continue;
            }
            
            // Check if name matches group patterns
            let pattern_matches: usize = group.patterns.iter()
                .map(|pattern| {
                    let pattern_lower = pattern.to_lowercase();
                    // Check for exact match
                    if name_lower == pattern_lower {
                        return 1;
                    }
                    // Check if name starts with pattern (with or without space/number)
                    if name_lower.starts_with(&pattern_lower) {
                        // Check if followed by space, number, or end of string
                        let remaining = &name_lower[pattern_lower.len()..];
                        if remaining.is_empty() 
                            || remaining.starts_with(' ')
                            || remaining.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false)
                        {
                            return 1;
                        }
                    }
                    // Check if name contains pattern with word boundaries
                    if name_lower.contains(&format!("{} ", pattern_lower))
                        || name_lower.contains(&format!(" {} ", pattern_lower))
                        || name_lower.ends_with(&format!(" {}", pattern_lower))
                    {
                        return 1;
                    }
                    0
                })
                .sum();
            
            if pattern_matches > 0 {
                // Calculate score = priority + pattern matches
                let priority = group.priority.unwrap_or(0);
                let score = priority + pattern_matches as i32;
                
                if self.debug {
                    eprintln!("[GROUP_FOLDER] Group {} matches with score {} (priority: {}, patterns: {})", 
                        group.name, score, priority, pattern_matches);
                }
                
                if score > best_score {
                    best_match = Some(group);
                    best_score = score;
                }
            }
        }
        
        if let Some(matched) = best_match {
            if self.debug {
                eprintln!("[GROUP_FOLDER] Best match: {} (score: {})", matched.name, best_score);
            }
        }
        
        best_match
    }
}

impl Default for DefaultGroupFolder {
    fn default() -> Self {
        Self::new()
    }
}
