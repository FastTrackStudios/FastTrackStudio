//! Tests for track structure building and display

#[cfg(test)]
mod tests {
    use crate::track_structure::{
        TrackNode, TrackRole, TrackCategory,
        TrackStructure, GroupingStrategy, EntryType,
    };
    use crate::track_structure::builder::{
        FlatTrack, build_structure_from_flat_list,
    };
    use crate::track_scope::TrackIdentifier;

    #[test]
    fn test_build_kick_structure_from_flat_list() {
        // Simulate what we'd get from REAPER
        let tracks = vec![
            FlatTrack::root(
                TrackIdentifier::name("KICK BUS"),
                "KICK BUS".to_string(),
            ),
            FlatTrack::child(
                TrackIdentifier::name("Kick SUM"),
                "Kick SUM".to_string(),
                TrackIdentifier::name("KICK BUS"),
                1,
            ),
            FlatTrack::child(
                TrackIdentifier::name("In"),
                "In".to_string(),
                TrackIdentifier::name("Kick SUM"),
                2,
            ),
            FlatTrack::child(
                TrackIdentifier::name("Out"),
                "Out".to_string(),
                TrackIdentifier::name("Kick SUM"),
                2,
            ),
            {
                let mut trig = FlatTrack::child(
                    TrackIdentifier::name("Trig"),
                    "Trig".to_string(),
                    TrackIdentifier::name("Kick SUM"),
                    2,
                );
                trig.is_midi = true;
                trig.is_audio = false;
                trig
            },
            FlatTrack::child(
                TrackIdentifier::name("Ambient"),
                "Ambient".to_string(),
                TrackIdentifier::name("KICK BUS"),
                1,
            ),
            FlatTrack::child(
                TrackIdentifier::name("Sub"),
                "Sub".to_string(),
                TrackIdentifier::name("KICK BUS"),
                1,
            ),
        ];
        
        let structure = build_structure_from_flat_list(
            tracks,
            vec![EntryType::Mono],
            GroupingStrategy::Flat,
        ).expect("Should build structure");
        
        // Test display
        let display = format!("{}", structure);
        println!("Built structure:\n{}", display);
        
        // Verify structure
        assert_eq!(structure.root.name, "KICK BUS");
        assert_eq!(structure.root.children.len(), 3); // Kick SUM, Ambient, Sub
        
        let kick_sum = structure.root.children.iter()
            .find(|c| c.name == "Kick SUM")
            .expect("Should have Kick SUM");
        
        assert_eq!(kick_sum.children.len(), 3); // In, Out, Trig
        
        // Verify we can select just audio tracks
        let audio_tracks: Vec<&TrackNode> = structure.root
            .all_descendants()
            .into_iter()
            .filter(|n| matches!(n.category, TrackCategory::Audio))
            .collect();
        
        println!("\nAudio tracks only:");
        for track in &audio_tracks {
            println!("  - {} ({})", track.name, track.role);
        }
        
        // Should have In, Out, Ambient, Sub (not Trig which is MIDI)
        assert!(audio_tracks.iter().any(|t| t.name == "In"));
        assert!(audio_tracks.iter().any(|t| t.name == "Out"));
        assert!(audio_tracks.iter().any(|t| t.name == "Ambient"));
        assert!(audio_tracks.iter().any(|t| t.name == "Sub"));
        assert!(!audio_tracks.iter().any(|t| t.name == "Trig")); // MIDI, not audio
    }
    
    #[test]
    fn test_display_track_node() {
        let mut root = TrackNode::new(
            TrackIdentifier::name("KICK BUS"),
            "KICK BUS".to_string(),
            TrackRole::Bus,
            TrackCategory::Bus,
        );
        
        let mut kick_sum = TrackNode::new(
            TrackIdentifier::name("Kick SUM"),
            "Kick SUM".to_string(),
            TrackRole::Sum,
            TrackCategory::Bus,
        );
        
        kick_sum.add_child(TrackNode::new(
            TrackIdentifier::name("In"),
            "In".to_string(),
            TrackRole::Input,
            TrackCategory::Audio,
        ));
        
        root.add_child(kick_sum);
        
        let display = format!("{}", root);
        println!("Track node display:\n{}", display);
        
        assert!(display.contains("KICK BUS"));
        assert!(display.contains("Kick SUM"));
        assert!(display.contains("In"));
    }
}

