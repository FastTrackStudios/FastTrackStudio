//! Group classification -> DAW track-layout tests.
//!
//! These live here rather than beside their group definitions because they
//! assert on `daw_proto` track structures, and the group definitions now
//! live in the DAW-free `music-convention` crate.

mod bass {
    use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
    use dynamic_template::{default_config, OrganizeIntoTracks};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn full_bass_integration_test() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Bass Guitar", "Bass Synth", "Upright Bass"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Bass")
            .track("Guitar")
            .item("Bass Guitar")
            .track("Synth")
            .item("Bass Synth")
            .track("Upright")
            .item("Upright Bass")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}

mod lead_vocals {
    use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
    use dynamic_template::{default_config, OrganizeIntoTracks};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn single_track_no_grouping_needed() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Vocal Chorus Cody DBL L"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .track("Vocals")
            .item("Vocal Chorus Cody DBL L")
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn multiple_sections_grouped() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Vocal Verse Cody", "Vocal Chorus Cody"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Chorus")
            .item("Vocal Chorus Cody")
            .track("Verse")
            .item("Vocal Verse Cody")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn multiple_performers_grouped() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Vocal Chorus Cody", "Vocal Chorus John"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Cody")
            .item("Vocal Chorus Cody")
            .track("John")
            .item("Vocal Chorus John")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn adding_layers() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Vocal Chorus Cody", "Vocal Chorus Cody DBL"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Main")
            .item("Vocal Chorus Cody")
            .track("DBL")
            .item("Vocal Chorus Cody DBL")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn adding_channels() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Vocal Chorus Cody L",
            "Vocal Chorus Cody C",
            "Vocal Chorus Cody R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("L")
            .item("Vocal Chorus Cody L")
            .track("C")
            .item("Vocal Chorus Cody C")
            .track("R")
            .item("Vocal Chorus Cody R")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn layers_and_channels_together() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Vocal Chorus Cody Main L",
            "Vocal Chorus Cody Main C",
            "Vocal Chorus Cody Main R",
            "Vocal Chorus Cody DBL L",
            "Vocal Chorus Cody DBL C",
            "Vocal Chorus Cody DBL R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .folder("Main")
            .track("L")
            .item("Vocal Chorus Cody Main L")
            .track("C")
            .item("Vocal Chorus Cody Main C")
            .track("R")
            .item("Vocal Chorus Cody Main R")
            .end()
            .folder("DBL")
            .track("L")
            .item("Vocal Chorus Cody DBL L")
            .track("C")
            .item("Vocal Chorus Cody DBL C")
            .track("R")
            .item("Vocal Chorus Cody DBL R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}

mod background_vocals {
    use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
    use dynamic_template::{default_config, OrganizeIntoTracks};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn bgvs_with_harmony_arrangements() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "BGV Chorus Cody Soprano",
            "BGV Chorus Cody Alto",
            "BGV Chorus JT High",
            "BGV Chorus JT Low",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .folder("Cody")
            .track("Soprano")
            .item("BGV Chorus Cody Soprano")
            .track("Alto")
            .item("BGV Chorus Cody Alto")
            .end()
            .folder("JT")
            .track("Low")
            .item("BGV Chorus JT Low")
            .track("High")
            .item("BGV Chorus JT High")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn bgvs_with_voice_parts() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "BGV Chorus Cody Soprano",
            "BGV Chorus Cody Alto",
            "BGV Chorus Cody Tenor",
            "BGV Chorus Cody Bass",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        // NOTE: "Bass" voice part gets stripped to "Vocals" (fallback) due to context stripping
        // TODO: Add "bass" to non-context words to preserve voice part names
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Soprano")
            .item("BGV Chorus Cody Soprano")
            .track("Alto")
            .item("BGV Chorus Cody Alto")
            .track("Tenor")
            .item("BGV Chorus Cody Tenor")
            .track("Vocals")
            .item("BGV Chorus Cody Bass")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn bgvs_with_harmony_descriptors() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "BGV Chorus Cody High",
            "BGV Chorus Cody Low",
            "BGV Chorus Cody Mid",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Low")
            .item("BGV Chorus Cody Low")
            .track("High")
            .item("BGV Chorus Cody High")
            .track("Mid")
            .item("BGV Chorus Cody Mid")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn bgvs_with_numbered_harmonies() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "BGV Chorus Cody Harmony 1",
            "BGV Chorus Cody Harmony 2",
            "BGV Chorus Cody Harmony 3",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Harmony 1")
            .item("BGV Chorus Cody Harmony 1")
            .track("Harmony 2")
            .item("BGV Chorus Cody Harmony 2")
            .track("Harmony 3")
            .item("BGV Chorus Cody Harmony 3")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn bgvs_without_harmony_arrangements() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["BGV Chorus Cody", "BGV Chorus JT", "BGV Chorus Bri"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Bri")
            .item("BGV Chorus Bri")
            .track("Cody")
            .item("BGV Chorus Cody")
            .track("JT")
            .item("BGV Chorus JT")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}

mod drum_kit {
    use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
    use dynamic_template::{default_config, OrganizeIntoTracks};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn full_drum_kit_integration_test() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Kick In",
            "Kick Out",
            "Snare Top",
            "Snare Bottom",
            "Tom 1",
            "Tom 2",
            "Tom 3",
            "Hi Hat",
            "Ride",
            "OH L",
            "OH R",
            "Rooms L",
            "Rooms R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Drums")
            .folder("Kick")
            .track("In")
            .item("Kick In")
            .track("Out")
            .item("Kick Out")
            .end()
            .folder("Snare")
            .track("Top")
            .item("Snare Top")
            .track("Bottom")
            .item("Snare Bottom")
            .end()
            .folder("Toms")
            .track("T1")
            .item("Tom 1")
            .track("T2")
            .item("Tom 2")
            .track("T3")
            .item("Tom 3")
            .end()
            .folder("Cymbals")
            .track("Hi Hat")
            .item("Hi Hat")
            .track("Ride")
            .item("Ride")
            .folder("OH")
            .track("L")
            .item("OH L")
            .track("R")
            .item("OH R")
            .end()
            .end()
            .folder("Rooms")
            .track("L")
            .item("Rooms L")
            .track("R")
            .item("Rooms R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}

mod electric_guitar {
    use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
    use dynamic_template::{default_config, OrganizeIntoTracks};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn single_track_no_grouping_needed() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Guitar Clean DBL L"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .track("Guitars")
            .item("Guitar Clean DBL L")
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn multiple_arrangements_grouped() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Guitar Clean", "Guitar Drive"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Clean")
            .item("Guitar Clean")
            .track("Drive")
            .item("Guitar Drive")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn guitars_with_multi_mics() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Guitar Clean", "Guitar Clean Amp", "Guitar Clean DI"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Amp")
            .item("Guitar Clean Amp")
            .track("DI")
            .item("Guitar Clean DI")
            .track("Electric Clean")
            .item("Guitar Clean")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn multiple_arrangements_with_multi_mics() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean",
            "Guitar Clean Amp",
            "Guitar Clean DI",
            "Guitar Drive",
            "Guitar Drive Amp",
            "Guitar Drive DI",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("Amp")
            .item("Guitar Clean Amp")
            .track("DI")
            .item("Guitar Clean DI")
            .track("Electric")
            .item("Guitar Clean")
            .end()
            .folder("Drive")
            .track("Amp")
            .item("Guitar Drive Amp")
            .track("DI")
            .item("Guitar Drive DI")
            .track("Electric")
            .item("Guitar Drive")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn adding_layers() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean",
            "Guitar Clean Amp",
            "Guitar Clean DI",
            "Guitar Clean DBL",
            "Guitar Clean Amp DBL",
            "Guitar Clean DI DBL",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Main")
            .track("Amp")
            .item("Guitar Clean Amp")
            .track("DI")
            .item("Guitar Clean DI")
            .track("Electric Clean")
            .item("Guitar Clean")
            .end()
            .folder("DBL")
            .track("Amp")
            .item("Guitar Clean Amp DBL")
            .track("DI")
            .item("Guitar Clean DI DBL")
            .track("Electric Clean DBL")
            .item("Guitar Clean DBL")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn adding_channels() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean L",
            "Guitar Clean Amp L",
            "Guitar Clean DI L",
            "Guitar Clean C",
            "Guitar Clean Amp C",
            "Guitar Clean DI C",
            "Guitar Clean R",
            "Guitar Clean Amp R",
            "Guitar Clean DI R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("L")
            .track("Amp")
            .item("Guitar Clean Amp L")
            .track("DI")
            .item("Guitar Clean DI L")
            .track("Electric Clean L")
            .item("Guitar Clean L")
            .end()
            .folder("C")
            .track("Amp")
            .item("Guitar Clean Amp C")
            .track("DI")
            .item("Guitar Clean DI C")
            .track("Electric Clean C")
            .item("Guitar Clean C")
            .end()
            .folder("R")
            .track("Amp")
            .item("Guitar Clean Amp R")
            .track("DI")
            .item("Guitar Clean DI R")
            .track("Electric Clean R")
            .item("Guitar Clean R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn layers_and_channels_together() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean Main L",
            "Guitar Clean Amp Main L",
            "Guitar Clean DI Main L",
            "Guitar Clean Main C",
            "Guitar Clean Amp Main C",
            "Guitar Clean DI Main C",
            "Guitar Clean Main R",
            "Guitar Clean Amp Main R",
            "Guitar Clean DI Main R",
            "Guitar Clean DBL L",
            "Guitar Clean Amp DBL L",
            "Guitar Clean DI DBL L",
            "Guitar Clean DBL C",
            "Guitar Clean Amp DBL C",
            "Guitar Clean DI DBL C",
            "Guitar Clean DBL R",
            "Guitar Clean Amp DBL R",
            "Guitar Clean DI DBL R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Main")
            .folder("L")
            .track("Amp")
            .item("Guitar Clean Amp Main L")
            .track("DI")
            .item("Guitar Clean DI Main L")
            .track("Electric Clean Main L")
            .item("Guitar Clean Main L")
            .end()
            .folder("C")
            .track("Amp")
            .item("Guitar Clean Amp Main C")
            .track("DI")
            .item("Guitar Clean DI Main C")
            .track("Electric Clean Main C")
            .item("Guitar Clean Main C")
            .end()
            .folder("R")
            .track("Amp")
            .item("Guitar Clean Amp Main R")
            .track("DI")
            .item("Guitar Clean DI Main R")
            .track("Electric Clean Main R")
            .item("Guitar Clean Main R")
            .end()
            .end()
            .folder("DBL")
            .folder("L")
            .track("Amp")
            .item("Guitar Clean Amp DBL L")
            .track("DI")
            .item("Guitar Clean DI DBL L")
            .track("Electric Clean DBL L")
            .item("Guitar Clean DBL L")
            .end()
            .folder("C")
            .track("Amp")
            .item("Guitar Clean Amp DBL C")
            .track("DI")
            .item("Guitar Clean DI DBL C")
            .track("Electric Clean DBL C")
            .item("Guitar Clean DBL C")
            .end()
            .folder("R")
            .track("Amp")
            .item("Guitar Clean Amp DBL R")
            .track("DI")
            .item("Guitar Clean DI DBL R")
            .track("Electric Clean DBL R")
            .item("Guitar Clean DBL R")
            .end()
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn arrangements_and_layers_without_multi_mics() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean",
            "Guitar Clean DBL",
            "Guitar Drive",
            "Guitar Drive DBL",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("Main")
            .item("Guitar Clean")
            .track("DBL")
            .item("Guitar Clean DBL")
            .end()
            .folder("Drive")
            .track("Main")
            .item("Guitar Drive")
            .track("DBL")
            .item("Guitar Drive DBL")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn arrangements_and_channels_without_multi_mics() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Guitar Clean L",
            "Guitar Clean C",
            "Guitar Clean R",
            "Guitar Drive L",
            "Guitar Drive C",
            "Guitar Drive R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("L")
            .item("Guitar Clean L")
            .track("C")
            .item("Guitar Clean C")
            .track("R")
            .item("Guitar Clean R")
            .end()
            .folder("Drive")
            .track("L")
            .item("Guitar Drive L")
            .track("C")
            .item("Guitar Drive C")
            .track("R")
            .item("Guitar Drive R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn just_layers_without_multi_mics_or_channels() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Guitar Clean", "Guitar Clean DBL"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Main")
            .item("Guitar Clean")
            .track("DBL")
            .item("Guitar Clean DBL")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }

    #[test]
    fn just_channels_without_multi_mics_or_layers() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec!["Guitar Clean L", "Guitar Clean C", "Guitar Clean R"];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw_proto::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("L")
            .item("Guitar Clean L")
            .track("C")
            .item("Guitar Clean C")
            .track("R")
            .item("Guitar Clean R")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}
