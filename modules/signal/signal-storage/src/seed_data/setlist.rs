//! Setlist seed data — demo ordered song lists.

use signal_proto::metadata::Metadata;
use signal_proto::seed_id;
use signal_proto::setlist::{Setlist, SetlistEntry};

/// All default setlist collections.
pub fn setlists() -> Vec<Setlist> {
    vec![demo_setlist()]
}

fn demo_setlist() -> Setlist {
    let feature = SetlistEntry::new(
        seed_id("demo-setlist-feature-demo-song"),
        "Feature-Demo Song",
        seed_id("feature-demo-song"),
    )
    .with_metadata(Metadata::new().with_tag("keys"));

    let dummy = SetlistEntry::new(
        seed_id("demo-setlist-dummy-song"),
        "Dummy Song",
        seed_id("dummy-song"),
    )
    .with_metadata(Metadata::new().with_tag("dummy"));

    let mut setlist = Setlist::new(seed_id("demo-setlist"), "Demo Setlist", feature).with_metadata(
        Metadata::new()
            .with_tag("demo")
            .with_tag("setlist")
            .with_description("Demo setlist containing feature and dummy songs"),
    );
    setlist.add_entry(dummy);
    setlist
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn setlist_count() {
        assert_eq!(setlists().len(), 1);
    }

    #[test]
    fn demo_setlist_contains_two_songs() {
        let setlist = &setlists()[0];
        assert_eq!(setlist.name, "Demo Setlist");
        assert_eq!(setlist.entries.len(), 2);
        assert_eq!(
            setlist.default_entry_id.as_str(),
            seed_id("demo-setlist-feature-demo-song").to_string()
        );
    }
}
