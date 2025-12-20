use monarchy::test_utils::StructureAssertions;
use monarchy::*;

// User's existing type - a simple file
#[derive(Clone, Debug)]
struct File {
    name: String,
}

// Make File convertible to String so it works with IntoInputs
impl Into<String> for File {
    fn into(self) -> String {
        self.name
    }
}

// Define minimal metadata using derive macro
#[derive(Metadata, Clone, Default, Debug)]
struct FileMetadata {
    category: Option<String>,
}

#[test]
fn test_minimal_existing_type() {
    // User has existing files
    let files = vec![
        File {
            name: "test.txt".to_string(),
        },
        File {
            name: "data.txt".to_string(),
        },
    ];

    // Create one group using builder
    let group = Group::<FileMetadata>::builder("T-Files")
        .pattern("test")
        .build();

    // Create config using builder
    let config = Config::builder().group(group).build();

    // Super clean - pass the files directly!
    // IntoInputs trait handles the conversion automatically
    let result = monarchy_sort(files, config).unwrap();

    // Print the tree to see the format
    println!("\nStructure:");
    result.print_tree();

    // Much cleaner assertion syntax!
    result
        .assert()
        .has_total_items(2)
        .has_groups(1)
        .has_root_items(1)
        .has_root_item("data.txt")
        .group("T-Files")
        .contains_exactly(&["test.txt"])
        .done();
}

#[test]
fn test_with_str_slice() {
    // Even cleaner - just use &str directly
    let inputs = ["test.txt", "data.txt"];

    let config = Config::<FileMetadata>::builder()
        .group(Group::builder("TestGroup").pattern("test").build())
        .build();

    let result = monarchy_sort(inputs, config).unwrap();

    result
        .assert()
        .has_total_items(2)
        .has_groups(1)
        .group("TestGroup")
        .contains("test.txt")
        .done();
}

#[test]
fn test_with_vec_string() {
    // Works with Vec<String> too
    let inputs = vec!["test.txt".to_string(), "data.txt".to_string()];

    let config = Config::<FileMetadata>::builder()
        .group(Group::builder("TestGroup").pattern("test").build())
        .build();

    let result = monarchy_sort(inputs, config).unwrap();

    result
        .assert()
        .has_total_items(2)
        .has_groups(1)
        .group("TestGroup")
        .contains("test.txt")
        .done();
}

#[test]
fn test_with_custom_type_iterator() {
    // User's type that implements Into<String>
    #[derive(Debug)]
    struct AudioFile {
        path: String,
    }

    impl Into<String> for AudioFile {
        fn into(self) -> String {
            self.path
        }
    }

    let files = vec![
        AudioFile {
            path: "kick.wav".to_string(),
        },
        AudioFile {
            path: "snare.wav".to_string(),
        },
    ];

    let config = Config::<FileMetadata>::builder()
        .group(
            Group::builder("Drums")
                .patterns(vec!["kick", "snare"])
                .build(),
        )
        .build();

    // Can pass the vec directly since AudioFile implements Into<String>
    let result = monarchy_sort(files, config).unwrap();

    result
        .assert()
        .has_total_items(2)
        .has_groups(1)
        .group("Drums")
        .contains_exactly(&["kick.wav", "snare.wav"])
        .done();
}

#[test]
fn test_with_iterator_of_references() {
    // Test that we can also use references if needed
    let files = vec![
        File {
            name: "document.pdf".to_string(),
        },
        File {
            name: "image.png".to_string(),
        },
        File {
            name: "document.txt".to_string(),
        },
    ];

    // Can still use iterator transformations if needed
    let names: Vec<String> = files.iter().map(|f| f.name.clone()).collect();

    let config = Config::<FileMetadata>::builder()
        .group(
            Group::builder("Documents")
                .patterns(vec!["document", "pdf", "txt"])
                .build(),
        )
        .build();

    let result = monarchy_sort(names, config).unwrap();

    result
        .assert()
        .has_total_items(3)
        .has_groups(1)
        .has_root_items(1) // image.png doesn't match
        .group("Documents")
        .contains_exactly(&["document.pdf", "document.txt"])
        .done();
}

#[test]
fn test_with_mixed_types() {
    // Test that IntoInputs works with various types

    // Direct string slices
    let result1 = monarchy_sort(
        ["file1.txt", "file2.txt"],
        Config::<FileMetadata>::builder().build(),
    );
    assert!(result1.is_ok());

    // Vec of Strings
    let result2 = monarchy_sort(
        vec!["file1.txt".to_string()],
        Config::<FileMetadata>::builder().build(),
    );
    assert!(result2.is_ok());

    // Iterator chain
    let result3 = monarchy_sort(
        (0..3).map(|i| format!("file{}.txt", i)),
        Config::<FileMetadata>::builder().build(),
    );
    assert!(result3.is_ok());
    result3.unwrap().assert().has_total_items(3);
}

#[test]
fn test_variant_fields() {
    // Test variant fields for organizing similar items by a differentiating field

    // Define metadata with a variant field
    #[derive(Metadata, Clone, Default, Debug)]
    struct VersionedMetadata {
        file_type: Option<String>,
        #[monarchy(variant)]
        environment: Option<String>, // This will be used to create variants
    }

    // For this simple test, we'll just show that the variant field is properly configured
    // In a real scenario, the parser would extract the environment from filenames
    let inputs = vec!["config.dev.json", "config.prod.json", "config.staging.json"];

    // Create a group that uses the auto-detected variant field
    let config = Config::<VersionedMetadata>::builder()
        .group(
            Group::builder("Config Files")
                .pattern("config")
                .use_auto_variants() // Automatically uses the #[monarchy(variant)] field
                .build(),
        )
        .build();

    // Check the variant field configuration before moving config
    let has_variant_field = config.groups[0].variant_field.is_some();
    let group_name = config.groups[0].name.clone();

    let result = monarchy_sort(inputs, config).unwrap();

    // Without metadata extraction, all items will go into a single "default" variant
    // This test primarily demonstrates that the variant field is properly configured
    result
        .assert()
        .has_total_items(3)
        .has_groups(1)
        .group("Config Files")
        .has_items(0) // Items would be in variant subgroups if metadata was extracted
        .has_subgroups(1) // Just "default" since parser doesn't extract environment yet
        .done();

    // Verify the group has variant field configured
    assert!(
        has_variant_field,
        "Group should have variant field configured"
    );

    // The variant field should be 'Environment' based on our metadata definition
    println!("\nGroup '{}' is configured with variant field", group_name);
    println!(
        "When the parser extracts environment metadata, items will be organized by environment"
    );

    // Print the actual structure to visualize the organization
    println!("\nActual structure:");
    result.print_tree();
}

#[test]
fn test_deep_hierarchy() {
    // Test to showcase the tree format with deeper hierarchy

    #[derive(Metadata, Clone, Default, Debug)]
    struct ProjectMetadata {
        file_type: Option<String>,
        language: Option<String>,
    }

    let config = Config::<ProjectMetadata>::builder()
        .group(
            Group::builder("Backend")
                .patterns(["backend", "server"])
                .priority(100)
                .group(
                    Group::builder("API")
                        .pattern("api")
                        .group(Group::builder("v1").pattern("v1").build())
                        .group(Group::builder("v2").pattern("v2").build())
                        .build(),
                )
                .group(
                    Group::builder("Database")
                        .patterns(["db", "database"])
                        .group(Group::builder("Migrations").pattern("migration").build())
                        .build(),
                )
                .build(),
        )
        .group(
            Group::builder("Frontend")
                .patterns(["frontend", "client"])
                .priority(90)
                .group(Group::builder("Components").pattern("component").build())
                .group(Group::builder("Styles").patterns(["style", "css"]).build())
                .build(),
        )
        .fallback(FallbackStrategy::CreateMisc)
        .build();

    let inputs = vec![
        "backend/api/v1/users.rs",
        "backend/api/v1/posts.rs",
        "backend/api/v2/users.rs",
        "backend/api/v2/auth.rs",
        "backend/db/migration_001.sql",
        "backend/db/migration_002.sql",
        "backend/server.rs",
        "frontend/components/header.jsx",
        "frontend/components/footer.jsx",
        "frontend/styles/main.css",
        "frontend/styles/theme.css",
        "frontend/index.html",
        "README.md",
        "package.json",
    ];

    let result = monarchy_sort(inputs, config).unwrap();

    // Print the hierarchical structure
    println!("\nDeep hierarchy structure:");
    result.print_tree();

    // Verify the structure
    result.assert().has_total_items(14).has_groups(3); // Backend, Frontend, Misc
}
