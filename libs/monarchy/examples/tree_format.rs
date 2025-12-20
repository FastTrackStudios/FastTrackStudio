//! Example showcasing the clean tree format for displaying hierarchical structures

use monarchy::*;

#[derive(Metadata, Clone, Default, Debug)]
struct ProjectMetadata {
    file_type: Option<String>,
    language: Option<String>,
    #[monarchy(variant)]
    environment: Option<String>,
}

fn main() {
    println!("=== Tree Format Showcase ===\n");

    // Create a configuration with various group levels
    let config = Config::<ProjectMetadata>::builder()
        .group(
            Group::builder("Source")
                .patterns(["src", "source"])
                .priority(100)
                .subgroup(
                    Group::builder("Frontend")
                        .patterns(["frontend", "client"])
                        .build(),
                )
                .subgroup(
                    Group::builder("Backend")
                        .patterns(["backend", "server"])
                        .build(),
                )
                .build(),
        )
        .group(
            Group::builder("Tests")
                .patterns(["test", "spec", "__tests__"])
                .priority(90)
                .build(),
        )
        .group(
            Group::builder("Config")
                .patterns(["config", "settings"])
                .use_auto_variants() // Will use environment variant
                .priority(80)
                .build(),
        )
        .group(
            Group::builder("Documentation")
                .patterns(["README", "CHANGELOG", "docs"])
                .priority(70)
                .build(),
        )
        .fallback(FallbackStrategy::CreateMisc)
        .build();

    // Example 1: Simple flat structure
    println!("1. Simple Flat Structure:\n");
    let simple_inputs = vec!["README.md", "test_main.rs", "src/lib.rs", "package.json"];

    let simple_result = monarchy_sort(simple_inputs, config.clone()).unwrap();
    simple_result.print_tree();

    println!("\n{}\n", "=".repeat(50));

    // Example 2: Larger structure with multiple groups
    println!("2. Multiple Groups with Many Items:\n");
    let medium_inputs = vec![
        "src/frontend/app.js",
        "src/frontend/index.html",
        "src/backend/server.rs",
        "src/backend/database.rs",
        "src/backend/auth.rs",
        "test/unit_test.rs",
        "test/integration_test.rs",
        "test/e2e_test.js",
        "config/dev.yaml",
        "config/prod.yaml",
        "docs/api.md",
        "docs/setup.md",
        "README.md",
        "CHANGELOG.md",
        ".gitignore",
        "Dockerfile",
    ];

    let medium_result = monarchy_sort(medium_inputs, config.clone()).unwrap();
    medium_result.print_tree();

    println!("\n{}\n", "=".repeat(50));

    // Example 3: Demonstrating the "... and N more" feature
    println!("3. Groups with Many Items (showing truncation):\n");
    let many_inputs = vec![
        "test/test1.rs",
        "test/test2.rs",
        "test/test3.rs",
        "test/test4.rs",
        "test/test5.rs",
        "test/test6.rs",
        "test/test7.rs",
        "test/test8.rs",
        "src/file1.rs",
        "src/file2.rs",
        "src/file3.rs",
        "src/file4.rs",
        "src/file5.rs",
    ];

    let many_result = monarchy_sort(many_inputs, config.clone()).unwrap();
    many_result.print_tree();

    println!("\n{}\n", "=".repeat(50));

    // Example 4: Variant fields in action (simulated)
    println!("4. Variant Fields (with simulated metadata):\n");

    // Create items with metadata already set (simulating parser extraction)
    let mut items = Vec::new();

    // Config files for different environments
    for env in ["dev", "staging", "prod"] {
        for file in ["database", "app"] {
            let mut item = Item {
                id: format!("config/{}.{}.yaml", file, env),
                original: format!("config/{}.{}.yaml", file, env),
                metadata: ProjectMetadata::default(),
                matched_group: Some("Config".to_string()),
            };
            // Simulate parser extracting environment
            item.metadata.environment = Some(env.to_string());
            items.push(item);
        }
    }

    // Add some other items
    items.push(Item {
        id: "README.md".to_string(),
        original: "README.md".to_string(),
        metadata: ProjectMetadata::default(),
        matched_group: Some("Documentation".to_string()),
    });

    // Use the organizer directly with pre-parsed items
    let organizer = Organizer::new(config.clone());
    let variant_result = organizer.organize(items);

    variant_result.print_tree();

    println!("\n{}\n", "=".repeat(50));

    // Example 5: Mixed structure with root items
    println!("5. Mixed Structure with Uncategorized Items:\n");
    let mixed_inputs = vec![
        "src/main.rs",
        "test/test.rs",
        "unknown_file.xyz", // Won't match any group
        "random.abc",       // Won't match any group
        "data.json",        // Won't match any group
        "README.md",
    ];

    let mixed_result = monarchy_sort(mixed_inputs, config).unwrap();
    mixed_result.print_tree();

    println!("\n=== Format Features ===\n");
    println!("• Clean dash-based hierarchy (-Target, --SubTarget)");
    println!("• Items shown inline with target: Target: [item1, item2]");
    println!("• Automatic truncation for many items (shows first 2 + count)");
    println!("• Variant fields create sub-structures automatically");
    println!("• Uncategorized items clearly marked at root level");
    println!("• Empty groups are not shown (keeps output clean)");
}
