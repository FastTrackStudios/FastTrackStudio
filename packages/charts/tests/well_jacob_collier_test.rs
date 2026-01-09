//! Test for rendering Well - Jacob Collier chart to LilyPond

use charts::{chart_to_lilypond, render_chart_to_pdf};
use keyflow::chart::Chart;

#[test]
fn test_well_jacob_collier_to_lilypond() {
    // Chart data from the example
    let chart_text = r#"
Well - Jacob Collier
120bpm 4/4 #E

Intro 4
1_2 2maj_2 | 4 | x2

VS 16
1_2 2maj_2 | 4 | x^

[Hits]
'1_2 '2maj_2 | '4 |

VS

[Hits]

INST 8

[SOLO Keys] 8

Br 16

br

Outro 16

"#;

    // Parse the chart
    let chart = Chart::parse(chart_text).expect("Failed to parse chart");

    // Convert to LilyPond
    let lilypond = chart_to_lilypond(&chart);

    // Verify the output contains expected elements
    assert!(
        lilypond.contains("Well") && lilypond.contains("Jacob Collier"),
        "Should contain title"
    );
    assert!(lilypond.contains("\\chordmode"), "Should contain chordmode");
    assert!(lilypond.contains("\\key"), "Should contain key signature");
    assert!(lilypond.contains("\\time"), "Should contain time signature");
    assert!(lilypond.contains("\\mark"), "Should contain section marks");

    // Print the output for manual inspection
    println!(
        "Generated LilyPond ({} bytes):\n{}",
        lilypond.len(),
        lilypond
    );

    // Verify it's valid LilyPond syntax (basic checks)
    assert!(lilypond.starts_with("\\header"), "Should start with header");
    assert!(lilypond.contains("\\score"), "Should contain score block");
    assert!(lilypond.contains("\\chordmode"), "Should contain chordmode");
}

#[test]
fn test_well_jacob_collier_to_pdf() {
    // Chart data from the example
    let chart_text = r#"
Well - Jacob Collier
120bpm 4/4 #E

Intro 4
1_2 2maj_2 | 4 | x2

VS 16
1_2 2maj_2 | 4 | x^

[Hits]
'1_2 '2maj_2 | '4 |

VS

[Hits]

INST 8

[SOLO Keys] 8

Br 16

br

Outro 16

"#;

    // Parse the chart
    let chart = Chart::parse(chart_text).expect("Failed to parse chart");

    // Use target directory for test output (workspace root target folder)
    let target_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent() // packages
        .unwrap()
        .parent() // FastTrackStudio
        .unwrap()
        .join("target")
        .join("test_output");

    // Create the directory if it doesn't exist
    std::fs::create_dir_all(&target_dir).expect("Failed to create target directory");

    let output_path = target_dir.join("well_jacob_collier_test.pdf");

    // First, generate the .ly file
    let result_ly = render_chart_to_pdf(&chart, output_path.to_str().unwrap(), false);

    match result_ly {
        Ok(ly_path) => {
            println!("✅ Successfully generated LilyPond file: {}", ly_path);

            // Verify the file exists
            assert!(
                std::path::Path::new(&ly_path).exists(),
                "LilyPond file should exist"
            );

            // Read and verify contents
            let contents =
                std::fs::read_to_string(&ly_path).expect("Failed to read generated LilyPond file");

            assert!(
                contents.contains("Well") && contents.contains("Jacob Collier"),
                "Should contain title"
            );
            assert!(contents.contains("\\chordmode"), "Should contain chordmode");

            println!("✅ LilyPond file verified!");
        }
        Err(e) => {
            panic!("Failed to render chart to LilyPond: {}", e);
        }
    }

    // Now compile to PDF (if lilypond is available)
    let result_pdf = render_chart_to_pdf(&chart, output_path.to_str().unwrap(), true);

    match result_pdf {
        Ok(pdf_path) => {
            println!("✅ Successfully compiled PDF: {}", pdf_path);

            // Verify the PDF file exists
            if std::path::Path::new(&pdf_path).exists() {
                println!("✅ PDF file exists at: {}", pdf_path);
            } else {
                println!(
                    "⚠️  PDF file not found (lilypond may not be installed or compilation failed)"
                );
            }
        }
        Err(e) => {
            // Don't fail the test if lilypond isn't installed, just warn
            println!(
                "⚠️  Could not compile to PDF (lilypond may not be installed): {}",
                e
            );
        }
    }
}
