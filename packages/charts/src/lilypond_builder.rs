use std::env;
use std::fs;

/// Generates inlined utility content instead of include statements
/// This eliminates all path issues by directly embedding the content
pub fn generate_inlined_utility_content(output_dir: &str) -> String {
    // Use the full list of utility files with inline mode
    let utility_files = vec![
        "third-party/esmuflily/ly/esmufl.ily",
        "third-party/esmuflily/ly/cosmufl.ily",
        "fonts.ly",
        "chord-display.ly",
        "note-placement.ly",
        "key-changes.ly",
        "paper-setup.ly",
        "capsules/measure-position-detection.ly",
        "capsules/capsule-utils.ly",
        "capsules/new-capsule-utils.ly",
        "rehearsal-marks/rehearsal-mark-positioning.ly",
        "breaks/auto-four-measure-breaks.ly",
        "breaks/pseudo-indents.ly",
        "breaks/auto-pseudo-indents.ly",
        "layout/spacing.ly",
        "layout/score-setup.ly",
        "staff/staff-setup.ly",
        "music/music-definitions.ly",
        "music/music-utils.ly",
        "music/music-engravers.ly",
        "section-engraver.ly",
        "custom-rhythmic-notation.ly",
        "header-template.ly",
        "testing.ly",
    ];
    
    generate_utility_content(output_dir, false, &utility_files)
}

/// Builds a complete LilyPond document with all utilities inlined
pub fn build_lilypond_document(content: &str, output_dir: &str) -> String {
    let inlined_utils = generate_inlined_utility_content(output_dir);
    format!(r#"\version "2.24.0"

% Set the font to Bravura for SMuFL support

{}

{}"#, inlined_utils, content)
}

/// Builds a minimal LilyPond document with only essential utilities
pub fn build_minimal_lilypond_document(content: &str, output_dir: &str) -> String {
    // Use the exact same include list as slash.ly for compatibility
    let utility_files = vec![
        "fonts.ly",
        "chord-display.ly",
        "key-changes.ly",
        "note-placement.ly",
        "paper-setup.ly",
        "capsules/measure-position-detection.ly",
        "capsules/capsule-utils.ly",
        "capsules/new-capsule-utils.ly",
        "rehearsal-marks/rehearsal-mark-positioning.ly",
        "breaks/pseudo-indents.ly",
        "breaks/auto-pseudo-indents.ly",
        "layout/spacing.ly",
        "layout/score-layout.ly",
        "custom-rhythmic-notation.ly",
        "header-template.ly",
    ];
    
    let include_utils = generate_utility_content(output_dir, true, &utility_files);
    format!(r#"\version "2.24.0"

% Set the font to Bravura for SMuFL support
ekmFont = "Bravura"

{}

{}"#, include_utils, content)
}

/// Generates minimal utility content with only note-placement
pub fn generate_minimal_utility_content(output_dir: &str) -> String {
    generate_utility_content(output_dir, true, &vec![
        "fonts.ly",
        "chord-display.ly",
        "note-placement.ly",
        "key-changes.ly",
        "paper-setup.ly",
        "custom-rhythmic-notation.ly",
        "header-template.ly",
    ])
}

/// Generates utility content with either includes or inline content
/// include_mode: true for \include statements, false for inline content
pub fn generate_utility_content(output_dir: &str, include_mode: bool, utility_files: &[&str]) -> String {
    let current_dir = env::current_dir().expect("Failed to get current directory");
    
    // Find the FTS-Extensions root directory by looking for Cargo.toml
    let mut utils_dir = String::new();
    let mut search_dir = current_dir.clone();
    
    loop {
        let cargo_toml_path = search_dir.join("Cargo.toml");
        let charts_dir = search_dir.join("charts");
        
        if cargo_toml_path.exists() && charts_dir.exists() {
            // Found FTS-Extensions root
            utils_dir = charts_dir.join("resources/utils/lilypond").to_string_lossy().to_string();
            break;
        }
        
        if let Some(parent) = search_dir.parent() {
            search_dir = parent.to_path_buf();
        } else {
            // Fallback to relative path
            utils_dir = "charts/resources/utils/lilypond".to_string();
            break;
        }
    }
    
    let mut content = String::new();
    
    // Add english.ly include (this is a system file)
    content.push_str(r#"\include "english.ly"

"#);
    
    if include_mode {
        // Generate relative include statements
        let output_path = std::path::Path::new(output_dir);
        let utils_path = std::path::Path::new(&utils_dir);
        
        // Calculate relative path from output to utils
        let relative_path = if let Some(rel_path) = pathdiff::diff_paths(utils_path, output_path) {
            rel_path.to_string_lossy().to_string()
        } else {
            // Fallback: use absolute path
            utils_dir.clone()
        };
        
        for file in utility_files {
            let include_path = if relative_path.is_empty() {
                format!("{}", file)
            } else {
                format!("{}/{}", relative_path, file)
            };
            content.push_str(&format!(r#"\include "{}"

"#, include_path));
        }
    } else {
        // Inline each utility file
        for file in utility_files {
            let file_path = format!("{}/{}", utils_dir, file);
            match std::fs::read_to_string(&file_path) {
                Ok(file_content) => {
                    content.push_str(&format!("\n% === {} ===\n", file));
                    content.push_str(&file_content);
                    content.push_str("\n\n");
                }
                Err(e) => {
                    println!("Warning: Could not read {}: {}", file_path, e);
                }
            }
        }
    }
    
    content
}

/// Compiles a LilyPond file to PDF
/// Returns true if compilation was successful, false otherwise
pub fn compile(filename: &str) -> bool {
    use std::process::Command;
    use std::path::Path;
    
    println!("Compiling {} with LilyPond...", filename);
    
    // Get the directory of the .ly file
    let file_path = Path::new(filename);
    let file_dir = file_path.parent().unwrap_or(Path::new("."));
    let file_name = file_path.file_name().unwrap();
    
    let output = Command::new("lilypond")
        .arg(file_name)
        .current_dir(file_dir)
        .output();
    
    match output {
        Ok(result) => {
            if result.status.success() {
                println!("✅ Successfully compiled {} to PDF!", filename);
                true
            } else {
                println!("❌ LilyPond compilation failed.");
                if !result.stderr.is_empty() {
                    eprintln!("Error output: {}", String::from_utf8_lossy(&result.stderr));
                }
                false
            }
        }
        Err(e) => {
            println!("❌ Failed to run lilypond command: {}", e);
            println!("Make sure LilyPond is installed and available in your PATH.");
            false
        }
    }
}

/// Check if a file is a valid LilyPond file (just checks extension)
pub fn is_lilypond_file(filename: &str) -> bool {
    filename.ends_with(".ly")
}

/// Write LilyPond content to a file and optionally compile it
/// Returns the path to the written file
pub fn write_and_compile(content: &str, output_path: &str, should_compile: bool) -> std::io::Result<String> {
    use std::fs;
    use std::path::Path;
    
    // Ensure the output directory exists
    if let Some(parent) = Path::new(output_path).parent() {
        fs::create_dir_all(parent)?;
    }
    
    // Write the content to the file
    fs::write(output_path, content)?;
    println!("✅ Written LilyPond content to: {}", output_path);
    
    // Compile if requested
    if should_compile {
        if compile(output_path) {
            // Get the PDF path (replace .ly with .pdf)
            let pdf_path = output_path.replace(".ly", ".pdf");
            println!("✅ PDF generated: {}", pdf_path);
        }
    }
    
    Ok(output_path.to_string())
}

/// Generate a slash notation example for testing
pub fn generate_slash_example(output_dir: &str) -> String {
    let slash_content = r#"
\version "2.24.0"

\include "english.ly"

\header {
  title = "Slash Notation Example"
  composer = "Demo"
}

\score {
  <<
  \new ChordNames = "chordProgression" {
    \set chordChanges = ##t
    \override ChordName.font-size = #3
    \override ChordName.font-name = #"MuseJazz Text"
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing.padding = #0.9
    
    % Add your chord progression here
    \chordmode {
      c1:maj7 | d:m7 | e:m7 | f:maj7 |
      g:7 | a:m7 | b:m7.5- | c:maj7 |
    }
  }

  \new Staff {
    \clef treble
    \time 4/4
    \key c \major
    
    \autoInlineBreaks {
        \rh{

      \mark "CH 8"
      s1*6
      \mark "VS 2"
      4 4 8 8. 16 16 16 s1*6
      \mark "CH 2"
      s1*5
      \mark "VS 3"
      s1*4
      \mark "CH 3"
        }
    }
  }
  >>
  
  \layout {
    indent = 0
    ragged-right = ##f
  }
}"#;

    build_lilypond_document(slash_content, output_dir)
}