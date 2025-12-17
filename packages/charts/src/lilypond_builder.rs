use std::env;
use std::fs;
use std::path::Path;

/// Generates inlined utility content instead of include statements
/// This eliminates all path issues by directly embedding the content
pub fn generate_inlined_utility_content(output_dir: &str) -> String {
    // Use the full list of utility files with inline mode
    let utility_files = vec![
        "third-party/esmuflily/ly/esmufl.ily",
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
    
    // Find the charts package directory by looking for packages/charts/resources/utils/lilypond
    let mut utils_dir = String::new();
    let mut search_dir = current_dir.clone();
    
    loop {
        // Check for packages/charts/resources/utils/lilypond structure
        let charts_resources = search_dir.join("packages").join("charts").join("resources").join("utils").join("lilypond");
        if charts_resources.exists() {
            utils_dir = charts_resources.to_string_lossy().to_string();
            break;
        }
        
        // Also check for charts/resources/utils/lilypond (direct charts directory)
        let charts_dir = search_dir.join("charts");
        let charts_resources_alt = charts_dir.join("resources").join("utils").join("lilypond");
        if charts_resources_alt.exists() {
            utils_dir = charts_resources_alt.to_string_lossy().to_string();
            break;
        }
        
        // Check if we're already in the charts package directory
        let current_charts = search_dir.join("resources").join("utils").join("lilypond");
        if current_charts.exists() {
            utils_dir = current_charts.to_string_lossy().to_string();
            break;
        }
        
        if let Some(parent) = search_dir.parent() {
            search_dir = parent.to_path_buf();
        } else {
            // Fallback: try to find from CARGO_MANIFEST_DIR if available
            if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
                let manifest_path = std::path::Path::new(&manifest_dir);
                let charts_resources = manifest_path.join("resources").join("utils").join("lilypond");
                if charts_resources.exists() {
                    utils_dir = charts_resources.to_string_lossy().to_string();
                    break;
                }
            }
            // Last fallback: relative path from workspace root
            utils_dir = "packages/charts/resources/utils/lilypond".to_string();
            break;
        }
    }
    
    let mut content = String::new();
    
    // Add english.ly include (this is a system file)
    content.push_str(r#"\include "english.ly"

"#);
    
    if include_mode {
        // Generate relative include statements
        let output_path = Path::new(output_dir);
        let utils_path = Path::new(&utils_dir);
        
        // Calculate relative path from output directory to utils directory
        // pathdiff::diff_paths(from, to) calculates: path from 'to' to 'from'
        // So we want: utils_path relative to output_path
        let relative_path = if let (Ok(output_abs), Ok(utils_abs)) = (output_path.canonicalize(), utils_path.canonicalize()) {
            // Calculate relative path using canonicalized absolute paths
            pathdiff::diff_paths(&utils_abs, &output_abs)
                .map(|p| p.to_string_lossy().replace('\\', "/"))
                .unwrap_or_else(|| {
                    // If pathdiff fails, try direct calculation
                    pathdiff::diff_paths(utils_path, output_path)
                        .map(|p| p.to_string_lossy().replace('\\', "/"))
                        .unwrap_or_else(|| utils_dir.clone())
                })
        } else {
            // If canonicalization fails, try direct relative path calculation
            pathdiff::diff_paths(utils_path, output_path)
                .map(|p| p.to_string_lossy().replace('\\', "/"))
                .unwrap_or_else(|| utils_dir.clone())
        };
        
        for file in utility_files {
            let include_path = if relative_path.is_empty() || relative_path == "." {
                format!("{}", file)
            } else {
                // Ensure path ends with / before appending filename
                let base = if relative_path.ends_with('/') {
                    relative_path.clone()
                } else {
                    format!("{}/", relative_path)
                };
                format!("{}{}", base, file)
            };
            content.push_str(&format!(r#"\include "{}"

"#, include_path));
        }
    } else {
        // Inline each utility file, resolving nested includes recursively
        let mut processed_files = std::collections::HashSet::new();
        
        fn inline_file_recursive(
            file_path: &Path,
            utils_dir: &Path,
            content: &mut String,
            processed: &mut std::collections::HashSet<String>,
        ) {
            let file_path_str = file_path.to_string_lossy().to_string();
            if processed.contains(&file_path_str) {
                return; // Avoid circular includes
            }
            processed.insert(file_path_str.clone());
            
            match fs::read_to_string(file_path) {
                Ok(mut file_content) => {
                    // Process \include statements in the file content and recursively inline them
                    let include_pattern = regex::Regex::new(r#"\\include\s+"([^"]+)"#).unwrap();
                    let mut replacements = Vec::new();
                    
                    for cap in include_pattern.captures_iter(&file_content) {
                        let include_path = cap.get(1).unwrap().as_str();
                        let full_match = cap.get(0).unwrap();
                        
                        // Skip system includes like "english.ly"
                        if include_path == "english.ly" {
                            continue;
                        }
                        
                        // Resolve the include path relative to the current file's directory
                        let current_file_dir = file_path.parent().unwrap_or(Path::new(""));
                        let resolved_path = if include_path.starts_with("../") {
                            // Relative path - resolve from current file's directory
                            current_file_dir.join(include_path)
                        } else if include_path.contains('/') {
                            // Path with directory - try from current file dir first, then utils_dir
                            let from_current = current_file_dir.join(include_path);
                            if from_current.exists() {
                                from_current
                            } else {
                                utils_dir.join(include_path)
                            }
                        } else {
                            // Just filename - resolve from current file's directory
                            current_file_dir.join(include_path)
                        };
                        
                        // Try multiple resolution strategies
                        let mut found_path = None;
                        
                        // Strategy 1: Try the resolved path directly
                        if resolved_path.exists() {
                            found_path = Some(resolved_path);
                        } else if let Ok(canonical) = resolved_path.canonicalize() {
                            // Strategy 2: Try canonicalized path
                            if canonical.exists() {
                                found_path = Some(canonical);
                            }
                        }
                        
                        // Strategy 3: Try relative to utils_dir if not found
                        if found_path.is_none() {
                            let utils_path = utils_dir.join(include_path);
                            if utils_path.exists() {
                                found_path = Some(utils_path);
                            } else if let Ok(canonical) = utils_path.canonicalize() {
                                if canonical.exists() {
                                    found_path = Some(canonical);
                                }
                            }
                        }
                        
                        // If we found the file, recursively inline it
                        if let Some(found) = found_path {
                            let canonical_str = found.to_string_lossy().to_string();
                            if !processed.contains(&canonical_str) {
                                // Recursively inline the included file
                                let mut included_content = String::new();
                                inline_file_recursive(&found, utils_dir, &mut included_content, processed);
                                replacements.push((
                                    full_match.start(),
                                    full_match.end(),
                                    format!("\n% === Inlined from: {} ===\n{}", include_path, included_content)
                                ));
                            }
                        } else {
                            println!("Warning: Could not find included file: {} (searched from {})", include_path, file_path.display());
                        }
                    }
                    
                    // Apply replacements in reverse order to maintain indices
                    for (start, end, replacement) in replacements.into_iter().rev() {
                        file_content.replace_range(start..end, &replacement);
                    }
                    
                    content.push_str(&format!("\n% === {} ===\n", file_path_str));
                    content.push_str(&file_content);
                    content.push_str("\n\n");
                }
                Err(e) => {
                    println!("Warning: Could not read {}: {}", file_path.display(), e);
                }
            }
        }
        
        let utils_path = Path::new(&utils_dir);
        for file in utility_files {
            let file_path = utils_path.join(file);
            inline_file_recursive(&file_path, utils_path, &mut content, &mut processed_files);
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