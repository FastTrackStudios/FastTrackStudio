use anyhow::{Result, Context};
use std::path::PathBuf;
use pdfium_render::prelude::Pdfium;
use tracing::{debug, error};

pub fn get_pdfium_library_path() -> Result<PathBuf> {
    debug!("Looking for PDFium library...");
    // Try application bundle path first (for .app files)
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(app_dir) = exe_path.parent() {
            // From .app/Contents/MacOS/ to .app/Contents/Resources/lib/
            let resources_lib_path = app_dir
                .parent()
                .unwrap_or(app_dir)
                .join("Resources")
                .join("lib")
                .join("libpdfium.dylib");
            
            if resources_lib_path.exists() {
                debug!("Found PDFium library at: {:?}", resources_lib_path);
                return Ok(resources_lib_path);
            } else {
                debug!("PDFium library not found at: {:?}", resources_lib_path);
            }
            
            // From .app/Contents/MacOS/ to .app/Contents/Frameworks/ (alternative path)
            let frameworks_path = app_dir
                .parent()
                .unwrap_or(app_dir)
                .join("Frameworks")
                .join("libpdfium.dylib");
            
            if frameworks_path.exists() {
                debug!("Found PDFium library at: {:?}", frameworks_path);
                return Ok(frameworks_path);
            } else {
                debug!("PDFium library not found at: {:?}", frameworks_path);
            }
            
            // From .app/Contents/MacOS/ to .app/Contents/lib/ (old path)
            let lib_path = app_dir
                .parent()
                .unwrap_or(app_dir)
                .join("lib")
                .join("libpdfium.dylib");
            
            if lib_path.exists() {
                debug!("Found PDFium library at: {:?}", lib_path);
                return Ok(lib_path);
            } else {
                debug!("PDFium library not found at: {:?}", lib_path);
            }
        }
    }
    
    // Development environment: try to find project root and look for lib folder
    // First, try relative to current working directory
    let mut lib_path = std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join("lib")
        .join(if cfg!(target_os = "macos") {
            "libpdfium.dylib"
        } else if cfg!(target_os = "windows") {
            "pdfium.dll"
        } else {
            "libpdfium.so"
        });
    
    if lib_path.exists() {
        debug!("Found PDFium library at: {:?}", lib_path);
        return Ok(lib_path);
    }
    
    // Try to find project root by looking for Cargo.toml
    if let Ok(exe_path) = std::env::current_exe() {
        let mut search_dir = exe_path.parent();
        while let Some(dir) = search_dir {
            let cargo_toml = dir.join("Cargo.toml");
            if cargo_toml.exists() {
                lib_path = dir.join("lib").join(if cfg!(target_os = "macos") {
                    "libpdfium.dylib"
                } else if cfg!(target_os = "windows") {
                    "pdfium.dll"
                } else {
                    "libpdfium.so"
                });
                if lib_path.exists() {
                    debug!("Found PDFium library at: {:?}", lib_path);
                    return Ok(lib_path);
                }
            }
            search_dir = dir.parent();
        }
    }
    
    // Try system library paths
    let system_paths = if cfg!(target_os = "macos") {
        vec![
            PathBuf::from("/usr/local/lib/libpdfium.dylib"),
            PathBuf::from("/opt/homebrew/lib/libpdfium.dylib"),
        ]
    } else if cfg!(target_os = "linux") {
        vec![
            PathBuf::from("/usr/lib/libpdfium.so"),
            PathBuf::from("/usr/local/lib/libpdfium.so"),
        ]
    } else {
        vec![]
    };
    
    for system_path in system_paths {
        if system_path.exists() {
            debug!("Found PDFium library at: {:?}", system_path);
            return Ok(system_path);
        }
    }
    
    let error_msg = format!("PDFium library not found. Searched in: {:?} and system paths", lib_path);
    error!("{}", error_msg);
    Err(anyhow::anyhow!(error_msg))
}

pub fn get_pdf_info(pdf_path: &str) -> Result<(usize, String)> {
    debug!("Getting PDF info for: {}", pdf_path);
    
    let library_path = get_pdfium_library_path()
        .context("Failed to locate PDFium library")?;
    debug!("PDFium library found at: {:?}", library_path);
    
    let pdfium = Pdfium::new(
        Pdfium::bind_to_library(library_path.clone())
            .with_context(|| format!("Failed to bind to PDFium library at: {:?}", library_path))?
    );
    debug!("PDFium instance created successfully");
    
    let document = pdfium.load_pdf_from_file(pdf_path, None)
        .with_context(|| format!("Failed to load PDF file: {}", pdf_path))?;
    debug!("PDF document loaded successfully");
    
    let page_count = document.pages().len();
    debug!("PDF has {} pages", page_count);
    
    let title = "Unknown PDF".to_string(); // Metadata API needs investigation
    
    Ok((page_count.into(), title))
}

