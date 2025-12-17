use anyhow::{Result, Context};
use pdfium_render::prelude::*;
use base64::Engine;
use crate::types::{PdfPageData, TextElement, TextBounds, RotationAngle};
use crate::utils::get_pdfium_library_path;
use crate::text::filter_overlapping_text;
use tracing::debug;

pub fn render_pdf_page_with_text(pdf_path: &str, page_index: usize, rotation: RotationAngle) -> Result<PdfPageData> {
    debug!("Rendering PDF page {} from: {} (rotation: {:?})", page_index, pdf_path, rotation);
    
    let library_path = get_pdfium_library_path()
        .context("Failed to locate PDFium library")?;
    let pdfium = Pdfium::new(
        Pdfium::bind_to_library(library_path.clone())
            .with_context(|| format!("Failed to bind to PDFium library at: {:?}", library_path))?
    );
    
    let document = pdfium.load_pdf_from_file(pdf_path, None)
        .with_context(|| format!("Failed to load PDF file: {}", pdf_path))?;
    
    let page = document.pages().get(page_index as u16)
        .with_context(|| format!("Failed to get page {} from PDF", page_index))?;
    debug!("Successfully retrieved page {} from PDF", page_index);
    
    // Get original page dimensions
    let original_width = page.width().value;
    let original_height = page.height().value;
    
    // Rendering configuration considering rotation
    let render_config = PdfRenderConfig::new()
        .set_target_width(1000)
        .set_maximum_height(1400)
        .rotate_if_landscape(PdfPageRenderRotation::None, false);
    
    // Manual rotation rendering configuration
    let render_config = match rotation {
        RotationAngle::Rotate90 => render_config.rotate(PdfPageRenderRotation::Degrees90, false),
        RotationAngle::Rotate180 => render_config.rotate(PdfPageRenderRotation::Degrees180, false),
        RotationAngle::Rotate270 => render_config.rotate(PdfPageRenderRotation::Degrees270, false),
        RotationAngle::None => render_config,
    };

    // Re-render (with rotation)
    let bitmap = page.render_with_config(&render_config)
        .with_context(|| format!("Failed to render page {} with rotation {:?}", page_index, rotation))?;
    debug!("Page {} rendered successfully ({}x{})", page_index, bitmap.width(), bitmap.height());
    
    // Use bitmap data directly without color conversion
    let width = bitmap.width() as usize;
    let height = bitmap.height() as usize;
    let bitmap_data = bitmap.as_raw_bytes();
    
    // PNG encode - using bitmap data directly as RGBA8
    let png_data = {
        let mut png_data = Vec::new();
        {
            use image::ImageEncoder;
            let encoder = image::codecs::png::PngEncoder::new(&mut png_data);
            encoder.write_image(&bitmap_data, width as u32, height as u32, image::ExtendedColorType::Rgba8)
                .with_context(|| format!("Failed to encode page {} as PNG", page_index))?;
        }
        debug!("Page {} encoded as PNG ({} bytes)", page_index, png_data.len());
        png_data
    };
    
    let base64_data = base64::engine::general_purpose::STANDARD.encode(&png_data);
    let data_url = format!("data:image/png;base64,{}", base64_data);
    
    // Text extraction
    let _text_page = page.text()
        .with_context(|| format!("Failed to extract text from page {}", page_index))?;
    let mut text_elements = Vec::new();
    
    // Simplified text extraction - API needs investigation
    // For now, just create a placeholder text element
    text_elements.push(TextElement {
        text: "Text extraction needs API update".to_string(),
        bounds: TextBounds {
            x: 0.0,
            y: 0.0,
            width: 100.0,
            height: 12.0,
        },
        font_size: 12.0,
    });
    
    // Filter overlapping text
    text_elements = filter_overlapping_text(text_elements, page_index);
    
    // Final page dimensions considering rotation
    let (final_width, final_height) = match rotation {
        RotationAngle::Rotate90 | RotationAngle::Rotate270 => (original_height, original_width),
        _ => (original_width, original_height),
    };
    
    Ok(PdfPageData {
        image_data: data_url,
        text_elements,
        page_width: final_width,
        page_height: final_height,
        page_index,
        rotation,
    })
}

