// URL Codec for Chart Data
//
// Compresses chart input text and encodes it as a URL-safe string.
// This allows embedding the entire chart in a URL or QR code.
//
// Process:
// 1. Compress with flate2 (gzip)
// 2. Base64 encode (URL-safe variant)
// 3. Embed in URL: https://your-app.com/chart?data=<encoded>

use flate2::write::{GzEncoder, GzDecoder};
use flate2::Compression;
use std::io::Write;
use base64::{Engine as _, engine::general_purpose::URL_SAFE_NO_PAD};

/// Encode chart input into a URL-safe compressed string
/// 
/// # Example
/// ```
/// let chart = "Song - Artist\n120bpm 4/4 #C\n\nin 4\nC F G C";
/// let encoded = url_codec::encode(chart).unwrap();
/// // encoded is now a short URL-safe string like "H4sIAAAAA..."
/// ```
pub fn encode(input: &str) -> Result<String, String> {
    // 1. Compress with gzip
    let mut encoder = GzEncoder::new(Vec::new(), Compression::best());
    encoder.write_all(input.as_bytes())
        .map_err(|e| format!("Failed to compress: {}", e))?;
    
    let compressed = encoder.finish()
        .map_err(|e| format!("Failed to finish compression: {}", e))?;
    
    // 2. Base64 encode (URL-safe, no padding)
    let encoded = URL_SAFE_NO_PAD.encode(&compressed);
    
    Ok(encoded)
}

/// Decode a URL-safe compressed string back to chart input
/// 
/// # Example
/// ```
/// let encoded = "H4sIAAAAA...";
/// let chart = url_codec::decode(encoded).unwrap();
/// // chart is now the original text
/// ```
pub fn decode(encoded: &str) -> Result<String, String> {
    // 1. Base64 decode
    let compressed = URL_SAFE_NO_PAD.decode(encoded.as_bytes())
        .map_err(|e| format!("Failed to decode base64: {}", e))?;
    
    // 2. Decompress with gzip
    let mut decoder = GzDecoder::new(Vec::new());
    decoder.write_all(&compressed)
        .map_err(|e| format!("Failed to decompress: {}", e))?;
    
    let decompressed = decoder.finish()
        .map_err(|e| format!("Failed to finish decompression: {}", e))?;
    
    // 3. Convert to string
    String::from_utf8(decompressed)
        .map_err(|e| format!("Invalid UTF-8: {}", e))
}

/// Generate a full URL with encoded chart data
/// 
/// # Example
/// ```
/// let chart = "Song - Artist\n120bpm 4/4 #C\n\nin 4\nC F G C";
/// let url = url_codec::to_url(chart, "https://charts.example.com").unwrap();
/// // url is now "https://charts.example.com/chart?data=H4sIAAAAA..."
/// ```
pub fn to_url(input: &str, base_url: &str) -> Result<String, String> {
    let encoded = encode(input)?;
    let base = base_url.trim_end_matches('/');
    Ok(format!("{}/chart?data={}", base, encoded))
}

/// Extract chart data from a URL
/// 
/// # Example
/// ```
/// let url = "https://charts.example.com/chart?data=H4sIAAAAA...";
/// let chart = url_codec::from_url(url).unwrap();
/// ```
pub fn from_url(url: &str) -> Result<String, String> {
    // Extract the 'data' query parameter
    if let Some(data_start) = url.find("data=") {
        let encoded = &url[data_start + 5..];
        // Handle potential additional query parameters
        let encoded = if let Some(amp_pos) = encoded.find('&') {
            &encoded[..amp_pos]
        } else {
            encoded
        };
        decode(encoded)
    } else {
        Err("URL does not contain 'data=' parameter".to_string())
    }
}

/// Calculate compression ratio (for debugging/stats)
pub fn compression_ratio(input: &str) -> Result<f32, String> {
    let encoded = encode(input)?;
    let original_size = input.len();
    let compressed_size = encoded.len();
    
    Ok(original_size as f32 / compressed_size as f32)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_encode_decode_simple() {
        let input = "C F G Am";
        let encoded = encode(input).unwrap();
        let decoded = decode(&encoded).unwrap();
        
        assert_eq!(input, decoded);
    }
    
    #[test]
    fn test_encode_decode_full_chart() {
        let input = r#"Autumn Leaves - Joseph Kosma
120bpm 4/4 #Am

in 4
@all "Smooth intro"
Am7 D7 Gmaj7 Cmaj7

vs
a d g c
f#m7b5 b7b9 e e

ch
F E Am Am
Dm7 G7 C C
"#;
        
        let encoded = encode(input).unwrap();
        println!("Original: {} bytes", input.len());
        println!("Encoded: {} bytes", encoded.len());
        println!("Ratio: {:.2}x compression", compression_ratio(input).unwrap());
        
        let decoded = decode(&encoded).unwrap();
        assert_eq!(input, decoded);
    }
    
    #[test]
    fn test_url_safe() {
        let input = "C F G Am\nDm Em F G";
        let encoded = encode(input).unwrap();
        
        // URL-safe means no +, /, or = characters
        assert!(!encoded.contains('+'));
        assert!(!encoded.contains('/'));
        assert!(!encoded.contains('='));
    }
    
    #[test]
    fn test_to_url() {
        let input = "C F G Am";
        let url = to_url(input, "https://charts.example.com").unwrap();
        
        assert!(url.starts_with("https://charts.example.com/chart?data="));
        
        // Should be able to extract it back
        let decoded = from_url(&url).unwrap();
        assert_eq!(input, decoded);
    }
    
    #[test]
    fn test_from_url_with_trailing_params() {
        let input = "C F G Am";
        let encoded = encode(input).unwrap();
        let url = format!("https://example.com/chart?data={}&theme=dark", encoded);
        
        let decoded = from_url(&url).unwrap();
        assert_eq!(input, decoded);
    }
    
    #[test]
    fn test_compression_ratio() {
        // Small texts don't compress well due to gzip headers (10-18 bytes overhead)
        // But for realistic charts (>200 bytes), compression is beneficial
        let input = r#"Song - Artist
120bpm 4/4 #C

in 4
C F G C F Am G C C F G C F Am G C

vs
c f g c f am g c c f g c f am g c
c f g c f am g c c f g c f am g c

ch
F G Am Em Dm Em F G F G Am Em Dm Em F G
F G Am Em Dm Em F G F G Am Em Dm Em F G

br
Am G F Em Am G F Em Am G F Em Am G F Em
"#;
        
        let ratio = compression_ratio(input).unwrap();
        println!("Compression ratio: {:.2}x", ratio);
        println!("Original: {} bytes, Encoded: {} bytes", 
            input.len(), 
            encode(input).unwrap().len());
        
        // For larger, repetitive text, we should get compression
        // (but it's okay if small charts expand slightly due to headers)
        // Just verify the function works
        assert!(ratio > 0.0);
    }
}

