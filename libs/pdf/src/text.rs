use crate::types::TextElement;

pub fn filter_overlapping_text(
    text_elements: Vec<TextElement>,
    _page_index: usize,
) -> Vec<TextElement> {
    let mut filtered_elements = Vec::new();

    for element in text_elements {
        let text = element.text.trim();

        // Skip empty text or meaningless strings
        if text.is_empty() || text.len() < 2 {
            continue;
        }

        // Skip short numeric-only strings (like page numbers)
        if text.len() <= 3 && text.chars().all(|c| c.is_numeric()) {
            continue;
        }

        // Duplicate check: check if the same text already exists
        let is_duplicate = filtered_elements.iter().any(|existing: &TextElement| {
            let existing_text = existing.text.trim();

            // Exact match
            if existing_text == text {
                return true;
            }

            // If one contains the other (keep the longer one)
            if text.len() > existing_text.len() && text.contains(existing_text) {
                return false; // New element is longer, so replace existing
            }

            if existing_text.len() > text.len() && existing_text.contains(text) {
                return true; // Existing element is longer, so skip new one
            }

            false
        });

        if !is_duplicate {
            // If replacing with longer text, remove the shorter existing element
            filtered_elements.retain(|existing| {
                let existing_text = existing.text.trim();
                !(text.len() > existing_text.len() && text.contains(existing_text))
            });

            filtered_elements.push(element);
        }
    }

    filtered_elements
}
