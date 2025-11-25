use dioxus::prelude::*;
use tracing::{debug, info, warn};

/// Mode for text fitting algorithm
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TextFitMode {
    /// Text fills width, no wrapping
    Oneline,
    /// Text fills width, wraps to multiple lines if needed
    Multiline,
    /// Text fills both width and height, allows wrapping
    Box,
    /// Text fills both width and height, no wrapping
    BoxOneline,
}

impl Default for TextFitMode {
    fn default() -> Self {
        TextFitMode::Multiline
    }
}

/// Props for the TextFit component
#[derive(Props, Clone, PartialEq)]
pub struct TextFitProps {
    /// The text content to fit
    pub text: String,

    /// Optional class name for additional styling
    #[props(default)]
    pub class: Option<String>,

    /// Text fitting mode
    #[props(default)]
    pub mode: TextFitMode,

    /// Minimum font size in pixels
    #[props(default = 8.0)]
    pub min_font_size_px: f64,

    /// Maximum font size in pixels
    #[props(default = 160.0)]
    pub max_font_size_px: f64,

    /// Font size precision in pixels (algorithm stops when reaching this precision)
    #[props(default = 0.1)]
    pub font_size_precision_px: f64,
}

mod wasm_impl {
    use super::*;
    use wasm_bindgen::prelude::*;
    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;
    use web_sys::{window, Element, HtmlElement, ResizeObserver, CssStyleDeclaration};

    /// Helper to get style() from an Element (casts to HtmlElement)
    fn get_style(element: &Element) -> Option<&CssStyleDeclaration> {
        element.dyn_ref::<HtmlElement>().map(|el| el.style())
    }

    /// Get content width of an element (clientWidth - padding)
    fn get_content_width(element: &Element) -> f64 {
        let computed_style = window()
            .and_then(|w| w.get_computed_style(element).ok().flatten())
            .or_else(|| get_style(element).map(|s| s.clone()));
        
        let client_width = element.client_width() as f64;
        let padding_left = computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("padding-left").ok())
            .and_then(|s| s.strip_suffix("px"))
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
        let padding_right = computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("padding-right").ok())
            .and_then(|s| s.strip_suffix("px"))
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
        
        client_width - padding_left - padding_right
    }

    /// Get content height of an element (clientHeight - padding)
    fn get_content_height(element: &Element) -> f64 {
        let computed_style = window()
            .and_then(|w| w.get_computed_style(element).ok().flatten())
            .or_else(|| get_style(element).map(|s| s.clone()));
        
        let client_height = element.client_height() as f64;
        let padding_top = computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("padding-top").ok())
            .and_then(|s| s.strip_suffix("px"))
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
        let padding_bottom = computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("padding-bottom").ok())
            .and_then(|s| s.strip_suffix("px"))
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(0.0);
        
        client_height - padding_top - padding_bottom
    }

    /// Anti-overflow algorithm - ensures no overflow occurs
    fn anti_overflow_algo(
        inner_el: &Element,
        container_el: &Element,
        mut font_size_px: f64,
        min_font_size_px: f64,
        font_size_precision_px: f64,
        break_predicate: impl Fn(&Element, &Element) -> bool,
    ) {
        let max_iter_count = (1.0 / font_size_precision_px).ceil() as u32;
        let mut iter_count = 0;

        while font_size_px > min_font_size_px && iter_count < max_iter_count {
            if break_predicate(inner_el, container_el) {
                break;
            }
            font_size_px = (font_size_px - font_size_precision_px).max(min_font_size_px);
            if let Some(style) = get_style(inner_el) {
                style
                    .set_property("font-size", &format!("{}px", font_size_px))
                    .ok();
            }
            iter_count += 1;
        }
    }

    /// Oneline algorithm - text fills width without wrapping
    fn oneline_algo(
        inner_el: &Element,
        container_el: &Element,
        mut font_size_px: f64,
        min_font_size_px: f64,
        max_font_size_px: f64,
        font_size_precision_px: f64,
    ) {
        let max_iter_count = 10;
        let mut iter_count = 0;
        let mut prev_overflow_factor = 1.0;

        while iter_count < max_iter_count {
            let w0 = inner_el.scroll_width() as f64;
            let w1 = get_content_width(container_el);

            let can_grow = font_size_px < max_font_size_px && w0 < w1;
            let can_shrink = font_size_px > min_font_size_px && w0 > w1;
            let overflow_factor = if w1 > 0.0 { w0 / w1 } else { 1.0 };

            // Browser cannot render a difference based on previous font size update
            if (prev_overflow_factor - overflow_factor).abs() < f64::EPSILON {
                break;
            }

            if !(can_grow || can_shrink) {
                break;
            }

            let update_px = if overflow_factor > 0.0 {
                font_size_px / overflow_factor - font_size_px
            } else {
                0.0
            };
            let prev_font_size_px = font_size_px;
            font_size_px = (font_size_px + update_px)
                .max(min_font_size_px)
                .min(max_font_size_px);
            
            if let Some(style) = get_style(inner_el) {
                style
                    .set_property("font-size", &format!("{}px", font_size_px))
                    .ok();
            }

            // Stop iterating when converging
            if (font_size_px - prev_font_size_px).abs() <= font_size_precision_px {
                break;
            }

            prev_overflow_factor = overflow_factor;
            iter_count += 1;
        }

        anti_overflow_algo(
            inner_el,
            container_el,
            font_size_px,
            min_font_size_px,
            font_size_precision_px,
            |inner, container| {
                inner.scroll_width() as f64 <= get_content_width(container)
            },
        );
    }

    /// Multiline algorithm - text fills width, wraps if needed
    fn multiline_algo(
        inner_el: &Element,
        container_el: &Element,
        mut font_size_px: f64,
        min_font_size_px: f64,
        max_font_size_px: f64,
        font_size_precision_px: f64,
    ) {
        debug!("TextFit: Running multiline algorithm");
        // Try oneline first
        if let Some(style) = get_style(inner_el) {
            style.set_property("white-space", "nowrap").ok();
        }
        oneline_algo(
            inner_el,
            container_el,
            font_size_px,
            min_font_size_px,
            max_font_size_px,
            font_size_precision_px,
        );

        // If still overflowing, allow wrapping
        if inner_el.scroll_width() as f64 > get_content_width(container_el) {
            if let Some(style) = get_style(inner_el) {
                style.set_property("white-space", "normal").ok();
            }
        }

        // Binary search for multiline
        let mut min_size = min_font_size_px;
        let mut max_size = max_font_size_px;
        let mut iter_count = 0;
        let max_iter_count = 100;

        while (max_size - min_size) > font_size_precision_px && iter_count < max_iter_count {
            font_size_px = (min_size + max_size) / 2.0;
            if let Some(style) = get_style(inner_el) {
                style
                    .set_property("font-size", &format!("{}px", font_size_px))
                    .ok();
            }

            let w0 = inner_el.scroll_width() as f64;
            let w1 = get_content_width(container_el);

            if w0 <= w1 {
                min_size = font_size_px;
            } else {
                max_size = font_size_px;
            }
            iter_count += 1;
        }

        anti_overflow_algo(
            inner_el,
            container_el,
            font_size_px,
            min_font_size_px,
            font_size_precision_px,
            |inner, container| {
                inner.scroll_width() as f64 <= get_content_width(container)
            },
        );
    }

    /// Box algorithm - text fills both width and height
    fn box_algo(
        inner_el: &Element,
        container_el: &Element,
        mut font_size_px: f64,
        min_font_size_px: f64,
        max_font_size_px: f64,
        font_size_precision_px: f64,
    ) {
        debug!("TextFit: Running box algorithm");
        // Start binary search in the middle
        font_size_px = (max_font_size_px - min_font_size_px) * 0.5;
        if let Some(style) = get_style(inner_el) {
            style
                .set_property("font-size", &format!("{}px", font_size_px))
                .ok();
        }

        // Each subsequent update will halve the search space
        let mut update_px = (max_font_size_px - min_font_size_px) * 0.25;
        let mut iter_count = 0;
        let max_iter_count = 100;

        while update_px > font_size_precision_px && iter_count < max_iter_count {
            let w0 = inner_el.scroll_width() as f64;
            let w1 = get_content_width(container_el);
            let h0 = inner_el.scroll_height() as f64;
            let h1 = get_content_height(container_el);

            if (w0 - w1).abs() < f64::EPSILON && (h0 - h1).abs() < f64::EPSILON {
                break;
            }

            if font_size_px < max_font_size_px && w0 <= w1 && h0 <= h1 {
                font_size_px = (font_size_px + update_px).min(max_font_size_px);
            } else if font_size_px > min_font_size_px && (w0 > w1 || h0 > h1) {
                font_size_px = (font_size_px - update_px).max(min_font_size_px);
            }

            if let Some(style) = get_style(inner_el) {
                style
                    .set_property("font-size", &format!("{}px", font_size_px))
                    .ok();
            }

            update_px *= 0.5; // Binary search
            iter_count += 1;
        }

        anti_overflow_algo(
            inner_el,
            container_el,
            font_size_px,
            min_font_size_px,
            font_size_precision_px,
            |inner, container| {
                inner.scroll_width() as f64 <= get_content_width(container)
                    && inner.scroll_height() as f64 <= get_content_height(container)
            },
        );
    }

    /// Update text size based on mode
    pub fn update_text_size(
        inner_el: &Element,
        container_el: &Element,
        mode: TextFitMode,
        min_font_size_px: f64,
        max_font_size_px: f64,
        font_size_precision_px: f64,
    ) {
        debug!(
            mode = ?mode,
            container_width = container_el.client_width(),
            container_height = container_el.client_height(),
            inner_width = inner_el.client_width(),
            inner_height = inner_el.client_height(),
            "TextFit: Starting update_text_size"
        );
        // Set container styles
        if let Some(style) = get_style(container_el) {
            style.set_property("display", "flex").ok();
            style.set_property("align-items", "center").ok();
            style.set_property("justify-content", "center").ok();
        }

        // Set inner element styles
        if let Some(style) = get_style(inner_el) {
            style.set_property("display", "block").ok();
            style.set_property("text-align", "center").ok();

            match mode {
                TextFitMode::Oneline => {
                    style.set_property("white-space", "nowrap").ok();
                }
                TextFitMode::Multiline => {
                    style.set_property("word-break", "break-word").ok();
                }
                TextFitMode::Box => {
                    style.set_property("white-space", "pre-wrap").ok();
                    style.set_property("word-break", "break-word").ok();
                }
                TextFitMode::BoxOneline => {
                    style.set_property("white-space", "nowrap").ok();
                }
            }
        }

        // Get initial font size
        let computed_style = window()
            .and_then(|w| w.get_computed_style(inner_el).ok().flatten())
            .or_else(|| get_style(inner_el).map(|s| s.clone()));
        
        let font_size_str = computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("font-size").ok())
            .unwrap_or_else(|| "16px".to_string());
        let mut font_size_px = font_size_str
            .strip_suffix("px")
            .and_then(|s| s.parse::<f64>().ok())
            .unwrap_or(16.0);

        // Clamp to min/max
        if font_size_px > max_font_size_px || font_size_px < min_font_size_px {
            font_size_px = font_size_px.max(min_font_size_px).min(max_font_size_px);
            if let Some(style) = get_style(inner_el) {
                style
                    .set_property("font-size", &format!("{}px", font_size_px))
                    .ok();
            }
        }

        // Run appropriate algorithm
        match mode {
            TextFitMode::Oneline => {
                oneline_algo(
                    inner_el,
                    container_el,
                    font_size_px,
                    min_font_size_px,
                    max_font_size_px,
                    font_size_precision_px,
                );
            }
            TextFitMode::Multiline => {
                multiline_algo(
                    inner_el,
                    container_el,
                    font_size_px,
                    min_font_size_px,
                    max_font_size_px,
                    font_size_precision_px,
                );
            }
            TextFitMode::Box | TextFitMode::BoxOneline => {
                box_algo(
                    inner_el,
                    container_el,
                    font_size_px,
                    min_font_size_px,
                    max_font_size_px,
                    font_size_precision_px,
                );
            }
        }
        
        // Log final state
        let final_computed_style = window()
            .and_then(|w| w.get_computed_style(inner_el).ok().flatten())
            .or_else(|| get_style(inner_el).map(|s| s.clone()));
        let final_font_size = final_computed_style
            .as_ref()
            .and_then(|s| s.get_property_value("font-size").ok())
            .unwrap_or_else(|| "unknown".to_string());
        debug!(
            final_font_size = %final_font_size,
            final_inner_width = inner_el.scroll_width(),
            final_inner_height = inner_el.scroll_height(),
            final_container_width = container_el.client_width(),
            final_container_height = container_el.client_height(),
            "TextFit: Completed update_text_size"
        );
    }

    /// Set up ResizeObserver and auto-text-size
    pub fn setup_auto_text_size(
        container_id: &str,
        inner_id: &str,
        mode: TextFitMode,
        min_font_size_px: f64,
        max_font_size_px: f64,
        font_size_precision_px: f64,
    ) -> Option<ResizeObserver> {
        use std::cell::RefCell;
        use std::rc::Rc;

        let window = window()?;
        let document = window.document()?;
        let container_el = document.get_element_by_id(container_id)?;
        let inner_el = document.get_element_by_id(inner_id)?;
        
        // Debug: Log element dimensions to verify they exist and have size
        debug!(
            container_id = %container_id,
            inner_id = %inner_id,
            container_width = container_el.client_width(),
            container_height = container_el.client_height(),
            inner_width = inner_el.client_width(),
            inner_height = inner_el.client_height(),
            "TextFit: Setting up elements"
        );

        // Throttle using requestAnimationFrame
        // Use Rc<RefCell<>> to share mutable state across closures
        let wait = Rc::new(RefCell::new(false));
        let wait_clone = wait.clone();

        let throttled_update = {
            let container_el = container_el.clone();
            let inner_el = inner_el.clone();
            let mode = mode;
            let min_font_size_px = min_font_size_px;
            let max_font_size_px = max_font_size_px;
            let font_size_precision_px = font_size_precision_px;
            let wait = wait_clone.clone();

            Closure::wrap(Box::new(move || {
                if !*wait.borrow() {
                    *wait.borrow_mut() = true;
                    let window_obj = web_sys::window().unwrap();
                    
                    let container_el = container_el.clone();
                    let inner_el = inner_el.clone();
                    let wait = wait.clone();
                    
                    let f = Closure::wrap(Box::new(move || {
                        update_text_size(
                            &inner_el,
                            &container_el,
                            mode,
                            min_font_size_px,
                            max_font_size_px,
                            font_size_precision_px,
                        );
                        *wait.borrow_mut() = false;
                    }) as Box<dyn FnMut()>);
                    
                    window_obj
                        .request_animation_frame(f.as_ref().unchecked_ref())
                        .ok();
                    f.forget();
                }
            }) as Box<dyn FnMut()>)
        };

        // Set up ResizeObserver
        let resize_observer = ResizeObserver::new(throttled_update.as_ref().unchecked_ref()).ok()?;
        
        let container_el_clone = container_el.clone();
        resize_observer.observe(&container_el_clone);

        // Initial run - delay to ensure layout is complete
        let container_el_init = container_el.clone();
        let inner_el_init = inner_el.clone();
        let mode_init = mode;
        let min_font_size_px_init = min_font_size_px;
        let max_font_size_px_init = max_font_size_px;
        let font_size_precision_px_init = font_size_precision_px;
        
        let init_closure = Closure::wrap(Box::new(move || {
            debug!("TextFit: Running initial sizing");
            update_text_size(
                &inner_el_init,
                &container_el_init,
                mode_init,
                min_font_size_px_init,
                max_font_size_px_init,
                font_size_precision_px_init,
            );
            debug!("TextFit: Initial sizing complete");
        }) as Box<dyn FnMut()>);
        
        window
            .request_animation_frame(init_closure.as_ref().unchecked_ref())
            .ok();
        init_closure.forget();

        throttled_update.forget();
        Some(resize_observer)
    }
}

/// A component that automatically adjusts text size to fit its container
/// 
/// This component uses the auto-text-size algorithm to ensure text precisely
/// fills its container without overflow or underflow. It automatically re-runs
/// when the container resizes or when the text content changes.
#[component]
pub fn TextFit(props: TextFitProps) -> Element {
    let container_id = use_signal(|| format!("text-fit-container-{}", uuid::Uuid::new_v4()));
    let inner_id = use_signal(|| format!("text-fit-inner-{}", uuid::Uuid::new_v4()));
    
    info!(
        container_id = %container_id.peek(),
        inner_id = %inner_id.peek(),
        text_length = props.text.len(),
        mode = ?props.mode,
        "TextFit: Component rendering"
    );

    // Build classes - combining default styling with optional custom classes
    // Use items-center justify-center for centering, but the algorithm needs flex
    let container_classes = vec![
        "flex items-center justify-center", // Center content and necessary for correct dimension computation
        props.class.as_deref().unwrap_or(""),
    ]
    .into_iter()
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>()
    .join(" ");

    let inner_classes = vec![
        "block", // Necessary to compute dimensions
        match props.mode {
            TextFitMode::Oneline | TextFitMode::BoxOneline => "whitespace-nowrap",
            TextFitMode::Multiline => "break-words",
            TextFitMode::Box => "whitespace-pre-wrap break-words",
        },
    ]
    .into_iter()
    .filter(|s| !s.is_empty())
    .collect::<Vec<_>>()
    .join(" ");

    // Set up the auto-text-size algorithm when component mounts or text changes
    // Note: We track text changes by re-running the effect when text changes
    let text_for_effect = props.text.clone();
    let container_id_str = container_id.peek().clone();
    let inner_id_str = inner_id.peek().clone();
    let mode = props.mode;
    let min_font_size = props.min_font_size_px;
    let max_font_size = props.max_font_size_px;
    let precision = props.font_size_precision_px;

    use_effect(move || {
        use wasm_impl::setup_auto_text_size;
        use wasm_bindgen::JsCast;
        use web_sys::window;
        
        info!(
            container_id = %container_id_str,
            inner_id = %inner_id_str,
            "TextFit: use_effect running"
        );
        
        // Text is rendered in RSX, so we just need to set up the observer
        // The observer will automatically re-run when the DOM updates
        let _text = text_for_effect; // Track text changes to re-run effect
        
        let container_id_str = container_id_str.clone();
        let inner_id_str = inner_id_str.clone();
        let mode = mode;
        let min_font_size = min_font_size;
        let max_font_size = max_font_size;
        let precision = precision;

        // Wait for DOM to be ready before accessing elements
        // Use requestAnimationFrame to ensure elements are mounted
        let window_obj = window().unwrap();
        let f = Closure::wrap(Box::new(move || {
            debug!(
                container_id = %container_id_str,
                inner_id = %inner_id_str,
                "TextFit: Attempting to set up auto-text-size"
            );
            
            // Try to set up the observer, retry if elements don't exist yet
            if let Some(resize_observer) = setup_auto_text_size(
                &container_id_str,
                &inner_id_str,
                mode,
                min_font_size,
                max_font_size,
                precision,
            ) {
                info!("TextFit: Successfully set up ResizeObserver");
                // Store resize observer to keep it alive
                // When component unmounts, the observer will be dropped and disconnected
                drop(resize_observer);
            } else {
                warn!(
                    container_id = %container_id_str,
                    inner_id = %inner_id_str,
                    "TextFit: Elements not found, retrying on next frame"
                );
                // Elements not ready yet, try again on next frame
                let window_obj2 = window().unwrap();
                let container_id_str2 = container_id_str.clone();
                let inner_id_str2 = inner_id_str.clone();
                let mode2 = mode;
                let min_font_size2 = min_font_size;
                let max_font_size2 = max_font_size;
                let precision2 = precision;
                
                let f2 = Closure::wrap(Box::new(move || {
                    if let Some(resize_observer) = setup_auto_text_size(
                        &container_id_str2,
                        &inner_id_str2,
                        mode2,
                        min_font_size2,
                        max_font_size2,
                        precision2,
                    ) {
                        info!("TextFit: Successfully set up ResizeObserver on retry");
                        drop(resize_observer);
                    } else {
                        warn!("TextFit: Failed to set up ResizeObserver after retry");
                    }
                }) as Box<dyn FnMut()>);
               window_obj2
                   .request_animation_frame(f2.as_ref().unchecked_ref())
                   .ok();
               f2.forget();
           }
       }) as Box<dyn FnMut()>);
       
       window_obj
           .request_animation_frame(f.as_ref().unchecked_ref())
           .ok();
       f.forget();
    });


    rsx! {
        div {
            id: "{container_id.peek()}",
            class: "{container_classes} w-full h-full",
            div {
                id: "{inner_id.peek()}",
                class: "{inner_classes}",
                {props.text}
            }
        }
    }
}
