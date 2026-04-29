//! AspectRatio — shadcn v4 maia style CSS aspect-ratio wrapper.

use dioxus::prelude::*;
use dioxus_primitives::aspect_ratio::AspectRatio as PrimitiveAspectRatio;

#[derive(Props, Clone, PartialEq)]
pub struct AspectRatioProps {
    /// The aspect ratio expressed as width / height (e.g. 16.0/9.0).
    #[props(default = 16.0 / 9.0)]
    pub ratio: f64,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-aspect-ratio
///
/// Common ratios: 16/9, 4/3, 1/1, 21/9.
#[component]
pub fn AspectRatio(props: AspectRatioProps) -> Element {
    rsx! {
        PrimitiveAspectRatio {
            ratio: props.ratio,
            class: crate::cn::merge_slice(&["relative w-full overflow-hidden", props.class.as_str()]),
            {props.children}
        }
    }
}
