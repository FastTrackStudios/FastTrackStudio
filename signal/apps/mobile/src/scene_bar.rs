//! Scene bar — horizontal scrolling scene/preset selector.

use crate::styles as s;
use dioxus::prelude::*;

#[derive(Clone, PartialEq)]
pub struct SceneEntry {
    pub id: usize,
    pub name: String,
}

#[component]
pub fn SceneBar(
    scenes: Vec<SceneEntry>,
    active_id: usize,
    on_select: EventHandler<usize>,
) -> Element {
    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:row; gap:8px; padding:8px 16px; \
                 overflow-x:auto; background:{bg}; border-bottom:1px solid {border}; \
                 -webkit-overflow-scrolling:touch; flex-shrink:0;",
                bg = s::BG_APP, border = s::BORDER,
            ),

            for scene in scenes {
                {
                    let id = scene.id;
                    let active = id == active_id;
                    rsx! {
                        button {
                            key: "{id}",
                            onclick: move |_| on_select.call(id),
                            style: format!(
                                "flex-shrink:0; height:{h}; padding:0 16px; \
                                 border-radius:{r}; border:1px solid {border}; \
                                 background:{bg}; color:{col}; \
                                 font-size:13px; font-weight:600; white-space:nowrap; cursor:pointer;",
                                h = s::TOUCH_TARGET, r = s::RADIUS_SM,
                                border = if active { s::ACCENT } else { s::BORDER },
                                bg = if active { s::ACCENT } else { s::BG_SURFACE },
                                col = if active { s::TEXT_BRIGHT } else { s::TEXT_DIM },
                            ),
                            "{scene.name}"
                        }
                    }
                }
            }

            // Add scene
            button {
                style: format!(
                    "flex-shrink:0; height:{h}; width:{h}; border-radius:{r}; \
                     border:1px dashed {border}; background:transparent; \
                     color:{dim}; font-size:20px; cursor:pointer;",
                    h = s::TOUCH_TARGET, r = s::RADIUS_SM,
                    border = s::BORDER, dim = s::TEXT_DIM,
                ),
                "+"
            }
        }
    }
}
