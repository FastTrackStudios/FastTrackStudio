//! Pure input handlers — old content + caret + event → new content +
//! caret + optional `StructuralOp`. No DOM mutation; the BlockEditor
//! re-renders from the new content and `caret.set_caret`s afterwards.

/// Result of one input event.
#[derive(Clone, Debug, PartialEq)]
pub struct InputCommand {
    pub new_content: String,
    pub new_caret: usize,
    pub structural: Option<StructuralOp>,
}

/// Block-level structural edits that bubble up to the outliner.
#[derive(Clone, Debug, PartialEq)]
pub enum StructuralOp {
    SplitBlock { at: usize },
    MergeWithPrev,
    IndentBlock,
    OutdentBlock,
    MoveBlockUp,
    MoveBlockDown,
    OpenSlashMenu { at: usize },
    OpenWikilinkPicker { at: usize },
    OpenBlockRefPicker { at: usize },
    OpenTagPicker { at: usize },
}

/// Subset of `InputEvent` data the handler needs. Constructed by the
/// component layer from `dioxus::events::Event<FormData>` or
/// `web_sys::InputEvent` depending on the call site.
#[derive(Clone, Debug)]
pub struct BeforeInputEvent {
    pub input_type: String,
    pub data: Option<String>,
    pub is_composing: bool,
}

#[derive(Clone, Debug)]
pub struct KeyEvent {
    pub key: String,
    pub shift: bool,
    pub meta: bool,
    pub ctrl: bool,
    pub alt: bool,
    pub is_composing: bool,
}

pub fn handle_beforeinput(content: &str, caret: usize, ev: &BeforeInputEvent) -> InputCommand {
    if ev.is_composing {
        // Let IME finalize; downstream `oninput` will pick up the
        // committed text once compositionend fires.
        return InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: None,
        };
    }
    match ev.input_type.as_str() {
        "insertText" => {
            let data = ev.data.clone().unwrap_or_default();
            // Picker / slash triggers
            if data == "[" && content.get(caret.saturating_sub(1)..caret) == Some("[") {
                // Just typed the second `[`
                return InputCommand {
                    new_content: insert_at(content, caret, &data),
                    new_caret: caret + data.len(),
                    structural: Some(StructuralOp::OpenWikilinkPicker {
                        at: caret + data.len(),
                    }),
                };
            }
            if data == "(" && content.get(caret.saturating_sub(1)..caret) == Some("(") {
                return InputCommand {
                    new_content: insert_at(content, caret, &data),
                    new_caret: caret + data.len(),
                    structural: Some(StructuralOp::OpenBlockRefPicker {
                        at: caret + data.len(),
                    }),
                };
            }
            if data == "/"
                && (caret == 0
                    || content.as_bytes()[caret - 1] == b' '
                    || content.as_bytes()[caret - 1] == b'\n')
            {
                return InputCommand {
                    new_content: insert_at(content, caret, &data),
                    new_caret: caret + 1,
                    structural: Some(StructuralOp::OpenSlashMenu { at: caret + 1 }),
                };
            }
            if data == "#"
                && (caret == 0
                    || content.as_bytes()[caret - 1] == b' '
                    || content.as_bytes()[caret - 1] == b'\n')
            {
                return InputCommand {
                    new_content: insert_at(content, caret, &data),
                    new_caret: caret + 1,
                    structural: Some(StructuralOp::OpenTagPicker { at: caret + 1 }),
                };
            }
            InputCommand {
                new_content: insert_at(content, caret, &data),
                new_caret: caret + data.len(),
                structural: None,
            }
        }
        "insertParagraph" | "insertLineBreak" => InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::SplitBlock { at: caret }),
        },
        "deleteContentBackward" => {
            if caret == 0 {
                return InputCommand {
                    new_content: content.to_string(),
                    new_caret: caret,
                    structural: Some(StructuralOp::MergeWithPrev),
                };
            }
            // Remove the byte (or char) before caret.
            let new_caret = prev_char_boundary(content, caret);
            let mut new_content = String::with_capacity(content.len());
            new_content.push_str(&content[..new_caret]);
            new_content.push_str(&content[caret..]);
            InputCommand {
                new_content,
                new_caret,
                structural: None,
            }
        }
        _ => InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: None,
        },
    }
}

pub fn handle_keydown(content: &str, caret: usize, ev: &KeyEvent) -> Option<InputCommand> {
    if ev.is_composing {
        return None;
    }
    match ev.key.as_str() {
        "Tab" if !ev.shift => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::IndentBlock),
        }),
        "Tab" if ev.shift => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::OutdentBlock),
        }),
        "ArrowUp" if ev.meta || ev.ctrl => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::MoveBlockUp),
        }),
        "ArrowDown" if ev.meta || ev.ctrl => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::MoveBlockDown),
        }),
        "Enter" if !ev.shift => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::SplitBlock { at: caret }),
        }),
        "Backspace" if caret == 0 => Some(InputCommand {
            new_content: content.to_string(),
            new_caret: caret,
            structural: Some(StructuralOp::MergeWithPrev),
        }),
        _ => None,
    }
}

fn insert_at(content: &str, at: usize, data: &str) -> String {
    let mut out = String::with_capacity(content.len() + data.len());
    out.push_str(&content[..at]);
    out.push_str(data);
    out.push_str(&content[at..]);
    out
}

fn prev_char_boundary(s: &str, i: usize) -> usize {
    if i == 0 {
        return 0;
    }
    let mut j = i - 1;
    while !s.is_char_boundary(j) {
        j -= 1;
    }
    j
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_text_appends() {
        let cmd = handle_beforeinput(
            "abc",
            3,
            &BeforeInputEvent {
                input_type: "insertText".into(),
                data: Some("d".into()),
                is_composing: false,
            },
        );
        assert_eq!(cmd.new_content, "abcd");
        assert_eq!(cmd.new_caret, 4);
    }

    #[test]
    fn second_left_bracket_opens_picker() {
        let cmd = handle_beforeinput(
            "see [",
            5,
            &BeforeInputEvent {
                input_type: "insertText".into(),
                data: Some("[".into()),
                is_composing: false,
            },
        );
        assert!(matches!(
            cmd.structural,
            Some(StructuralOp::OpenWikilinkPicker { .. })
        ));
        assert_eq!(cmd.new_content, "see [[");
    }

    #[test]
    fn enter_splits() {
        let cmd = handle_keydown(
            "hello",
            3,
            &KeyEvent {
                key: "Enter".into(),
                shift: false,
                meta: false,
                ctrl: false,
                alt: false,
                is_composing: false,
            },
        )
        .unwrap();
        assert!(matches!(
            cmd.structural,
            Some(StructuralOp::SplitBlock { at: 3 })
        ));
    }

    #[test]
    fn backspace_at_start_merges() {
        let cmd = handle_beforeinput(
            "abc",
            0,
            &BeforeInputEvent {
                input_type: "deleteContentBackward".into(),
                data: None,
                is_composing: false,
            },
        );
        assert!(matches!(cmd.structural, Some(StructuralOp::MergeWithPrev)));
    }
}
