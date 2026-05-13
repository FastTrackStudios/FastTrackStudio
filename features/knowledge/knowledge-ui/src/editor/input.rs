//! Pure input handlers — current content + caret + event → text-edit
//! ops + new caret + optional structural op. Ops are unicode-scalar
//! coords (matches the crdt layer). The component layer translates
//! between DOM (UTF-16) and these.

/// Char-level text edit. Mirrors `crdt::codec::TextOp` shape; defined
/// here so `knowledge-ui` stays free of a `crdt` dep. The route layer
/// maps `editor::TextOp` → `crdt::codec::TextOp` when calling the
/// `BlockRepoLoro::apply_text_ops` fast path.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TextOp {
    Insert { pos: u32, text: String },
    Delete { pos: u32, len: u32 },
}

/// Result of one input event.
#[derive(Clone, Debug, PartialEq)]
pub struct InputCommand {
    pub edits: Vec<TextOp>,
    pub new_caret: u32,
    pub structural: Option<StructuralOp>,
}

/// Block-level structural edits that bubble up to the outliner.
#[derive(Clone, Debug, PartialEq)]
pub enum StructuralOp {
    SplitBlock { at: u32 },
    MergeWithPrev,
    IndentBlock,
    OutdentBlock,
    MoveBlockUp,
    MoveBlockDown,
    OpenSlashMenu { at: u32 },
    OpenWikilinkPicker { at: u32 },
    OpenBlockRefPicker { at: u32 },
    OpenTagPicker { at: u32 },
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

/// Map a `beforeinput` event to text ops + optional structural op.
/// Unknown `input_type` returns no ops + no structural — the caller
/// must fall back to its `oninput` full-content path so the user
/// doesn't see lost keystrokes.
pub fn handle_beforeinput(content: &str, caret: u32, ev: &BeforeInputEvent) -> InputCommand {
    if ev.is_composing {
        return InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: None,
        };
    }
    let caret_u = caret as usize;
    match ev.input_type.as_str() {
        "insertText" | "insertCompositionText" | "insertFromComposition" => {
            let data = ev.data.clone().unwrap_or_default();
            let data_chars = data.chars().count() as u32;
            // Slash / picker triggers (single-char data only).
            if data == "[" && char_at(content, caret_u.saturating_sub(1)) == Some('[') {
                return InputCommand {
                    edits: vec![TextOp::Insert {
                        pos: caret,
                        text: data,
                    }],
                    new_caret: caret + data_chars,
                    structural: Some(StructuralOp::OpenWikilinkPicker {
                        at: caret + data_chars,
                    }),
                };
            }
            if data == "(" && char_at(content, caret_u.saturating_sub(1)) == Some('(') {
                return InputCommand {
                    edits: vec![TextOp::Insert {
                        pos: caret,
                        text: data,
                    }],
                    new_caret: caret + data_chars,
                    structural: Some(StructuralOp::OpenBlockRefPicker {
                        at: caret + data_chars,
                    }),
                };
            }
            let prev = if caret_u == 0 {
                None
            } else {
                char_at(content, caret_u - 1)
            };
            if data == "/" && matches!(prev, None | Some(' ') | Some('\n')) {
                return InputCommand {
                    edits: vec![TextOp::Insert {
                        pos: caret,
                        text: data,
                    }],
                    new_caret: caret + data_chars,
                    structural: Some(StructuralOp::OpenSlashMenu {
                        at: caret + data_chars,
                    }),
                };
            }
            if data == "#" && matches!(prev, None | Some(' ') | Some('\n')) {
                return InputCommand {
                    edits: vec![TextOp::Insert {
                        pos: caret,
                        text: data,
                    }],
                    new_caret: caret + data_chars,
                    structural: Some(StructuralOp::OpenTagPicker {
                        at: caret + data_chars,
                    }),
                };
            }
            InputCommand {
                edits: vec![TextOp::Insert {
                    pos: caret,
                    text: data,
                }],
                new_caret: caret + data_chars,
                structural: None,
            }
        }
        "insertParagraph" | "insertLineBreak" => InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::SplitBlock { at: caret }),
        },
        "deleteContentBackward" => {
            if caret == 0 {
                return InputCommand {
                    edits: Vec::new(),
                    new_caret: caret,
                    structural: Some(StructuralOp::MergeWithPrev),
                };
            }
            InputCommand {
                edits: vec![TextOp::Delete {
                    pos: caret - 1,
                    len: 1,
                }],
                new_caret: caret - 1,
                structural: None,
            }
        }
        "deleteContentForward" => {
            let total = content.chars().count() as u32;
            if caret >= total {
                return InputCommand {
                    edits: Vec::new(),
                    new_caret: caret,
                    structural: None,
                };
            }
            InputCommand {
                edits: vec![TextOp::Delete { pos: caret, len: 1 }],
                new_caret: caret,
                structural: None,
            }
        }
        "deleteWordBackward" => {
            let start = word_start_before(content, caret);
            if start >= caret {
                return InputCommand {
                    edits: Vec::new(),
                    new_caret: caret,
                    structural: None,
                };
            }
            InputCommand {
                edits: vec![TextOp::Delete {
                    pos: start,
                    len: caret - start,
                }],
                new_caret: start,
                structural: None,
            }
        }
        "insertFromPaste" => {
            let data = ev.data.clone().unwrap_or_default();
            let data_chars = data.chars().count() as u32;
            if data.is_empty() {
                return InputCommand {
                    edits: Vec::new(),
                    new_caret: caret,
                    structural: None,
                };
            }
            InputCommand {
                edits: vec![TextOp::Insert {
                    pos: caret,
                    text: data,
                }],
                new_caret: caret + data_chars,
                structural: None,
            }
        }
        // Unknown — caller falls back to oninput full-content path.
        _ => InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: None,
        },
    }
}

pub fn handle_keydown(content: &str, caret: u32, ev: &KeyEvent) -> Option<InputCommand> {
    if ev.is_composing {
        return None;
    }
    let _ = content;
    match ev.key.as_str() {
        "Tab" if !ev.shift => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::IndentBlock),
        }),
        "Tab" if ev.shift => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::OutdentBlock),
        }),
        "ArrowUp" if ev.meta || ev.ctrl => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::MoveBlockUp),
        }),
        "ArrowDown" if ev.meta || ev.ctrl => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::MoveBlockDown),
        }),
        "Enter" if !ev.shift => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::SplitBlock { at: caret }),
        }),
        "Backspace" if caret == 0 => Some(InputCommand {
            edits: Vec::new(),
            new_caret: caret,
            structural: Some(StructuralOp::MergeWithPrev),
        }),
        _ => None,
    }
}

/// Apply a sequence of ops to a plain string. The component layer
/// uses this to reconstruct the new content when it needs to fire a
/// full-string callback alongside the ops fast path.
pub fn apply_ops_to_string(content: &str, ops: &[TextOp]) -> String {
    let mut chars: Vec<char> = content.chars().collect();
    for op in ops {
        match op {
            TextOp::Insert { pos, text } => {
                let p = (*pos as usize).min(chars.len());
                for (i, ch) in text.chars().enumerate() {
                    chars.insert(p + i, ch);
                }
            }
            TextOp::Delete { pos, len } => {
                let p = (*pos as usize).min(chars.len());
                let l = (*len as usize).min(chars.len() - p);
                chars.drain(p..p + l);
            }
        }
    }
    chars.into_iter().collect()
}

fn char_at(s: &str, idx_chars: usize) -> Option<char> {
    s.chars().nth(idx_chars)
}

fn word_start_before(s: &str, caret: u32) -> u32 {
    let chars: Vec<char> = s.chars().collect();
    let mut i = (caret as usize).min(chars.len());
    while i > 0 && chars[i - 1].is_whitespace() {
        i -= 1;
    }
    while i > 0 && !chars[i - 1].is_whitespace() {
        i -= 1;
    }
    i as u32
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insert_text_emits_insert_op() {
        let cmd = handle_beforeinput(
            "abc",
            3,
            &BeforeInputEvent {
                input_type: "insertText".into(),
                data: Some("d".into()),
                is_composing: false,
            },
        );
        assert_eq!(
            cmd.edits,
            vec![TextOp::Insert {
                pos: 3,
                text: "d".into()
            }]
        );
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
        assert_eq!(
            cmd.edits,
            vec![TextOp::Insert {
                pos: 5,
                text: "[".into()
            }]
        );
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
        assert!(cmd.edits.is_empty());
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
        assert!(cmd.edits.is_empty());
    }

    #[test]
    fn backspace_mid_emits_delete() {
        let cmd = handle_beforeinput(
            "abcd",
            3,
            &BeforeInputEvent {
                input_type: "deleteContentBackward".into(),
                data: None,
                is_composing: false,
            },
        );
        assert_eq!(cmd.edits, vec![TextOp::Delete { pos: 2, len: 1 }]);
        assert_eq!(cmd.new_caret, 2);
    }

    #[test]
    fn paste_emits_one_insert() {
        let cmd = handle_beforeinput(
            "hello ",
            6,
            &BeforeInputEvent {
                input_type: "insertFromPaste".into(),
                data: Some("world".into()),
                is_composing: false,
            },
        );
        assert_eq!(
            cmd.edits,
            vec![TextOp::Insert {
                pos: 6,
                text: "world".into()
            }]
        );
        assert_eq!(cmd.new_caret, 11);
    }

    #[test]
    fn unknown_input_type_falls_back_to_noop() {
        let cmd = handle_beforeinput(
            "abc",
            3,
            &BeforeInputEvent {
                input_type: "formatBold".into(),
                data: None,
                is_composing: false,
            },
        );
        assert!(cmd.edits.is_empty());
        assert!(cmd.structural.is_none());
    }

    #[test]
    fn delete_word_backward() {
        let cmd = handle_beforeinput(
            "hello world",
            11,
            &BeforeInputEvent {
                input_type: "deleteWordBackward".into(),
                data: None,
                is_composing: false,
            },
        );
        assert_eq!(cmd.edits, vec![TextOp::Delete { pos: 6, len: 5 }]);
        assert_eq!(cmd.new_caret, 6);
    }

    #[test]
    fn apply_ops_reconstructs_content() {
        let s = apply_ops_to_string(
            "hello",
            &[
                TextOp::Insert {
                    pos: 5,
                    text: " world".into(),
                },
                TextOp::Delete { pos: 0, len: 1 },
                TextOp::Insert {
                    pos: 0,
                    text: "H".into(),
                },
            ],
        );
        assert_eq!(s, "Hello world");
    }
}
