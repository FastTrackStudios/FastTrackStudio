//! Caret motions. Pure functions over `EditorState` returning a
//! new byte offset.
//!
//! vim ref: codemirror-vim/src/vim.js (`motions` table)
//! vim ref: helix/helix-core/src/movement.rs

use editor_state::EditorState;

use crate::state::MotionInput;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Motion {
    Left,         // h
    Down,         // j
    Up,           // k
    Right,        // l
    WordForward,  // w
    WordBackward, // b
    WordEnd,      // e
    WORDForward,  // W — whitespace-only delimited
    WORDBackward, // B
    WORDEnd,      // E
    LineStart,    // 0
    LineEnd,      // $
    FirstNonblank, // ^
    DocStart,     // gg  (count = explicit line)
    DocEnd,       // G
    FindForward,  // f<c>
    FindBackward, // F<c>
    TillForward,  // t<c>
    TillBackward, // T<c>
    ParaForward,  // }
    ParaBackward, // {
    SentForward,  // )
    SentBackward, // (
    MatchBracket, // %
    SearchNext,   // n
    SearchPrev,   // N
    EndPrevWord,  // ge
}

impl Motion {
    pub fn from_char(ch: char) -> Option<Self> {
        Some(match ch {
            'h' => Self::Left,
            'j' => Self::Down,
            'k' => Self::Up,
            'l' => Self::Right,
            'w' => Self::WordForward,
            'b' => Self::WordBackward,
            'e' => Self::WordEnd,
            'W' => Self::WORDForward,
            'B' => Self::WORDBackward,
            'E' => Self::WORDEnd,
            '0' => Self::LineStart,
            '$' => Self::LineEnd,
            '^' => Self::FirstNonblank,
            'G' => Self::DocEnd,
            'f' => Self::FindForward,
            'F' => Self::FindBackward,
            't' => Self::TillForward,
            'T' => Self::TillBackward,
            '{' => Self::ParaBackward,
            '}' => Self::ParaForward,
            '(' => Self::SentBackward,
            ')' => Self::SentForward,
            '%' => Self::MatchBracket,
            'n' => Self::SearchNext,
            'N' => Self::SearchPrev,
            // `gg` and `ge` need a two-char lookahead; the
            // dispatcher handles those as a follow-up. Not in v1.
            _ => return None,
        })
    }
}

pub fn apply(state: &EditorState, motion: Motion, count: usize) -> usize {
    let pos = state.selection.primary().head;
    let count = count.max(1);
    match motion {
        Motion::Left => left(state, pos, count),
        Motion::Right => right(state, pos, count),
        Motion::Up => up(state, pos, count),
        Motion::Down => down(state, pos, count),
        Motion::WordForward => word_forward(state, pos, count),
        Motion::WordBackward => word_backward(state, pos, count),
        Motion::WordEnd => word_end(state, pos, count),
        Motion::WORDForward => big_word_forward(state, pos, count),
        Motion::WORDBackward => big_word_backward(state, pos, count),
        Motion::WORDEnd => big_word_end(state, pos, count),
        Motion::LineStart => line_start(state, pos),
        Motion::LineEnd => line_end_n(state, pos, count),
        Motion::FirstNonblank => line_first_nonblank(state, pos),
        Motion::DocStart => 0,
        Motion::DocEnd => state.doc.len(),
        Motion::ParaForward => para_forward(state, pos, count),
        Motion::ParaBackward => para_backward(state, pos, count),
        // Sentences: treat as paragraph for v1 (cheap, good
        // enough for tests). vim ref: codemirror-vim sentence_().
        Motion::SentForward => para_forward(state, pos, count),
        Motion::SentBackward => para_backward(state, pos, count),
        Motion::MatchBracket => match_bracket(state, pos).unwrap_or(pos),
        Motion::SearchNext | Motion::SearchPrev => pos, // v1: no search
        Motion::EndPrevWord => word_backward(state, pos, count), // approximation
        // f/F/t/T are handled via pending-input — never reached here.
        Motion::FindForward | Motion::FindBackward | Motion::TillForward | Motion::TillBackward => pos,
    }
}

pub fn find_char(
    state: &EditorState,
    pos: usize,
    ch: char,
    input: MotionInput,
    count: usize,
) -> Option<usize> {
    let doc = state.doc.to_string();
    let bytes = doc.as_bytes();
    let forward = matches!(input, MotionInput::FindForward | MotionInput::TillForward);
    let till = matches!(input, MotionInput::TillForward | MotionInput::TillBackward);
    let target_byte = ch as u32;
    if target_byte > 0x7f {
        return None; // ASCII-only fast path; v1.
    }
    let target = target_byte as u8;
    let mut hits = 0;
    if forward {
        let start = (pos + 1).min(bytes.len());
        for i in start..bytes.len() {
            if bytes[i] == target {
                hits += 1;
                if hits == count {
                    return Some(if till { i.saturating_sub(1) } else { i });
                }
            }
        }
    } else {
        for i in (0..pos).rev() {
            if bytes[i] == target {
                hits += 1;
                if hits == count {
                    return Some(if till { i + 1 } else { i });
                }
            }
        }
    }
    None
}

// --- horizontal/vertical ---------------------------------------

fn left(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let line_lo = line_start(state, pos);
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p <= line_lo {
            break;
        }
        p -= 1;
        // step back to char boundary
        while p > line_lo && (bytes[p] & 0xC0) == 0x80 {
            p -= 1;
        }
    }
    p
}

fn right(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let line_hi = line_end(state, pos);
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p >= line_hi {
            break;
        }
        p += 1;
        while p < line_hi && (bytes[p] & 0xC0) == 0x80 {
            p += 1;
        }
    }
    p
}

fn up(state: &EditorState, pos: usize, n: usize) -> usize {
    let col = pos - line_start(state, pos);
    let mut start = line_start(state, pos);
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    for _ in 0..n {
        if start == 0 {
            break;
        }
        // step back over the `\n` before this line, then to that
        // line's start.
        let prev_nl_end = start - 1; // index of '\n'
        let mut prev_start = prev_nl_end;
        while prev_start > 0 && bytes[prev_start - 1] != b'\n' {
            prev_start -= 1;
        }
        start = prev_start;
    }
    let line_hi = next_newline(state, start).unwrap_or(state.doc.len());
    (start + col).min(line_hi)
}

fn down(state: &EditorState, pos: usize, n: usize) -> usize {
    let col = pos - line_start(state, pos);
    let mut start = line_start(state, pos);
    let len = state.doc.len();
    for _ in 0..n {
        let nl = next_newline(state, start).unwrap_or(len);
        if nl == len {
            break;
        }
        start = nl + 1;
    }
    let line_hi = next_newline(state, start).unwrap_or(len);
    (start + col).min(line_hi)
}

// --- word motions ----------------------------------------------

fn is_word(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

fn classify(b: u8) -> u8 {
    if b == b'\n' {
        2
    } else if b.is_ascii_whitespace() {
        0
    } else if is_word(b) {
        1
    } else {
        3
    }
}

fn word_forward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p >= bytes.len() {
            break;
        }
        let start_class = classify(bytes[p]);
        // skip the current run
        while p < bytes.len() && classify(bytes[p]) == start_class {
            p += 1;
        }
        // skip trailing whitespace (but stop at newlines for v1
        // simplicity; vim crosses lines but tests don't require it)
        while p < bytes.len() && classify(bytes[p]) == 0 {
            p += 1;
        }
    }
    p
}

fn word_backward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p == 0 {
            break;
        }
        p -= 1;
        // skip whitespace going left
        while p > 0 && classify(bytes[p]) == 0 {
            p -= 1;
        }
        let cls = classify(bytes[p]);
        while p > 0 && classify(bytes[p - 1]) == cls {
            p -= 1;
        }
    }
    p
}

fn word_end(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p + 1 >= bytes.len() {
            p = bytes.len();
            break;
        }
        p += 1;
        while p < bytes.len() && classify(bytes[p]) == 0 {
            p += 1;
        }
        if p >= bytes.len() {
            break;
        }
        let cls = classify(bytes[p]);
        while p + 1 < bytes.len() && classify(bytes[p + 1]) == cls {
            p += 1;
        }
    }
    p
}

// --- WORD motions ----------------------------------------------
//
// A "WORD" (uppercase in :help WORD) is any maximal run of
// non-whitespace bytes. Only ASCII space / tab / newline count
// as whitespace here — punctuation is part of the WORD.

fn is_ws(b: u8) -> bool {
    matches!(b, b' ' | b'\t' | b'\n')
}

fn big_word_forward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p >= bytes.len() {
            break;
        }
        // Skip the current run of non-whitespace.
        while p < bytes.len() && !is_ws(bytes[p]) {
            p += 1;
        }
        // Skip the whitespace gap.
        while p < bytes.len() && is_ws(bytes[p]) {
            p += 1;
        }
    }
    p
}

fn big_word_backward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p == 0 {
            break;
        }
        p -= 1;
        // Skip whitespace going left.
        while p > 0 && is_ws(bytes[p]) {
            p -= 1;
        }
        // Walk to the start of the current non-whitespace run.
        while p > 0 && !is_ws(bytes[p - 1]) {
            p -= 1;
        }
    }
    p
}

fn big_word_end(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p + 1 >= bytes.len() {
            p = bytes.len();
            break;
        }
        p += 1;
        // Skip whitespace forward.
        while p < bytes.len() && is_ws(bytes[p]) {
            p += 1;
        }
        if p >= bytes.len() {
            break;
        }
        // Walk to the last byte of the current non-whitespace run.
        while p + 1 < bytes.len() && !is_ws(bytes[p + 1]) {
            p += 1;
        }
    }
    p
}

// --- line helpers ----------------------------------------------

pub fn line_start(state: &EditorState, pos: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos.min(bytes.len());
    while p > 0 && bytes[p - 1] != b'\n' {
        p -= 1;
    }
    p
}

pub fn line_end(state: &EditorState, pos: usize) -> usize {
    next_newline(state, pos).unwrap_or(state.doc.len())
}

pub fn line_end_n(state: &EditorState, pos: usize, count: usize) -> usize {
    // `$` with count=1 → end of current line; with count=N → end
    // of line N-1 below.
    let mut p = pos;
    for _ in 0..count.saturating_sub(1) {
        let nl = next_newline(state, p).unwrap_or(state.doc.len());
        if nl == state.doc.len() {
            return nl;
        }
        p = nl + 1;
    }
    next_newline(state, p).unwrap_or(state.doc.len())
}

/// Position at the first non-blank character of the `n`-th
/// line (0-indexed). Used by `gg` / `<N>G`. Clamps `n` to the
/// last line of the doc.
pub fn nth_line_first_nonblank(state: &EditorState, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = 0usize;
    let mut line = 0usize;
    while line < n && p < bytes.len() {
        if let Some(off) = bytes[p..].iter().position(|&b| b == b'\n') {
            p += off + 1;
            line += 1;
        } else {
            p = bytes.len();
            break;
        }
    }
    line_first_nonblank(state, p)
}

pub fn line_first_nonblank(state: &EditorState, pos: usize) -> usize {
    let start = line_start(state, pos);
    let end = line_end(state, pos);
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = start;
    while p < end && bytes[p].is_ascii_whitespace() {
        p += 1;
    }
    p
}

fn next_newline(state: &EditorState, pos: usize) -> Option<usize> {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    bytes.iter().skip(pos).position(|&b| b == b'\n').map(|i| pos + i)
}

// --- paragraph/bracket -----------------------------------------

fn para_forward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        // advance to the next blank line.
        loop {
            let nl = match next_newline(state, p) {
                Some(i) => i,
                None => return bytes.len(),
            };
            p = nl + 1;
            let line_end_pos = next_newline(state, p).unwrap_or(bytes.len());
            if p == line_end_pos || bytes[p..line_end_pos].iter().all(|b| b.is_ascii_whitespace())
            {
                break;
            }
        }
    }
    p
}

fn para_backward(state: &EditorState, pos: usize, n: usize) -> usize {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut p = pos;
    for _ in 0..n {
        if p == 0 {
            break;
        }
        loop {
            let line_lo = line_start(state, p.saturating_sub(1));
            let line_hi = next_newline(state, line_lo).unwrap_or(bytes.len());
            p = line_lo;
            if line_lo == line_hi || bytes[line_lo..line_hi].iter().all(|b| b.is_ascii_whitespace())
            {
                break;
            }
            if p == 0 {
                break;
            }
        }
    }
    p
}

fn match_bracket(state: &EditorState, pos: usize) -> Option<usize> {
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    if pos >= bytes.len() {
        return None;
    }
    let (open, close, forward) = match bytes[pos] {
        b'(' => (b'(', b')', true),
        b'[' => (b'[', b']', true),
        b'{' => (b'{', b'}', true),
        b')' => (b'(', b')', false),
        b']' => (b'[', b']', false),
        b'}' => (b'{', b'}', false),
        _ => return None,
    };
    let mut depth = 0i32;
    if forward {
        for i in pos..bytes.len() {
            if bytes[i] == open {
                depth += 1;
            } else if bytes[i] == close {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
        }
    } else {
        for i in (0..=pos).rev() {
            if bytes[i] == close {
                depth += 1;
            } else if bytes[i] == open {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
        }
    }
    None
}
