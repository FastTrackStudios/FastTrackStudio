//! Roman numeral helpers (key-agnostic for now). Key-aware mapping will come later.

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RomanNumeral {
    // Major (uppercase)
    I,
    II,
    III,
    IV,
    V,
    VI,
    VII,
    // Minor (lowercase)
    i,
    ii,
    iii,
    iv,
    v,
    vi,
    vii,
}

impl RomanNumeral {
    pub fn from_degree_major(degree: u8) -> Option<Self> {
        match degree {
            1 => Some(RomanNumeral::I),
            2 => Some(RomanNumeral::II),
            3 => Some(RomanNumeral::III),
            4 => Some(RomanNumeral::IV),
            5 => Some(RomanNumeral::V),
            6 => Some(RomanNumeral::VI),
            7 => Some(RomanNumeral::VII),
            _ => None,
        }
    }

    pub fn from_degree_minor(degree: u8) -> Option<Self> {
        match degree {
            1 => Some(RomanNumeral::i),
            2 => Some(RomanNumeral::ii),
            3 => Some(RomanNumeral::iii),
            4 => Some(RomanNumeral::iv),
            5 => Some(RomanNumeral::v),
            6 => Some(RomanNumeral::vi),
            7 => Some(RomanNumeral::vii),
            _ => None,
        }
    }

    /// Convert to a string representation
    pub fn to_string(self) -> String {
        match self {
            RomanNumeral::I => "I".into(),
            RomanNumeral::II => "II".into(),
            RomanNumeral::III => "III".into(),
            RomanNumeral::IV => "IV".into(),
            RomanNumeral::V => "V".into(),
            RomanNumeral::VI => "VI".into(),
            RomanNumeral::VII => "VII".into(),
            RomanNumeral::i => "i".into(),
            RomanNumeral::ii => "ii".into(),
            RomanNumeral::iii => "iii".into(),
            RomanNumeral::iv => "iv".into(),
            RomanNumeral::v => "v".into(),
            RomanNumeral::vi => "vi".into(),
            RomanNumeral::vii => "vii".into(),
        }
    }

    /// Return base roman string (uppercase) without quality info
    pub fn raw(self) -> &'static str {
        match self {
            RomanNumeral::I | RomanNumeral::i => "I",
            RomanNumeral::II | RomanNumeral::ii => "II",
            RomanNumeral::III | RomanNumeral::iii => "III",
            RomanNumeral::IV | RomanNumeral::iv => "IV",
            RomanNumeral::V | RomanNumeral::v => "V",
            RomanNumeral::VI | RomanNumeral::vi => "VI",
            RomanNumeral::VII | RomanNumeral::vii => "VII",
        }
    }

    /// Apply accidentals to a roman numeral: -1=b, +1=#, etc.
    pub fn with_accidental(self, alter: i8) -> String {
        let acc = match alter {
            -2 => "bb",
            -1 => "b",
            0 => "",
            1 => "#",
            2 => "##",
            _ => "",
        };
        format!("{}{}", acc, self.to_string())
    }
}

/// Parse a roman-numeral token (with optional accidental) and return
/// (numeral, alteration) where alteration is -2, -1, 0, +1, +2.
pub fn parse_roman_token(input: &str) -> Option<(RomanNumeral, i8)> {
    if input.is_empty() { return None; }
    let chars: Vec<char> = input.chars().collect();
    let mut idx = 0;

    // Accidental prefix
    let mut alter: i8 = 0;
    if idx < chars.len() {
        match chars[idx] {
            'b' => { alter = -1; idx += 1; },
            '#' => { alter = 1;  idx += 1; },
            _ => {}
        }
    }
    // Optional double accidental
    if idx < chars.len() {
        match chars[idx] {
            'b' if alter == -1 => { alter = -2; idx += 1; },
            '#' if alter == 1  => { alter = 2;  idx += 1; },
            _ => {}
        }
    }

    // Collect numeral letters
    let mut numeral = String::new();
    while idx < chars.len() && "IVivXx".contains(chars[idx]) {
        numeral.push(chars[idx]);
        idx += 1;
    }
    if numeral.is_empty() { return None; }

    let is_minor = numeral.chars().any(|c| c.is_lowercase());
    let upper = numeral.to_uppercase();
    let rn = match (upper.as_str(), is_minor) {
        ("I", false) => RomanNumeral::I,
        ("II", false) => RomanNumeral::II,
        ("III", false) => RomanNumeral::III,
        ("IV", false) => RomanNumeral::IV,
        ("V", false) => RomanNumeral::V,
        ("VI", false) => RomanNumeral::VI,
        ("VII", false) => RomanNumeral::VII,
        ("I", true) => RomanNumeral::i,
        ("II", true) => RomanNumeral::ii,
        ("III", true) => RomanNumeral::iii,
        ("IV", true) => RomanNumeral::iv,
        ("V", true) => RomanNumeral::v,
        ("VI", true) => RomanNumeral::vi,
        ("VII", true) => RomanNumeral::vii,
        _ => return None,
    };
    Some((rn, alter))
}


