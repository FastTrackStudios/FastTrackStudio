//! Obsidian Bases (.base YAML) parser. Bases are saved queries over
//! the vault, with one or more views (table / board / gallery /
//! calendar / list). See <https://help.obsidian.md/bases/syntax> for
//! the wire syntax. The leaf filter strings are parsed by a tiny
//! expression parser (see [`expr_parser`]) — recognized identifier
//! prefixes:
//!
//! - `file.<name>` → [`Expr::FileProp`] (file.name, file.mtime, …)
//! - `note.<name>` → [`Expr::NoteProp`] (frontmatter access)
//! - `formula.<name>` → [`Expr::FormulaRef`]
//! - bare `<name>` → [`Expr::NoteProp`]
//!
//! Function calls (`recv.fn(arg, …)`) are parsed generically; the
//! evaluator (in `knowledge-ui::bases`) maps `hasTag` / `hasLink` /
//! `inFolder` / `contains` / `startsWith` / `endsWith` semantics on
//! top of [`FilterNode::Call`].

use serde::{Deserialize, Serialize};

// ── AST ──────────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ParsedBase {
    pub global_filter: FilterNode,
    pub formulas: Vec<Formula>,
    /// Ordered — controls default column layout.
    pub properties: Vec<PropertyConfig>,
    pub views: Vec<ViewSpec>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "node", rename_all = "snake_case")]
pub enum FilterNode {
    And {
        args: Vec<FilterNode>,
    },
    Or {
        args: Vec<FilterNode>,
    },
    Not {
        arg: Box<FilterNode>,
    },
    Cmp {
        left: Expr,
        op: CmpOp,
        right: Expr,
    },
    Call {
        receiver: Expr,
        name: String,
        args: Vec<Expr>,
    },
    Truthy {
        expr: Expr,
    },
    /// Empty filter — matches everything.
    None,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum CmpOp {
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    Contains,
    StartsWith,
    EndsWith,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Expr {
    /// `file.name`, `file.mtime`, `file.ctime`, `file.path`,
    /// `file.size`, `file.ext`, `file.folder`.
    FileProp { name: String },
    /// `note.author` or bare `status` — frontmatter access.
    NoteProp { name: String },
    /// `formula.foo` — reference into the `formulas:` block.
    FormulaRef { name: String },
    /// Literal JSON value (string, number, bool, null, list, map).
    Literal { value: serde_json::Value },
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Formula {
    pub name: String,
    /// Raw source — evaluated on demand by the evaluator.
    pub expression: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PropertyConfig {
    pub key: String,
    pub display_name: Option<String>,
    /// Date format, number locale, etc.
    pub format: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ViewSpec {
    pub kind: ViewKind,
    pub name: String,
    /// View-scoped — AND-ed onto `global_filter` at run time.
    pub filter: Option<FilterNode>,
    /// Visible columns / projection.
    pub order: Vec<String>,
    pub sort: Vec<SortKey>,
    pub limit: Option<u32>,
    pub group_by: Option<String>,
    /// Kind-specific (image property, card size, date property,
    /// columns…). Kept opaque so the schema can grow.
    pub extras: serde_json::Value,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum ViewKind {
    Table,
    Board,
    Gallery,
    Calendar,
    List,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct SortKey {
    pub property: String,
    pub direction: SortDir,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub enum SortDir {
    Asc,
    Desc,
}

#[derive(thiserror::Error, Debug)]
pub enum BaseParseError {
    #[error("yaml: {0}")]
    Yaml(String),
    #[error("invalid filter: {0}")]
    Filter(String),
    #[error("invalid view: {0}")]
    View(String),
    #[error("invalid expression: {0}")]
    Expr(String),
}

// ── Top-level parse / serialize ──────────────────────────────────────

/// Parse a .base YAML string into a typed AST.
pub fn parse(yaml: &str) -> Result<ParsedBase, BaseParseError> {
    let root: serde_yaml::Value =
        serde_yaml::from_str(yaml).map_err(|e| BaseParseError::Yaml(e.to_string()))?;
    let map = match &root {
        serde_yaml::Value::Mapping(m) => m,
        serde_yaml::Value::Null => {
            return Ok(ParsedBase {
                global_filter: FilterNode::None,
                formulas: Vec::new(),
                properties: Vec::new(),
                views: Vec::new(),
            });
        }
        _ => return Err(BaseParseError::Yaml("root must be a mapping".into())),
    };

    let global_filter = match map.get(serde_yaml::Value::String("filters".into())) {
        Some(v) => parse_filter_node(v)?,
        None => FilterNode::None,
    };

    let formulas = match map.get(serde_yaml::Value::String("formulas".into())) {
        Some(serde_yaml::Value::Mapping(fm)) => {
            let mut out = Vec::new();
            for (k, v) in fm {
                let name = yaml_str(k)
                    .ok_or_else(|| BaseParseError::Yaml("formula name not string".into()))?
                    .to_string();
                let expression = yaml_str(v)
                    .ok_or_else(|| BaseParseError::Yaml("formula body not string".into()))?
                    .to_string();
                out.push(Formula { name, expression });
            }
            out
        }
        Some(_) => return Err(BaseParseError::Yaml("formulas must be a mapping".into())),
        None => Vec::new(),
    };

    let properties = match map.get(serde_yaml::Value::String("properties".into())) {
        Some(serde_yaml::Value::Mapping(pm)) => {
            let mut out = Vec::new();
            for (k, v) in pm {
                let key = yaml_str(k)
                    .ok_or_else(|| BaseParseError::Yaml("property key not string".into()))?
                    .to_string();
                let (display_name, format) = match v {
                    serde_yaml::Value::Mapping(m) => (
                        m.get(serde_yaml::Value::String("displayName".into()))
                            .and_then(yaml_str)
                            .map(str::to_string),
                        m.get(serde_yaml::Value::String("format".into()))
                            .and_then(yaml_str)
                            .map(str::to_string),
                    ),
                    serde_yaml::Value::Null => (None, None),
                    _ => (None, None),
                };
                out.push(PropertyConfig {
                    key,
                    display_name,
                    format,
                });
            }
            out
        }
        Some(_) => return Err(BaseParseError::Yaml("properties must be a mapping".into())),
        None => Vec::new(),
    };

    let views = match map.get(serde_yaml::Value::String("views".into())) {
        Some(serde_yaml::Value::Sequence(vs)) => {
            let mut out = Vec::with_capacity(vs.len());
            for v in vs {
                out.push(parse_view(v)?);
            }
            out
        }
        Some(_) => return Err(BaseParseError::View("views must be a sequence".into())),
        None => Vec::new(),
    };

    Ok(ParsedBase {
        global_filter,
        formulas,
        properties,
        views,
    })
}

/// Serialize back to YAML — used when the user edits a Base in our UI.
pub fn serialize(b: &ParsedBase) -> Result<String, BaseParseError> {
    let mut root = serde_yaml::Mapping::new();
    if !matches!(b.global_filter, FilterNode::None) {
        root.insert(
            serde_yaml::Value::String("filters".into()),
            filter_to_yaml(&b.global_filter)?,
        );
    }
    if !b.formulas.is_empty() {
        let mut fm = serde_yaml::Mapping::new();
        for f in &b.formulas {
            fm.insert(
                serde_yaml::Value::String(f.name.clone()),
                serde_yaml::Value::String(f.expression.clone()),
            );
        }
        root.insert(
            serde_yaml::Value::String("formulas".into()),
            serde_yaml::Value::Mapping(fm),
        );
    }
    if !b.properties.is_empty() {
        let mut pm = serde_yaml::Mapping::new();
        for p in &b.properties {
            let mut entry = serde_yaml::Mapping::new();
            if let Some(d) = &p.display_name {
                entry.insert(
                    serde_yaml::Value::String("displayName".into()),
                    serde_yaml::Value::String(d.clone()),
                );
            }
            if let Some(f) = &p.format {
                entry.insert(
                    serde_yaml::Value::String("format".into()),
                    serde_yaml::Value::String(f.clone()),
                );
            }
            pm.insert(
                serde_yaml::Value::String(p.key.clone()),
                serde_yaml::Value::Mapping(entry),
            );
        }
        root.insert(
            serde_yaml::Value::String("properties".into()),
            serde_yaml::Value::Mapping(pm),
        );
    }
    if !b.views.is_empty() {
        let mut vs = Vec::with_capacity(b.views.len());
        for v in &b.views {
            vs.push(view_to_yaml(v)?);
        }
        root.insert(
            serde_yaml::Value::String("views".into()),
            serde_yaml::Value::Sequence(vs),
        );
    }
    serde_yaml::to_string(&serde_yaml::Value::Mapping(root))
        .map_err(|e| BaseParseError::Yaml(e.to_string()))
}

// ── Helpers ──────────────────────────────────────────────────────────

fn yaml_str(v: &serde_yaml::Value) -> Option<&str> {
    match v {
        serde_yaml::Value::String(s) => Some(s.as_str()),
        _ => None,
    }
}

fn parse_view(v: &serde_yaml::Value) -> Result<ViewSpec, BaseParseError> {
    let m = match v {
        serde_yaml::Value::Mapping(m) => m,
        _ => return Err(BaseParseError::View("view must be a mapping".into())),
    };
    let kind_str = m
        .get(serde_yaml::Value::String("type".into()))
        .and_then(yaml_str)
        .ok_or_else(|| BaseParseError::View("view missing `type`".into()))?;
    let kind = match kind_str {
        "table" => ViewKind::Table,
        "board" => ViewKind::Board,
        "gallery" => ViewKind::Gallery,
        "calendar" => ViewKind::Calendar,
        "list" => ViewKind::List,
        other => return Err(BaseParseError::View(format!("unknown view type {other}"))),
    };
    let name = m
        .get(serde_yaml::Value::String("name".into()))
        .and_then(yaml_str)
        .unwrap_or("")
        .to_string();
    let filter = match m.get(serde_yaml::Value::String("filters".into())) {
        Some(node) => Some(parse_filter_node(node)?),
        None => None,
    };
    let order = match m.get(serde_yaml::Value::String("order".into())) {
        Some(serde_yaml::Value::Sequence(s)) => {
            s.iter().filter_map(yaml_str).map(str::to_string).collect()
        }
        _ => Vec::new(),
    };
    let sort = match m.get(serde_yaml::Value::String("sort".into())) {
        Some(serde_yaml::Value::Sequence(s)) => {
            let mut out = Vec::new();
            for entry in s {
                let em = match entry {
                    serde_yaml::Value::Mapping(m) => m,
                    _ => continue,
                };
                let property = em
                    .get(serde_yaml::Value::String("property".into()))
                    .and_then(yaml_str)
                    .unwrap_or("")
                    .to_string();
                let direction = match em
                    .get(serde_yaml::Value::String("direction".into()))
                    .and_then(yaml_str)
                {
                    Some("DESC") | Some("desc") => SortDir::Desc,
                    _ => SortDir::Asc,
                };
                out.push(SortKey {
                    property,
                    direction,
                });
            }
            out
        }
        _ => Vec::new(),
    };
    let limit = m
        .get(serde_yaml::Value::String("limit".into()))
        .and_then(|v| v.as_u64())
        .map(|n| n as u32);
    let group_by = m
        .get(serde_yaml::Value::String("groupBy".into()))
        .and_then(yaml_str)
        .map(str::to_string);

    // Extras = everything we didn't claim. Convert to JSON via yaml→json.
    let mut extras_map = serde_yaml::Mapping::new();
    for (k, v) in m {
        let key = match yaml_str(k) {
            Some(s) => s,
            None => continue,
        };
        if matches!(
            key,
            "type" | "name" | "filters" | "order" | "sort" | "limit" | "groupBy"
        ) {
            continue;
        }
        extras_map.insert(k.clone(), v.clone());
    }
    let extras = yaml_to_json(&serde_yaml::Value::Mapping(extras_map));

    Ok(ViewSpec {
        kind,
        name,
        filter,
        order,
        sort,
        limit,
        group_by,
        extras,
    })
}

fn view_to_yaml(v: &ViewSpec) -> Result<serde_yaml::Value, BaseParseError> {
    let mut m = serde_yaml::Mapping::new();
    let kind_str = match v.kind {
        ViewKind::Table => "table",
        ViewKind::Board => "board",
        ViewKind::Gallery => "gallery",
        ViewKind::Calendar => "calendar",
        ViewKind::List => "list",
    };
    m.insert(
        serde_yaml::Value::String("type".into()),
        serde_yaml::Value::String(kind_str.into()),
    );
    if !v.name.is_empty() {
        m.insert(
            serde_yaml::Value::String("name".into()),
            serde_yaml::Value::String(v.name.clone()),
        );
    }
    if let Some(f) = &v.filter {
        m.insert(
            serde_yaml::Value::String("filters".into()),
            filter_to_yaml(f)?,
        );
    }
    if !v.order.is_empty() {
        m.insert(
            serde_yaml::Value::String("order".into()),
            serde_yaml::Value::Sequence(
                v.order
                    .iter()
                    .map(|s| serde_yaml::Value::String(s.clone()))
                    .collect(),
            ),
        );
    }
    if !v.sort.is_empty() {
        let seq = v
            .sort
            .iter()
            .map(|s| {
                let mut em = serde_yaml::Mapping::new();
                em.insert(
                    serde_yaml::Value::String("property".into()),
                    serde_yaml::Value::String(s.property.clone()),
                );
                em.insert(
                    serde_yaml::Value::String("direction".into()),
                    serde_yaml::Value::String(
                        match s.direction {
                            SortDir::Asc => "ASC",
                            SortDir::Desc => "DESC",
                        }
                        .into(),
                    ),
                );
                serde_yaml::Value::Mapping(em)
            })
            .collect();
        m.insert(
            serde_yaml::Value::String("sort".into()),
            serde_yaml::Value::Sequence(seq),
        );
    }
    if let Some(l) = v.limit {
        m.insert(
            serde_yaml::Value::String("limit".into()),
            serde_yaml::Value::Number(l.into()),
        );
    }
    if let Some(g) = &v.group_by {
        m.insert(
            serde_yaml::Value::String("groupBy".into()),
            serde_yaml::Value::String(g.clone()),
        );
    }
    if let serde_yaml::Value::Mapping(extras) = json_to_yaml(&v.extras) {
        for (k, val) in extras {
            m.insert(k, val);
        }
    }
    Ok(serde_yaml::Value::Mapping(m))
}

// ── Filter parsing ───────────────────────────────────────────────────

fn parse_filter_node(v: &serde_yaml::Value) -> Result<FilterNode, BaseParseError> {
    match v {
        serde_yaml::Value::Null => Ok(FilterNode::None),
        serde_yaml::Value::String(s) => parse_filter_string(s),
        serde_yaml::Value::Mapping(m) => {
            // Expect exactly one of: and / or / not — or a wrapped
            // expression like `{ filter: "<expr>" }`.
            if let Some(args) = m.get(serde_yaml::Value::String("and".into())) {
                let args = parse_filter_list(args)?;
                return Ok(FilterNode::And { args });
            }
            if let Some(args) = m.get(serde_yaml::Value::String("or".into())) {
                let args = parse_filter_list(args)?;
                return Ok(FilterNode::Or { args });
            }
            if let Some(arg) = m.get(serde_yaml::Value::String("not".into())) {
                return Ok(FilterNode::Not {
                    arg: Box::new(parse_filter_node(arg)?),
                });
            }
            Err(BaseParseError::Filter(
                "filter mapping must use and/or/not".into(),
            ))
        }
        serde_yaml::Value::Sequence(seq) => {
            // Bare sequence = implicit AND.
            let mut args = Vec::new();
            for v in seq {
                args.push(parse_filter_node(v)?);
            }
            Ok(FilterNode::And { args })
        }
        _ => Err(BaseParseError::Filter("invalid filter shape".into())),
    }
}

fn parse_filter_list(v: &serde_yaml::Value) -> Result<Vec<FilterNode>, BaseParseError> {
    match v {
        serde_yaml::Value::Sequence(seq) => {
            let mut args = Vec::with_capacity(seq.len());
            for v in seq {
                args.push(parse_filter_node(v)?);
            }
            Ok(args)
        }
        _ => Err(BaseParseError::Filter("and/or expects a sequence".into())),
    }
}

fn parse_filter_string(src: &str) -> Result<FilterNode, BaseParseError> {
    let src = src.trim();
    if src.is_empty() {
        return Ok(FilterNode::None);
    }
    expr_parser::parse_filter(src)
}

fn filter_to_yaml(f: &FilterNode) -> Result<serde_yaml::Value, BaseParseError> {
    Ok(match f {
        FilterNode::None => serde_yaml::Value::Null,
        FilterNode::And { args } => {
            let seq: Result<Vec<_>, _> = args.iter().map(filter_to_yaml).collect();
            let mut m = serde_yaml::Mapping::new();
            m.insert(
                serde_yaml::Value::String("and".into()),
                serde_yaml::Value::Sequence(seq?),
            );
            serde_yaml::Value::Mapping(m)
        }
        FilterNode::Or { args } => {
            let seq: Result<Vec<_>, _> = args.iter().map(filter_to_yaml).collect();
            let mut m = serde_yaml::Mapping::new();
            m.insert(
                serde_yaml::Value::String("or".into()),
                serde_yaml::Value::Sequence(seq?),
            );
            serde_yaml::Value::Mapping(m)
        }
        FilterNode::Not { arg } => {
            let mut m = serde_yaml::Mapping::new();
            m.insert(
                serde_yaml::Value::String("not".into()),
                filter_to_yaml(arg)?,
            );
            serde_yaml::Value::Mapping(m)
        }
        FilterNode::Cmp { left, op, right } => serde_yaml::Value::String(format!(
            "{} {} {}",
            expr_to_source(left),
            cmp_to_source(*op),
            expr_to_source(right)
        )),
        FilterNode::Call {
            receiver,
            name,
            args,
        } => {
            let arg_src: Vec<String> = args.iter().map(expr_to_source).collect();
            serde_yaml::Value::String(format!(
                "{}.{}({})",
                expr_to_source(receiver),
                name,
                arg_src.join(", ")
            ))
        }
        FilterNode::Truthy { expr } => serde_yaml::Value::String(expr_to_source(expr)),
    })
}

fn cmp_to_source(op: CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "==",
        CmpOp::Neq => "!=",
        CmpOp::Lt => "<",
        CmpOp::Le => "<=",
        CmpOp::Gt => ">",
        CmpOp::Ge => ">=",
        CmpOp::Contains => "contains",
        CmpOp::StartsWith => "startsWith",
        CmpOp::EndsWith => "endsWith",
    }
}

fn expr_to_source(e: &Expr) -> String {
    match e {
        Expr::FileProp { name } => {
            if name.is_empty() {
                "file".into()
            } else {
                format!("file.{}", name)
            }
        }
        Expr::NoteProp { name } => {
            if name.is_empty() {
                "note".into()
            } else if name.contains('.') {
                format!("note.{}", name)
            } else {
                name.clone()
            }
        }
        Expr::FormulaRef { name } => {
            if name.is_empty() {
                "formula".into()
            } else {
                format!("formula.{}", name)
            }
        }
        Expr::Literal { value } => match value {
            serde_json::Value::String(s) => format!("\"{}\"", s.replace('"', "\\\"")),
            serde_json::Value::Number(n) => n.to_string(),
            serde_json::Value::Bool(b) => b.to_string(),
            serde_json::Value::Null => "null".into(),
            other => other.to_string(),
        },
    }
}

// ── YAML <-> JSON ────────────────────────────────────────────────────

fn yaml_to_json(v: &serde_yaml::Value) -> serde_json::Value {
    match v {
        serde_yaml::Value::Null => serde_json::Value::Null,
        serde_yaml::Value::Bool(b) => serde_json::Value::Bool(*b),
        serde_yaml::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                serde_json::Value::Number(i.into())
            } else if let Some(u) = n.as_u64() {
                serde_json::Value::Number(u.into())
            } else if let Some(f) = n.as_f64() {
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null)
            } else {
                serde_json::Value::Null
            }
        }
        serde_yaml::Value::String(s) => serde_json::Value::String(s.clone()),
        serde_yaml::Value::Sequence(s) => {
            serde_json::Value::Array(s.iter().map(yaml_to_json).collect())
        }
        serde_yaml::Value::Mapping(m) => {
            let mut out = serde_json::Map::new();
            for (k, v) in m {
                let key = match k {
                    serde_yaml::Value::String(s) => s.clone(),
                    other => serde_yaml::to_string(other)
                        .unwrap_or_default()
                        .trim()
                        .to_string(),
                };
                out.insert(key, yaml_to_json(v));
            }
            serde_json::Value::Object(out)
        }
        serde_yaml::Value::Tagged(t) => yaml_to_json(&t.value),
    }
}

fn json_to_yaml(v: &serde_json::Value) -> serde_yaml::Value {
    match v {
        serde_json::Value::Null => serde_yaml::Value::Null,
        serde_json::Value::Bool(b) => serde_yaml::Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                serde_yaml::Value::Number(i.into())
            } else if let Some(u) = n.as_u64() {
                serde_yaml::Value::Number(u.into())
            } else if let Some(f) = n.as_f64() {
                serde_yaml::Value::Number(f.into())
            } else {
                serde_yaml::Value::Null
            }
        }
        serde_json::Value::String(s) => serde_yaml::Value::String(s.clone()),
        serde_json::Value::Array(a) => {
            serde_yaml::Value::Sequence(a.iter().map(json_to_yaml).collect())
        }
        serde_json::Value::Object(o) => {
            let mut m = serde_yaml::Mapping::new();
            for (k, v) in o {
                m.insert(serde_yaml::Value::String(k.clone()), json_to_yaml(v));
            }
            serde_yaml::Value::Mapping(m)
        }
    }
}

// ── Expression parser (leaf filter strings) ──────────────────────────

pub mod expr_parser {
    //! Tiny recursive-descent parser for Bases filter expressions.
    //!
    //! Grammar:
    //!   filter   := or
    //!   or       := and ("||" and)*
    //!   and      := not ("&&" not)*
    //!   not      := "!" not | cmp
    //!   cmp      := postfix (("==" | "!=" | "<=" | ">=" | "<" | ">") postfix)?
    //!   postfix  := primary ("." IDENT ("(" args? ")")?)*
    //!   primary  := literal | ident | "(" or ")"
    //!   literal  := STRING | NUMBER | "true" | "false" | "null"
    //!   args     := or ("," or)*

    use super::{BaseParseError, CmpOp, Expr, FilterNode};

    pub fn parse_filter(src: &str) -> Result<FilterNode, BaseParseError> {
        let mut p = Parser::new(src);
        let node = p.parse_or()?;
        p.skip_ws();
        if p.pos < p.src.len() {
            return Err(BaseParseError::Expr(format!(
                "trailing input at byte {}: {:?}",
                p.pos,
                &p.src[p.pos..]
            )));
        }
        Ok(node)
    }

    struct Parser<'a> {
        src: &'a [u8],
        pos: usize,
    }

    impl<'a> Parser<'a> {
        fn new(src: &'a str) -> Self {
            Self {
                src: src.as_bytes(),
                pos: 0,
            }
        }

        fn skip_ws(&mut self) {
            while self.pos < self.src.len() && self.src[self.pos].is_ascii_whitespace() {
                self.pos += 1;
            }
        }

        fn peek(&self) -> Option<u8> {
            self.src.get(self.pos).copied()
        }

        fn eat(&mut self, lit: &[u8]) -> bool {
            self.skip_ws();
            if self.src[self.pos..].starts_with(lit) {
                self.pos += lit.len();
                true
            } else {
                false
            }
        }

        fn parse_or(&mut self) -> Result<FilterNode, BaseParseError> {
            let mut left = self.parse_and()?;
            loop {
                self.skip_ws();
                if self.eat(b"||") {
                    let right = self.parse_and()?;
                    left = match left {
                        FilterNode::Or { mut args } => {
                            args.push(right);
                            FilterNode::Or { args }
                        }
                        other => FilterNode::Or {
                            args: vec![other, right],
                        },
                    };
                } else {
                    break;
                }
            }
            Ok(left)
        }

        fn parse_and(&mut self) -> Result<FilterNode, BaseParseError> {
            let mut left = self.parse_not()?;
            loop {
                self.skip_ws();
                if self.eat(b"&&") {
                    let right = self.parse_not()?;
                    left = match left {
                        FilterNode::And { mut args } => {
                            args.push(right);
                            FilterNode::And { args }
                        }
                        other => FilterNode::And {
                            args: vec![other, right],
                        },
                    };
                } else {
                    break;
                }
            }
            Ok(left)
        }

        fn parse_not(&mut self) -> Result<FilterNode, BaseParseError> {
            self.skip_ws();
            if self.peek() == Some(b'!') && self.src.get(self.pos + 1) != Some(&b'=') {
                self.pos += 1;
                let inner = self.parse_not()?;
                return Ok(FilterNode::Not {
                    arg: Box::new(inner),
                });
            }
            self.parse_cmp()
        }

        fn parse_cmp(&mut self) -> Result<FilterNode, BaseParseError> {
            let left_out = self.parse_postfix()?;
            // If the left side is a Call, that already IS a FilterNode
            // (predicate). It cannot be the lhs of a comparison.
            let left = match left_out {
                PostfixOut::Expr(e) => e,
                PostfixOut::Call {
                    receiver,
                    name,
                    args,
                } => {
                    return Ok(FilterNode::Call {
                        receiver,
                        name,
                        args,
                    });
                }
            };
            self.skip_ws();
            let op = if self.eat(b"==") {
                Some(CmpOp::Eq)
            } else if self.eat(b"!=") {
                Some(CmpOp::Neq)
            } else if self.eat(b"<=") {
                Some(CmpOp::Le)
            } else if self.eat(b">=") {
                Some(CmpOp::Ge)
            } else if self.eat(b"<") {
                Some(CmpOp::Lt)
            } else if self.eat(b">") {
                Some(CmpOp::Gt)
            } else {
                None
            };
            match op {
                Some(op) => {
                    let right = self.parse_postfix_expr()?;
                    Ok(FilterNode::Cmp { left, op, right })
                }
                None => Ok(FilterNode::Truthy { expr: left }),
            }
        }

        /// Parse a postfix expression that may also produce a Call as
        /// a FilterNode (when followed by `()`).
        fn parse_postfix(&mut self) -> Result<PostfixOut, BaseParseError> {
            let mut expr = self.parse_primary()?;
            loop {
                self.skip_ws();
                if self.peek() == Some(b'.') {
                    self.pos += 1;
                    let ident = self.parse_ident()?;
                    self.skip_ws();
                    if self.peek() == Some(b'(') {
                        self.pos += 1;
                        let args = self.parse_args()?;
                        // Function call. Return as a FilterNode::Call
                        // wrapped in PostfixOut::Call.
                        return Ok(PostfixOut::Call {
                            receiver: expr,
                            name: ident,
                            args,
                        });
                    } else {
                        // Property access — fold into the existing
                        // identifier path.
                        expr = match expr {
                            Expr::FileProp { name } => Expr::FileProp {
                                name: join_path(&name, &ident),
                            },
                            Expr::NoteProp { name } => Expr::NoteProp {
                                name: join_path(&name, &ident),
                            },
                            Expr::FormulaRef { name } => Expr::FormulaRef {
                                name: join_path(&name, &ident),
                            },
                            other => Expr::NoteProp {
                                name: join_path(expr_root_name(&other), &ident),
                            },
                        };
                    }
                } else {
                    break;
                }
            }
            Ok(PostfixOut::Expr(expr))
        }

        /// Like parse_postfix but always returns an Expr — for use on
        /// the RHS of a comparison.
        fn parse_postfix_expr(&mut self) -> Result<Expr, BaseParseError> {
            match self.parse_postfix()? {
                PostfixOut::Expr(e) => Ok(e),
                PostfixOut::Call { .. } => Err(BaseParseError::Expr(
                    "function call not allowed on rhs of comparison".into(),
                )),
            }
        }

        fn parse_args(&mut self) -> Result<Vec<Expr>, BaseParseError> {
            let mut out = Vec::new();
            self.skip_ws();
            if self.peek() == Some(b')') {
                self.pos += 1;
                return Ok(out);
            }
            loop {
                let e = self.parse_postfix_expr()?;
                out.push(e);
                self.skip_ws();
                if self.eat(b",") {
                    continue;
                }
                if self.eat(b")") {
                    break;
                }
                return Err(BaseParseError::Expr("expected , or ) in args".into()));
            }
            Ok(out)
        }

        fn parse_primary(&mut self) -> Result<Expr, BaseParseError> {
            self.skip_ws();
            match self.peek() {
                Some(b'"') | Some(b'\'') => self.parse_string(),
                Some(b'(') => {
                    self.pos += 1;
                    // Sub-expressions in primary position are
                    // restricted to plain Exprs (no AND/OR) — Bases
                    // doesn't actually need grouping here.
                    let e = self.parse_postfix_expr()?;
                    if !self.eat(b")") {
                        return Err(BaseParseError::Expr("expected )".into()));
                    }
                    Ok(e)
                }
                Some(c) if c.is_ascii_digit() || c == b'-' => self.parse_number(),
                Some(c) if is_ident_start(c) => {
                    let ident = self.parse_ident()?;
                    match ident.as_str() {
                        "true" => Ok(Expr::Literal {
                            value: serde_json::Value::Bool(true),
                        }),
                        "false" => Ok(Expr::Literal {
                            value: serde_json::Value::Bool(false),
                        }),
                        "null" => Ok(Expr::Literal {
                            value: serde_json::Value::Null,
                        }),
                        _ => Ok(expr_from_ident(ident)),
                    }
                }
                Some(c) => Err(BaseParseError::Expr(format!(
                    "unexpected char {:?} at byte {}",
                    c as char, self.pos
                ))),
                None => Err(BaseParseError::Expr("unexpected end of input".into())),
            }
        }

        fn parse_ident(&mut self) -> Result<String, BaseParseError> {
            self.skip_ws();
            let start = self.pos;
            while let Some(c) = self.peek() {
                if is_ident_continue(c) {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            if start == self.pos {
                return Err(BaseParseError::Expr("expected identifier".into()));
            }
            Ok(std::str::from_utf8(&self.src[start..self.pos])
                .map_err(|e| BaseParseError::Expr(e.to_string()))?
                .to_string())
        }

        fn parse_string(&mut self) -> Result<Expr, BaseParseError> {
            let quote = self.src[self.pos];
            self.pos += 1;
            let start = self.pos;
            let mut buf = String::new();
            while self.pos < self.src.len() {
                let c = self.src[self.pos];
                if c == b'\\' && self.pos + 1 < self.src.len() {
                    let n = self.src[self.pos + 1];
                    buf.push(match n {
                        b'n' => '\n',
                        b't' => '\t',
                        b'"' => '"',
                        b'\'' => '\'',
                        b'\\' => '\\',
                        other => other as char,
                    });
                    self.pos += 2;
                    continue;
                }
                if c == quote {
                    self.pos += 1;
                    return Ok(Expr::Literal {
                        value: serde_json::Value::String(buf),
                    });
                }
                buf.push(c as char);
                self.pos += 1;
            }
            Err(BaseParseError::Expr(format!(
                "unterminated string starting at {}",
                start
            )))
        }

        fn parse_number(&mut self) -> Result<Expr, BaseParseError> {
            let start = self.pos;
            if self.peek() == Some(b'-') {
                self.pos += 1;
            }
            while let Some(c) = self.peek() {
                if c.is_ascii_digit() || c == b'.' {
                    self.pos += 1;
                } else {
                    break;
                }
            }
            let raw = std::str::from_utf8(&self.src[start..self.pos])
                .map_err(|e| BaseParseError::Expr(e.to_string()))?;
            let n: f64 = raw
                .parse()
                .map_err(|e: std::num::ParseFloatError| BaseParseError::Expr(e.to_string()))?;
            Ok(Expr::Literal {
                value: serde_json::Number::from_f64(n)
                    .map(serde_json::Value::Number)
                    .unwrap_or(serde_json::Value::Null),
            })
        }
    }

    enum PostfixOut {
        Expr(Expr),
        Call {
            receiver: Expr,
            name: String,
            args: Vec<Expr>,
        },
    }

    fn is_ident_start(c: u8) -> bool {
        c.is_ascii_alphabetic() || c == b'_'
    }

    fn is_ident_continue(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_'
    }

    fn expr_from_ident(ident: String) -> Expr {
        if ident == "file" {
            Expr::FileProp {
                name: String::new(),
            }
        } else if ident == "note" {
            Expr::NoteProp {
                name: String::new(),
            }
        } else if ident == "formula" {
            Expr::FormulaRef {
                name: String::new(),
            }
        } else {
            Expr::NoteProp { name: ident }
        }
    }

    fn join_path(prefix: &str, ident: &str) -> String {
        if prefix.is_empty() {
            ident.to_string()
        } else {
            format!("{prefix}.{ident}")
        }
    }

    fn expr_root_name(e: &Expr) -> &str {
        match e {
            Expr::FileProp { name } | Expr::NoteProp { name } | Expr::FormulaRef { name } => name,
            Expr::Literal { .. } => "",
        }
    }
}

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    const REP_BASE: &str = r#"
filters:
  and:
    - file.hasTag("book")
    - or:
        - status == "reading"
        - status == "to-read"
    - not: file.inFolder("Archive")
formulas:
  formattedPrice: 'toFixed(note.price, 2)'
  ageDays: '(now() - file.ctime) / 86400'
properties:
  note.author:
    displayName: "Author"
  status:
    displayName: "Status"
  formula.formattedPrice:
    displayName: "Price"
views:
  - type: table
    name: "All books"
    order:
      - file.name
      - status
      - formula.formattedPrice
    sort:
      - property: file.name
        direction: ASC
    limit: 100
  - type: board
    name: "By status"
    groupBy: status
  - type: gallery
    name: "Covers"
    image: note.cover
  - type: calendar
    name: "Due"
    dateProperty: note.due
  - type: list
    name: "Compact"
"#;

    #[test]
    fn parses_representative_base() {
        let parsed = parse(REP_BASE).expect("parse");
        assert_eq!(parsed.formulas.len(), 2);
        assert_eq!(parsed.properties.len(), 3);
        assert_eq!(parsed.views.len(), 5);
        // Filters round-trip into an And with 3 children.
        match parsed.global_filter {
            FilterNode::And { ref args } => assert_eq!(args.len(), 3),
            other => panic!("expected And, got {:?}", other),
        }
    }

    #[test]
    fn round_trip_parse_serialize_parse() {
        let parsed = parse(REP_BASE).expect("parse");
        let yaml = serialize(&parsed).expect("serialize");
        let reparsed = parse(&yaml).expect("reparse");
        assert_eq!(parsed.formulas, reparsed.formulas);
        assert_eq!(parsed.properties, reparsed.properties);
        assert_eq!(parsed.views.len(), reparsed.views.len());
        for (a, b) in parsed.views.iter().zip(reparsed.views.iter()) {
            assert_eq!(a.kind, b.kind);
            assert_eq!(a.name, b.name);
            assert_eq!(a.order, b.order);
            assert_eq!(a.sort, b.sort);
            assert_eq!(a.limit, b.limit);
            assert_eq!(a.group_by, b.group_by);
        }
    }

    #[test]
    fn bare_comparison() {
        let f = expr_parser::parse_filter(r#"status == "done""#).unwrap();
        match f {
            FilterNode::Cmp { left, op, right } => {
                assert_eq!(op, CmpOp::Eq);
                assert_eq!(
                    left,
                    Expr::NoteProp {
                        name: "status".into()
                    }
                );
                assert_eq!(
                    right,
                    Expr::Literal {
                        value: serde_json::Value::String("done".into())
                    }
                );
            }
            other => panic!("expected Cmp, got {:?}", other),
        }
    }

    #[test]
    fn nested_and_or_not() {
        let yaml = r#"
filters:
  and:
    - or:
        - status == "a"
        - status == "b"
    - not: file.inFolder("X")
"#;
        let p = parse(yaml).unwrap();
        match p.global_filter {
            FilterNode::And { args } => {
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], FilterNode::Or { .. }));
                assert!(matches!(args[1], FilterNode::Not { .. }));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn function_call_with_string_and_number() {
        let f = expr_parser::parse_filter(r#"file.hasTag("book", 2)"#).unwrap();
        match f {
            FilterNode::Call {
                receiver,
                name,
                args,
            } => {
                assert_eq!(
                    receiver,
                    Expr::FileProp {
                        name: String::new()
                    }
                );
                assert_eq!(name, "hasTag");
                assert_eq!(args.len(), 2);
                assert!(matches!(args[0], Expr::Literal { .. }));
                assert!(matches!(args[1], Expr::Literal { .. }));
            }
            other => panic!("expected Call, got {:?}", other),
        }
    }

    #[test]
    fn formula_reference() {
        let f = expr_parser::parse_filter(r#"formula.price > 10"#).unwrap();
        match f {
            FilterNode::Cmp { left, op, .. } => {
                assert_eq!(op, CmpOp::Gt);
                assert_eq!(
                    left,
                    Expr::FormulaRef {
                        name: "price".into()
                    }
                );
            }
            _ => panic!(),
        }
    }

    #[test]
    fn all_view_kinds_parse() {
        let p = parse(REP_BASE).unwrap();
        let kinds: Vec<ViewKind> = p.views.iter().map(|v| v.kind).collect();
        assert!(kinds.contains(&ViewKind::Table));
        assert!(kinds.contains(&ViewKind::Board));
        assert!(kinds.contains(&ViewKind::Gallery));
        assert!(kinds.contains(&ViewKind::Calendar));
        assert!(kinds.contains(&ViewKind::List));
    }

    #[test]
    fn empty_yaml_is_ok() {
        let p = parse("").unwrap();
        assert_eq!(p.global_filter, FilterNode::None);
        assert!(p.views.is_empty());
    }
}
