//! Bridge from `.cook` files to the wiki graph.
//!
//! The wiki feature indexes wikilinks (`[[name]]`) emanating
//! from each page so the backlinks pane on `Wiki/flour.md`
//! lists every page that mentions flour. Recipes don't write
//! `[[flour]]` — they write `@flour{500%g}` — so without a
//! bridge, recipes would be invisible to wiki backlinks.
//!
//! [`recipe_wiki_edges`] projects each cooklang `@ingredient`
//! into a wikilink edge `recipe_path → ingredient_name`.
//! Wiki indexers call this on every `.cook` file they discover
//! and feed the result into the same edge store that handles
//! markdown `[[...]]` links.

use crate::model::Recipe;

/// One wiki edge derived from a recipe.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WikiEdge {
    /// Source — the recipe path (vault-relative `.cook`).
    pub source: String,
    /// Target — the wikilink basename (e.g. `"flour"`).
    /// Wiki resolution does case-insensitive lookup against
    /// page basenames in the rest of the vault.
    pub target: String,
    /// What flavor of cooklang reference produced the edge.
    pub kind: WikiEdgeKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum WikiEdgeKind {
    /// `@ingredient` → wikilink to the ingredient's wiki
    /// page (typically a pantry item like `[[flour]]`).
    Ingredient,
    /// `#cookware` → wikilink to the cookware's wiki page
    /// (`[[skillet]]`, etc.).
    Cookware,
    /// `@@./path/to/recipe{}` → wikilink to another recipe.
    /// `target` is the relative path (cooklang convention),
    /// not a basename.
    RecipeRef,
    /// `[[wikilink]]` embedded in a step body — concept
    /// pages, techniques, related recipes. Cooklang treats
    /// `[[...]]` as plain text, so the syntax round-trips
    /// through the parser unchanged; we extract it here so
    /// the wiki graph picks up the same backlinks it would
    /// for a markdown page.
    Concept,
}

/// Project the recipe into a flat list of wiki edges. One
/// edge per `@ingredient` (deduplicated), one per
/// `#cookware`, one per `@@recipe-ref`.
#[must_use]
pub fn recipe_wiki_edges(recipe: &Recipe) -> Vec<WikiEdge> {
    let mut out: Vec<WikiEdge> = Vec::new();
    let mut seen: std::collections::HashSet<(String, WikiEdgeKind)> =
        std::collections::HashSet::new();

    for ing in &recipe.ingredients {
        if ing.is_recipe_ref {
            // Recipe refs are handled below from
            // `nested_recipes` to keep `target` as the path.
            continue;
        }
        let key = (ing.name.to_ascii_lowercase(), WikiEdgeKind::Ingredient);
        if seen.insert(key) {
            out.push(WikiEdge {
                source: recipe.path.clone(),
                target: ing.name.clone(),
                kind: WikiEdgeKind::Ingredient,
            });
        }
    }

    for cw in &recipe.cookware {
        let key = (cw.to_ascii_lowercase(), WikiEdgeKind::Cookware);
        if seen.insert(key) {
            out.push(WikiEdge {
                source: recipe.path.clone(),
                target: cw.clone(),
                kind: WikiEdgeKind::Cookware,
            });
        }
    }

    for path in &recipe.nested_recipes {
        out.push(WikiEdge {
            source: recipe.path.clone(),
            target: path.clone(),
            kind: WikiEdgeKind::RecipeRef,
        });
    }

    // `[[wikilinks]]` embedded in step text. Cooklang passes
    // these through as plain text, so we scan the rendered
    // step strings plus the raw source as a fallback (covers
    // links sitting in metadata values or text-only sections).
    for step in &recipe.steps {
        for target in scan_wikilinks(step) {
            let key = (target.to_ascii_lowercase(), WikiEdgeKind::Concept);
            if seen.insert(key) {
                out.push(WikiEdge {
                    source: recipe.path.clone(),
                    target,
                    kind: WikiEdgeKind::Concept,
                });
            }
        }
    }
    for target in scan_wikilinks(&recipe.source) {
        let key = (target.to_ascii_lowercase(), WikiEdgeKind::Concept);
        if seen.insert(key) {
            out.push(WikiEdge {
                source: recipe.path.clone(),
                target,
                kind: WikiEdgeKind::Concept,
            });
        }
    }

    out
}

/// Find every `[[target]]` in `s`. Targets are returned
/// trimmed; nested or escaped brackets are not supported
/// (matches Obsidian-style wikilink semantics).
fn scan_wikilinks(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == b'[' && bytes[i + 1] == b'[' {
            let rest = &s[i + 2..];
            if let Some(end) = rest.find("]]") {
                let target = rest[..end].trim();
                // Strip pipe-display syntax: `[[target|alias]]`.
                let target = target.split('|').next().unwrap_or(target).trim();
                if !target.is_empty() && !target.contains('\n') {
                    out.push(target.to_string());
                }
                i += 2 + end + 2;
                continue;
            }
        }
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse_cook;

    #[test]
    fn emits_one_edge_per_unique_ingredient() {
        let src = "\
>> title: Pasta
Cook @flour{200%g} and toss with @flour{50%g} for dusting and @salt{1%tsp}.
";
        let r = parse_cook("Cookbook/Pasta.cook", src).unwrap();
        let edges = recipe_wiki_edges(&r);
        let names: Vec<&str> = edges
            .iter()
            .filter(|e| e.kind == WikiEdgeKind::Ingredient)
            .map(|e| e.target.as_str())
            .collect();
        assert_eq!(names, vec!["flour", "salt"]);
    }

    #[test]
    fn extracts_concept_wikilinks_from_step_bodies() {
        let src = "\
>> title: Carbonara

Whisk @eggs{2} per [[mise en place]] guidelines.
Cook @pancetta{100%g} until crisp - see [[render fat]] for technique.
Reference [[render fat]] again to dedupe.
";
        let r = parse_cook("Cookbook/Carbonara.cook", src).unwrap();
        let edges = recipe_wiki_edges(&r);
        let concepts: Vec<&str> = edges
            .iter()
            .filter(|e| e.kind == WikiEdgeKind::Concept)
            .map(|e| e.target.as_str())
            .collect();
        // Order: first-occurrence; dedup case-insensitive.
        assert!(concepts.contains(&"mise en place"));
        assert!(concepts.contains(&"render fat"));
        // Dedup: "render fat" appears twice in source but
        // only once in edges.
        assert_eq!(concepts.iter().filter(|t| **t == "render fat").count(), 1);
    }

    #[test]
    fn wikilink_with_pipe_alias() {
        let src = "Use [[saute|sautéing]] on medium heat.";
        let r = parse_cook("Cookbook/X.cook", src).unwrap();
        let edges = recipe_wiki_edges(&r);
        assert!(
            edges
                .iter()
                .any(|e| e.kind == WikiEdgeKind::Concept && e.target == "saute"),
            "got {edges:?}"
        );
    }

    #[test]
    fn includes_cookware_and_recipe_refs() {
        let src = "\
>> title: Pizza

Make @@./Shared/Dough{} on a #stone{}.
";
        let r = parse_cook("Cookbook/Pizza.cook", src).unwrap();
        let edges = recipe_wiki_edges(&r);
        assert!(
            edges
                .iter()
                .any(|e| e.kind == WikiEdgeKind::Cookware && e.target == "stone")
        );
        assert!(
            edges
                .iter()
                .any(|e| e.kind == WikiEdgeKind::RecipeRef && e.target.ends_with("Dough"))
        );
    }
}
