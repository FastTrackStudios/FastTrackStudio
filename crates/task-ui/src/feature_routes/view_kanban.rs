//! `/views/kanban` — stub demo of the `view-kanban` crate.
//!
//! No CRDT wiring yet; columns + cards live in a local signal and
//! mutations are applied via the kanban's event stream. Drop-in
//! replacement for a real `TaskRepoLoro`-backed wrapper later.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use view::kanban::Kanban;
use view::kanban::{
    CardId, ColorTag, KanbanCard, KanbanColumn, KanbanEvent, KanbanState, store::apply,
};

#[component]
pub fn KanbanView() -> Element {
    let mut state = use_signal(seed_state);

    let on_event = EventHandler::new(move |ev: KanbanEvent| {
        state.with_mut(|s| apply(s, &ev));
    });

    let mut readonly = use_signal(|| false);

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3",
            div { class: "flex items-center gap-3",
                Heading { level: HeadingLevel::H1, "Kanban" }
                Spacer {}
                label {
                    class: "flex items-center gap-2 text-sm text-muted-foreground select-none",
                    input {
                        r#type: "checkbox",
                        checked: *readonly.read(),
                        onchange: move |e: FormEvent| {
                            readonly.set(e.value() == "true");
                        },
                    }
                    "Read-only"
                }
            }
            Text { variant: TextVariant::Muted,
                "Drag cards within a column to reorder, or across columns to move. Click '+ Add card' to append. Native HTML5 drag — no virtualization yet."
            }
            div { class: "flex-1 min-h-0",
                Kanban {
                    columns: state.read().columns.clone(),
                    cards: state.read().cards.values().cloned().collect(),
                    readonly: *readonly.read(),
                    on_event,
                }
            }
        }
    }
}

fn seed_state() -> KanbanState {
    let mut backlog = KanbanColumn::new("Backlog", ColorTag::Primary);
    let mut doing = KanbanColumn::new("In progress", ColorTag::Warning);
    let mut done = KanbanColumn::new("Done", ColorTag::Success);

    let seed: &[(&KanbanColumn, &[&str])] = &[
        (
            &backlog.clone(),
            &[
                "Spec out vault diff API",
                "Wire view-kanban into task-ui",
                "Decide on cross-column drop UX",
            ],
        ),
        (
            &doing.clone(),
            &["Port shadcn-kanban-board chrome", "Plumb DragContext"],
        ),
        (&done.clone(), &["Carve out view facade crate"]),
    ];

    let mut state = KanbanState::default();
    let cols: &mut [&mut KanbanColumn] = &mut [&mut backlog, &mut doing, &mut done];

    for (i, (_, titles)) in seed.iter().enumerate() {
        let col = &mut cols[i];
        for t in *titles {
            let card = KanbanCard::new(*t);
            let id: CardId = card.id;
            col.cards.push(id);
            state.cards.insert(id, card);
        }
    }

    state.columns = vec![backlog, doing, done];
    state
}
