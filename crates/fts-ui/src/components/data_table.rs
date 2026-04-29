//! Data table — TanStack-inspired headless table state for Dioxus.

use std::collections::{HashMap, HashSet};

use dioxus::prelude::*;

#[derive(Clone)]
pub struct DataTableColumn<T> {
    pub id: &'static str,
    pub header: &'static str,
    pub accessor: fn(&T) -> String,
    pub sortable: bool,
    pub filterable: bool,
    pub class: &'static str,
    pub head_class: &'static str,
    pub cell_class: &'static str,
}

impl<T> PartialEq for DataTableColumn<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
            && self.header == other.header
            && self.sortable == other.sortable
            && self.filterable == other.filterable
            && self.class == other.class
            && self.head_class == other.head_class
            && self.cell_class == other.cell_class
    }
}

impl<T> DataTableColumn<T> {
    pub fn new(id: &'static str, header: &'static str, accessor: fn(&T) -> String) -> Self {
        Self {
            id,
            header,
            accessor,
            sortable: true,
            filterable: true,
            class: "",
            head_class: "",
            cell_class: "",
        }
    }

    pub fn sortable(mut self, sortable: bool) -> Self {
        self.sortable = sortable;
        self
    }

    pub fn filterable(mut self, filterable: bool) -> Self {
        self.filterable = filterable;
        self
    }

    pub fn class(mut self, class: &'static str) -> Self {
        self.class = class;
        self
    }

    pub fn head_class(mut self, class: &'static str) -> Self {
        self.head_class = class;
        self
    }

    pub fn cell_class(mut self, class: &'static str) -> Self {
        self.cell_class = class;
        self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DataTableSortDirection {
    Asc,
    Desc,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DataTableSort {
    pub column_id: String,
    pub direction: DataTableSortDirection,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct DataTableState {
    pub sorting: Vec<DataTableSort>,
    pub global_filter: String,
    pub page_index: usize,
    pub page_size: usize,
    pub column_visibility: HashMap<String, bool>,
    pub row_selection: HashSet<String>,
}

impl Default for DataTableState {
    fn default() -> Self {
        Self {
            sorting: Vec::new(),
            global_filter: String::new(),
            page_index: 0,
            page_size: 10,
            column_visibility: HashMap::new(),
            row_selection: HashSet::new(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct DataTableModel<T> {
    pub rows: Vec<T>,
    pub columns: Vec<DataTableColumn<T>>,
    pub state: DataTableState,
}

impl<T: Clone> DataTableModel<T> {
    pub fn new(rows: Vec<T>, columns: Vec<DataTableColumn<T>>, state: DataTableState) -> Self {
        Self {
            rows,
            columns,
            state,
        }
    }

    pub fn visible_columns(&self) -> Vec<&DataTableColumn<T>> {
        self.columns
            .iter()
            .filter(|column| {
                self.state
                    .column_visibility
                    .get(column.id)
                    .copied()
                    .unwrap_or(true)
            })
            .collect()
    }

    pub fn row_model(&self) -> Vec<T> {
        let filter = self.state.global_filter.trim().to_lowercase();
        let mut rows = self
            .rows
            .iter()
            .filter(|row| {
                if filter.is_empty() {
                    return true;
                }

                self.columns
                    .iter()
                    .filter(|column| column.filterable)
                    .any(|column| (column.accessor)(row).to_lowercase().contains(&filter))
            })
            .cloned()
            .collect::<Vec<_>>();

        for sort in self.state.sorting.iter().rev() {
            if let Some(column) = self
                .columns
                .iter()
                .find(|column| column.id == sort.column_id && column.sortable)
            {
                rows.sort_by(|a, b| {
                    let ordering = (column.accessor)(a).cmp(&(column.accessor)(b));
                    match sort.direction {
                        DataTableSortDirection::Asc => ordering,
                        DataTableSortDirection::Desc => ordering.reverse(),
                    }
                });
            }
        }

        rows
    }

    pub fn page_count(&self) -> usize {
        let page_size = self.state.page_size.max(1);
        self.row_model().len().div_ceil(page_size)
    }

    pub fn page_rows(&self) -> Vec<T> {
        let page_size = self.state.page_size.max(1);
        let start = self.state.page_index.saturating_mul(page_size);
        self.row_model()
            .into_iter()
            .skip(start)
            .take(page_size)
            .collect()
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct DataTableApi<T: Clone + PartialEq + 'static> {
    rows: ReadSignal<Vec<T>>,
    columns: ReadSignal<Vec<DataTableColumn<T>>>,
    state: Signal<DataTableState>,
}

pub fn use_data_table<T: Clone + PartialEq + 'static>(
    rows: impl Into<ReadSignal<Vec<T>>>,
    columns: impl Into<ReadSignal<Vec<DataTableColumn<T>>>>,
) -> DataTableApi<T> {
    DataTableApi {
        rows: rows.into(),
        columns: columns.into(),
        state: use_signal(DataTableState::default),
    }
}

impl<T: Clone + PartialEq + 'static> DataTableApi<T> {
    pub fn state(&self) -> DataTableState {
        (self.state)()
    }

    pub fn set_state(&mut self, state: DataTableState) {
        self.state.set(state);
    }

    pub fn model(&self) -> DataTableModel<T> {
        DataTableModel::new((self.rows)(), (self.columns)(), self.state())
    }

    pub fn set_global_filter(&mut self, filter: impl Into<String>) {
        self.state.with_mut(|state| {
            state.global_filter = filter.into();
            state.page_index = 0;
        });
    }

    pub fn set_page_size(&mut self, page_size: usize) {
        self.state.with_mut(|state| {
            state.page_size = page_size.max(1);
            state.page_index = 0;
        });
    }

    pub fn next_page(&mut self) {
        let page_count = self.model().page_count();
        self.state.with_mut(|state| {
            if state.page_index + 1 < page_count {
                state.page_index += 1;
            }
        });
    }

    pub fn previous_page(&mut self) {
        self.state.with_mut(|state| {
            state.page_index = state.page_index.saturating_sub(1);
        });
    }

    pub fn toggle_sort(&mut self, column_id: impl Into<String>) {
        let column_id = column_id.into();
        self.state.with_mut(|state| {
            if let Some(sort) = state
                .sorting
                .iter_mut()
                .find(|sort| sort.column_id == column_id)
            {
                match sort.direction {
                    DataTableSortDirection::Asc => {
                        sort.direction = DataTableSortDirection::Desc;
                    }
                    DataTableSortDirection::Desc => {
                        state.sorting.retain(|sort| sort.column_id != column_id);
                    }
                }
            } else {
                state.sorting.push(DataTableSort {
                    column_id,
                    direction: DataTableSortDirection::Asc,
                });
            }
        });
    }

    pub fn set_column_visible(&mut self, column_id: impl Into<String>, visible: bool) {
        self.state
            .write()
            .column_visibility
            .insert(column_id.into(), visible);
    }

    pub fn toggle_row_selected(&mut self, row_id: impl Into<String>) {
        let row_id = row_id.into();
        self.state.with_mut(|state| {
            if !state.row_selection.remove(&row_id) {
                state.row_selection.insert(row_id);
            }
        });
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DataTableFilterProps<T: Clone + PartialEq + 'static> {
    pub table: DataTableApi<T>,
    #[props(default = "Search...".to_string())]
    pub placeholder: String,
    #[props(default)]
    pub class: String,
}

#[component]
pub fn DataTableFilter<T: Clone + PartialEq + 'static>(
    mut props: DataTableFilterProps<T>,
) -> Element {
    rsx! {
        input {
            class: crate::cn::merge_slice(&[
                "h-9 w-full max-w-sm rounded-lg border border-input bg-transparent px-3 py-1 text-sm shadow-xs transition-colors placeholder:text-muted-foreground focus-visible:border-ring focus-visible:outline-none focus-visible:ring-[3px] focus-visible:ring-ring/50",
                props.class.as_str(),
            ]),
            value: "{props.table.state().global_filter}",
            placeholder: "{props.placeholder}",
            oninput: move |event| props.table.set_global_filter(event.value()),
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DataTablePaginationProps {
    pub page_index: usize,
    pub page_count: usize,
    pub on_previous: Callback<MouseEvent>,
    pub on_next: Callback<MouseEvent>,
    #[props(default)]
    pub class: String,
}

#[component]
pub fn DataTablePagination(props: DataTablePaginationProps) -> Element {
    let can_previous = props.page_index > 0;
    let can_next = props.page_index + 1 < props.page_count;

    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex items-center justify-end gap-2", props.class.as_str()]),
            span {
                class: "text-sm text-muted-foreground",
                "Page {props.page_index + 1} of {props.page_count.max(1)}"
            }
            button {
                class: "inline-flex h-8 items-center justify-center rounded-lg border border-input bg-background px-3 text-sm shadow-xs transition-colors hover:bg-accent hover:text-accent-foreground disabled:pointer-events-none disabled:opacity-50",
                disabled: !can_previous,
                onclick: move |event| props.on_previous.call(event),
                "Previous"
            }
            button {
                class: "inline-flex h-8 items-center justify-center rounded-lg border border-input bg-background px-3 text-sm shadow-xs transition-colors hover:bg-accent hover:text-accent-foreground disabled:pointer-events-none disabled:opacity-50",
                disabled: !can_next,
                onclick: move |event| props.on_next.call(event),
                "Next"
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, PartialEq)]
    struct User {
        id: u32,
        name: &'static str,
        role: &'static str,
    }

    fn users() -> Vec<User> {
        vec![
            User {
                id: 1,
                name: "Ada",
                role: "Admin",
            },
            User {
                id: 2,
                name: "Grace",
                role: "Editor",
            },
        ]
    }

    #[test]
    fn filters_rows() {
        let mut state = DataTableState::default();
        state.global_filter = "edit".into();
        let table = DataTableModel::new(
            users(),
            vec![DataTableColumn::new("role", "Role", |user: &User| {
                user.role.to_string()
            })],
            state,
        );

        assert_eq!(table.row_model().len(), 1);
        assert_eq!(table.row_model()[0].name, "Grace");
    }

    #[test]
    fn sorts_rows() {
        let mut state = DataTableState::default();
        state.sorting = vec![DataTableSort {
            column_id: "name".into(),
            direction: DataTableSortDirection::Desc,
        }];
        let table = DataTableModel::new(
            users(),
            vec![DataTableColumn::new("name", "Name", |user: &User| {
                user.name.to_string()
            })],
            state,
        );

        assert_eq!(table.row_model()[0].name, "Grace");
    }

    #[test]
    fn paginates_rows() {
        let state = DataTableState {
            page_index: 1,
            page_size: 1,
            ..Default::default()
        };
        let table = DataTableModel::new(
            users(),
            vec![DataTableColumn::new("id", "ID", |user: &User| {
                user.id.to_string()
            })],
            state,
        );

        assert_eq!(table.page_count(), 2);
        assert_eq!(table.page_rows()[0].name, "Grace");
    }
}
