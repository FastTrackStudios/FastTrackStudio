use dioxus::prelude::*;
use fts_ui::prelude::*;
use task_core::Task;

use crate::components::TaskCard;

#[derive(Props, Clone, PartialEq)]
pub struct UpcomingViewProps {
    pub tasks: Vec<Task>,
    pub on_complete: EventHandler<String>,
}

#[derive(Debug, Clone, PartialEq)]
enum Bucket {
    Today,
    Tomorrow,
    ThisWeek,
    ThisMonth,
    Later,
}

impl Bucket {
    fn label(&self) -> &'static str {
        match self {
            Bucket::Today => "Today",
            Bucket::Tomorrow => "Tomorrow",
            Bucket::ThisWeek => "This Week",
            Bucket::ThisMonth => "This Month",
            Bucket::Later => "Later",
        }
    }
    fn order(&self) -> u8 {
        match self {
            Bucket::Today => 0,
            Bucket::Tomorrow => 1,
            Bucket::ThisWeek => 2,
            Bucket::ThisMonth => 3,
            Bucket::Later => 4,
        }
    }
}

fn bucket_for(task: &Task, today: chrono::NaiveDate) -> Option<Bucket> {
    let date = task.due.or(task.scheduled)?;
    if date < today {
        return None;
    }
    let days = (date - today).num_days();
    Some(match days {
        0 => Bucket::Today,
        1 => Bucket::Tomorrow,
        2..=7 => Bucket::ThisWeek,
        8..=30 => Bucket::ThisMonth,
        _ => Bucket::Later,
    })
}

#[component]
pub fn UpcomingView(props: UpcomingViewProps) -> Element {
    let today = chrono::Local::now().date_naive();

    let mut buckets: Vec<(Bucket, Vec<Task>)> = Vec::new();

    for task in &props.tasks {
        if !task.has_started() || task.is_complete() {
            continue;
        }
        let Some(bucket) = bucket_for(task, today) else { continue };
        if let Some(entry) = buckets.iter_mut().find(|(b, _)| b == &bucket) {
            entry.1.push(task.clone());
        } else {
            buckets.push((bucket, vec![task.clone()]));
        }
    }

    buckets.sort_by_key(|(b, _)| b.order());
    for (_, tasks) in &mut buckets {
        tasks.sort_by_key(|t| std::cmp::Reverse(t.urgency_score()));
    }

    rsx! {
        VStack { gap: "6".to_string(),
            SectionHeader { label: "Upcoming".to_string() }

            if buckets.is_empty() {
                EmptyState { message: "Nothing scheduled.".to_string() }
            } else {
                for (bucket, tasks) in buckets {
                    VStack { gap: "1".to_string(),
                        div { class: "flex items-center gap-2 px-3 py-1",
                            span { class: "text-xs font-semibold uppercase tracking-wider",
                                "{bucket.label()}"
                            }
                            Badge { variant: BadgeVariant::Secondary, "{tasks.len()}" }
                        }
                        for task in tasks {
                            {
                                let title = task.title.clone();
                                rsx! {
                                    TaskCard {
                                        key: "{task.title}",
                                        task: task.clone(),
                                        on_complete: move |_| props.on_complete.call(title.clone()),
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
