//! Items control — ergonomic facade over ItemServiceLive

use super::live::ItemServiceLive;
use super::proto::*;
use crate::example::auth::proto::SessionToken;
use crate::example::context::{DefaultContextFactory, SharedContextFactory};
use std::marker::PhantomData;
use std::sync::Arc;

pub struct RawName;
pub struct ValidName;

#[derive(Debug, Clone)]
pub struct ItemName<State = RawName> {
    value: String,
    _state: PhantomData<State>,
}

impl ItemName<RawName> {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            _state: PhantomData,
        }
    }

    pub fn validate(self) -> Result<ItemName<ValidName>, String> {
        let trimmed = self.value.trim();
        if trimmed.is_empty() {
            return Err("Item name cannot be empty".into());
        }
        Ok(ItemName {
            value: trimmed.to_string(),
            _state: PhantomData,
        })
    }
}

impl ItemName<ValidName> {
    pub fn as_str(&self) -> &str {
        &self.value
    }
}

pub struct ItemControl<S = ItemServiceLive>
where
    S: ItemService,
{
    service: Arc<S>,
    context_factory: SharedContextFactory,
}

impl<S> Clone for ItemControl<S>
where
    S: ItemService,
{
    fn clone(&self) -> Self {
        Self {
            service: self.service.clone(),
            context_factory: self.context_factory.clone(),
        }
    }
}

impl<S> PartialEq for ItemControl<S>
where
    S: ItemService,
{
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.service, &other.service)
    }
}

impl<S> ItemControl<S>
where
    S: ItemService,
{
    pub fn new(service: Arc<S>) -> Self {
        Self::new_with_context(service, Arc::new(DefaultContextFactory))
    }

    pub fn new_with_context(service: Arc<S>, context_factory: SharedContextFactory) -> Self {
        Self {
            service,
            context_factory,
        }
    }

    pub async fn list(&self, token: &SessionToken) -> Vec<ItemInfo> {
        let cx = self.context_factory.make_context();
        self.service.list_items(token.clone()).await
    }

    pub async fn create(&self, token: &SessionToken, name: ItemName<ValidName>) -> Option<String> {
        let cx = self.context_factory.make_context();
        let id = self
            .service
            .create_item(token.clone(), name.as_str().to_string())
            .await;
        if id.is_empty() {
            None
        } else {
            Some(id)
        }
    }

    pub async fn toggle(&self, token: &SessionToken, id: &str) {
        let cx = self.context_factory.make_context();
        self.service
            .toggle_item(token.clone(), id.to_string())
            .await;
    }

    pub async fn delete(&self, token: &SessionToken, id: &str) {
        let cx = self.context_factory.make_context();
        self.service
            .delete_item(token.clone(), id.to_string())
            .await;
    }

    pub async fn subscribe(&self, token: &SessionToken) -> eyre::Result<vox::Rx<ItemEvent>> {
        let cx = self.context_factory.make_context();
        let (tx, rx) = vox::channel::<ItemEvent>();
        self.service.subscribe(token.clone(), tx).await;
        Ok(rx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::example::context::ContextFactory;
    use vox::Tx;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Mutex;

    struct CountingContextFactory {
        calls: AtomicUsize,
    }

    impl CountingContextFactory {
        fn new() -> Self {
            Self {
                calls: AtomicUsize::new(0),
            }
        }

        fn call_count(&self) -> usize {
            self.calls.load(Ordering::SeqCst)
        }
    }

    impl ContextFactory for CountingContextFactory {
        fn make_context(&self) -> vox::Context {
            self.calls.fetch_add(1, Ordering::SeqCst);
            Context::new(
                Default::default(),
                Default::default(),
                Default::default(),
                Default::default(),
                vec![],
            )
        }
    }

    #[derive(Default)]
    struct FakeItemService {
        next_create_id: Mutex<String>,
    }

    impl FakeItemService {
        fn with_create_id(id: &str) -> Self {
            Self {
                next_create_id: Mutex::new(id.to_string()),
            }
        }
    }

    impl ItemService for FakeItemService {
        async fn list_items(&self, __token: SessionToken) -> Vec<ItemInfo> {
            vec![ItemInfo {
                id: "i-1".into(),
                name: "demo".into(),
                done: false,
            }]
        }

        async fn create_item(&self, __token: SessionToken, _name: String) -> String {
            self.next_create_id.lock().unwrap().clone()
        }

        async fn toggle_item(&self, __token: SessionToken, _id: String) {}

        async fn delete_item(&self, __token: SessionToken, _id: String) {}

        async fn subscribe(&self, __token: SessionToken, _tx: Tx<ItemEvent>) {}
    }

    fn token() -> SessionToken {
        SessionToken {
            token: "token".into(),
            user_id: "u1".into(),
            user_email: "demo@example.com".into(),
            user_name: "Demo".into(),
        }
    }

    fn valid_name(name: &str) -> ItemName<ValidName> {
        ItemName::new(name).validate().expect("expected valid item name")
    }

    #[test]
    fn item_name_typestate_validation() {
        assert!(ItemName::new("   ").validate().is_err());
        let valid = ItemName::new("  task  ").validate().unwrap();
        assert_eq!(valid.as_str(), "task");
    }

    #[tokio::test]
    async fn create_maps_empty_id_to_none() {
        let service = Arc::new(FakeItemService::with_create_id(""));
        let control = ItemControl::new(service);
        let result = control.create(&token(), valid_name("task")).await;
        assert_eq!(result, None);
    }

    #[tokio::test]
    async fn create_maps_id_to_some() {
        let service = Arc::new(FakeItemService::with_create_id("item-123"));
        let control = ItemControl::new(service);
        let result = control.create(&token(), valid_name("task")).await;
        assert_eq!(result.as_deref(), Some("item-123"));
    }

    #[tokio::test]
    async fn context_factory_is_used_for_each_control_call() {
        let service = Arc::new(FakeItemService::with_create_id("x"));
        let factory = Arc::new(CountingContextFactory::new());
        let control = ItemControl::new_with_context(service, factory.clone());
        let t = token();

        let _ = control.list(&t).await;
        let _ = control.create(&t, valid_name("task")).await;
        control.toggle(&t, "x").await;
        control.delete(&t, "x").await;
        let _ = control.subscribe(&t).await.unwrap();

        assert_eq!(factory.call_count(), 5);
    }
}
