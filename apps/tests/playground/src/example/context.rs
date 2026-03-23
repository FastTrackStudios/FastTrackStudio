//! Shared VOX context factory used by controls.

use std::sync::Arc;

pub trait ContextFactory: Send + Sync {
    fn make_context(&self) -> vox::Context;
}

pub type SharedContextFactory = Arc<dyn ContextFactory>;

#[derive(Default)]
pub struct DefaultContextFactory;

impl ContextFactory for DefaultContextFactory {
    fn make_context(&self) -> vox::Context {
        vox::Context::new(
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            vec![],
        )
    }
}
