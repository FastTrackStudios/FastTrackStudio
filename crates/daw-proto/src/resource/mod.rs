//! REAPER resource paths — service trait.

mod service;

pub use service::{
    ResourceService, ResourceServiceClient, ResourceServiceDispatcher,
    resource_service_service_descriptor,
};
