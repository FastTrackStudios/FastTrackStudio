//! `RecipeStep` — child rows of `Recipe`, ordered by `sequence`.

pub mod model;

pub use model::{
    ActiveModel, Column, Entity, Model, Model as RecipeStep, RecipeStepApi, RecipeStepApiCreate,
    RecipeStepApiList, RecipeStepApiUpdate,
};
