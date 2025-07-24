//! Universal Scene Description (Core)
//!
//! Usd is the core client-facing module for authoring, composing, and reading Universal Scene Description.
//! USD is designed to encode scalable, hierarchically organized, static and time-sampled data, for the primary
//! purpose of interchanging and augmenting the data between cooperating Digital Content Creation applications.

mod attribute;
mod object;
mod prim;
mod property;
mod relationschip;
mod schema_base;
mod stage;

pub use attribute::*;
pub use object::*;
pub use prim::*;
pub use property::*;
pub use relationschip::*;
pub use schema_base::*;
pub use stage::*;
