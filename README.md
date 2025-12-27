<div align="center">

# OpenUSD-rs
**Implementation of [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD) in pure Rust**

[![crates.io](https://img.shields.io/crates/v/openusd-rs?logo=rust&style=flat-square)](https://crates.io/crates/openusd-rs)
[![docs.rs](https://img.shields.io/docsrs/openusd-rs?logo=docs.rs&style=flat-square)](https://docs.rs/openusd-rs)
[![ci](https://img.shields.io/github/actions/workflow/status/FloatyMonkey/openusd-rs/ci.yml?branch=main&logo=github&style=flat-square)](https://github.com/FloatyMonkey/openusd-rs/actions/workflows/ci.yml)

</div>

This library supports reading binary USDC and text USDA files and includes support for composition. It implements the low-level *SDF* API for single-layer access and the high-level *USD* API for working with composed stages. It also provides schemas for easy access to common domain concepts like geometry, lights and skinning.

We're focused on reaching compliance with the USD Core Specification. Expect major API changes during these early stages of development. Currently the codebase mostly reflects the needs of our own [engine](https://github.com/FloatyMonkey/engine) but contributions are more than welcome.

## Example

```rust
use openusd_rs::{gf, vt, usd, usd_geom};

let stage = usd::Stage::open("RootLayer.usd");
let mesh = usd_geom::Mesh::define(&stage, "/Path/To/Mesh/Prim");

// These are arrays of 3D vectors and integers,
// they can be used directly with a graphics API.
let points = mesh.points_attr().get::<vt::Array<gf::Vec3f>>();
let normals = mesh.normals_attr().get::<vt::Array<gf::Vec3f>>();
let indices = usd_geom::triangulate(&mesh);

// Iterate over all top level prims on the stage.
for prim in stage.pseudo_root().children() {
    println!("{}", prim.path());
}
```

## Features

- **File formats**
  - USDC (binary): most features supported
  - USDA (text): support for parsing composition is being worked on
  - USDZ (zip archive): planned

- **Composition**

  Initial composition support is implemented. Ongoing work aims to pass more compliance tests (see the `tests` directory).

- **Schemas**

  - Rust code is generated automatically from USD schema definitions.
  - Additional bespoke APIs for working with schemas include:
    - usdGeom: Primvars and Xform
    - usdSkel: Topology

## Installation

Add `openusd-rs` to the `[dependencies]` section of your `Cargo.toml`.

## Credits

Based on Pixar Animations Studios [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD).

Maintained by Lauro Oyen ([@laurooyen](https://github.com/laurooyen)).

Licensed under [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE).
