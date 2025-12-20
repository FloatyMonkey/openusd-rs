<div align="center">

# OpenUSD-rs
**Work in progress, opinionated implementation of [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD) in pure Rust**

[![crates.io](https://img.shields.io/crates/v/openusd-rs?logo=rust&style=flat-square)](https://crates.io/crates/openusd-rs)
[![docs.rs](https://img.shields.io/docsrs/openusd-rs?logo=docs.rs&style=flat-square)](https://docs.rs/openusd-rs)
[![ci](https://img.shields.io/github/actions/workflow/status/FloatyMonkey/openusd-rs/ci.yml?branch=main&logo=github&style=flat-square)](https://github.com/FloatyMonkey/openusd-rs/actions/workflows/ci.yml)

</div>

For now, the focus is on reaching compliance with the recently released USD Core Specification. It is already possible to extract basic scene information for rendering. Expect major API changes during the early stages of development.

Currently mostly reflects the needs of our own [engine](https://github.com/FloatyMonkey/engine) but contributions are more than welcome.

## Example

```rust
use openusd_rs::{gf, vt, usd, usd_geom};

let stage = usd::Stage::open("FlattenedUsdFile.usdc");
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

## Installation

Add `openusd-rs` to the `[dependencies]` section of your `Cargo.toml`.

## Credits

Based on Pixar Animations Studios [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD).

Maintained by Lauro Oyen ([@laurooyen](https://github.com/laurooyen)).

Licensed under [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE).
