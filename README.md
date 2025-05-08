<div align="center">

# OpenUSD-rs
**Pure Rust implementation of [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD)**

</div>

Work in progress, opinionated implementation of OpenUSD in pure Rust. For now, the focus lies on reading from flattened files. It is already possible to extract basic scene information for rendering. Expect major API changes during the early stages of development. Once the official OpenUSD specification is released, we aim to be compliant.

Currently mostly reflects the needs of our own [engine](https://github.com/FloatyMonkey/engine) but contributions are more than welcome.

## Example

```rust
use openusd::{gf, vt, usd, usd_geom};

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

Add the following to the `[dependencies]` section of your `Cargo.toml`:

```
openusd = { git = "https://github.com/FloatyMonkey/openusd-rs.git" }
```

## Credits

Based on Pixar Animations Studios [OpenUSD](https://github.com/PixarAnimationStudios/OpenUSD).

Maintained by Lauro Oyen ([@laurooyen](https://github.com/laurooyen)).

Licensed under [MIT](LICENSE-MIT) or [Apache-2.0](LICENSE-APACHE).
