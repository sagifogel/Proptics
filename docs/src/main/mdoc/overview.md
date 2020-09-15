---
id: overview
title: Overview
---

Proptics is a Profunctor Optics and Lenses library for Scala (and Scala.js)

## Getting Started

Proptics is built from several modules:

* *core* - contains optics (e.g. `Iso`, `Lens`, `Prism`, `Traversal`), type class definitions (e.g. `Wander`, `Choice`)
  type class instances, and syntax for standard library types and cats data types
* *profunctor* - profunctor type classes (e.g `Choice`, `Closed`) and their instances  
* *newtype* - A type class for newtypes to enable convenient wrapping and unwrapping, and convenient wrapper classes
* *laws* - laws for the optics and type classes
* *tests* - tests the lawfulness of optics and type class instances
* *docs* - source for this website, and documentation

To get started with [sbt](https://scala-sbt.org), simply add the following line to your `build.sbt` file.

```scala
libraryDependencies += Seq(
  "@ORGANIZATION@" %% "@CORE_MODULE_NAME@" % "@LATEST_VERSION@"
  "@ORGANIZATION@" %% "proptics-profunctor" % "@LATEST_VERSION@"
  "@ORGANIZATION@" %% "proptics-newtype" % "@LATEST_VERSION@"
)
```

Published for Scala @SCALA_PUBLISH_VERSIONS@. For changes, refer to the [release notes](https://github.com/sagifogel/proptics/releases).

## Inspiration

Library is inspired by ideas from [purescript-profunctor-lenses](https://github.com/purescript-contrib/purescript-profunctor-lenses).

## License

Licensed under the [MIT License](https://github.com/sagifogel/Proptics/blob/master/LICENSE).