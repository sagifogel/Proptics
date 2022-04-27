![Proptics Logo](./proptics.png)

[![CI](https://github.com/sagifogel/Proptics/actions/workflows/ci.yml/badge.svg)](https://github.com/sagifogel/Proptics/actions/workflows/ci.yml)
[![Maven Central](https://img.shields.io/maven-central/v/io.github.sagifogel/proptics-core_2.13.svg?color=32c954)](https://maven-badges.herokuapp.com/maven-central/io.github.sagifogel/proptics-core_2.13)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

### Overview

Proptics is a Profunctor Optics and Lenses library for [Scala programming language](https://scala-lang.org).</br>
It uses a Profunctor encoding for its internal representation of optics, you can learn about profunctors in the [profunctor](https://sagifogel.github.io/Proptics/docs/profunctors/profunctor) guide.</br>
Proptics is available for Scala 2.12, Scala 2.13, Scala 3.0, and [Scala.js](http://www.scala-js.org/),
and is built on top of [Cats](https://typelevel.org/cats/), and [Spire](https://typelevel.org/spire/).


### Getting Started

Add to your `build.sbt`
```scala
libraryDependencies ++= Seq(
  "io.github.sagifogel" %% "proptics-core" % "0.4.2",
  "io.github.sagifogel" %% "proptics-profunctor" % "0.4.2",
  "io.github.sagifogel" %% "proptics-macros" % "0.4.2"
)
```

Quick links:

* [Website][website]
* [Guide][guide]

[website]: https://sagifogel.github.io/Proptics/
[guide]: https://sagifogel.github.io/Proptics/docs/overview
[proptics-api]: https://sagifogel.github.io/Proptics/api/proptics/

### Documentation

* There is a Scaladoc API documentation for the [library][proptics-api], which includes optics ([Lens](https://sagifogel.github.io/Proptics/api/proptics/Lens_), [Traversal](https://sagifogel.github.io/Proptics/api/proptics/Traversal_) [Fold](https://sagifogel.github.io/Proptics/api/proptics/Fold_), and more)
  [Profunctors](https://sagifogel.github.io/Proptics/api/proptics/profunctor/), and [Data types](https://sagifogel.github.io/Proptics/api/proptics/internal/) </br>
