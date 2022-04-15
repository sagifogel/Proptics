---
id: lens
title: Lens
---

A `Lens` is an optic used to focus on a particular element in a deeply nested data structure.

```scala
import proptics.syntax.all._
import proptics.instances.all._

final case class Oscars(bestPicture: Map[Int, String])

val oscarsLens = 
  Oscars(Map(1975 -> "The Godfather: Part II", 2008 -> "No Country for Old Men"))
    .lens(_.bestPicture)

oscarsLens
  .view

// val res0: Map[Int,String] = Map(1975 -> The Godfather: Part II, 2008 -> No Country for Old Men)

oscarsLens
  .set(Map(1972 -> "The Godfather"))

// val res1: Oscars = Oscars(Map(1972 -> The Godfather))
```