---
id: quick-example
title: Quick Example
---

Following is an example showing how to update, view, and set values within nested data structures.<br/>
In this example we apply the `first` [Lens](optics/lens.md) three times in order to reach the leftmost
element of the tupled instance:


```scala
import proptics.instances.all._
import proptics.syntax.all._

val leftmost = ((("hi!", 3), 2), 1).first.first.first

leftmost.view
// val res0: String = hi!

leftmost.set("Bye!")
// val res1: (((String, Int), Int), Int) = (((Bye!,3),2),1)

leftmost.over(_.toUpperCase)
// val res2: (((String, Int), Int), Int) = (((HI!,3),2),1)
```