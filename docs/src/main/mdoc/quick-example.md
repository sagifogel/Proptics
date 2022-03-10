---
id: quick-example
title: Quick Example
---

Following is an example showing how to update, view, and set values within nested data structures.<br/>
The example shows the use of the `first` [Lens](optics/lens.md), which basically gets the first element of a tuple,
and its type signature is: 
```scala
  def first: Lens[S, A]
```

In this example we apply the `first` [Lens](optics/lens.md) three times in order to reach the leftmost
element of the tupled instance:
```scala
val tupled: (((String, Int), Int), Int) = ((("hi!", 3), 2), 1)
// tupled: (((String, Int), Int), Int) = (((hi!,3),2),1)
```

```scala
import proptics.instances.all._
// import proptics.instances.all._

import proptics.syntax.all._
// import proptics.syntax.all._

val leftmost = ((("hi!", 3), 2), 1).first.first.first
// val leftmost: proptics.AppliedLens[(((String, Int), Int), Int),String]

leftmost.view
// res0: String = hi!

leftmost.set("Bye!")
// res1: (((String, Int), Int), Int) = (((Bye!,3),2),1)

leftmost.over(_.toUpperCase)
// res2: (((String, Int), Int), Int) = (((HI!,3),2),1)
```