---
id: quick-example
title: Quick Example
---

Following is an example showing how to update, view, and set values within nested data structures.<br/>
The example shows the use of the `_1` [Lens](/Proptics/docs/optics/lens), which basically get the first element of a tuple,
and its type signature is: 
```scala
 def _1[A, B]: Lens[(A, B), A]
```

In this example we apply the `_1` Lens three times (using  composition) in order to reach the leftmost
element of the tupled instance:
```scala
val tupled: (((String, Int), Int), Int) = ((("hi!", 3), 2), 1)
// tupled: (((String, Int), Int), Int) = (((hi!,3),2),1)
```

```scala
import proptics.Lens
// import proptics.Lens

import proptics.instances.tuple._
// import proptics.instances.tuple._

val tupled: (((String, Int), Int), Int) = ((("hi!", 3), 2), 1)
// tupled: (((String, Int), Int), Int) = (((hi!,3),2),1)

val leftmost = _1[((String, Int), Int), Int] compose _1[(String, Int), Int] compose _1[String, Int]
// leftmost: proptics.Lens[(((String, Int), Int), Int),String] = proptics.Lens_$$anon$2@716c8dae

leftmost.view(((("hi!", 3), 2), 1))
// res0: String = hi!

leftmost.set("Bye!")(tupled)
// res1: (((String, Int), Int), Int) = (((Bye!,3),2),1)

leftmost.over(_.toUpperCase)(tupled)
// res2: (((String, Int), Int), Int) = (((HI!,3),2),1)
```

## In the context of cats.effect.IOApp

```scala
import cats.Show
import cats.effect.{ExitCode, IO, IOApp}
import proptics.Lens
import cats.syntax.show._
import proptics.instances.tuple._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      tupled <- IO.pure(((("hi!", 3), 2), 1))
      _ <- putStrLn(leftmost.view(tupled))
      _ <- putStrLn(leftmost.set("Bye!")(tupled))
      _ <- putStrLn(leftmost.over(_.toUpperCase)(tupled))
    } yield ExitCode.Success

  val leftmost: Lens[(((String, Int), Int), Int), String] =
    _1[((String, Int), Int), Int] compose _1[(String, Int), Int] compose _1[String, Int]

  def putStrLn[A: Show](x: A): IO[Unit] = IO(println(x.show))
}
```
This will print the output:
```
hi!
(((Bye!,3),2),1)
(((HI!,3),2),1)
```
