---
id: stall
title: Stall
---

`Stall[A, B, S, T]` is a data type shaped like a `Profunctor`, which characterizes the construction of an <a href="/Proptics/docs/optics/affine-traversal" target="_blank">AffineTraversal</a> and <a href="/Proptics/docs/an-optics/an-affine-traversal" target="_blank">AnAffineTraversal</a>.</br>
`AffineTraversal_[S, T, A, B]` and `AnAffineTraversal_[S, T, A, B]` both take two functions as arguments, `viewOrModify: S => Either[T, A]`, which is a matching function that produces an `Either[T, A]` given an `S`,
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns a structure of `T`.
`Stall[A, B, S, T]` also takes these two function, thus making it a data type that embeds the way to construct an `AffineTraversal` or an `AffineTraversal`.

```scala
case class Stall[A, B, S, T](viewOrModify: S => Either[T, A], set: S => B => T)
```

While `AffineTraversal` and `AnAffineTraversal` construction is the same, their internal encodings is different.


#### AffineTraversal

```scala
object AffineTraversal_ {
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(set: S => B => T): AffineTraversal_[S, T, A, B]
}
```

`AffineTraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a <a href="/Proptics/docs/profunctors/choice" target="_blank">Choice</a>, and
 <a href="/Proptics/docs/profunctors/strong" target="_blank">Strong</a> of P[_, _].

```scala
abstract class AffineTraversal_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]
}
```

#### AnAffineTraversal

```scala
object AnAffineTraversal_ {
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(set: S => B => T): AnAffineTraversal_[S, T, A, B]
}
```

`AnAffineTraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data </br> type of `Stall`

```scala
abstract class AnAffineTraversal_[S, T, A, B] {
  def apply(pab: Stall[A, B, A, B]): Stall[A, B, S, T]
}
```

In order for `AnAffineTraversal_[S, T, A, B]` to be compatible with `AffineTraversal_[S, T, A, B]`, an instance of `Strong` of `Stall` and an instance of `Choice` of `Stall` were
introduced.

<a href="/Proptics/docs/profunctors/strong" target="_blank">Strong</a> and <a href="/Proptics/docs/profunctors/choice" target="_blank">Choice</a> both are type constructors that takes 2 type parameters. `Stall[A, B, S, T]` is a type that has 4 type parameters, so we need
to fix two of the type parameters of `Stall` in order to create an instance of `Strong` of `Stall` and `Choice` of `Stall`.

```scala
implicit def strongStall[E, F]: Strong[({ type P[S, T] = Stall[E, F, S, T] })#P] =
  new Strong[({ type P[S, T] = Stall[E, F, S, T] })#P] {
    override def first[A, B, C](fa: Stall[E, F, A, B]): Stall[E, F, (A, C), (B, C)] =
      Stall(
        { case (a, c) => fa.viewOrModify(a).leftMap((_, c)) },
        { case (a, c) =>
          f => (fa.set(a)(f), c)
        })
        
    override def second[A, B, C](fa: Stall[E, F, A, B]): Stall[E, F, (C, A), (C, B)] =
      Stall(
        { case (c, a) => fa.viewOrModify(a).leftMap((c, _)) },
        { case (c, a) => 
          f => (c, fa.set(a)(f))
        })
            
    override def dimap[A, B, C, D](fab: Stall[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Stall[E, F, C, D] =
      Stall(c => fab.viewOrModify(f(c)).leftMap(g), c => ff => g(fab.set(f(c))(ff)))
  }
```

```scala
implicit def choiceStall[E, F]: Choice[({ type P[S, T] = Stall[E, F, S, T] })#P] =
  new Choice[({ type P[S, T] = Stall[E, F, S, T] })#P] {
    override def left[A, B, C](pab: Stall[E, F, A, B]): Stall[E, F, Either[A, C], Either[B, C]] =
      Stall(
        {
          case Left(a)  => pab.viewOrModify(a).fold(_.asLeft[C].asLeft[E], _.asRight[Either[B, C]])
          case Right(c) => c.asRight[B].asLeft[E]
        },
        either => f => either.leftMap(pab.set(_)(f))
      )
    
    override def right[A, B, C](pab: Stall[E, F, A, B]): Stall[E, F, Either[C, A], Either[C, B]] =
      Stall(
        {
          case Left(c)  => c.asLeft[B].asLeft[E]
          case Right(a) => pab.viewOrModify(a).fold(_.asRight[C].asLeft[E], _.asRight[Either[C, B]])
        },
        either => f => either.map(pab.set(_)(f))
      )
    
    override def dimap[A, B, C, D](fab: Stall[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Stall[E, F, C, D] =
      Stall(c => fab.viewOrModify(f(c)).leftMap(g), c => ff => g(fab.set(f(c))(ff)))
  }
```

`AnAffineTraversal` allows us to export its internal construction logic to a `Stall` using the `toStall` method.

```scala
import proptics.AnAffineTraversal
// import proptics.AnAffineTraversal

sealed trait Json
// defined trait Json

case object JNull extends Json
// defined object JNull

case class JNumber(value: Double) extends Json
// defined class JNumber

val jsonAffineTraversal =
  AnAffineTraversal.fromPartial[Json, Double] { case JNumber(i) => i } { json => i =>
    json match {
      case JNumber(_) => JNumber(i)
      case _          => json
    }
  }
// jsonAffineTraversal: AnAffineTraversal[Json,Double] = AnAffineTraversal_$$anon$6@27ce826e

val stall = jsonAffineTraversal.toStall
//  stall: proptics.internal.Stall[Double,Double,Json,Json] = 
//    Stall(proptics.AnAffineTraversal_$$$Lambda$6037/0x0000000801cb3040@1adf1c6a,
//          proptics.AnAffineTraversal_$$$Lambda$6038/0x0000000801cb3840@1a9cda87)

stall.viewOrModify(JNumber(9))
// res0: Either[Json,Double] = Right(9.0)

stall.set(JNumber(1))(9)
// res1: Json = JNumber(9.0)
```

We can later on create a new instance of an `AffineTraversal` or an `AnAffineTraversal` from the stall instance

```scala
import proptics.AffineTraversal
// import proptics.AffineTraversal

import proptics.AnAffineTraversal
// import proptics.AnAffineTraversal

val anAffineTraversalFromStall: AnAffineTraversal[Json, Double] = 
  AnAffineTraversal[Json, Double](stall.viewOrModify)(stall.set)
// anAffineTraversalFromStall: proptics.AnAffineTraversal[Json,Double] = 
//   proptics.AnAffineTraversal_$$anon$6@77d28f9

val affineTraversalFromStall: AffineTraversal[Json, Double] = 
  AffineTraversal[Json, Double](stall.viewOrModify)(stall.set)
// affineTraversalFromStall: proptics.AffineTraversal[Json,Double] = 
//   proptics.AffineTraversal_$$anon$10@7995e246
```

