---
id: market
title: Market
---

`Market[A, B, S, T]` is a data type shaped like a `Profunctor`, which characterizes the construction of a <a href="/Proptics/docs/optics/prism" target="_blank">Prism</a> and <a href="/Proptics/docs/an-optics/a-prism" target="_blank">APrism</a>.
`Prism_[S, T, A, B]` and `APrism_[S, T, A, B]` both take two functions as arguments,<br/> `viewOrModify: S => Either[T, A]`, which is a matching function that produces an `Either[T, A]` given an `S`, and `review: B => T ` function which takes a focus of `B` and returns a structure of `T`.</br>
`Market[A, B, S, T]` also takes these two function, thus making it a data type that embeds the way to construct a `Prism` or an `APrism`.

```scala
case class Market[A, B, S, T](viewOrModify: S => Either[T, A], review: B => T)
```

While `Prism` and `APrism` construction is the same, their internal encodings is different.


#### Prism

```scala
object Prism_ {
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(review: B => T): Prism_[S, T, A, B]
}
```

`Prism_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a <a href="/Proptics/docs/profunctors/choice" target="_blank">Choice</a> of P[_, _].

```scala
abstract class Prism_[S, T, A, B] {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]
}
```

#### APrism

```scala
object APrism_ {
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(review: B => T): APrism_[S, T, A, B]
}
```

`APrism_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of `Market`

```scala
abstract class APrism_[S, T, A, B] {
  private[proptics] def apply(market: Market[A, B, A, B]): Market[A, B, S, T]
}
```

In order for `APrism_[S, T, A, B]` to be compatible with `Prism_[S, T, A, B]`, an instance of `Profunctor` of `Market` has been
introduced.

<a href="/Proptics/docs/profunctors/profunctor" target="_blank">Profunctor[_, _]</a> is a type constructor that takes 2 type parameters. `Market[A, B, S, T]` is a type that has 4 type parameters, so we need
to fix two of the type parameters of `Market` in order to create an instance of `Profunctor` of `Market`. We can use Scala's type lambda syntax:

```scala
implicit def profunctorMarket[E, F]: Profunctor[({ type P[S, T] = Market[E, F, S, T] })#P] =
  new Profunctor[({ type P[S, T] = Market[E, F, S, T] })#P] {
    override def dimap[A, B, C, D](fab: Market[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Market[E, F, C, D] =
      Market(c => fab.viewOrModify(f(c)).leftMap(g), g compose fab.review)
  }
```

or we can use the <a href="https://github.com/typelevel/kind-projector" target="_blank">kind projector</a> compiler plugin:

```scala
implicit def profunctorMarket[E, F]: Profunctor[Market[E, F, *, *]] = 
  new Profunctor[Market[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Market[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Market[E, F, C, D] =
      Market(c => fab.viewOrModify(f(c)).leftMap(g), g compose fab.review)
  }
```

`APrism` allows us to export its internal construction logic to a `Market` using the `toMarket` method.

```scala
import proptics.APrism
// import proptics.APrism

sealed trait Json
// defined trait Json

case object JNull extends Json
// defined object JNull

case class JString(value: String) extends Json
// defined class JString

val jsonPrism = APrism.fromPartial[Json, Int] { case JNumber(i) => i }(JNumber)
// jsonPrism: proptics.APrism[Json,Int] = proptics.APrism_$$anon$14@46d23947

val market = jsonPrism.toMarket
// market: proptics.internal.Market[Int,Int,Json,Json] = 
//   Market(proptics.APrism$$$Lambda$10055/0x0000000801e21040@42cd49b6,JNumber)

jsonPrism.viewOrModify(JNumber(9))
// res0: Either[Json,Int] = Right(9)

jsonPrism.review(9)
// res1: Json = JNumber(9)
```

We can later on create a new instance of an `APrism` or a `Prism` from the Market instance

```scala
import proptics.Prsim
// import proptics.Prsim

import proptics.APrsim
// import proptics.APrsim

val aPrismFromMarket: APrism[Json, Int] = APrism[Json, Int](market.viewOrModify)(market.review)
// aPrismFromMarket: proptics.APrism[Json,Int] = proptics.APrism_$$anon$14@4d448e44

val prismFormMarket: Prism[Json, Int] = Prism[Json, Int](market.viewOrModify)(market.review)
// prismFormMarket: proptics.Prism[Json,Int] = proptics.Prism_$$anon$13@31871fea
```