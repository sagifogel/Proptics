---
id: exchange
title: Exchange
---

`Exchange[A, B, S, T]` is a data type shaped like a `Profunctor`, which characterizes the construction of an <a href="/Proptics/docs/optics/iso" target="_blank">Iso</a> and <a href="/Proptics/docs/an-optics/an-iso" target="_blank">AnIso</a>.
`Iso_[S, T, A, B]` and `AnIso_[S, T, A, B]` both take two conversion functions as arguments,<br/> `view: S => A` which produces an `A` given an `S`, and `review: B => T` which produces a `T` given a `B`.</br>
`Exchange[A, B, S, T]` also takes these two function, thus making it a data type that embeds the way to construct an `Iso` or `AnIso`.

```scala
case class Exchange[A, B, S, T](view: S => A, review: B => T)
```


While `Iso` and `AnIso` construction is the same, their internal encodings is different.


#### Iso

```scala
object Iso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): Iso_[S, T, A, B]
}
```

`Iso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a <a href="/Proptics/docs/profunctors/profunctor" target="_blank">Profunctor</a> of P[_, _].

```scala
abstract class Iso_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
}
```

#### AnIso

```scala
object AnIso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): AnIso_[S, T, A, B]
}
```

`AnIso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` where's the `P[_, _]` is a data type of `Exchange`

```scala
abstract class AnIso_[S, T, A, B] {
  def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]
}
```

In order for `AnIso_[S, T, A, B]` to be compatible with `Iso_[S, T, A, B]`, an instance of `Profunctor` of `Exchange` has been
introduced.

<a href="/Proptics/docs/profunctors/profunctor" target="_blank">Profunctor[_, _]</a> is a type constructor that takes 2 type parameters. `Exchange[A, B, S, T]` is a type that has 4 type parameters, so we need
to fix two of the type parameters of `Exchange` in order to create an instance of `Profunctor` of `Exchange`.

```scala
implicit def profunctorExchange[E, F]: Profunctor[({ type P[S, T] = Exchange[E, F, S, T] })#P] =
  new Profunctor[({ type P[S, T] = Exchange[E, F, S, T] })#P] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Exchange[E, F, C, D] =
      Exchange(fab.view compose f, g compose fab.review)
  }
```

`AnIso` allows us to export its internal construction logic to an `Exchange` using the `toExchange` method.

```scala
import proptics.AnIso
// import proptics.AnIso

val anIsoStringToList: AnIso[String, List[Char]] = AnIso[String, List[Char]](_.toList)(_.mkString)
// anIsoStringToList: proptics.AnIso[String,List[Char]] = proptics.AnIso_$$anon$17@74561208

val exchange = anIsoStringToList.toExchange
// exchange: proptics.internal.Exchange[List[Char],List[Char],String,String] = 
//   Exchange(scala.Function1$$Lambda$9364/0x0000000801a34040@419490d4,
//            scala.Function1$$Lambda$9364/0x0000000801a34040@78d86219)

exchange.view("Proptics")
// res0: List[Char] = List(P, r, o, p, t, i, c, s)

exchange.review("Proptics".toList)
// res1: String = Proptics
```

We can later on create a new instance of `AnIso` or `Iso` from the exchange instance

```scala
import proptics.Iso
// import proptics.Iso

import proptics.AnIso
// import proptics.AnIso

val anIsoFromExchange: AnIso[String, List[Char]] = 
  AnIso[String, List[Char]](exchange.view)(exchange.review)
// anIsoFromExchange: proptics.AnIso[String,List[Char]] = proptics.AnIso_$$anon$17@bf55e9c

val isoFromExchange: Iso[String, List[Char]] = Iso[String, List[Char]](exchange.view)(exchange.review)
// isoFromExchange: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4c6f5ff7
```

