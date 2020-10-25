---
id: an-optic
title: AnOptic
---

An-optic is similar to a regular optic, but has an internal different encoding than optic.<br/>
`Optic_[S, T, A, B]` takes an argument of `P[A, B]`, and an implicit instance of some kind of `Profunctor[P[_, _]]` and returns `P[S, T]`. 
`AnOptic_[S, T, A, B]` takes a data type shaped like a `Profunctor`, that characterizes the construction of the an-optic, and returns
a new instance of the data type with modified types. `AnOptic_[S, T, A, B]` like `Optic_[S, T, A, B]` is not really used for the encoding 
of all `an-optic(s)` in `Proptics`, and it is only shown for explanation purposes.

For example `Iso_[S, T, A, B]` vs `AnIso_[S, T, A, B]`<br/>
An `Iso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a profunctor.<br/>
In order to construct an `Iso_[S, T, A, B]`, we need to provide two functions `S => A` and `B => T`

```scala
object Iso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): Iso_[S, T, A, B]
}
```

This is the internal representation of an `Iso_[S, T, A, B]`:

```scala
abstract class Iso_[S, T, A, B] extends Serializable {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
}
```

`AnIso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of [Exchange](/Proptics/docs/data-types/exchange).<br/>
The construction mechanism for `AnIso_[S, T, A, B]` is the same construction for `Iso_[S, T, A, B]`, but the functions are encoded within the `Exchange` type.

```scala
import proptics.internal.Exchange
abstract class AnIso_[S, T, A, B] {
  private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]
}
```

```scala
case class Exchange[A, B, S, T](view: S => A, review: B => T)
```

In order for `AnIso_[S, T, A, B]` to be compatible with `Iso_[S, T, A, B]`, an instance of `Profunctor` of `Exchange` has been
introduced. 

`Profunctor[_, _]` is a type constructor that takes 2 type parameters. `Exchange[A, B, S, T]` is a type that has 4 type parameters, so we need
to fix two of the type parameters of `Exchange` in order to create an instance of `Profunctor` of `Exchange`. We can use Scala's type lambda syntax:

```scala
implicit def profunctorExchange[E, F]: Profunctor[({ type P[S, T] = Exchange[E, F, S, T] })#P] =
  new Profunctor[({ type P[S, T] = Exchange[E, F, S, T] })#P] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Exchange[E, F, C, D] =
      Exchange(fab.view compose f, g compose fab.review)
  }
```

or we can use the <a href="https://github.com/typelevel/kind-projector" target="_blank">kind projector</a> compiler plugin:

```scala
implicit def profunctorExchange[E, F]: Profunctor[Exchange[E, F, *, *]] = 
  new Profunctor[Exchange[E, F, *, *]] {
    override def dimap[A, B, C, D](fab: Exchange[E, F, A, B])
                                  (f: C => A)
                                  (g: B => D): Exchange[E, F, C, D] = 
      Exchange(fab.view compose f, g compose fab.review)
}
```

## Why does AnOptic exist?

`Proptics` is inspired by ideas from <a href="https://github.com/purescript-contrib/purescript-profunctor-lenses" target="_blank">purescript-profunctor-lenses</a>.
In `purescript` we cannot put `Optic` directly into a container (e.g. an `Option`), therefore a new optic has been designed to have the ability to export
its representation to a data type. Although it is not the case for Scala, still, passing an optic to a function might seems awkward,
therefore this option has been adopted in `Proptics`.<br/> The data type representing `AnIso_[S, T, A, B]` is the `Exchange[A, B, S, T]`.
We can use the `toExchange` method of `AnIso_[S, T, A, B]` in order to get an `Exchange[A, B, S, T]`

```scala
import proptics.AnIso
// import proptics.AnIso

val anIsoStringToList: AnIso[String, List[Char]] = AnIso[String, List[Char]](_.toList)(_.mkString)
// anIsoStringToList: proptics.AnIso[String,List[Char]] = proptics.AnIso_$$anon$17@74561208

val exchange = anIsoStringToList.toExchange
// exchange: proptics.internal.Exchange[List[Char],List[Char],String,String] = 
//   Exchange(scala.Function1$$Lambda$9364/0x0000000801a34040@419490d4,
//            scala.Function1$$Lambda$9364/0x0000000801a34040@78d86219)

anIsoStringToList.view("Proptics")
// res0: List[Char] = List(P, r, o, p, t, i, c, s)

exchange.review("Proptics".toList)
// res1: String = Proptics
```

We can later on create a new instance of `AnIso` or `Iso` from the exchange instance

```scala
import proptics.Iso
// import proptics.Iso

val anIsoFromExchange: AnIso[String, List[Char]] = 
  AnIso[String, List[Char]](exchange.view)(exchange.review)
// anIsoFromExchange: proptics.AnIso[String,List[Char]] = proptics.AnIso_$$anon$17@bf55e9c

val isoFromExchange: Iso[String, List[Char]] = Iso[String, List[Char]](exchange.view)(exchange.review)
// isoFromExchange: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4c6f5ff7
``` 



