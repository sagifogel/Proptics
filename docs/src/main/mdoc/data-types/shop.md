---
id: shop
title: Shop
---

`Shop[A, B, S, T]` is a data type shaped like a `Profunctor`, which characterizes the construction of a <a href="/Proptics/docs/optics/lens" target="_blank">Lens</a> and <a href="/Proptics/docs/an-optics/an-iso" target="_blank">ALens</a>.
`Lens_[S, T, A, B]` and `ALens_[S, T, A, B]` both take two conversion functions as arguments,<br/> `view: S => A` which produces an `A` given an `S`, and `set: S => B => T`, function which takes a structure `S` and a new focus `B` and returns a structure of `T`.
`Shop[A, B, S, T]` also takes these two function, thus making it a data type that embeds the way to construct a `Lens` or `ALens`.

```scala
case class Shop[A, B, S, T](view: S => A, set: S => B => T)
```


While `Lens` and `ALens` construction is the same, their internal encodings is different.


#### Lens

```scala
object Lens_ {
  def apply[S, T, A, B](view: S => A)(set: S => B => T): Lens_[S, T, A, B]
}
```

`Lens_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a <a href="/Proptics/docs/profunctors/strong" target="_blank">Strong</a> of P[_, _].

```scala
abstract class Lens_[S, T, A, B] {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]
}
```

#### ALens

```scala
object ALens_ {
  def apply[S, T, A, B](view: S => A)(set: S => B => T): ALens_[S, T, A, B]
}
```

`ALens_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of `Shop`

```scala
abstract class ALens_[S, T, A, B] {
  private[proptics] def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T]
}
```

In order for `ALens_[S, T, A, B]` to be compatible with `Lens_[S, T, A, B]`, an instance of `Strong` of `Shop` has been
introduced.

<a href="/Proptics/docs/profunctors/strong" target="_blank">Strong[_, _]</a> is a type constructor that takes 2 type parameters. `Shop[A, B, S, T]` is a type that has 4 type parameters, so we need
to fix two of the type parameters of `Shop` in order to create an instance of `Strong` of `Shop`. We can use Scala's type lambda syntax:

```scala
implicit def strongShop[E, F]: Strong[({ type P[S, T] = Shop[E, F, S, T] })#P] =
  new Strong[({ type P[S, T] = Shop[E, F, S, T] })#P] {
    override def first[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (A, C), (B, C)] =
      Shop({ case (a, _) => fa.view(a) }, { case (a, c) => e => (fa.set(a)(e), c) })

    override def second[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (C, A), (C, B)] =
      Shop({ case (_, a) => fa.view(a) }, { case (c, a) => e => (c, fa.set(a)(e)) })

    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      Shop(fab.view compose f, c => g compose fab.set(f(c)))
  }
```

or we can use the <a href="https://github.com/typelevel/kind-projector" target="_blank">kind projector</a> compiler plugin:

```scala
implicit def strongShop[E, F]: Strong[Shop[E, F, *, *]] =
  new Strong[Shop[E, F, *, *]] {
    override def first[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (A, C), (B, C)] =
      Shop({ case (a, _) => fa.view(a) }, { case (a, c) => e => (fa.set(a)(e), c) })

    override def second[A, B, C](fa: Shop[E, F, A, B]): Shop[E, F, (C, A), (C, B)] =
      Shop({ case (_, a) => fa.view(a) }, { case (c, a) => e => (c, fa.set(a)(e)) })

    override def dimap[A, B, C, D](fab: Shop[E, F, A, B])(f: C => A)(g: B => D): Shop[E, F, C, D] =
      Shop(fab.view compose f, c => g compose fab.set(f(c)))
  }
```

`ALens` allows us to export its internal construction logic to a `Shop` using the `toShop` method.

```scala
import proptics.ALens
// import proptics.ALens

val tupleLens: ALens[(Int, String), Int] = ALens[(Int, String), Int](_._1) { 
  case(_, s) => i => (i, s) 
}
// tupleLens: proptics.ALens[(Int, String),Int] = proptics.ALens_$$anon$12@28eb4316

val shop = tupleLens.toShop
//shop: proptics.internal.Shop[Int,Int,(Int, String),(Int, String)] = 
//  Shop(scala.Function1$$Lambda$32794/0x000000080398f840@51b6fb0e,
//       proptics.ALens_$$$Lambda$32795/0x000000080398d840@64eeb60e)

shop.view((9, "Hello"))
// res0: Int = 9

shop.set((1, "Hello"))(9)
// res1: (Int, String) = (9,Hello)
```

We can later on create a new instance of `ALens` or `Lens` from the shop instance

```scala
import proptics.Lens
// import proptics.Lens

import proptics.ALens
// import proptics.ALens

val aLensFromShop: ALens[(Int, String), Int] = ALens[(Int, String), Int](shop.view)(shop.set)
// aLensFromShop: proptics.ALens[(Int, String),Int] = proptics.ALens_$$anon$12@1e797afb

val lensFromShop: Lens[(Int, String), Int] = Lens[(Int, String), Int](shop.view)(shop.set)
// lensFromShop: proptics.Lens[(Int, String),Int] = proptics.Lens_$$anon$11@7f2ed0a1
```




