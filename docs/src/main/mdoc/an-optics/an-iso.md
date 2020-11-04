---
id: an-iso
title: AnIso
---

`AnIso` is similar to <a href="/Proptics/docs/optics/iso" target="_blank">Iso</a>, but has different internal encodings, it enables 
you to transform back and forth between two types without losing information.</br>
`AnIso` is useful when you need to convert between types, a simple example would be, transform a `String` into a `List[Char]`.

## AnIso internal encoding

#### Polymorphic AnIso

```scala
AnIso_[S, T, A, B]
```

`AnIso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of [Exchange](/Proptics/docs/data-types/exchange), thus making 
it a function `Exchange[A, B, A, B] => Exchange[A, B, S, T]`.

```scala
/**
  * @tparam S the source of a AnIso_
  * @tparam T the modified source of an AnIso_
  * @tparam A the focus of an AnIso_
  * @tparam B the modified focus of an AnIso_
  */
abstract class AnIso_[S, T, A, B] {
  private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]
}
```

`AnIso_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of type to the full structure from
`S` to `T`.</br>
`AnIso` that changes its focus/structure, is called `Polymorphic AnIso`.

#### Monomorphic Iso

```scala
AnIso[S, A]
```

`AnIso[S, A]` is a type alias for `AnIso_[S, S, A, A]`,  which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type AnIso[S, A] = AnIso_[S, S, A, A]
``` 

`AnIso[S, A]` means that `S` and `A` are isomorphic – the two types represent the same information.</br>
`AnIso` that does not change its focus/structure, is called `Monomorphic AnIso`.

## Constructing AnIsos

`AnIso_[S, T, A, B]` is constructed using the [AnIso_[S, T, A, B]#apply](/Proptics/api/proptics/AnIso_$.html) function.</br>
For a given `AnIso_[S, T, A, B]` it takes two conversion functions as arguments, `view: S => A` which produces an `A` given an `S`, 
and `review: B => T` which produces a `T` given an `B`.

```scala
object AnIso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): AnIso_[S, T, A, B]
}
```

`AnIso[S, A]` is constructed using the [AnIso[S, A]#apply](/Proptics/api/proptics/AnIso$.html) function. For a given `AnIso[S, A]` it takes two conversion functions as arguments,
`view: S => A` which produces an `A` given an `S`, and `review: A => S` which produces an `S` given an `A`.

```scala
object AnIso {
  def apply[S, A](view: S => A)(review: A => S): AnIso[S, A]
}
```

```scala
import proptics.AnIso
// import proptics.AnIso

val anIsoStringToList = AnIso[String, List[Char]](_.toList)(_.mkString)
// anIsoStringToList: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4b898027  
```

## Common functions of an AnIso

#### view
```scala
anIsoStringToList.view("Proptics") 
// res0: List[Char] = List(P, r, o, p, t, i, c, s)
```

#### review
```scala
anIsoStringToList.review(chars)
// res1: String = Proptics
```

#### exists
```scala
anIsoStringToList.exists(_.length === 8)("Proptics")
// res2: Boolean = true
```

#### contains
```scala
anIsoStringToList.contains(_.contains(80))("Proptics")
// res3: Boolean = true
```

#### find
```scala
anIsoStringToList.find(_.contains(80))("Proptics")
// res4: Option[List[Char]] = Some(List(P, r, o, p, t, i, c, s))
```

## Exporting Exchange as data type of AnIso

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

## Laws

`AnIso` must satisfy all [AnIsoLaws](/Proptics/api/proptics/law/AnIsoLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>
All laws constructed from the reversibility law, which says that we can completely reverse the transformation.

```scala
import proptics.AnIso
// import proptics.AnIso

import cats.Eq
// import cats.Eq

import cats.instances.string._
// import cats.instances.string._ 

import cats.syntax.eq._
// import cats.syntax.eq._

val anIsoStringToList = AnIso[String, List[Char]](_.toList)(_.mkString)
// anIsoStringToList: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4b898027  
```

#### Source reversibility
```scala
def sourceReversibility[S: Eq, A](anIso: AnIso[S, A], s: S): Boolean = 
  anIso.review(iso.view(s)) === s
// sourceReversibility: [S, A](anIso: AnIso[S,A], s: S)(implicit evidence$1: cats.Eq[S])Boolean
 
sourceReversibility(anIsoStringToList, "Proptics")
 // res0: Boolean = true
```

#### Focus reversibility

```scala
def focusReversibility[S, A: Eq](anIso: AnIso[S, A], a: A): Boolean = 
  anIso.view(iso.review(a)) === a
// focusReversibility: [S, A](anIso: AnIso[S,A], a: A)(implicit evidence$1: cats.Eq[A])Boolean

focusReversibility(anIsoStringToList, "Proptics".toList)
// res1: Boolean = true
```
