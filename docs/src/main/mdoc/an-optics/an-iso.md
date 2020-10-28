---
id: an-iso
title: AnIso
---

`AnIso` is a similar to <a href="/Proptics/docs/optics/iso" target="_blank">Iso</a>, but has different internal encodings, it enables 
you to transform back and forth between two types without losing information. `AnIso[S, A]` means that `S` and `A` are isomorphic – the two types 
represent the same information. `AnIso` is useful when you need to convert between types, a simple example would be, transform a `String` into a `List[Char]`.

## Constructing AnIsos

`AnIso` is constructed using the [AnIso[S, A]#apply](/Proptics/api/proptics/AnIso$.html) function. For a given `AnIso[S, A]` it takes two conversion functions as arguments,
`view` which produces an `A` given an `S`, and `review` which produces an `S` given an `A`.

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

## AnIso internal encoding

`AnIso[S, A]` is the monomorphic short notation version (does not change the type of the structure) of the polymorphic one `AnIso_[S, T, A, B]`

```scala
type AnIso[S, A] = AnIso_[S, S, A, A]
``` 

`AnIso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of [Exchange](/Proptics/docs/data-types/exchange), thus making 
it a function `Exchange[A, B, A, B] => Exchange[A, B, S, T]`.

```scala
abstract class AnIso_[S, T, A, B] { self =>
  private[proptics] def apply(exchange: Exchange[A, B, A, B]): Exchange[A, B, S, T]
}
```

## Exporting AnIso as data type of Exchange

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
