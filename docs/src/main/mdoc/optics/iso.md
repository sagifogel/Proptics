---
id: iso
title: Iso
---

An `Iso` enables you to transform back and forth between two types without losing information.<br/>
`Iso` is useful when you need to convert between types, a simple example would be, transform a `String` into a `List[Char]` and from `List[Char]` to `String`.

## Iso internal encoding

#### Polymorphic Iso

```scala
Iso_[S, T, A, B]
```

`Iso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Profunctor](/Proptics/docs/profunctors/profunctor) of P[_, _].
 
 ```scala
/**
  * @tparam S the source of an Iso_
  * @tparam T the modified source of an Iso_
  * @tparam A the focus of an Iso_
  * @tparam B the modified focus of a Iso_
  */
abstract class Iso_[S, T, A, B] {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
}
```

`Iso_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of type to the full structure from
`S` to `T`, the same as changing one element of a tuple (focus), would give us a new type (structure) of tuple.</br>
An `Iso` that changes its focus/structure, is called `Polymorphic Iso`.

#### Monomorphic Iso

```scala
Iso[S, A]
```

`Iso[S, A]` is a type alias for `Iso_[S, S, A, A]`,  which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Iso[S, A] = Iso_[S, S, A, A]
```

`Iso[S, A]` means that `S` and `A` are isomorphic – the two types represent the same information.</br>
An `Iso` that does not change its focus/structure, is called `Monomorphic Iso`.

## Constructing Isos

`Iso_[S, T, A, B]` is constructed using the [Iso_[S, T, A, B]#apply](/Proptics/api/proptics/Iso_$.html) function.</br>
For a given `Iso_[S, T, A, B]` it takes two conversion functions as arguments, `view: S => A` which produces an `A` given an `S`, 
and `review: B => T` which produces a `T` given an `B`.

```scala
object Iso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): Iso_[S, T, A, B]
}
```

```scala
import proptics.Iso_
// import proptics.Iso_

import cats.syntax.either._
// import cats.syntax.either._

val swap: Either[Int, String] => Either[String, Int] = _.fold(_.asRight[String], _.asLeft[Int])
// swap: Either[Int,String] => Either[String,Int] = $Lambda$11000/0x0000000802b65040@608aee55

val iso: Iso_[Either[Int, String], Either[String, Int], Either[String, Int], Either[Int, String]] =
  Iso_[Either[Int, String], Either[String, Int], Either[String, Int], Either[Int, String]](swap)(swap)
// iso: proptics.Iso_[Either[Int,String],Either[String,Int],Either[String,Int],Either[Int,String]] =
//   proptics.Iso_$$anon$16@ec762e8
```

`Iso[S, A]` is constructed using the [Iso[S, A]#apply](/Proptics/api/proptics/Iso$.html) function. For a given `Iso[S, A]` it takes two conversion functions as arguments,
`view: S => A` which produces an `A` given an `S`, and `review: A => S` which produces an `S` given an `A`.

```scala
object Iso {
  def apply[S, A](view: S => A)(review: A => S): Iso[S, A]
}
```

```scala
import proptics.Iso
// import proptics.Iso

val isoStringToList = Iso[String, List[Char]](_.toList)(_.mkString)
// isoStringToList: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4b898027  
```

## Common functions of an Iso

#### view
```scala
isoStringToList.view("Proptics") 
// res0: List[Char] = List(P, r, o, p, t, i, c, s)
```

#### review
```scala
isoStringToList.review(chars)
// res1: String = Proptics
```

#### exists
```scala
isoStringToList.exists(_.length === 8)("Proptics")
// res2: Boolean = true
```

#### contains
```scala
isoStringToList.contains(_.contains(80))("Proptics")
// res3: Boolean = true
```

#### find
```scala
isoStringToList.find(_.contains(80))("Proptics")
// res4: Option[List[Char]] = Some(List(P, r, o, p, t, i, c, s))
```

## Laws

An `Iso` must satisfy all [IsoLaws](/Proptics/api/proptics/law/IsoLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>
All laws constructed from the reversibility law, which says that we can completely reverse the transformation.

```scala
import proptics.Iso
// import proptics.Iso

import cats.Eq
// import cats.Eq

import cats.instances.string._
// import cats.instances.string._ 

import cats.syntax.eq._
// import cats.syntax.eq._

val isoStringToList = Iso[String, List[Char]](_.toList)(_.mkString)
// isoStringToList: proptics.Iso[String,List[Char]] = proptics.Iso_$$anon$16@4b898027 
```

#### Source reversibility
```scala
def sourceReversibility[S: Eq, A](iso: Iso[S, A], s: S): Boolean = 
  iso.review(iso.view(s)) === s
// sourceReversibility: [S, A](iso: proptics.Iso[S,A], s: S)(implicit evidence$1: cats.Eq[S])Boolean
 
sourceReversibility(isoStringToList, "Proptics")
 // res0: Boolean = true
```

#### Focus reversibility

```scala
def focusReversibility[S, A: Eq](iso: Iso[S, A], a: A): Boolean = 
  iso.view(iso.review(a)) === a
// focusReversibility: [S, A](iso: proptics.Iso[S,A], a: A)(implicit evidence$1: cats.Eq[A])Boolean

focusReversibility(isoStringToList, "Proptics".toList)
// res1: Boolean = true
```