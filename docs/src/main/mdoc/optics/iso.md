---
id: iso
title: Iso
---

An `Iso` enables you to transform back and forth between two types without losing information.<br/>
`Iso` is useful when you need to convert between types, a simple example would be, transform a `String` into a `List[Char]` and from `List[Char]` to `String`.

## Constructing an Iso

`Iso[S, A]` is constructed using the <a href="../../api/proptics/Iso$">Iso[S, A]#apply</a> function. For a given `Iso[S, A]` it takes two conversion functions as arguments,
`view: S => A` which produces an `A` given an `S`, and `review: A => S` which produces an `S` given an `A`.

```scala
object Iso {
  def apply[S, A](view: S => A)(review: A => S): Iso[S, A]
}
```

```scala
import proptics.Iso

val isoStringToList = Iso[String, List[Char]](_.toList)(_.mkString) 
```

## Constructing a polymorphic Iso

`Iso_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Iso_$">Iso_[S, T, A, B]#apply</a> function.</br>
For a given `Iso_[S, T, A, B]` it takes two conversion functions as arguments, `view: S => A` which produces an `A` given an `S`,
and `review: B => T` which produces a `T` given an `B`.

```scala
object Iso_ {
  def apply[S, T, A, B](view: S => A)(review: B => T): Iso_[S, T, A, B]
}
```

```scala
import proptics.Iso_

val swap: Either[Int, String] => Either[String, Int] = _.swap

val iso: Iso_[Either[Int, String], Either[String, Int], Either[String, Int], Either[Int, String]] =
  Iso_[Either[Int, String], Either[String, Int], Either[String, Int], Either[Int, String]](swap)(swap)
```

## Methods

#### [view](../../api/proptics/Iso_.html#view(s:S):A)
```scala
/** view the focus of an Iso */
def view(s: S): A
```

```scala
isoStringToList.view("Proptics") 
// val res0: List[Char] = List(P, r, o, p, t, i, c, s)
```

#### [review](../../api/proptics/Iso_.html#review(b:B):T)

```scala
/** view the source of an Iso (construct an S from an A) */
def review(a: A): S
```

```scala

isoStringToList.review()
// val res1: String = Proptics
```

#### [set](../../api/proptics/Iso_.html#set(b:B):S=>T)
```scala
/** set the focus of an Iso */
def set(a: A): S => S
```

```scala
val source: String = "Profunctor"

isoStringToList.set(List('P', 'r', 'o', 'p', 't', 'i', 'c', 's'))(source)
// val res2: String = Proptics
```

#### [over](../../api/proptics/Iso_.html#over(f:A=>B):S=>T)
```scala
/** modify the focus of an Iso using a function */
def over(f: A => A): S => S
```

```scala
isoStringToList.over(_.map(_.toUpper))("Proptics")
// val res3: String = PROPTICS
```

#### [traverse](../../api/proptics/Iso_.html#traverse[F[_]](s:S)(f:A=>F[B])(implicitev:cats.Applicative[F]):F[T])
```scala
/** modify the focus of an Iso using a Functor */
def traverse[F[_]](s: S)(f: A => F[A])(implicit arg0: Applicative[F]): F[S]
```

```scala
def isProSuffix(chars: List[Char]): Option[List[Char]] =
  Option.when(chars.startsWith("Prop"))(chars)

isoStringToList.traverse("Proptics")(isProSuffix)
// val res4: Option[String] = Some(Proptics)

isoStringToList.traverse("Profunctor")(isProSuffix)
// val res5: Option[String] = None
```

#### [overF](../../api/proptics/Iso_.html#overF[F[_]](f:A=>F[B])(s:S)(implicitevidence$2:cats.Applicative[F]):F[T])
```scala
/** synonym for traverse, flipped */
def overF[F[_]](f: A => F[B])(s: S)(implicit arg0: Applicative[F]): F[T]
```

```scala
def isProSuffix(chars: List[Char]): Option[List[Char]] =
  Option.when(chars.startsWith("Prop"))(chars)
  
val partialLens = isoStringToList.overF(isProSuffix) _

partialLens("Proptics")
// val res6: : Option[String] = Some(Proptics)

partialLens("Profunctor")
// val res7: Option[User] = None
```

#### [exists](../../api/proptics/Iso_.html#exists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate holds for the focus of an Iso */
def exists(f: A => Boolean): S => Boolean
```

```scala
isoStringToList.exists(_.length === 8)("Proptics")
// val res8: Boolean = true
```

#### [notExists](../../api/proptics/Iso_.html#notExists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate does not hold for the focus of an Iso */
def notExists(f: A => Boolean): S => Boolean
```

```scala
isoStringToList.notExists(_.startsWith("Prop"))("Proptics")
// val res9: Boolean = false
```

#### [contains](../../api/proptics/Iso_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of an Iso contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
val chars = List('P', 'r', 'o', 'p', 't', 'i', 'c', 's')

isoStringToList.contains(chars)("Proptics")
// val res10: Boolean = true
```

#### [notContains](../../api/proptics/Iso_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of an Iso does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
val chars = List('P', 'r', 'o', 'p', 't', 'i', 'c', 's')

isoStringToList.notContains(chars)("Profunctor")
// val res11: Boolean = true
```

#### [find](../../api/proptics/Iso_.html#find(f:A=>Boolean):S=>Option[A])
```scala
/** find the focus of an Iso that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
isoStringToList.find(_.contains('P'))("Proptics")
// val res12: Option[List[Char]] = Some(List(P, r, o, p, t, i, c, s))
```

#### [cotraverse](../../api/proptics/Iso_.html#cotraverse[F[_]](fs:F[S])(f:F[A]=>B)(implicitev:cats.Applicative[F]):T)
```scala
/** modify an effectual focus of an Iso into the modified focus */
def cotraverse[F[_]](fs: F[S])(f: F[A] => A)(implicit arg0: Comonad[F]): S
```

```scala
import cats.Id
val functorChars = List('f', 'u', 'n', 'c', 't', 'o', 'r')

isoStringToList.cotraverse(Id("Proptics"))(_.take(3) ++ functorChars)
// val res13: String = Profunctor
```

#### [zipWithF](../../api/proptics/Iso_.html#zipWithF[F[_]](f:F[A]=>B)(fs:F[S])(implicitevidence$1:cats.Applicative[F]):T)
```scala
/** synonym for [[cotraverse]], flipped */
def zipWithF[F[_]](fs: F[S])(f: F[A] => A)(implicit arg0: Comonad[F]): S
```

```scala
import cats.Id
val functorChars = List('f', 'u', 'n', 'c', 't', 'o', 'r')

isoStringToList.zipWithF[Id](_.take(3) ++ functorChars)("Proptics")
// val res14: String = Profunctor
```

#### <a href="../../api/proptics/Iso_.html#zipWith(s1:S,s2:S)(f:(A,A)=>B):T">zipWith</a>
```scala
/** zip two sources of an Iso together provided a binary operation */
def zipWith(s1: S, s2: S)(f: (A, A) => A): S
```

```scala
isoStringToList.zipWith("Pro", "ptics")(_ ++ _)
// val res15: String = Proptics
```

#### [use](../../api/proptics/Iso_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** view the focus of an Iso in the state of a monad */
def use(implicit ev: State[S, A]): State[S, A]
```

```scala
implicit val state: State[String, List[Char]] = State.set("Proptics").inspect(_.toList)

isoStringToList.use.run("Profunctor").value
// val res16: (String, List[Char]) = (Proptics,List(P, r, o, p, t, i, c, s))
```

## Iso internal encoding

#### Polymorphic Iso

```scala
Iso_[S, T, A, B]
```

`Iso_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Profunctor](../profunctors/profunctor.md) of P[_, _].
 
 ```scala
/**
  * @tparam S the source of an Iso_
  * @tparam T the modified source of an Iso_
  * @tparam A the focus of an Iso_
  * @tparam B the modified focus of a Iso_
  */
abstract class Iso_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
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

## Laws

An `Iso` must satisfy all <a href="../../api/proptics/law/IsoLaws">IsoLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>
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

sourceReversibility(isoStringToList, "Proptics")
 // val res0: Boolean = true
```

#### Focus reversibility

```scala
def focusReversibility[S, A: Eq](iso: Iso[S, A], a: A): Boolean = 
  iso.view(iso.review(a)) === a

focusReversibility(isoStringToList, "Proptics".toList)
// val res1: Boolean = true
```