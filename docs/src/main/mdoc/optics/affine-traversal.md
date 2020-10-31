---
id: affine-traversal
title: Affine Traversal
---

An `AffineTraversal` is a <a href="/Proptics/docs/optics/traversal" target="_blank">Traversal</a> that may contain zero or one element.
It is a combination of a <a href="/Proptics/docs/optics/lens" target="_blank">Lens</a> and a <a href="/Proptics/docs/optics/prism" target="_blank">Prism</a>.<br/>

## AffineTraversal internal encoding

#### Polymorphic AffineTraversal 

```scala
AffineTraversal_[S, T, A, B]
```

`AffineTraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Choice](/Proptics/docs/profunctors/choice) and
a [Strong](/Proptics/docs/profunctors/strong) of  P[_, _].

```scala
/**
  * @tparam S the source of an AffineTraversal_
  * @tparam T the modified source of an AffineTraversal_
  * @tparam A the focus of an AffineTraversal_
  * @tparam B the modified focus of an AffineTraversal_
  */
abstract class AffineTraversal_[S, T, A, B] extends Serializable {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]
}
```

`AffineTraversal_[S, T, A, B]` changes its foci from `A` to `B`, resulting in a change of structure from </br> `S` to `T`.<br/>
 An `AffineTraversal` that changes its foci/structure, is called `Polymorphic AffineTraversal`.
 
#### Monomorphic Traversal
 
`AffineTraversal[S, A]` is a type alias for `AffineTraversal_[S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.

```scala
type AffineTraversal[S, A] = AffineTraversal_[S, S, A, A]
``` 

An `AffineTraversal[S, A]` means that the type `S` might contain zero or one value of type `A`. <br/>
An `AffineTraversal` that does not change its foci/structure, is called `Monomorphic AffineTraversal`.

## Constructing AffineTraversals

`AffineTraversal_[S, T, A, B]` is constructed using the [AffineTraversal_[S, T, A, B]#apply](/Proptics/api/proptics/AffineTraversal_$.html) function.<br/>
For a given `AffineTraversal_[S, T, A, B]` it takes two functions as arguments, </br> `viewOrModify: S => Either[T, A]` which is a matching function that produces an `Either[T, A]` given an `S`,
and `set: S => B => T` function which takes a structure `S` and a focus `B` and returns a structure of `T`.

```scala
object AffineTraversal_ {
  def apply[S, A](viewOrModify: S => Either[T, A])(set: S => B => T): AffineTraversal_[S, T, A, B]
}
```

`AffineTraversal[S, A]` is constructed using the [AffineTraversal[S, A]#apply](/Proptics/api/proptics/AffineTraversal$.html) function.<br/>
For a given `AffineTraversal_[S, T, A, B]` it takes two functions as arguments,</br> `viewOrModify: S => Either[S, A]` which is a matching function that produces an `Either[S, A]` given </br> an `S`,
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a </br> new structure of `S`.

```scala
object AffineTraversal {
  def apply[S, A](viewOrModify: S => Either[S, A])(set: S => A => S): AffineTraversal[S, A]
}
```

Consider a Json ADT/Sum type

```scala
sealed trait Json
// defined trait Json

case object JNull extends Json
// defined object JNull

case class JString(value: String) extends Json
// defined class JString

case class JNumber(value: Double) extends Json
// defined class JNumber
```

We can define an `AffineTraversal` which focuses on `JNumber`

```scala
import proptics.AffineTraversal
// import proptics.AffineTraversal

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import cats.syntax.either._
// import cats.syntax.either._

def viewOrModify(json: Json): Either[Json, Double] = json match {
  case JNumber(value) => value.asRight[Json]
  case json           => json.asLeft[Double]
}
// viewOrModify: (json: Json)Either[Json,Double]

def setJson(json: Json, i: Double): Json = json match {
  case JNumber(_) => JNumber(i)
  case _              => json
}
// setJson: (json: Json, i: Double)Json

val jsonAffineTraversal: AffineTraversal[Json, Double] =
  AffineTraversal[Json, Double](viewOrModify)(setJson _ curried)
// jsonAffineTraversal: AffineTraversal[Json,Double] = AffineTraversal_$$anon$10@210c3b6
```

A more concise version would be using the `fromPreview` method

```scala
object AffineTraversal {
  def fromPreview[S, A](preview: S => Option[A])(set: S => A => S): AffineTraversal[S, A]
}
```

```scala
import proptics.AffineTraversal
// import proptics.AffineTraversal

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import cats.syntax.option._
// import cats.syntax.option._

def preview(json: Json): Option[Double] = json match {
  case JNumber(_) => value.some
  case _              => None
}
// preview: (json: Json)Option[Double]

def setJson(json: Json, i: Double): Json = json match {
  case JNumber(_) => JNumber(i)
  case _              => json
}
// setJson: (json: Json, i: Double)Json

val jsonAffineTraversal: AffineTraversal[Json, Double] =
  AffineTraversal.fromPreview[Json, Double](preview)(setJson _ curried)
// jsonAffineTraversal: AffineTraversal[Json,Double] = AffineTraversal_$$anon$10@2662603b
```

An even more concise version would be using the `fromPartial` method

```scala
object AffineTraversal {
  def fromPartial[S, A](preview: PartialFunction[S, A])(set: S => A => S): AffineTraversal[S, A]
}
```

```scala
import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.AffineTraversal
// import proptics.AffineTraversal

def setJson(json: Json, i: Double): Json = json match {
  case JNumber(_) => JNumber(i)
  case _              => json
}
// setJson: (json: Json, i: Double)Json

val jsonAffineTraversal: AffineTraversal[Json, Double] =
  AffineTraversal.fromPartial[Json, Double] { case JNumber(value) => value }(setJson _ curried)
// jsonAffineTraversal: AffineTraversal[Json,Double] = AffineTraversal_$$anon$10@13a4ef3c
```

## Common functions of an AffineTraversal

#### viewOrModify
```scala
jsonAffineTraversal.viewOrModify(JNumber(9))
// res0: Either[Json,Double] = Right(9.0)

jsonAffineTraversal.viewOrModify(JNull())
// res1: Either[Json,Double] = Left(JNull())
```

#### preview
```scala
jsonAffineTraversal.preview(JNumber(9))
// res2: Option[Double] = Some(9.0)

jsonAffineTraversal.preview(JNull)
// res3: Option[Double] = None
```

#### set
```scala
jsonAffineTraversal.set(9)(JNumber(1))
// res4: Json = JNumber(9.0)

jsonAffineTraversal.set(1)(JNull)
// res5: Json = JNull()
```

#### setOption
```scala
jsonAffineTraversal.setOption(9)(JNumber(1))
// res6: Option[Json] = Some(JNumber(9.0))

jsonAffineTraversal.setOption(9)(JNull)
// res7: Option[Request] = None
```

#### over
```scala

jsonAffineTraversal.over(_ + 1)(JNumber(8))
// res8: Json = JNumber(9.0)

jsonAffineTraversal.over(_ + 1)(JNull)
// res9: Json = JNull
```

#### traverse
```scala
val partialTraverse: Json => Option[Json] = jsonAffineTraversal.traverse[Option](_: Json) {
  case 1 => 9.0.some
  case _ => None
}
// partialTraverse: Json => Option[Json] = $Lambda$6157/0x0000000801d89040@b34f099

partialTraverse(JNumber(9))
// res10: Option[Json] = Some(JNumber(9.0))

partialTraverse(JNumber(100))
// res11: Option[Json] = None
```

#### forall
```scala
jsonAffineTraversal.forall(_ === 9)(JNumber(9))
// res12: Boolean = true

jsonAffineTraversal.forall(_ === 9)(JNull)
// res13: Boolean = true

jsonAffineTraversal.forall(_ === 9)(JNumber(10))
// res14: Boolean = false
```

#### exists
```scala
jsonAffineTraversal.exists(_ === 9)(JNumber(9))
// res15: Boolean = true
```

#### contains
```scala
jsonAffineTraversal.contains(9)(JNumber(9))
// res16: Boolean = true
```

#### isEmpty
```scala
jsonAffineTraversal.isEmpty(JNumber(9))
// res17: Boolean = false

jsonAffineTraversal.isEmpty(JString("test"))
// res18: Boolean = true
```

#### find
```scala
jsonAffineTraversal.find(_ === 9)(JNumber(9))
// res19: Option[Int] = Some(9.0)

jsonAffineTraversal.find(_ === 10)(JNumber(9))
// res20: Option[Int] = None
```

## Laws

A `AffineTraversal` must satisfy all [AffineTraversalLaws](/Proptics/api/proptics/law/AffineTraversalLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.instances.option._
// import cats.instances.option._

import Function.const
// import Function.const

import cats.instances.string._
// import cats.instances.string._

import cats.instances.int._
// import cats.instances.int._

// triple equals operator (===)
implicit val eqJson: Eq[Json] = Eq.instance[Json] { (x: Json, y: Json) =>
  (x, y) match {
    case (JNumber(v1), JNumber(v2)) => v1 === v2
    case (JString(v1), JString(v2)) => v1 === v2
    case (JNull, JNull)             => true
    case _                          => false
  }
}
// eqJson: cats.Eq[Json] = cats.kernel.Eq$$anon$5@426d60f9
```

#### What you get is what you set

```scala
def getSet[S: Eq, A](affineTraversal: AffineTraversal[S, A], s: S): Boolean =
  affineTraversal.viewOrModify(s).fold(identity, affineTraversal.set(_)(s)) === s
// getSet: [S, A](affineTraversal: AffineTraversal[S,A], s: S)
//               (implicit evidence$1: cats.Eq[S])Boolean

getSet[Json, Double](jsonAffineTraversal, JNumber(9.0))
// res0: Boolean = true
``` 

#### Set and then preview a value, will get you an `Option` of the same value

```scala
def previewSet[S, A: Eq](affineTraversal: AffineTraversal[S, A], s: S, a: A): Boolean =
  affineTraversal.preview(affineTraversal.set(a)(s)) === affineTraversal.preview(s).map(const(a))

// previewSet: [S, A](affineTraversal: AffineTraversal[S,A], s: S, a: A)
//                   (implicit evidence$1: cats.Eq[A])Boolean

previewSet[Json, Double](jsonAffineTraversal, JNumber(9.0), 1.0)
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](affineTraversal: AffineTraversal[S, A], s: S, a: A): Boolean =
  affineTraversal.set(a)(affineTraversal.set(a)(s)) === affineTraversal.set(a)(s)
// setSet: [S, A](affineTraversal: AffineTraversal[S,A], s: S, a: A)
//               (implicit evidence$1: cats.Eq[S])Boolean

setSet[Json, Double](jsonAffineTraversal, JNumber(1.0), 9.0)
// res2: Boolean = true 
```