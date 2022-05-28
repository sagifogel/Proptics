---
id: affine-traversal 
title: AffineTraversal
---

An `AffineTraversal` is similar to a [Traversal](traversal.md) that may contain zero or one element. It is a combination
of a [Lens](lens.md) and a [Prism](prism.md).

## Constructing a monomorphic AffineTraversal

Consider a Json ADT/Sum type

```scala
sealed trait Json
case object JNull extends Json
case class JString(value: String) extends Json
case class JNumber(value: Double) extends Json

val jNumber: Json = JNumber(9)
```

### Using companion object

`AffineTraversal[S, A]` is constructed using the <a href="../../api/proptics/AffineTraversal$">AffineTraversal[S, A]
#apply</a> function.<br/>
For a given `AffineTraversal[S, A]` it takes two functions as arguments,</br> `viewOrModify: S => Either[S, A]` which is
a matching function that produces an `Either[S, A]` given </br> an `S`, and `set: S => A => S` function which takes a
structure `S` and a focus `A` and returns a </br> new structure of `S`.

```scala
object AffineTraversal {
  def apply[S, A](viewOrModify: S => Either[S, A])(set: S => A => S): AffineTraversal[S, A]
}
```

We can define an `AffineTraversal` which focuses on `JNumber`

```scala
import cats.syntax.either._
import proptics.AffineTraversal

def viewOrModify(json: Json): Either[Json, Double] = json match {
  case JNumber(value) => value.asRight[Json]
  case json => json.asLeft[Double]
}

def setJson(json: Json): Double => Json = (i: Double) =>
  json match {
    case JNumber(_) => JNumber(i)
    case _ => json
  }

val jsonAffineTraversal: AffineTraversal[Json, Double] =
  AffineTraversal(viewOrModify)(setJson)
```

```scala
jsonAffineTraversal.viewOrModify(jNumber)
// val res0: Either[Json,Double] = Right(9.0)

jsonAffineTraversal.viewOrModify(JNull)
// val res1: Either[Json,Double] = Left(JNull)

jsonAffineTraversal.set(10)(jNumber)
// val res2: : Json = JNumber(10.0)

jsonAffineTraversal.set(10)(JNull)
// val res3: : Json = JNull
```

### Using fromPreview method

```scala
object AffineTraversal {
  def fromPreview[S, A](preview: S => Option[A])(set: S => A => S): AffineTraversal[S, A]
}
```

```scala
import proptics.AffineTraversal
import cats.syntax.option._

def preview(json: Json): Option[Double] = json match {
  case JNumber(value) => value.some
  case _ => None
}

def setJson(json: Json): Double => Json = (i: Double) =>
  json match {
    case JNumber(_) => JNumber(i)
    case _ => json
  }

val jsonAffineTraversal: AffineTraversal[Json, Double] =
  AffineTraversal.fromPreview(preview)(setJson)
```

### Using fromPartial method

```scala
object AffineTraversal {
  def fromPartial[S, A](preview: PartialFunction[S, A])(set: S => A => S): AffineTraversal[S, A]
}
```

```scala
import proptics.AffineTraversal

def setJson(json: Json): Double => Json = (i: Double) =>
  json match {
    case JNumber(_) => JNumber(i)
    case _ => json
  }

val jsonAffineTraversal =
  AffineTraversal.fromPartial[Json, Double] { case JNumber(value) => value }(setJson)
```

## Constructing a polymorphic AffineTraversal

`AffineTraversal_[S, T, A, B]` is constructed using the <a href="../../api/proptics/AffineTraversal_$">
AffineTraversal_[S, T, A, B]#apply</a> function.<br/>
For a given `AffineTraversal_[S, T, A, B]` it takes two functions as arguments, </br> `viewOrModify: S => Either[T, A]`
which is a matching function that produces an `Either[T, A]` given an `S`, and `set: S => B => T` function which takes a
structure `S` and a focus `B` and returns a structure of `T`.

```scala
object AffineTraversal_ {
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(set: S => B => T): AffineTraversal_[S, T, A, B]
}
```

## Methods

#### [viewOrModify](../../api/proptics/AffineTraversal_.html#viewOrModify(s:S):Either[T,A])

```scala
/** view the focus or return the modified source of an AffineTraversal */
def viewOrModify(s: S): Either[S, A]
```

```scala
jsonAffineTraversal.viewOrModify(jNumber)
// val res0: Either[Json,Double] = Right(9.0)

jsonAffineTraversal.viewOrModify(JNull)
// val res1: Either[Json,Double] = Left(JNull)
```

#### [preview](../../api/proptics/AffineTraversal_.html#preview(s:S):Option[A])

```scala
/** view the focus of an AffineTraversal, if there is any */
def preview(s: S): Option[A]
```

```scala
jsonAffineTraversal.preview(JNumber(9.0))
// val res2: Option[Double] = Some(9.0)

jsonAffineTraversal.preview(JNull)
// val res3: Option[Double] = None
```

#### [set](../../api/proptics/AffineTraversal_.html#set(b:B):S=>T)

```scala
/** set the focus of an AffineTraversal */
def set(b: A): S => S
```

```scala
jsonAffineTraversal.set(9.0)(JNumber(1))
// val res4: Json = JNumber(9.0)

jsonAffineTraversal.set(1.0)(JNull)
// val res5: Json = JNull
```

#### [setOption](../../api/proptics/AffineTraversal_.html#setOption(b:B):S=>Option[T])

```scala
/** set the focus of an AffineTraversal conditionally if it is not None */
def setOption(a: A): S => Option[S]
```

```scala
jsonAffineTraversal.setOption(9.0)(JNumber(1))
// val res6: Option[Json] = Some(JNumber(9.0))

jsonAffineTraversal.setOption(9)(JNull)
// val res7: Option[Request] = None
```

#### [over](../../api/proptics/AffineTraversal_.html#over(f:A=>B):S=>T)

```scala
/** modify the focus type of an AffineTraversal using a function */
def over(f: A => A): S => S
```

```scala

jsonAffineTraversal.over(_ + 1)(JNumber(8))
// val res8: Json = JNumber(9.0)

jsonAffineTraversal.over(_ + 1)(JNull)
// val res9: Json = JNull
```

#### [overOption](../../api/proptics/AffineTraversal_.html#overOption(f:A=>B):S=>Option[T])

```scala
/** modify the focus of an AffineTraversal using a function conditionally if it is not None */
def overOption(f: A => A): S => Option[S]
```

```scala
jsonAffineTraversal.overOption(_ + 1)(JNumber(8))
// val res10: Option[Json] = Some(JNumber(9.0))

jsonAffineTraversal.overOption(_ + 1)(JNull)
// val res11: Option[Json] = None
```

#### [traverse](../../api/proptics/AffineTraversal_.html#traverse[F[_]](s:S)(f:A=>F[B])(implicitevidence$1:cats.Applicative[F]):F[T])

```scala
/** modify the focus type of an AffineTraversal using a cats.Functor */
def traverse[F[_]](s: S)(f: A => F[A])(implicit arg0: Applicative[F]): F[S]
```

```scala
import cats.syntax.eq._

def powerOf2IfNumberOf3(i: Double): Option[Double] =
  Option.when(i === 3.0)(Math.pow(i, 2))

jsonAffineTraversal.traverse[Option](jNumber)(powerOf2IfNumberOf3)
// val res12: Option[Json] = None

jsonAffineTraversal.traverse[Option](JNumber(3))(powerOf2IfNumberOf3)
// val res13: Option[Json] = Some(JNumber(9.0))
```

#### [overF](../../api/proptics/AffineTraversal_.html#overF[F[_]](f:A=>F[B])(s:S)(implicitevidence$2:cats.Applicative[F]):F[T])

```scala
/** synonym for [[traverse]], flipped */
def overF[F[_]](f: A => F[A])(s: S)(implicit arg0: Applicative[F]): F[S]
```

```scala
import cats.syntax.eq._

def powerOf2IfNumberOf3(i: Double): Option[Double] =
  Option.when(i === 3)(Math.pow(i, 2))

val partialAffineTraversal = jsonAffineTraversal.overF[Option](powerOf2IfNumberOf3) _

partialAffineTraversal(jNumber)
// val res14: Option[Json] = None

partialAffineTraversal(JNumber(3.0))
// val res15: Option[Json] = Some(JNumber(9.0))
```

#### [exists](../../api/proptics/AffineTraversal_.html#exists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate holds for the focus of an AffineTraversal */
def exists(f: A => Boolean): S => Boolean
```

```scala
import cats.syntax.eq._

jsonAffineTraversal.exists(_ === 9.0)(jNumber)
// val res16: Boolean = true
```

#### [notExists](../../api/proptics/AffineTraversal_.html#notExists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate does not hold for the focus of an AffineTraversal */
def notExists(f: A => Boolean): S => Boolean
```

```scala
jsonAffineTraversal.notExists(_ === 9.0)(jNumber)
// val res17: Boolean = false
```

#### [contains](../../api/proptics/AffineTraversal_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of an AffineTraversal contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
jsonAffineTraversal.contains(9.0)(jNumber)
// val res18: Boolean = true
```

#### [notContains](../../api/proptics/AffineTraversal_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of an AffineTraversal does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
jsonAffineTraversal.notContains(9)(jNumber)
// val res19: Boolean = false
```

#### [isEmpty](../../api/proptics/AffineTraversal_.html#isEmpty(s:S):Boolean)

```scala
/** check if the AffineTraversal does not contain a focus */
def isEmpty(s: S): Boolean
```

```scala
jsonAffineTraversal.isEmpty(jNumber)
// val res20: Boolean = false

jsonAffineTraversal.isEmpty(JNull)
// val res21: Boolean = true
```

#### [nonEmpty](../../api/proptics/AffineTraversal_.html#nonEmpty(s:S):Boolean)

```scala
/** check if the AffineTraversal contains a focus */
def nonEmpty(s: S): Boolean
```

```scala
jsonAffineTraversal.nonEmpty(jNumber)
// val res22: Boolean = true

jsonAffineTraversal.nonEmpty(JNull)
// val res23: Boolean = false
```

#### [find](../../api/proptics/AffineTraversal_.html#find(f:A=>Boolean):S=>Option[A])

```scala
/** find the focus of an AffineTraversal that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
import cats.syntax.eq._

jsonAffineTraversal.find(_ === 9.0)(jNumber)
// val res24: Option[Double] = Some(9.0)

jsonAffineTraversal.find(_ === 9)(JNull)
// val res25: Option[String] = None
```

#### <a href="../../api/proptics/AffineTraversal_.html#failover[F[_]](f:A=>B)(s:S)(implicitev0:proptics.profunctor.Choice[[β$3$,γ$4$]cats.data.Kleisli[[β$2$](proptics.data.Disj[Boolean],β$2$),β$3$,γ$4$]],implicitev1:cats.arrow.Strong[[β$6$,γ$7$]cats.data.Kleisli[[β$5$](proptics.data.Disj[Boolean],β$5$),β$6$,γ$7$]],implicitev2:cats.Alternative[F]):F[T]">failover</a>

```scala
/** try to map a function over this AffineTraversal, failing if the AffineTraversal has no focus */
def failover[F[_]](f: A => B)
                  (s: S)
                  (implicit ev0: Choice[Star[(Disj[Boolean], *), *, *]], 
                            ev1: Strong[Star[(Disj[Boolean], *), *, *]], 
                            ev2: Alternative[F]): F[T]
```

```scala
import spire.implicits._

jsonAffineTraversal.failover[Option](_ + 1)(JNumber(8))
// val res26: Option[Json] = Some(JNumber(9.0))

jsonAffineTraversal.failover[Option](_ + 1)(JNull)
// val res27: Option[Request] = None
```

#### [forall](../../api/proptics/AffineTraversal_.html#forall(f:A=>Boolean):S=>Boolean)

```scala
/** test whether there is no focus or a predicate holds for the focus of an AffineTraversal */
def forall(f: A => Boolean): S => Boolean
```

```scala
jsonAffineTraversal.forall(_ === 9.0)(jNumber)
// val res28: Boolean = true

jsonAffineTraversal.forall(_ === 1.0)(jNumber)
// val res29: Boolean = false

jsonAffineTraversal.forall(_ === 1.0)(JNull)
// val res30: Boolean = true
```

#### [forall](../../api/proptics/AffineTraversal_.html#forall[R](s:S)(f:A=>R)(implicitevidence$1:spire.algebra.lattice.Heyting[R]):R)

```scala
/** 
 * test whether there is no focus or a predicate holds for the focus of 
 * an AffineTraversal, using Heyting algebra
 */
def forall[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```
```scala
import spire.std.boolean._
import cats.syntax.eq._

jsonAffineTraversal.forall(jNumber)(_ === 9.0)
// val res31: Boolean = true

jsonAffineTraversal.forall(jNumber)(_ === 1.0)
// val res32: Boolean = false

jsonAffineTraversal.forall(JNull)(_ === 1.0)
// val res33: Boolean = true
```

## AffineTraversal internal encoding

#### Polymorphic AffineTraversal

```scala
AffineTraversal_[S, T, A, B]
```

`AffineTraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Choice](../profunctors/choice.md) and
<br /> a [Strong](../profunctors/strong.md) of P[_, _].

```scala
/**
 * @tparam S the source of an AffineTraversal_
 * @tparam T the modified source of an AffineTraversal_
 * @tparam A the focus of an AffineTraversal_
 * @tparam B the modified focus of an AffineTraversal_
 */
abstract class AffineTraversal_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev0: Choice[P], ev1: Strong[P]): P[S, T]
}
```

`AffineTraversal_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from </br> `S`
to `T`.<br/>
An `AffineTraversal` that changes its focus/structure, is called `Polymorphic AffineTraversal`.

#### Monomorphic AffineTraversal

 ```scala
AffineTraversal[S, A]
 ```

`AffineTraversal[S, A]` is a type alias for `AffineTraversal_[S, S, A, A]`, which has the same type of focus `A`, thus
preserving the same type of structure `S`.

```scala
type AffineTraversal[S, A] = AffineTraversal_[S, S, A, A]
``` 

An `AffineTraversal[S, A]` means that the type `S` might contain zero or one value of type `A`. <br/>
An `AffineTraversal` that does not change its focus/structure, is called `Monomorphic AffineTraversal`.

## Laws

An `AffineTraversal` must satisfy all <a href="../../api/proptics/law/AffineTraversalLaws">AffineTraversalLaws</a>.
These laws reside in the <a href="../../api/proptics/law/">proptics.law</a> package.<br/>

```scala
import cats.Eq
import cats.instances.option._
import Function.const
import cats.instances.string._
import cats.instances.int._

implicit val eqJson: Eq[Json] = Eq.instance[Json] { (x: Json, y: Json) =>
  (x, y) match {
    case (JNumber(v1), JNumber(v2)) => v1 === v2
    case (JString(v1), JString(v2)) => v1 === v2
    case (JNull, JNull) => true
    case _ => false
  }
}
```

#### What you get is what you set

```scala
def getSet[S: Eq, A](affineTraversal: AffineTraversal[S, A], s: S): Boolean =
  affineTraversal.viewOrModify(s).fold(identity, affineTraversal.set(_)(s)) === s

getSet[Json, Double](jsonAffineTraversal, JNumber(9.0))
// val res0: Boolean = true
``` 

#### Set and then preview a value, will get you an `Option` of the same value

```scala
def previewSet[S, A: Eq](affineTraversal: AffineTraversal[S, A], s: S, a: A): Boolean =
  affineTraversal.preview(affineTraversal.set(a)(s)) === affineTraversal.preview(s).map(const(a))

previewSet[Json, Double](jsonAffineTraversal, JNumber(9.0), 1.0)
// val res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](affineTraversal: AffineTraversal[S, A], s: S, a: A): Boolean =
  affineTraversal.set(a)(affineTraversal.set(a)(s)) === affineTraversal.set(a)(s)

setSet[Json, Double](jsonAffineTraversal, JNumber(1.0), 9.0)
// val res2: Boolean = true 
```