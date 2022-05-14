---
id: prism
title: Prism
---

A `Prism` is an optic used to focus on one case of a sum type like `Option` and `Either`.<br/>

## Constructing a Prism

Consider a Request ADT/Sum type

```scala
sealed trait Request
case object Pending extends Request
final case class Success(httpCode: Int) extends Request
final case class Error(reason: String) extends Request

val successRequest: Request = Success(200)
```

### Using companion object

`Prism[S, A]` is constructed using the <a href="../../api/proptics/Prism$">Prism[S, A]#apply</a> function. For a
given `Prism[S, A]` it takes two functions as arguments,
`viewOrModify: S => Either[S, A]` which is a matching function that produces an `Either[S, A]` given an `S`,
and `review: A => S` function which takes a focus of `A` and returns a new structure of `S`.

```scala
object Prism {
 def apply[S, A](viewOrModify: S => Either[S, A])(review: A => S): Prism[S, A]
}
```

We can define a `Prism` which focuses on the value of a `Success` Request

```scala
import proptics.Prism
import cats.syntax.either._

val successHTTPCodePrism: Prism[Request, Int] =
 Prism[Request, Int] {
  case Success(httpCode) => httpCode.asRight[Request]
  case req => req.asLeft[Int]
 }(Success)
```

```scala
val errorRequest = Error("exception")

successHTTPCodePrism.viewOrModify(successRequest)
// val res0: Either[Request,Int] = Right(200)

successHTTPCodePrism.set(204)(successRequest)
// val res1: Request = Success(204)

successHTTPCodePrism.set(204)(errorRequest)
// val res2: Request = Error(exception)
```

### Using fromPreview method

```scala
object Prism {
 def fromPreview[S, A](preview: S => Option[A])(review: A => S): Prism[S, A]
}
```

```scala
import proptics.Prism
import cats.syntax.option._

val successHTTPCodePrism: Prism[Request, Int] =
 Prism.fromPreview[Request, Int] {
  case Success(httpCode) => httpCode.some
  case _ => None
 }(Success)
```

### Using fromPartial method

```scala
object Prism {
 def fromPartial[S, A](preview: PartialFunction[S, A])(review: A => S): Prism[S, A]
}
```

```scala
import proptics.Prism
import cats.syntax.eq._

val successHTTPCodePrism: Prism[Request, Int] =
 Prism.fromPartial[Request, Int] { case Success(httpCode) => httpCode }(Success)
```

### Using macros

Macros can simplify the way to focus on particular subclass

```scala
import proptics.macros._

val successHTTPCodePrism = GPrism[Request, Success]

successHTTPCodePrism.set(Success(204))(Success(200))
// val res0: Request = Success(204)

successHTTPCodePrism.set(Success(204))(Error("exception"))
// val res1: Request = Error(exception)
```

## Constructing a polymorphic Prism

`Prism_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Prism_$">Prism_[S, T, A, B]#apply</a>
function.</br>
For a given `Prism_[S, T, A, B]` it takes two functions as arguments, `viewOrModify: S => Either[T, A]`, which is a
matching function that produces an `Either[T, A]` given an `S`
and `review: B => T ` function which takes a focus of `B` and returns a structure of `T`.

```scala
object Prism_ {
 def apply[S, T, A, B](viewOrModify: S => Either[T, A])(review: B => T): Prism_[S, T, A, B]
}
```

```scala
/** extract the right element of an either using polymorphic Prism */
def right[A, B, C]: Prism_[Either[C, A], Either[C, B], A, B] =
 Prism_[Either[C, A], Either[C, B], A, B]((either: Either[C, A]) =>
  either match {
   case Right(a) => Right(a)
   case Left(c) => Left(Left(c))
  })(Right.apply)

val intToStringPrism = right[Int, String, Exception]

intToStringPrism.set("√")(Right(10))
// val res0: Either[Exception,String] = Right(√)

intToStringPrism.set("√")(Left(new IllegalArgumentException("arg")))
// val res1: Either[Exception,String] = Left(java.lang.IllegalArgumentException: arg)
```

## Methods

#### [viewOrModify](../../api/proptics/Prism_.html#viewOrModify(s:S):Either[T,A])

```scala
/** view the focus or return the modified source of a Prism */
def viewOrModify(s: S): Either[S, A]
```

```scala
successHTTPCodePrism.viewOrModify(successRequest)
// val res0: Either[Request,Int] = Right(200)
```

#### [preview](../../api/proptics/Prism_.html#preview(s:S):Option[A])

```scala
/** view the focus of a Prism, if there is any */
def preview(s: S): Option[A]
```

```scala
successHTTPCodePrism.preview(successRequest)
// val res1: Option[Int] = Some(200)
```

#### [review](../../api/proptics/Prism_.html#review(b:B):T)

```scala
/** view the source of a Prism, (construct an S from an A)  */
def review(a: A): S
```

```scala
successHTTPCodePrism.review(201)
// val res2: Request = Success(201)
```

#### [set](../../api/proptics/Prism_.html#set(b:B):S=>T)

```scala
/** set the focus of a Prism */
def set(b: A): S => S
```

```scala
successHTTPCodePrism.set(202)(successRequest)
// val res3: : Request = Success(202)
```

#### [setOption](../../api/proptics/Prism_.html#setOption(b:B):S=>Option[T])

```scala
/** set the focus of a Prism conditionally if it is not None */
def setOption(a: A): S => Option[S]
```

```scala
successHTTPCodePrism.setOption(204)(successRequest)
// val res4: Option[Request] = Some(Success(204))

successHTTPCodePrism.setOption(204)(Pending)
// val res5: Option[Request] = None
```

#### [over](../../api/proptics/Prism_.html#over(f:A=>B):S=>T)

```scala
/** modify the focus type of a Prism using a function */
def over(f: A => A): S => S
```

```scala
successHTTPCodePrism.over(_ + 4)(successRequest)
// val res6: Request = Success(204)

successHTTPCodePrism.over(_ + 4)(Pending)
// val res7: Request = Pending
```

#### [overOption](../../api/proptics/Prism_.html#overOption(f:A=>B):S=>Option[T])

```scala
/** modify the focus of a Prism using a function conditionally if it is not None */
def overOption(f: A => A): S => Option[S]
```

```scala
successHTTPCodePrism.overOption(_ + 4)(successRequest)
// val res8: Option[Request] = Some(Success(204))

successHTTPCodePrism.overOption(_ + 4)(Pending)
// val res9: Option[Request] = None
```

#### [traverse](../../api/proptics/Prism_.html#traverse[F[_]](s:S)(f:A=>F[B])(implicitevidence$1:cats.Applicative[F]):F[T])

```scala
/** modify the focus type of a Prism using a cats.Functor */
def traverse[F[_]](s: S)(f: A => F[A])(implicit arg0: Applicative[F]): F[S]
```

```scala
import cats.syntax.eq._

def is200Response(httpCode: Int): Option[Int] =
  Option.when(httpCode === 200)(httpCode)

successHTTPCodePrism.traverse(successRequest)(is200Response)
// val res10: Option[Request] = Some(Success(200))

successHTTPCodePrism.traverse(Pending)(is200Response)
// val res11: Option[Request] = Some(Pending)
```

#### [overF](../../api/proptics/Prism_.html#overF[F[_]](f:A=>F[B])(s:S)(implicitevidence$2:cats.Applicative[F]):F[T])

```scala
/** synonym for traverse, flipped */
def overF[F[_]](f: A => F[A])(s: S)(implicit arg0: Applicative[F]): F[S]
```

```scala
import cats.syntax.eq._

def is200Response(httpCode: Int): Option[Int] =
  Option.when(httpCode === 200)(httpCode)

val partialPrism = successHTTPCodePrism.overF(is200Response) _

partialPrism(successRequest)
// val res12: Option[Request] = Some(Success(200))

partialPrism(Pending)
// val res13: Option[Request] = Some(Pending)
```

#### [exists](../../api/proptics/Prism_.html#exists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate holds for the focus of a Prism */
def exists(f: A => Boolean): S => Boolean
```

```scala
import cats.syntax.eq._

successHTTPCodePrism.exists(_ === 200)(successRequest)
// val res14: Boolean = true
```

#### [notExists](../../api/proptics/Prism_.html#notExists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate does not hold for the focus of a Prism */
def notExists(f: A => Boolean): S => Boolean
```

```scala
import cats.syntax.eq._

successHTTPCodePrism.notExists(_ === 200)(successRequest)
// val res15: Boolean = false
```

#### [contains](../../api/proptics/Prism_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of a Prism contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
successHTTPCodePrism.contains(204)(successRequest)
// val res16: Boolean = false
```

#### [notContains](../../api/proptics/Prism_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of a Prism does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
successHTTPCodePrism.notContains(204)(successRequest)
// val res17: Boolean = true
```

#### [isEmpty](../../api/proptics/Prism_.html#isEmpty(s:S):Boolean)

```scala
/** check if the Prism does not contain a focus */
def isEmpty(s: S): Boolean
```

```scala
successHTTPCodePrism.isEmpty(successRequest)
// val res18: Boolean = false

successHTTPCodePrism.isEmpty(Pending)
// val res19: Boolean = true
```

#### [nonEmpty](../../api/proptics/Prism_.html#nonEmpty(s:S):Boolean)

```scala
/** check if the Prism contains a focus */
def nonEmpty(s: S): Boolean
```

```scala
successHTTPCodePrism.nonEmpty(successRequest)
// val res20: Boolean = true

successHTTPCodePrism.nonEmpty(Pending)
// val res21: Boolean = false
```

#### [find](../../api/proptics/Prism_.html#find(f:A=>Boolean):S=>Option[A])
```scala
/** find the focus of a Prism that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
import cats.syntax.eq._

successHTTPCodePrism.find(_ === 200)(Pending)
// val res22: Option[String] = None
```

#### <a href="../../api/proptics/Prism_.html#failover[F[_]](f:A=>B)(s:S)(implicitev0:proptics.profunctor.Choice[[β$3$,γ$4$]cats.data.Kleisli[[β$2$](proptics.data.Disj[Boolean],β$2$),β$3$,γ$4$]],implicitev1:cats.Alternative[F]):F[T]">failover</a>

```scala
/** try to map a function over this Prism, failing if the Prism has no focus */
def failover[F[_]](f: A => A)
                  (s: S)
                  (implicit ev0: Choice[Star[(Disj[Boolean], *), *, *]], 
                            ev1: Alternative[F]): F[S]
```

```scala
import spire.implicits._

successHTTPCodePrism.failover[Option](_ + 4)(successRequest)
// val res23: Option[Request] = Some(Success(204))

successHTTPCodePrism.failover[Option](_ + 4)(Pending)
// val res24: Option[Request] = None
```

#### [forall](../../api/proptics/Prism_.html#forall(f:A=>Boolean):S=>Boolean)

```scala
/** test whether there is no focus or a predicate holds for the focus of a Prism */
def forall(f: A => Boolean): S => Boolean
```

```scala
successHTTPCodePrism.forall(_ === 200)(successRequest)
// val res25 Boolean = false

successHTTPCodePrism.forall(_ === 204)(successRequest)
// val res26: Boolean = false

successHTTPCodePrism.forall(_ === 200)(Pending)
// val res27: Boolean = true
```

#### [forall](../../api/proptics/Prism_.html#forall[R](s:S)(f:A=>R)(implicitevidence$1:spire.algebra.lattice.Heyting[R]):R)

```scala
/** 
 * test whether there is no focus or a predicate holds for the focus of a Prism, using Heyting algebra
 */
def forall[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```

```scala
import spire.std.boolean._
import cats.syntax.eq._

successHTTPCodePrism.forall(successRequest)(_ === 200)
// val res28: Boolean = true

successHTTPCodePrism.forall(successRequest)(_ === 200)
// val res29: Boolean = false

successHTTPCodePrism.forall(Pending)(_ === 200)
// val res30: Boolean = true
```

## Prism internal encoding

#### Polymorphic Prism

```scala
Prism_[S, T, A, B]
```

`Prism_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Choice](../profunctors/choice.md) of P[_, _].

 ```scala
/**
 * @tparam S the source of a Prism_
 * @tparam T the modified source of a Prism_
 * @tparam A the focus of a Prism_
 * @tparam B the modified focus of a Prism_
 */
abstract class Prism_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]
}
```

`Prism_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`.</br>
A `Prism` that changes its focus/structure, is called `Polymorphic Prism`.

#### Monomorphic Prism

```scala
Prism[S, A]
```

`Prism[S, A]` is a type alias for `Prism_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same
type of structure `S`.

```scala
type Prism[S, A] = Prism_[S, S, A, A]
```

`Prism[S, A]` means that the type `S` might contain a value of type `A`. <br/>
A `Prism` determines whether a single value matches some set of properties, therefore, is a natural candidate for
pattern-matching semantics.</br>
A `Prism` that does not change its focus/structure, is called `Monomorphic Prism`.

## Laws

A `Prism` must satisfy all <a href="../../api/proptics/law/PrismLaws">PrismLaws</a>. These laws reside in the <a href="
../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.Eq
import cats.syntax.eq._

implicit val eqRequest: Eq[Request] = Eq.instance {
  case (Pending, Pending) => true
  case (Success(v1), Success(v2)) => v1 === v2
  case (Error(s1), Error(s2)) => s1 === s2
  case _ => false
}
```

#### Review and then preview a value, will get you back the value wrapped with `Some`

```scala
def previewReview[S, A: Eq](prism: Prism[S, A], a: A): Boolean =
  prism.preview(prism.review(a)) match {
    case Some(value) => value === a
    case _ => false
  }

previewReview(successRequest, 200)
// val res0: Boolean = true
```

#### Preview and then review the structure, will get you back the same structure

> If you can extract a value `A` using a Prism `P` from a value `S`,
> then the value `S`<br/> is completely described by `P` and `A`

```scala
def viewOrModifyReview[S: Eq, A](prism: Prism[S, A], s: S): Boolean =
  prism.viewOrModify(s).fold(identity, prism.review) === s

viewOrModifyReview(successRequest, Success(200))
// val res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](prism: Prism[S, A], s: S, a: A): Boolean =
 prism.set(a)(prism.set(a)(s)) === prism.set(a)(s)

setSet(successRequest, Success(200), 204)
// val res2: Boolean = true
```



