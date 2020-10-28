---
id: prism
title: Prism
---

A `Prism` is an optic used to focus on one case of a sum type like `Option` and `Either`.<br/>
`Prism[S, A]` means that the type `S` might contain a value of type `A`. <br/>
A `Prism` determines whether a single value matches some set of properties, therefore, is a natural candidate for pattern-matching semantics.

## Constructing Prisms

`Prism` is constructed using the [Prism[S, A]#apply](/Proptics/api/proptics/Prism$.html) function. For a given `Prism[S, A]` it takes two functions as arguments,
`viewOrModify` which is a matching function that produces an `Either[S, A]` given an `S`, that is `Left[S]` if `A` does not exists, or `Right[A]` if `A` exists,
and `review` function which takes a focus of `A` and returns a new structure of `S`.

```scala
object Prism {
  def apply[S, A](viewOrModify: S => Either[S, A])(review: A => S): Prism[S, A]
}
```

Consider a Request ADT/Sum type

```scala
sealed trait Request
// defined trait Request

case object Pending extends Request
// defined object Pending

final case class Success(value: Int) extends Request
// defined class Success

final case class Error(reason: String) extends Request
// defined class Error

val request: Request = Success(200)
// request: Request = Success(200)
```

We can define a `Prism` which focuses on `Success` request

```scala
import proptics.Prism
// import proptics.Prism

import cats.syntax.either._
// import cats.syntax.either._

val successRequestPrism: Prism[Request, Int] = Prism[Request, Int] {
  case Success(value) => value.asRight[Request]
  case req            => req.asLeft[Int]
}(Success)
// successRequestPrism: proptics.Prism[Request,Int] = proptics.Prism_$$anon$13@be4228d
```
 
A more concise version would be using the `fromPreview` method

```scala
object Prism {
  def fromPreview[S, A](preview: S => Option[A])(review: A => S): Prism[S, A]
}
```

```scala
import proptics.Prism
// import proptics.Prism

import cats.syntax.option._
// import cats.syntax.option._

val successRequestPrism: Prism[Request, Int] = Prism.fromPreview[Request, Int] {
  case Success(value) => value.some
  case _            => None
}(Success)
// successRequestPrism: proptics.Prism[Request,Int] = proptics.Prism_$$anon$13@237ad392
```

An even more concise version would be using the `fromPartial` method

```scala
object Prism {
  def fromPartial[S, A](preview: PartialFunction[S, A])(review: A => S): Prism[S, A]
}
```

```scala
import proptics.Prism
// import proptics.Prism

import cats.syntax.eq._ // triple equals (===) 
// import cats.syntax.eq._

val successRequestPrism: Prism[Request, Int] =
  Prism.fromPartial[Request, Int] { case Success(value) => value }(Success)
// successRequestPrism: proptics.Prism[Request,Int] = proptics.Prism_$$anon$13@1fa5e3fb
```

## Common functions of a Prism

#### viewOrModify
```scala
successRequestPrism.viewOrModify(successRequest)
// res0: Either[Request,Int] = Right(200)
```

#### preview
```scala
successRequestPrism.preview(successRequest)
// res1: Option[Int] = Some(200)
```

#### review
```scala
successRequestPrism.review(201)
// res2: Request = Success(201)
```

#### set
```scala
successRequestPrism.set(202)(successRequest)
// res3: : Request = Success(202)

successRequestPrism.set(202)(Pending)
// res4: Request = Pending
```

#### setOption
```scala
successRequestPrism.setOption(204)(successRequest)
// res5: Option[Request] = Some(Success(204))

successRequestPrism.setOption(204)(successRequest)
// res6: Option[Request] = None
```

#### over
```scala
val to204: Int => Int = _ + 4 
// to204: Int => Int = $Lambda$12332/517676320@419cdca6

successRequestPrism.over(_ + 4)(successRequest)
// res7: Request = Success(204)

successRequestPrism.over(_ + 4)(Pending)
// res8: Request = Pending
```

#### traverse
```scala
val partialTraverse = successRequestPrism.traverse[Option](_: Request) {
  case 200 => 200.some
  case _   => None
}
// partialTraverse: Request => Option[Request] = $Lambda$12334/494843037@9d50a46

partialTraverse(successRequest)
// res9: Option[Request] = Some(Success(200))

partialTraverse(Success(204))
// res10: Option[Request] = None
```

#### forall
```scala
successRequestPrism.forall(_ === 204)(successRequest)
// res11: Boolean = false

successRequestPrism.forall(_ === 204)(Pending)
// res12: Boolean = true

successRequestPrism.forall(_ === 200)(successRequest)
// res13: Boolean = true
```

#### exists
```scala
successRequestPrism.exists(_ === 204)(successRequest)
// res14: Boolean = true
```

#### contains
```scala
successRequestPrism.contains(204)(successRequest)
// res15: Boolean = false
```

#### isEmpty
```scala
successRequestPrism.isEmpty(successRequest)
// res16: Boolean = false

successRequestPrism.isEmpty(Pending)
// res17: Boolean = true
```

#### find
```scala
successRequestPrism.find(_ === 200)(successRequest)
// res18: Option[Int] = Some(200)

successRequestPrism.find(_ === 204)(successRequest)
// res19: Option[Int] = None
```

## Prism internal encoding

`Prism[S, A]` is the monomorphic short notation version (does not change the type of the structure) of the polymorphic one `Prism_[S, T, A, B]`

```scala
type Prism[S, A] = Prism_[S, S, A, A]
``` 

`Prism_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Choice](/Proptics/docs/profunctors/choice) of P[_, _].

```scala
abstract class Prism_[S, T, A, B] extends Serializable {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Choice[P]): P[S, T]
}
```

## Laws

A `Prism` must satisfy all [PrismLaws](/Proptics/api/proptics/law/PrismLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.syntax.eq._
// import cats.syntax.eq._

// triple equals operator (===)
implicit val eqRequest: Eq[Request] = Eq.instance { 
  case (Pending, Pending)         => true
  case (Success(v1), Success(v2)) => v1 === v2
  case (Error(s1), Error(s2))     => s1 === s2
  case _                          => false
}
// eqRequest: cats.Eq[Request] = cats.kernel.Eq$$anon$5@5a9a178c
```

#### Review and then preview a value, will get you back the value wrapped with `Some`
```scala
def previewReview[S, A: Eq](prism: Prism[S, A], a: A): Boolean = 
  prism.preview(prism.review(a)) match {
    case Some(value) => value === a
    case _           => false
  }
// previewReview: [S, A](prism: proptics.Prism[S,A], a: A)(implicit evidence$1: cats.Eq[A])Boolean

previewReview(successRequest, 200)
// res0: Boolean = true
```

#### Preview and then review the structure, will get you back the same structure

>If you can extract a value `A` using a Prism `P` from a value `S`,
> then the value `S`<br/> is completely described by `P` and `A`

```scala
def viewOrModifyReview[S: Eq, A](prism: Prism[S, A], s: S): Boolean =
  prism.viewOrModify(s).fold(identity, prism.review) === s
// viewOrModifyReview: [S, A](prism: Prism[S,A], s: S)(implicit evidence$1: cats.Eq[S])Boolean
    
viewOrModifyReview(successRequest, Success(200))
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](prism: Prism[S, A], s: S, a: A): Boolean =
  prism.set(a)(prism.set(a)(s)) === prism.set(a)(s)
// setSet: [S, A](lens: proptics.Prism[S,A], s: S, a: A)(implicit evidence$1: cats.Eq[S])Boolean

setSet(successRequest, Success(200), 204)
// res2: Boolean = true
```



