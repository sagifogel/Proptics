---
id: grate
title: Grate
---

A `Grate` is an optic which allows `zipWith` operations.<br/>

## Constructing Grates

`Grate` can be constructed using the [Grate[S, A]#apply](/Proptics/api/proptics/Grate$.html) function.<br/>
`Grate[S, A]` takes a `grate` function, `((S => A) => A) => S`, which is a function that given a function from `(S => A) => A)` will return an `S`, that is
if we can extract an `A` out of an `S` we will get an `A` and we will have to use this `A` in order to construct a new `S`.

```scala
object Grate {
  def apply[S, A](to: ((S => A) => A) => S): Grate[S, A]
}
```

A simple example would be the accumulation of two tuples of ints `(Int, Int)`

```scala
import proptics.{Grate, Grate_}
// import proptics.{Grate, Grate_}

val grateTuples = Grate[(Int, Int), Int](f => (f(_._1), f(_._2)))
// grateTuples: proptics.Grate[(Int, Int),Int] = proptics.Grate_$$anon$5@703a4e5

grateTuples.zipWith((1, 5), (8, 4))(_ + _)
//res0: (Int, Int) = (9,9)
```

In this example the `Grate` optic takes a function `f` in the form of

```
(((Int, Int)) => Int) => Int)
```

and its return type is `Int`, which means that we need to supply a function `(Int, Int) => Int` In order to get an `Int`, 
which we will use to construct a new `(Int, Int)`, so we apply our function twice for each side of the tuple, and we will get two `Int`s.

```scala
f => (f(_._1), f(_._2))
```

## Intuition

We will try to gain some intuition using an example. Let's assume that we developed a successful recommendation service for TV series and
in order to interact with our system, one must create an HTTP Request to our recommendation service.

```scala
import cats.syntax.option._
// import cats.syntax.option._

case class Request(id: Option[String], queryString: Map[String, String] = Map.empty)
// defined class Request

sealed trait Response
// defined trait Response

case class Ok[A](value: A) extends Response
// defined class Ok

case object NoContent extends Response
// defined object NoContent

// recommendation service
def recommendation(seriesId: Option[String]): Option[List[String]] =
  seriesId.flatMap(seriesMap.get(_) orElse List.empty[String].some)
// recommendation: (seriesId: Option[String])Option[List[String]]

// client
def recommendationRoute(request: Request): Response =
  recommendation(request.id).fold(NoContent: Response) {
    case list @ _ :: _ => Ok(list)
    case _             => NoContent
  }
// recommendationRoute: (request: Request)Response

val seriesMap: Map[String, List[String]] = Map[String, List[String]](
  "tt0903747" -> List("True Detective", "Fargo", "Dexter"),
  "tt2356777" -> List("Breaking Bad", "Fargo", "Dexter"),
  "tt2802850" -> List("Breaking Bad", "True Detective", "Dexter"),
  "tt0773262" -> List("Breaking Bad", "True Detective", "Fargo")
)
// seriesMap: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter), 
//   tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//   tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//   tt0773262 -> List(Breaking Bad, True Detective, Fargo))

recommendationRoute(Request("tt2356777".some))
// res0: Response = Ok(List(Breaking Bad, Fargo, Dexter))

recommendationRoute(Request("tt23567771".some))
// res1: Response = NoContent
```

At some point of time a new requirement to secure the system has been requested. We can use `Grate` in order to add
additional logic prior to querying our recommendation service. `Grate` takes a [Closed](/Proptics/docs/profunctors/closed) Profunctor

```scala
trait Closed[P[_, _]] extends Profunctor[P] {
  def closed[A, B, C](pab: P[A, B]): P[C => A, C => B]
}
```
The `Closed` profunctor defines a `closed` method. We will specialize the `P[_, _]` type constructor to the `Function` type. Now the type signatures 
of `closed` methods is:

```scala
def closed[A, B, C](pab: A => B): (C => A) => C => B
```

Consider the `A` an input an the `B` an output, the `closed` method lets us close/lock a computation <br/> from `A -> B` using 
a lock of `C`. once we release the lock we can use our computation to acquire a `B`. The lock can be a function that authenticates the user, and  once 
the user is authenticated, we could release the lock and use the recommendation service. In order to lock the recommendation service, we need
to create a polymorphic `Grate` such that it can convert between `Request` and `Response`

```
S ~ Request // initial structure
T ~ Response // modofied structure
A ~ Option[String] // intial focus
B ~ Option[List[String]] // modified focus 
```

The `grate` function would become

```scala
((Request => Option[String]) => Option[List[String]]) => Response
```

```scala
case object Forbidden extends Response
// defined object Forbidden

def extractSeriesId(req: Request): Option[String] =
  req.queryString.get("password").flatMap {
    case "123456" => req.id orElse "".some
    case _        => None
  }
// extractSeriesId: (req: Request)Option[String]

//  grate: ((Request => Option[String]) => Option[List[String]]) => Response
def mkRequest(f: (Request => Option[String]) => Option[List[String]]): Response =
  f(extractSeriesId) match {
    case Some(ls @ _ :: _) => Ok(ls)
    case Some(_)           => NoContent
    case _                 => Forbidden
  }
// mkRequest: (f: (Request => Option[String]) => Option[List[String]])Response

val grate: Grate_[Request, Response, Option[String], Option[List[String]]] = Grate_(mkRequest)
// grate: Grate_[Request,Response,Option[String],Option[List[String]]] = Grate_$$anon$5@1bfd0b08

// client
def recommendationRoute(request: Request): Response =
  grate.over(recommendation)(request)
// recommendationRoute: (request: Request)Response

recommendationRoute(Request("tt2356777".some))
// res2: Response = Forbidden

recommendationRoute(Request("tt2356777".some, queryString = Map("password" -> "123456")))
// res3: Response = Ok(List(Breaking Bad, Fargo, Dexter))

recommendationRoute(Request("tt23567771".some, queryString = Map("password" -> "123456")))
// res4: Response = NoContent

recommendationRoute(Request(None, queryString = Map("password" -> "123456")))
// res5: Response = NoContent
```

We created an `extractSeriesId` method which unlocks the service by sending `Some` to the recommendation service
when the user is authenticated, or `None` otherwise. We created another `Response` type `Forbidden`, in order to respond when
the authentication process fails, moreover we created a `Grate` optic which uses the `mkRequest` as its `grate` method.
 
```scala 
val grate: Grate_[Request, Response, Option[String], Option[List[String]]] = Grate_(mkRequest) 
``` 
The client now uses our `Grate` instance instead of calling the recommendation service directly.

## Common functions of a Grate

#### review

```scala
grate.review(List("True Detective", "Fargo", "Dexter").some)
// res6: Response = Ok(List(True Detective, Fargo, Dexter))
```

#### set

```scala
grate.set(List("True Detective", "Fargo", "Dexter").some)(Request(None))
// res7: Response = Ok(List(True Detective, Fargo, Dexter))
```

#### over

```scala
grate.over(recommendation)(Request(None, queryString = Map("password" -> "123456")))
// res8: Response = NoContent
```

#### zipWith

```scala
import cats.instances.list._ // for semigroup
// import cats.instances.list._

import cats.syntax.semigroup._
// import cats.syntax.semigroup._
  
val request1 = Request("tt2356777".some, Map("password" -> "123456"))
// request1: Request(Some(tt2356777),Map(password -> 123456))

val request2 = Request("tt0773262".some, Map("password" -> "123456"))
// request2: Request(Some(tt0773262),Map(password -> 123456))

grate.zipWith(request1, request2) {
  case (None, None) => None
  case (op1, op2)   => recommendation(op1) |+| recommendation(op2) map (_.toSet.toList)
}
// res9: Response = Ok(List(Breaking Bad, Fargo, Dexter, True Detective))
```

## Grate internal encoding

`Grate[S, A]` is the monomorphic short notation version (does not change the type of the structure) of the polymorphic one `Grate_[S, T, A, B]`

```scala
type Grate[S, A] = Grate_[S, S, A, A]
``` 

`Grate_[S, T, A, B]` is basically a function `P[A, B] => P[S, T]` that takes a [Closed](/Proptics/docs/profunctors/closed) of P[_, _].

```scala
abstract class Grate_[S, T, A, B] extends Serializable {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]
}
```

## Laws

A `Grate` must satisfy all [GrateLaws](/Proptics/api/proptics/law/GrateLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.instances.int._
// import cats.instances.int._

import cats.syntax.eq._ // // triple equals operator (===)
// import cats.syntax.eq._

import proptics.Grate
// import proptics.Grate

import proptics.profunctor.Closed.closedFunction
// import proptics.profunctor.Closed.closedFunction
```

#### identity

```scala
def identityLaw[S, A: Eq](a: A): Boolean =
    Grate.id[A](identity[A] _)(closedFunction)(a) === a
// identityLaw: [S, A](a: A)(implicit evidence$1: cats.Eq[A])Boolean

identityLaw(9)
// res0: Boolean = true
```

#### composition

```scala
def composeOver[S: Eq, A](grate: Grate[S, A])(s: S)(f: A => A)(g: A => A): Boolean =
  grate.over(g)(grate.over(f)(s)) === grate.over(g compose f)(s)
// composeOver: [S, A](grate: proptics.Grate[S,A])(s: S)
//                                                (f: A => A)
//                                                (g: A => A)
//                                                (implicit evidence$1: cats.Eq[S])Boolean 

composeOver(Grate.id[Int])(8)(_ + 1)(identity)
// res1: Boolean = true
```



