---
id: grate
title: Grate
---

A `Grate` is an optic which allows `zipWith` operations.

## Constructing a monomorphic Grate

`Grate[S, A]` is constructed using the <a href="../../api/proptics/Grate$">Grate[S, A]#apply</a> function.<br/>
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

val grateTuples = Grate[(Int, Int), Int](f => (f(_._1), f(_._2)))

grateTuples.zipWith((2, 5), (3, 4))(_ + _)
// val res0: (Int, Int) = (5,9)
```

In this example the `Grate` optic takes a function `f` in the form of

```
(((Int, Int)) => Int) => Int)
```

and its return type is `Int`, which means that we need to supply a function `(Int, Int) => Int` in order to get an `Int`,
which we will be used to construct a new `(Int, Int)`, so we apply our function twice for each side of the tuple.

```scala
f => (f(_._1), f(_._2))
```

## Constructing a polymorphic Grate

`Grate_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Grate_$">Grate_[S, T, A, B]#apply</a> function.<br/>
`Grate_[S, T, A, B]` takes a `grate` function, `((S => A) => B) => T`, which is a function that given a function from `(S => A) => B)` will return a `T`, that is
if we can extract an `A` out of an `S` we will get a `B` and we will have to use this `B` in order to construct a `T`.

```scala
object Grate_ {
  def apply[S, T, A, B](to: ((S => A) => B) => T): Grate_[S, T, A, B]
}
```

We will try to gain some intuition using an example. Let's assume that we developed a successful recommendation service for TV series and
in order to interact with our system, one must create an HTTP Request to use our recommendation service.

```scala
import cats.syntax.option._

case class Request(id: Option[String], queryString: Map[String, String] = Map.empty)

sealed trait Response
case class Ok[A](value: A) extends Response
case object NoContent extends Response

val seriesMap: Map[String, List[String]] = Map[String, List[String]](
 "tt0903747" -> List("True Detective", "Fargo", "Dexter"),
 "tt2356777" -> List("Breaking Bad", "Fargo", "Dexter"),
 "tt2802850" -> List("Breaking Bad", "True Detective", "Dexter"),
 "tt0773262" -> List("Breaking Bad", "True Detective", "Fargo")
)

def recommendation(seriesId: Option[String]): Option[List[String]] =
  seriesId.flatMap(seriesMap.get(_) orElse List.empty[String].some)

def recommendationRoute(request: Request): Response =
  recommendation(request.id).fold(NoContent: Response) {
    case list @ _ :: _ => Ok(list)
    case _             => NoContent
  }

recommendationRoute(Request("tt2356777".some))
// val res0: Response = Ok(List(Breaking Bad, Fargo, Dexter))

recommendationRoute(Request("tt23567771".some))
// val res1: Response = NoContent
```

At some point of time a new requirement to secure the system has been requested. We can use `Grate` in order to add
additional logic prior to querying our recommendation service. `Grate` takes a [Closed](../profunctors/closed.md) Profunctor

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

Consider the `A` an input and the `B` an output, the `closed` method lets us close/lock a computation <br/> from `A -> B` using
a lock of `C`. once we release the lock we can use our computation to acquire a `B`. The lock can be a function that authenticates the user, and  once
the user is authenticated, we could release the lock and use the recommendation service. In order to lock the recommendation service, we need
to create a polymorphic `Grate` such that it can convert a `Request` to a `Response`

```scala
S ~ Request // initial structure
T ~ Response // modified structure
A ~ Option[String] // initial focus
B ~ Option[List[String]] // modified focus 
```

The `grate` function would become

```scala
((Request => Option[String]) => Option[List[String]]) => Response
```

```scala
case object Forbidden extends Response

def extractSeriesId(req: Request): Option[String] =
  req.queryString.get("password").flatMap {
    case "123456" => req.id orElse "".some
    case _        => None
  }

def mkRequest(f: (Request => Option[String]) => Option[List[String]]): Response =
  f(extractSeriesId) match {
    case Some(ls @ _ :: _) => Ok(ls)
    case Some(_)           => NoContent
    case _                 => Forbidden
  }

val grate: Grate_[Request, Response, Option[String], Option[List[String]]] = Grate_(mkRequest)

def recommendationRoute(request: Request): Response =
  grate.over(recommendation)(request)

recommendationRoute(Request("tt2356777".some))
// val res2: Response = Forbidden

recommendationRoute(Request("tt2356777".some, queryString = Map("password" -> "123456")))
// val res3: Response = Ok(List(Breaking Bad, Fargo, Dexter))

recommendationRoute(Request("tt23567771".some, queryString = Map("password" -> "123456")))
// val res4: Response = NoContent

recommendationRoute(Request(None, queryString = Map("password" -> "123456")))
// val res5: Response = NoContent
```

We created an `extractSeriesId` method which unlocks the service by sending `Some` to the recommendation service
when the user is authenticated, or `None` otherwise. We created another `Response` type `Forbidden`, in order to respond when
the authentication process fails, moreover we created a `Grate` optic which uses the `mkRequest` as its `grate` method.
The client now uses our `Grate` instance instead of calling the recommendation service directly.

## Methods

#### [review](../../api/proptics/Grate_.html#review(b:B):T)

```scala
/** view the modified source of a Grate */
def review(a: A): S
```

```scala
grateTuples.review(9)
// val res0: (Int, Int) = (9,9)
```

#### [set](../../api/proptics/Grate_.html#set(b:B):S=>T)

```scala
/** set the modified focus of a Grate */
def set(b: A): S => S
```

```scala
grateTuples.set(9)((5, 3))
// val res1: (Int, Int) = (9,9)
```

#### [over](../../api/proptics/Grate_.html#over(f:A=>B):S=>T)

```scala
 /** modify the focus type of a Grate */
 def over(f: A => A): S => S
```

```scala
grateTuples.over(_ + 1)((5, 2))
// val res2: (Int, Int) = (6,3)
```

#### <a href ="../../api/proptics/Grate_.html#zipWith(s1:S,s2:S)(f:(A,A)=>B):T">zipWith</a>

```scala
/** zip two sources of a Grate together provided a binary operation which modify the focus of a Grate */
def zipWith(s1: S, s2: S)(f: (A, A) => A): S
```

```scala
grateTuples.zipWith((2, 5), (3, 4))(_ + _)
// val res3: (Int, Int) = (5,9)
```

#### [cotraverse](../../api/proptics/Grate_.html#cotraverse[F[_]](fs:F[S])(f:F[A]=>B)(implicitevidence$1:cats.Applicative[F]):T)

```scala
/** modify an effectful focus of a Grate to the type of the modified focus */
def cotraverse[F[_]: Applicative](fs: F[S])(f: F[A] => A): S
```

```scala
grateTuples.cotraverse[Option]((5, 2).some)(_.fold(0)(_ +  1))
// val res4: (Int, Int) = (5,9)
```

#### [zipWithF](../../api/proptics/Grate_.html#zipWithF[F[_]](f:F[A]=>B)(fs:F[S])(implicitevidence$2:cats.Applicative[F]):T)

```scala
/** synonym for cotraverse, flipped */
def zipWithF[F[_]: Applicative](f: F[A] => A)(fs: F[S]): S
```

```scala
grateTuples.zipWithF[Option](_.fold(0)(_ +  1))((5, 2).some)
// val res5: (Int, Int) = (5,9)
```

## Grate internal encoding

#### Polymorphic Grate

```scala
Grate_[S, T, A, B]
```

`Grate_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Closed](../profunctors/closed.md) of P[_, _].

```scala
/**
  * @tparam S the source of a Grate_
  * @tparam T the modified source of a Grate_
  * @tparam A the focus of a Grate_
  * @tparam B the modified focus of a Grate_
  */
abstract class Grate_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Closed[P]): P[S, T]
}
```

`Grate_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`.</br>
 A `Grate` that changes its focus/structure, is called `Polymorphic Grate`.
 
#### Monomorphic Grate

```scala
Grate[S, A]
```
 
`Grate[S, A]` is a type alias for `Grate_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Grate[S, A] = Grate_[S, S, A, A]
``` 

A `Grate` that does not change its focus/structure, is called `Monomorphic Grate`.

## Laws

A `Grate` must satisfy all <a href="../../api/proptics/law/GrateLaws">GrateLaws</a>. These laws reside in the <a href="../../api/proptics/law/">proptics.law</a> package.<br/>

```scala
import cats.Eq
import cats.instances.int._
import cats.syntax.eq._
import proptics.Grate
import proptics.profunctor.Closed.closedFunction
```

#### identity

```scala
def identityLaw[S, A: Eq](a: A): Boolean =
  Grate.id[A](identity[A] _)(closedFunction)(a) === a

identityLaw(9)
// val res0: Boolean = true
```

#### composition

```scala
def composeOver[S: Eq, A](grate: Grate[S, A])(s: S)(f: A => A)(g: A => A): Boolean =
  grate.over(g)(grate.over(f)(s)) === grate.over(g compose f)(s)

composeOver(Grate.id[Int])(8)(_ + 1)(identity)
// val res1: Boolean = true
```