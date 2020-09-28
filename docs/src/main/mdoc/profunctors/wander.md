---
id: wander
title: Wander
---

The `Wander` class extends [Strong](/Proptics/docs/profunctors/strong) and [Choice](/Proptics/docs/profunctors/choice), and it is used to define `Traversal`s.

```scala
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: P[A, B]): P[S, T]
}
```

It defines a `wander` method which takes a `Traversing[S T, A, B]` and `P[A, B]` and returns `P[S, T]`

## Traversing

```scala
import cats.Applicative

trait Traversing[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]
}
```

In order to understand `Traversing[S, T, A, B]`, we should look at a simpler construct, the `over` method.
The `over` method lets us modify the focus/foci using a function. If we define a polymorphic `Traversal`, or any kind of 
polymorphic `Optic` that supports modifications, the `over` method's return type will be different from the initial structure. The initial structure `S` 
will become, `T`

```scala
  def over(f: A => B): S => T
```

Consider a polymorphic `Traversal` of structure `(Int, Int)` with a focus of the second element of the tuple `Int` and a modified focus of `String`, the modified 
structure will be `(Int, String)`

```
S ~ (Int, Int) // initial structure
T ~ (Int, String) // modofied structure
A ~ Int // intial focus
B ~ String // modified focus 
```

```scala
import proptics.Traversal_
// import proptics.Traversal_

val traversal: Traversal_[(Int, Int), (Int, String), Int, String] =
  Traversal_[(Int, Int), (Int, String), Int, String](_._2) { case (i, _) => str => (i, str) }
// traversal: Traversal_[(Int, Int),(Int, String),Int,String] = proptics.Traversal_$$anon$13@2859e95

val initialStructure: (Int, Int) = (9, 10)
// initialStructure: (Int, Int) = (9,10)

val modifiedStructure: (Int, String) = traversal.over(_ => "Hello")(initialStructure)
// modifiedStructure: (Int, String) = (9,Hello)
```

`Traversing`'s apply method is similar to `over` but takes `A -> F[B]` instead of `A -> B` and the return type is `F[T]` instead of `T`.
Basically `Traversing` is an `over` method that lifts the value to an `Applicative` context. 

```scala
  def over(f: A => B): S => T

  def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]
```

So if we want to implement `over` using `Wander` we have to take into consideration `Traversing` too.<br/>
`Wander` takes a `Traversing[S, T, A, B]` and `P[A, B]` and returns `P[S, T]`. `Traversing` deals with lifted types and returns `F[T]`, 
so the types are not aligned with each other, Therefore, we need to peek the `Function` type as our `P[_, _]` and an `F[_]` for `Traversing` such that we can "peel" of the 
`F[_]` from the return type of `f: A => F[B]` and the return type `F[T]`, and that is exactly what `Cats.Id` does.

## Function as Wander profunctor

```scala
import cats.Id
// import cats.Id

import proptics.profunctor.{Traversing, Wander}
// import proptics.profunctor.{Traversing, Wander}

implicit final def wanderFunction: Wander[Function] = new Wander[Function] {
  override def wander[S, T, A, B](traversing: Traversing[S, T, A, B])(pab: A => B): S => T = 
    traversing[Id](pab)
}
// wanderFunction: Wander[Function]
```

If we curry the `wander` method, than it would take the form of:<br/> `Traversing[S, T, A, B] => (A => B) => S => T`. We can think of it as
if we get an argument of `Traversing[S, T, A, B]` than we would return a function that when given an `A => B` returns an `S => T`,

```scala
(A => B) => S => T
```

which is equivalent to the `over` method.






