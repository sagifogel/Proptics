---
id: traversal
title: Traversal
---

A `Traversal` is an optic used to focus on zero, one, or many values.</br>
`Traversal` is usually used for collections like `List`, `Map`, `Array`.

## Traversal internal encoding

#### Polymorphic Traversal 

```scala
Traversal_[S, T, A, B]
```

`Traversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Wander](../profunctors/wander.md) of P[_, _].

```scala
/** @tparam S the source of a Traversal_
  * @tparam T the modified source of a Traversal_
  * @tparam A the foci of a Traversal_
  * @tparam B the modified foci of a Traversal_
  */
abstract class Traversal_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
}
```

`Traversal_[S, T, A, B]` changes its foci from `A` to `B`, resulting in a change of structure from `S` to `T`.<br/>
 A `Traversal` that changes its foci/structure, is called `Polymorphic Traversal`.

#### Monomorphic Traversal

```scala
Traversal[S, A]
```

`Traversal[S, A]` is a type alias for `Traversal_[S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.

```scala
type Traversal[S, A] = Traversal_[S, S, A, A]
``` 

`Traversal[S, A]` means that the type `S` might contain zero, one, or many values of type `A`.</br>
A `Traversal` that does not change its foci/structure, is called `Monomorphic Traversal`.

## Constructing Traversals

`Traversal_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Traversal_$">Traversal_[S, T, A, B]#apply</a> function.</br>
For a given `Traversal_[S, T, A, B]` it takes two functions as arguments,
`view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`.

```scala
object Traversal_ {
  def apply[S, T, A, B](get: S => A)(set: S => B => T): Traversal_[S, T, A, B]
}
```

`Traversal[S, A]` is constructed using the <a href="../../api/proptics/Traversal$">Traversal[S, A]#apply</a> function.</br>
For a given `Traversal[S, A]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object Traversal {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A]
}
```

Most of the time we will be dealing with collections. This is the way to create a `Traversal[S, A]` for a collection:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.Traversal
// import proptics.Traversal 

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: Int => Option[Int] = i => if (i % 2 === 0) i.some else none[Int]
// optionByPredicate: Int => Option[Int] = $Lambda$12694/0x0000000802bfc3e0@5f962a8b

val listTraversal: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]
// listTraversal: proptics.Traversal[List[Int],Int] = proptics.Traversal_$$anon$12@4e2da5c7
```

## Common functions of a Traversal

#### viewAll
```scala
listTraversal.viewAll(list)
// res0: List[Int] = List(1, 2, 3, 4)
```

#### set
```scala
listTraversal.set(9)(list)
// res1: List[Int] = List(9, 9, 9, 9)
```

#### preview/first
```scala
listTraversal.preview(list)
// res2: Option[Int] = Some(1)

// synonym for preview
listTraversal.first(list) 
// res3: Option[Int] = Some(1)
```

#### over
```scala
listTraversal.over(_ + 1)(list)
// res4: List[Int] = List(2, 3, 4, 5)
```

#### traverse

```scala
import cats.instances.option._ // summons an Applicative[Option] instance
// import cats.instances.option._

listTraversal.traverse(list)(optionByPredicate)
// res5: Option[List[Int]] = None

listTraversal.traverse(list)(_.some)
// res6: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listTraversal.foldMap(list)(optionByPredicate)
// res7: Option[Int] = Some(6)
```

#### foldRight

```scala
listTraversal.foldRight(list)(List.empty[Int])(cons)
// res8: List[Int] = List(1, 2, 3, 4)
```

#### foldLeft

```scala
listTraversal.foldLeft(list)(List.empty[Int])((b, a) => a :: b)
// res9: List[Int] = List(4, 3, 2, 1)
```

#### forall

```scala
listTraversal.forall(_ < 9)(list)
// res10: Boolean = true
```

#### exists

```scala
listTraversal.exists(_ < 9)(list)
// res11: Boolean = true
```

#### contains

```scala
listTraversal.contains(9)(list)
// res12: Boolean = false
```

#### isEmpty

```scala
listTraversal.isEmpty(list)
// res13: Boolean = false
```

#### find

```scala
listTraversal.find(_ === 9)(list)
// res14: Option[Int] = None
```

#### last

```scala
listTraversal.last(list)
// res15: Option[Int] = Some(4)
```

#### minimum

```scala
listTraversal.minimum(list)
// res16: Option[Int] = Some(1)
```

#### maximum

```scala
listTraversal.maximum(list)
// res17: Option[Int] = Some(4)
```

## Syntax

We can take advantage of the `syntax` package for `Traversal`.
If we create a polymorphic `Traversal_[F[G[A]], F[A], G[A], A]` such that the source would be a nested structure of `F[G[A]]` and  the initial foci `G[A]` would have an `Applicative[G]` instance, and the modified foci would be an `A` , then we could
flip types `F[_]` and `G[_]` from `F[G[A]]` to `G[F[A]]` using the `sequence` method.<br/>
Consider `F[_]` to be a `List`, `G[_]` to be an `Option` and `A` to be an `Int`, the `Traversal` would be:

```scala
Traversal_[List[Option[Int]], List[Int], Option[Int], Int]
``` 

```scala
import cats.syntax.eq._
// import cats.syntax.eq._

import cats.syntax.option._
// import cats.syntax.option._

import cats.instances.list._
// import cats.instances.list._

import proptics.Traversal_
// import proptics.Traversal_

import proptics.syntax.traversal._
// import proptics.syntax.traversal._

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: Int => Option[Int] = i => if (i % 2 === 0) i.some else none[Int]
// optionByPredicate: Int => Option[Int] = $Lambda$34954/0x0000000802317840@5d5007af

val listOfOptions: List[Option[Int]] = list.map(optionByPredicate)
// listOfOptions: List[Option[Int]] = List(None, Some(2), None, Some(4))

val listOfSomeOptions: List[Option[Int]] = list.map(_.some)
// listOfSomeOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4))

val traversal: Traversal_[List[Option[Int]], List[Int], Option[Int], Int] = 
  Traversal_.fromTraverse[List, Option[Int], Int]
// traversal: Traversal_[List[Option[Int]],List[Int],Option[Int],Int] = Traversal_$$anon$12@65892367
```

#### sequence

```scala
traversal.sequence(listOfOptions)
// res0: Option[List[Int]] = None

traversal.sequence(listOfSomeOptions)
// res1: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

## Laws

A `Traversal` must satisfy all <a href="../../api/proptics/law/TraversalLaws">TraversalLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.instances.list._
// import cats.instances.list._

import cats.syntax.eq._
// import cats.syntax.eq._

import cats.{Applicative, Eq}
// import cats.{Applicative, Eq}

import proptics.Traversal
// import proptics.Traversal
```

#### Traversing with "empty" handler shouldn't change anything

```scala
def respectPurity[F[_]: Applicative, S, A](traversal: Traversal[S, A], s: S)
                                          (implicit ev: Eq[F[S]]): Boolean =
  traversal.traverse[F](s)(Applicative[F].pure _) === Applicative[F].pure(s)

val listTraversal = Traversal.fromTraverse[List, Int]
// listTraversal: proptics.Traversal[List[Int],Int] = proptics.Traversal_$$anon$12@4fb75a8c

respectPurity(listTraversal, List.range(1, 5))
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[S: Eq, A](traversal: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
  (traversal.over(f) compose traversal.over(g))(s) === traversal.over(f compose g)(s)

consistentFoci[List[Int], Int](listTraversal, List.range(1, 5), _ + 1, _ * 2)
// res1: Boolean = true
```