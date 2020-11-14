---
id: indexed-traversal
title: IndexedTraversal
---

An `IndexedTraversal` is an optic used to focus on zero, one, or many values and their indices.</br>
`IndexedTraversal` is usually used for collections like `List`, `Map`, `Array`.

## IndexedTraversal internal encoding

#### Polymorphic IndexedTraversal 

```scala
IndexedTraversal_[I, S, T, A, B]
```

`IndexedTraversal_[I, S, T, A, B]` is a function `Indexed[P, I, A, B] => P[S, T]` that takes</br>  a [Wander](/Proptics/docs/profunctors/wander) of P[_, _].

```scala
/**
  * @tparam I the index of an IndexedTraversal_
  * @tparam S the source of an IndexedTraversal_
  * @tparam T the modified source of an IndexedTraversal_
  * @tparam A the foci of an IndexedTraversal_
  * @tparam B the modified foci of an IndexedTraversal_
  */
abstract class IndexedTraversal_[I, S, T, A, B] {
  private[proptics] def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]
}
```

`IndexedTraversal_[I, S, T, A, B]` changes its foci from `A` to `B`, resulting in a change of structure</br> from `S` to `T`.<br/>
 A `IndexedTraversal` that changes its foci/structure, is called `Polymorphic IndexedTraversal`.

#### Monomorphic IndexedTraversal

```scala
IndexedTraversal[I, S, A]
```

`IndexedTraversal[I, S, A]` is a type alias for `IndexedTraversal_[I, S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.

```scala
type IndexedTraversal[I, S, A] = IndexedTraversal_[I, S, S, A, A]
``` 

`IndexedTraversal[I, S, A]` means that the type `S` might contain zero, one, or many values of type `A`, and the `I` is the index or the location of the focus.</br>
An `IndexedTraversal` that does not change its foci/structure, is called `Monomorphic IndexedTraversal`.

## Constructing IndexedTraversals

`IndexedTraversal_[I, S, T, A, B]` can be constructed using the [IndexedTraversal_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedTraversal_$.html) function.
For a given `IndexedTraversal_[I, S, T, A, B]` it takes two functions as arguments,</br>
`view: S => (I, A)` which is a getter function, that produces zero, one, or many elements of `A`, each tupled with its index `I` given an `S`, 
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`.

```scala
object IndexedTraversal_ {
  def apply[I, S, T, A, B](get: S => (I, A))(set: S => B => T): IndexedTraversal_[I, S, T, A, B]
}
```

`IndexedTraversal[I, S, A]` can be constructed using the [IndexedTraversal[I, S, A]#apply](/Proptics/api/proptics/IndexedTraversal$.html) function.</br>
For a given `IndexedTraversal[S, A]` it takes two functions as arguments, `view: S => (I, A)` which is a getter function, that produces zero, one, or many elements of `A`, each tupled with its index `I` given an `S`, 
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object IndexedTraversal {
  def apply[S, A](get: S => (I, A))(set: S => A => S): IndexedTraversal[I, S, A]
}
```

Most of the time you will be dealing with collections. We can use the `fromTraverse` method to create an `IndexedTraversal[I, S, A]` for a collection.
Consider the case of a `List`. In order to use the `IndexedTraversal[I, S, A]` we need to create a list containing tuples of an element with its index

```scala
List("A", "B", "C").zipWithIndex.map(_.swap)
res0: List[(Int, String)] = List((0,A), (1,B), (2,C))
```

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.{IndexedTraversal, IndexedTraversal_}
// import proptics.IndexedTraversal 

val listWithIdx: List[(Int, Int)] = List.range(1, 5).zipWithIndex.map(_.swap)
// listWithIdx: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

val optionByPredicate: ((Int, Int)) => Option[Int] = 
  { case (i, a) => if (i % 2 === 0) a.some else none[Int] }
// optionByPredicate: ((Int, Int)) => Option[Int] = $Lambda$10219/0x0000000802b8d040@e88bac3

val listIndexedTraversal: IndexedTraversal_[Int, List[(Int, Int)], List[Int], Int, Int] =
  IndexedTraversal.fromTraverse[List, Int, Int]
// listIndexedTraversal: proptics.IndexedTraversal_[Int,List[(Int, Int)],List[Int],Int,Int] = 
//   proptics.IndexedTraversal_$$anon$8@4134b784
```

If our collection has an index of type `Int`, we can use the `fromIndexableTraverse` method to create an `IndexedTraversal[Int, S, A]`, which
will handle the `zipWithIndex` internally, thus allowing us to pass a regular `List[A]`

```scala
import proptics.IndexedTraversal
// import proptics.IndexedTraversal 

val seasons: List[String] = List("Summer", "Fall", "Winter", "Spring")
// seasons: List[String] = List(Summer, Fall, Winter, Spring)

val fromIdxTraverse: IndexedTraversal[Int, List[String], String] =
  IndexedTraversal.fromIndexableTraverse[List, String]
// fromIdxTraverse: proptics.IndexedTraversal[Int,List[String],String] = 
//   proptics.IndexedTraversal_$$anon$8@2245a36a

fromIdxTraverse.preview(seasons)
// res0: Option[(Int, String)] = Some((0,Summer))
```

## Common functions of a IndexedTraversal

#### viewAll
```scala
listIndexedTraversal.viewAll(listWithIdx)
// res0: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))
```

#### set
```scala
listIndexedTraversal.set(listWithIdx)
// res1: List[Int] = List(9, 9, 9, 9)
```

#### preview/first
```scala
listIndexedTraversal.set(listWithIdx)
// res2: Option[(Int, Int)] = Some((0,1))

// synonym for preview
listIndexedTraversal.first(listWithIdx) 
// res3: Option[(Int, Int)] = Some((0,1))
```

#### over
```scala
listIndexedTraversal.over(_._2 + 1)(listWithIdx)
// res4: List[Int] = List(2, 3, 4, 5)
```

#### traverse

```scala
import cats.instances.option._ // summons an Applicative[Option] instance
// import cats.instances.option._

listIndexedTraversal.traverse(listWithIdx)(optionByPredicate)
// res5: Option[List[Int]] = None

listIndexedTraversal.traverse(listWithIdx)(_._2.some)
// res6: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listIndexedTraversal.foldMap(listWithIdx)(optionByPredicate)
// res7: Option[Int] = Some(4)
```

#### foldr

```scala
val cons: (Int, List[Int]) => List[Int] = _ * 2 :: _
// cons: (Int, List[Int]) => List[Int] = $Lambda$5802/0x0000000801e68040@11044fd5

listIndexedTraversal.foldr(listWithIdx)(List.empty[(Int, Int)])(_ :: _)
// res8: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))
```

#### foldl

```scala
listIndexedTraversal.foldl(listWithIdx)(List.empty[(Int, Int)])((xs, x) => x :: xs)
// res9: List[(Int, Int)] = List((3,4), (2,3), (1,2), (0,1))
```

#### forall

```scala
listIndexedTraversal.forall(_ < 9)(listWithIdx)
// res10: Boolean = true
```

#### exists

```scala
listIndexedTraversal.forall(_._2 < 9)(listWithIdx)
// res11: Boolean = true
```

#### contains

```scala
listIndexedTraversal.contains((0, 9))(listWithIdx)
// res12: Boolean = false

listIndexedTraversal.contains((1, 2))(listWithIdx)
// res13: Boolean = true
```

#### isEmpty

```scala
listIndexedTraversal.isEmpty(listWithIdx)
// res14: Boolean = false
```

#### find

```scala
listIndexedTraversal.find(_._2 === 9)(listWithIdx)
// res15: Option[Int] = None
```

#### last

```scala
listIndexedTraversal.last(listWithIdx)
// res16: Option[Int] = Some((3, 4))
```

#### minimum

```scala
listIndexedTraversal.minimum(listWithIdx)
// res17: Option[Int] = Some(1)
```

#### maximum

```scala
listIndexedTraversal.maximum(listWithIdx)
// res18: Option[Int] = Some(4)
```

## Syntax

We can take advantage of the `syntax` package for `IndexedTraversal`.
If we create a polymorphic `IndexedTraversal_[I, F[G[A]], F[A], G[A], A]` such that the source would be a nested structure of `F[G[A]]` and  the initial foci `G[A]` would have an `Applicative[G]` instance, and the modified foci would be an `A` , then we could
flip types `F[_]` and `G[_]` from `F[G[A]]` to `G[F[A]]` using the `sequence` method.<br/>
Consider `F[_]` to be a `List`, `G[_]` to be an `Option` and `A` to be an `Int`, the `IndexedTraversal` would be:

```scala
IndexedTraversal_[Int, List[Option[Int]], List[Int], Option[Int], Int]
``` 

```scala
import cats.syntax.eq._
// import cats.syntax.eq._

import cats.syntax.option._
// import cats.syntax.option._

import cats.instances.list._
// import cats.instances.list._

import proptics.IndexedTraversal_
// import proptics.IndexedTraversal_

import proptics.syntax.indexedTraversal._
// import proptics.syntax.indexedTraversal._

val listWithIdx: List[(Int, Int)] = List.range(1, 5).zipWithIndex.map(_.swap)
// listWithIdx: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

val optionByPredicate: ((Int, Int)) => (Int, Option[Int]) = {
  case (i, a) => if (i % 2 === 0) (i, a.some) else (i, none[Int])
}
// optionByPredicate: ((Int, Int)) => (Int, Option[Int]) = $Lambda$11359/0x00000008026ce840@5eeb22a9

val listOfOptions = listWithIdx.map(optionByPredicate)
// listOfOptions: List[Option[(Int, Int)]] = List(Some((0,1)), None, Some((2,3)), None)

val listOfSomeOptions: List[(Int, Option[Int])] = listWithIdx.map { case (i, a) => (i, a.some) }
// listOfSomeOptions: List[(Int, Option[Int])] = 
//   List((0,Some(1)), (1,Some(2)), (2,Some(3)), (3,Some(4)))

val indexedTraversal: IndexedTraversal_[Int, List[(Int, Option[Int])], List[Int], Option[Int], Int] =
  IndexedTraversal_.fromTraverse[List, Int, Option[Int], Int]
// indexedTraversal: IndexedTraversal_[Int,List[(Int, Option[Int])],List[Int],Option[Int],Int] = 
//   proptics.IndexedTraversal_$$anon$8@50ebce6a
```

#### sequence

```scala
indexedTraversal.sequence(listOfOptions)
// res0: Option[List[Int]] = None

indexedTraversal.sequence(listOfSomeOptions)
// res1: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

## Laws

An `IndexedTraversal` must satisfy all [IndexedTraversalLaws](/Proptics/api/proptics/law/IndexedTraversalLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

```scala
import cats.instances.list._
// import cats.instances.list._

import cats.syntax.eq._
// import cats.syntax.eq._

import cats.{Applicative, Eq}
// import cats.{Applicative, Eq}

import proptics.IndexedTraversal
// import proptics.IndexedTraversal

val nel = NonEmptyList.fromListUnsafe(List(1, 2, 3, 4, 5, 6))
// nel: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3, 4, 5, 6)

val headIndexedTraversal: IndexedTraversal[Int, NonEmptyList[Int], Int] =
  IndexedTraversal[Int, NonEmptyList[Int], Int](nel => (0, nel.head))(nel => i => nel.copy(head = i))
// headIndexedTraversal: proptics.IndexedTraversal[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.IndexedTraversal_$$anon$9@1e81167a
```

#### Traversing with "empty" handler shouldn't change anything

```scala
def respectPurity[F[_]: Applicative, S, A](indexedTraversal: IndexedTraversal[I, S, A], s: S)
                                          (implicit ev: Eq[F[S]]): Boolean =
  indexedTraversal.traverse[F](s)(Applicative[F].pure _) === Applicative[F].pure(s)

respectPurity[Id, Int, NonEmptyList[Int], Int](nel, headIndexedTraversal)
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[I, S: Eq, A](s: S, f: (I, A) => A, 
                                g: (I, A) => A, 
                                indexedTraversal: IndexedTraversal[I, S, A]): Boolean =
  (indexedTraversal.overF[Id](f.tupled) _ compose indexedTraversal.overF[Id](g.tupled))(s) ===
    indexedTraversal.overF[Id]({ case (i, a) => f(i, g(i, a)) })(s)

consistentFoci(nel, (_, a) => a + 1, (_, a) => a * 2, headIndexedTraversal)
// res1: Boolean = true
```




