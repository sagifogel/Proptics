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
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Wander[P]): P[S, T]
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

`IndexedTraversal_[I, S, T, A, B]` is constructed using the [IndexedTraversal_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedTraversal_$.html) function.
For a given `IndexedTraversal_[I, S, T, A, B]` it takes two functions as arguments,</br>
`view: S => (A, I)` which is a getter function, that produces zero, one, or many elements of `A`, each tupled with its index `I` given an `S`, 
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`.

```scala
object IndexedTraversal_ {
  def apply[I, S, T, A, B](get: S => (A, I))(set: S => B => T): IndexedTraversal_[I, S, T, A, B]
}
```

`IndexedTraversal[I, S, A]` is constructed using the [IndexedTraversal[I, S, A]#apply](/Proptics/api/proptics/IndexedTraversal$.html) function.</br>
For a given `IndexedTraversal[S, A]` it takes two functions as arguments, `view: S => (A, I)` which is a getter function, that produces zero, one, or many elements of `A`, each tupled with its index `I` given an `S`, 
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object IndexedTraversal {
  def apply[S, A](get: S => (A, I))(set: S => A => S): IndexedTraversal[I, S, A]
}
```

Most of the time we will be dealing with collections. We can use the `fromTraverse` and `fromTraverseWithIndex` methods.</br>
This is how we can create an `IndexedTraversal` for a `Traverse` type, which has an `Int` index:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.{IndexedTraversal, IndexedTraversal_}
// import proptics.IndexedTraversal 

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)))

val optionByPredicate: ((Int, Int)) => Option[Int] = 
  { case (i, a) => if (i % 2 === 0) a.some else none[Int] }
// optionByPredicate: ((Int, Int)) => Option[Int] = $Lambda$10219/0x0000000802b8d040@e88bac3

val fromTraverse: IndexedTraversal[Int, List[Int], Int] =
 IndexedTraversal.fromTraverse[List, Int]
// fromTraverse: proptics.IndexedTraversal[Int,List[Int],Int] = 
//   proptics.IndexedTraversal_$$anon$8@2245a36a
```

This is how we can create an `IndexedTraversal` for a `TraverseWithIndex` type:

```scala
import proptics.instances.traverseWithIndex._
// import proptics.instances.traverseWithIndex._

import cats.syntax.eq._
// import cats.syntax.eq._

import cats.syntax.semigroup._
// import cats.syntax.semigroup._

val emptyMap = Map.empty[String, List[String]]
// emptyMap: scala.collection.immutable.Map[String,List[String]] = Map()

val seriesMap: Map[String, List[String]] = Map[String, List[String]](
  "tt0903747" -> List("True Detective", "Fargo", "Dexter"),
  "tt2356777" -> List("Breaking Bad", "Fargo", "Dexter"),
  "tt2802850" -> List("Breaking Bad", "True Detective", "Dexter"),
  "tt0773262" -> List("Breaking Bad", "True Detective", "Fargo")
)
// seriesMap: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))

val mapIndexedTraverse =
  IndexedTraversal.fromTraverseWithIndex[Map[String, *], String, List[String]]
// mapIndexedTraverse: 
//   proptics.IndexedTraversal[String,immutable.Map[String,List[String]],List[String]] = 
//     proptics.IndexedTraversal_$$anon$28@58891f02

def bbIsFirst(list: List[String]): Boolean = list.headOption.exists(_ === "Breaking Bad")
// bbIsFirst(list: List[String]): Boolean
```

## Common functions of a IndexedTraversal

#### viewAll
```scala
listIndexedTraversal.viewAll(list)
// res0: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

mapIndexedTraverse.viewAll(seriesMap)
// res1: List[(List[String], String)] = 
//   List((List(True Detective, Fargo, Dexter),tt0903747), 
//        (List(Breaking Bad, Fargo, Dexter),tt2356777), 
//        (List(Breaking Bad, True Detective, Dexter),tt2802850), 
//        (List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### set
```scala
listIndexedTraversal.set(list)
// res2: List[Int] = List(9, 9, 9, 9)

mapIndexedTraverse.set(List.empty)(seriesMap)
// res3: scala.collection.immutable.Map[String,List[String]] = 
//   Map(tt0903747 -> List(), 
//       tt2356777 -> List(), 
//       tt2802850 -> List(), 
//       tt0773262 -> List())
```

#### preview/first
```scala
listIndexedTraversal.set(list)
// res4: Option[(Int, Int)] = Some((0,1))

// synonym for preview
listIndexedTraversal.first(list) 
// res5: Option[(Int, Int)] = Some((0,1))

mapIndexedTraverse.preview(list)
// res6: Option[(List[String], String)] = 
//   Some((List(True Detective, Fargo, Dexter),tt0903747))
```

#### over
```scala
listIndexedTraversal.over(_._2 + 1)(list)
// res7: List[Int] = List(2, 3, 4, 5)

mapIndexedTraverse.over { case (ls, _) => ls.take(1) }(seriesMap)
// res8: scala.collection.immutable.Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective), 
//       tt2356777 -> List(Breaking Bad), 
//       tt2802850 -> List(Breaking Bad), 
//       tt0773262 -> List(Breaking Bad))
```

#### traverse

```scala
import cats.instances.option._ // summons an Applicative[Option] instance
// import cats.instances.option._

listIndexedTraversal.traverse(list)(optionByPredicate)
// res9: Option[List[Int]] = None

listIndexedTraversal.traverse(list)(_._2.some)
// res10: Option[List[Int]] = Some(List(1, 2, 3, 4))

mapIndexedTraverse.traverse(seriesMap) { 
 case (ls, _) => ls.headOption.map(List(_)) 
}
// res11: Option[scala.collection.immutable.Map[String,List[String]]] = 
//   Some(Map(tt0903747 -> List(True Detective), 
//            tt2356777 -> List(Breaking Bad), 
//            tt2802850 -> List(Breaking Bad), 
//            tt0773262 -> List(Breaking Bad)))

mapIndexedTraverse.traverse[Option](seriesMap) { case (ls, _) =>
  ls.headOption.filter(_ === "Breaking Bad").map(List(_))
}
// res12: Option[scala.collection.immutable.Map[String,List[String]]] = None
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listIndexedTraversal.foldMap(list)(optionByPredicate)
// res13: Option[Int] = Some(4)

mapIndexedTraverse.foldMap[Map[String, List[String]]](seriesMap) { case (ls, k) =>
  if (bbIsFirst(ls)) Map(k -> ls) else emptyMap
}
// res14: Map[String,List[String]] = 
//   Map(tt0773262 -> List(Breaking Bad, True Detective, Fargo), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter))
```

#### foldRight

```scala
listIndexedTraversal.foldRight(list)(List.empty[(Int, Int)])(_ :: _)
// res15: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

mapIndexedTraverse.foldRight[Map[String, List[String]]](seriesMap)(emptyMap) { case ((ls, k), map) =>
 (if (bbIsFirst(ls)) Map(k -> ls) else emptyMap) |+| map
}
// res16: Map[String,List[String]] = 
//   Map(tt0773262 -> List(Breaking Bad, True Detective, Fargo), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter))
```

#### foldLeft

```scala
listIndexedTraversal.foldLeft(list)(List.empty[(Int, Int)])((xs, x) => x :: xs)
// res17: List[(Int, Int)] = List((3,4), (2,3), (1,2), (0,1))

mapIndexedTraverse.foldLeft[Map[String, List[String]]](seriesMap)(emptyMap) { case (map, (ls, k)) =>
 (if (bbIsFirst(ls)) Map(k -> ls) else emptyMap) |+| map
}
// res18: Map[String,List[String]] = 
//   Map(tt2356777 -> List(Breaking Bad, Fargo, Dexter, 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter),
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))
```

#### forall

```scala
listIndexedFold.forall(_._2 < 9)(list)
// res19: Boolean = true

mapIndexedTraverse.forall(pair => bbIsFirst(pair._1))(seriesMap)
// res20: Boolean = false
```

#### exists

```scala
listIndexedTraversal.exists(_._2 < 9)(list)
// res21: Boolean = true

mapIndexedTraverse.exists(pair => bbIsFirst(pair._1))(seriesMap)
// res22: Boolean = true
```

#### contains

```scala
listIndexedTraversal.contains((0, 9))(list)
// res23: Boolean = false

listIndexedTraversal.contains((1, 2))(list)
// res24: Boolean = true

mapIndexedTraverse.contains((List("Fargo"), "tt0773262"))(seriesMap)
// res25: Boolean = false
```

#### isEmpty

```scala
listIndexedTraversal.isEmpty(list)
// res26: Boolean = false

mapIndexedTraverse.isEmpty(seriesMap)
// res27: Boolean = false
```

#### find

```scala
listIndexedTraversal.find(_._2 === 9)(list)
// res28: Option[Int] = None

mapIndexedTraverse.find(pair => bbIsFirst(pair._1))(seriesMap)
// res29: Option[(List[String], String)] = Some((List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### last

```scala
listIndexedTraversal.last(list)
// res30: Option[Int] = Some((3, 4))

mapIndexedTraverse.last(seriesMap)
// res31: Option[(List[String], String)] = 
//   Some((List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### minimum

```scala
listIndexedTraversal.minimum(list)
// res32: Option[Int] = Some(1)
```

#### maximum

```scala
listIndexedTraversal.maximum(list)
// res33: Option[Int] = Some(4)
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

val list: List[(Int, Int)] = List.range(1, 5).zipWithIndex.map(_.swap)
// list: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

val optionByPredicate: ((Int, Int)) => (Int, Option[Int]) = {
  case (i, a) => if (i % 2 === 0) (i, a.some) else (i, none[Int])
}
// optionByPredicate: ((Int, Int)) => (Int, Option[Int]) = $Lambda$11359/0x00000008026ce840@5eeb22a9

val listOfOptions = list.map(optionByPredicate)
// listOfOptions: List[Option[(Int, Int)]] = List(Some((0,1)), None, Some((2,3)), None)

val listOfSomeOptions: List[(Int, Option[Int])] = list.map { case (i, a) => (i, a.some) }
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
def respectPurity[F[_]: Applicative, I, S, A](indexedTraversal: IndexedTraversal[I, S, A], s: S)
                                          (implicit ev: Eq[F[S]]): Boolean =
 indexedTraversal.traverse[F](s) { case (a, _) => Applicative[F].pure(a) } === Applicative[F].pure(s)

respectPurity[Id, Int, NonEmptyList[Int], Int](headIndexedTraversal, nel)
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[I, S: Eq, A](s: S, f: (A, I) => A, g: (A, I) => A, indexedTraversal: IndexedTraversal[I, S, A]): Boolean =
  (indexedTraversal.over(f.tupled) compose indexedTraversal.over(g.tupled))(s) ===
    indexedTraversal.over { case (a, i) => f(g(a, i), i) }(s)

consistentFoci[Int, NonEmptyList[Int], Int](nel, (a, _) => a + 1, (a, _) => a * 2, headIndexedTraversal)
// res1: Boolean = true
```




