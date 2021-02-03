---
id: indexed-fold
title: IndexedFold
---

An `IndexedFold` describes how to retrieve multiple values, each with its index. It is similar to an <a href="/Proptics/docs/indexed-optics/indexed-traversal" target="_blank">IndexedTraversal</a>, but it 
cannot modify its foci. Everything you can do with a Foldable, you can do with an `IndexedFold`.

## IndexedFold internal encoding

#### Polymorphic IndexedFold 

```scala
IndexedFold_[I, S, T, A, B]
```

`IndexedFold_[I, S, T, A, B]` is a function `Indexed[Forget[R, *, *], I, A, B] => Forget[R, S, T]`. [Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets the `B` value and returns an accumulated value of type `R`.

```scala
/**
  * @tparam I the index of an IndexedFold_
  * @tparam S the source of an IndexedFold_
  * @tparam T the modified source of an IndexedFold_
  * @tparam A the foci of an IndexedFold_
  * @tparam B the modified foci of an IndexedFold_
  */
abstract class IndexedFold_[I, S, T, A, B] extends Serializable { self =>
  def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, S, T]
}
```

Although `IndexedFold_[I, S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic IndexedFold`, serving as
a base type from which a `Monomorphic IndexedFold` can be obtained.

#### Monomorphic IndexedFold

```scala
IndexedFold[I, S, A]
```

`IndexedFold[I, S, A]` is a type alias for `IndexedFold_[I, S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.

```scala
type IndexedFold[I, S, A] = IndexedFold_[I, S, S, A, A]
``` 

Since `IndexedFold_` is read-only, `IndexedFold[I, S, A]` is isomorphic to `IndexedFold_[I, S, T, A, B]`.</br>
`IndexedFold_[I, S, T, A, B]` takes an `Indexed[Forget[R, *, *], I, A, B]` and returns</br> `Forget[R, S, T]`.
`Indexed[Forget[R, *, *], I, A, B]` wraps a value of  `Forget[R, (A, I), B]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[R, (A, I), B] => Forget[R, S, T]
``` 

`Forget[R, (A, I), B]` wraps a function `(A, I) => R` and forgets its last type parameter `B`.</br>
`Forget[R, S, T]` wraps a function `S => R` and forgets its last type parameter `T`, 
so the representation of can be even more simplified to:

```scala
((A, I) => R) => S => R
```

Let's compare it to `IndexedFold_[I, S, S, A, A]` which is equivalent to `IndexedFold[I, S, A]`.</br> 

```scala
def ifold[I, S, A]: IndexedFold_[I, S, S, A, A] = new IndexedFold[I, S, A] {
  def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, A]): Forget[R, S, S]
}
```
`IndexedFold_[I, S, S, A, A]` takes `Indexed[Forget[R, *, *], I, A, A]` and returns `Forget[R, S, S]`.</br>
`Indexed[Forget[R, *, *], I, A, A]` wraps a value of  `Forget[R, (A, I), A]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[R, (A, I), A] => Forget[R, S, S]
``` 

`Forget[R, (A, I), A]` wraps a function `(A, I) => R` and forgets its last type parameter `A`.</br>
`Forget[R, S, S]` wraps a function `S => R` and forgets its last type parameter `S`, 
so the representation of can be even more simplified to:

```scala
((A, I) => R) => S => R
```

## Constructing IndexedFolds

`IndexedFold_[I, S, T, A, B]` is constructed using the [IndexedFold_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedFold_$.html) function.</br>
For a given `IndexedFold_[I, S, T, A, B]` takes a fold function `S => (A, I)` as an argument.

```scala
object IndexedFold_ {
  def apply[I, S, T, A, B](get: S => (A, I)): IndexedFold_[I, S, T, A, B]
}
```

`IndexedFold[I, S, A]` is constructed using the [IndexedFold[I, S, A]#apply](/Proptics/api/proptics/IndexedFold$.html) function.</br>
For a given `IndexedFold[I, S, A]` it takes a fold function `S => (A, I)` as argument.

```scala
object IndexedFold {
  def apply[I, S, A](get: S => (A, I)): IndexedFold[I, S, A]
}
```

Most of the time we will be dealing with `Foldable` and `FoldableWithIndex` types.<br/>
This is how we can create an `IndexedFold` for a `Foldable` type, which has an `Int` index:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.IndexedFold
// import proptics.IndexedFold

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: ((Int, Int)) => Option[Int] = {
  case (a, i) => if (i % 2 === 0) a.some else none[Int]
}
// optionByPredicate: ((Int, Int)) => Option[Int] = $Lambda$13713/0x0000000802548840@16fd17e7

val listIndexedFold: IndexedFold[Int, List[Int], Int] = IndexedFold.fromFoldable[List, Int]
// listIndexedFold: proptics.Fold[List[Int],Int] = proptics.Fold_$$anon$10@6f126b09
```

This is how we can create an `IndexedFold` for a `FoldableWithIndex` for a` Map[String, List[String]]`:

```scala
import proptics.instances.foldableWithIndex._
// import proptics.instances.foldableWithIndex._

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

val mapIndexedFold = IndexedFold.fromFoldableWithIndex[Map[String, *], String, List[String]]
//  mapIndexedFold: proptics.IndexedFold[String,immutable.Map[String,List[String]],List[String]] = 
//    proptics.IndexedFold$$anon$30@7fa157af

def bbIsFirst(list: List[String]): Boolean = list.headOption.exists(_ === "Breaking Bad")
// bbIsFirst(list: List[String]): Boolean
```

## Common functions of an IndexedFold

#### viewAll
```scala
listIndexedFold.viewAll(list)
// res0: List[(Int, Int)] = List((1,0), (2,1), (3,2), (4,3))

mapIndexedFold.viewAll(seriesMap)
// res1: List[(List[String], String)] = 
//   List((List(True Detective, Fargo, Dexter),tt0903747), 
//        (List(Breaking Bad, Fargo, Dexter),tt2356777), 
//        (List(Breaking Bad, True Detective, Dexter),tt2802850), 
//        (List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### preview/first
```scala
listIndexedFold.preview(list)
// res2: Option[(Int, Int)] = Some((1,0))

// synonym for preview
listIndexedFold.first(list) 
// res3: Option[(Int, Int)] = Some((1,0))

mapIndexedFold.preview(seriesMap)
// res4 : Option[(List[String], String)] = 
//   Some((List(True Detective, Fargo, Dexter),tt0903747))
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listIndexedFold.foldMap(list)(optionByPredicate)
// res5: Option[Int] = Some(4)

mapIndexedFold.foldMap[Map[String, List[String]]](seriesMap) { case (ls, k) =>
  if (bbIsFirst(ls)) Map(k -> ls) else emptyMap
}
// res6: Map[String,List[String]] = 
//   Map(tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo)) 
```

#### foldRight

```scala
listIndexedFold.foldRight(list)(List.empty[(Int, Int)])(_ :: _)
// res7: List[(Int, Int)] = List((1,0), (2,1), (3,2), (4,3))

mapIndexedFold.foldRight[Map[String, List[String]]](seriesMap)(emptyMap) { case ((ls, k), map) =>
  (if (bbIsFirst(ls)) Map(k -> ls) else emptyMap) |+| map
}
// res8: Map[String,List[String]] = 
//   Map(tt0773262 -> List(Breaking Bad, True Detective, Fargo), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter))
```

#### foldLeft

```scala
listIndexedFold.foldLeft(list)(List.empty[(Int, Int)])((xs, x) => x :: xs)
// res9: List[(Int, Int)] = List((4,3), (3,2), (2,1), (1,0))

mapIndexedFold.foldLeft[Map[String, List[String]]](seriesMap)(emptyMap) { case (map, (ls, k)) =>
  (if (bbIsFirst(ls)) Map(k -> ls) else emptyMap) |+| map
}
// res10: Map[String,List[String]] = 
//   Map(tt2356777 -> List(Breaking Bad, Fargo, Dexter, 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter),
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))
```

#### forall

```scala
listIndexedFold.forall(_._1 < 9)(list)
// res11: Boolean = true

mapIndexedFold.forall(pair => bbIsFirst(pair._1))(seriesMap)
// res12: Boolean = false
```

#### exists

```scala
listIndexedFold.exists(_._1 < 9)(list)
// res13: Boolean = true

mapIndexedFold.exists(pair => bbIsFirst(pair._1))(seriesMap)
// res14: Boolean = true
```

#### contains

```scala
listIndexedFold.contains((0, 9))(list)
// res15: Boolean = false

mapIndexedFold.contains((List("Fargo"), "tt0773262"))(seriesMap)
// res16: Boolean = false
```

#### isEmpty

```scala
listIndexedFold.isEmpty(list)
// res17: Boolean = false

mapIndexedFold.isEmpty(seriesMap)
// res18: Boolean = false
```

#### find

```scala
listIndexedFold.find(_._1 === 9)(list)
// res19: Option[(Int, Int)] = None

mapIndexedFold.find(pair => bbIsFirst(pair._1))(seriesMap)
// res20: Option[(List[String], String)] = Some((List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### last

```scala
listIndexedFold.last(list)
// res21: Option[(Int, Int)] = Some((4,3))

mapIndexedFold.last(seriesMap)
// res22: Option[(List[String], String)] = 
//   Some((List(Breaking Bad, True Detective, Fargo),tt0773262))
```

#### minimum

```scala
listIndexedFold.minimum(list)
// res23: Option[Int] = Some(1)
```

#### maximum

```scala
listIndexedFold.maximum(list)
// res24: Option[Int] = Some(4)
```


## Laws

Since a `IndexedFold` cannot be used to write back there are no Lens laws that apply to it.

