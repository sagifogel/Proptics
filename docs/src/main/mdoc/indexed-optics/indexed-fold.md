---
id: indexed-fold
title: IndexedFold
---

An `IndexedFold` describes how to retrieve multiple values, each with its index. It is similar to a <a href="/Proptics/docs/indexed-optics/indexed-traversal" target="_blank">IndexedTraversal</a>, but it 
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

Although `IndexedFold_[S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic IndexedFold`, serving as
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
`Indexed[Forget[R, *, *], I, A, B]` wraps a value of  `Forget[R, (I, A), B]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[R, (I, A), B] => Forget[R, S, T]
``` 

`Forget[R, (I, A), B]` wraps a function `(I, A) => R` and forgets its last type parameter `B`.</br>
`Forget[R, S, T]` wraps a function `S => R` and forgets its last type parameter `T`, 
so the representation of can be even more simplified to:

```scala
((I, A) => R) => S => R
```

Let's compare it to `IndexedFold_[I, S, S, A, A]` which is equivalent to `IndexedFold[I, S, A]`.</br> 

```scala
def ifold[I, S, A]: IndexedFold_[I, S, S, A, A] = new IndexedFold[I, S, A] {
  def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, A]): Forget[R, S, S]
}
```
`IndexedFold_[I, S, S, A, A]` takes `Indexed[Forget[R, *, *], I, A, A]` and returns `Forget[R, S, S]`.</br>
`Indexed[Forget[R, *, *], I, A, A]` wraps a value of  `Forget[R, (I, A), A]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[R, (I, A), A] => Forget[R, S, S]
``` 

`Forget[R, (I, A), A]` wraps a function `(I, A) => R` and forgets its last type parameter `A`.</br>
`Forget[R, S, S]` wraps a function `S => R` and forgets its last type parameter `S`, 
so the representation of can be even more simplified to:

```scala
((I, A) => R) => S => R
```

## Constructing IndexedFolds

`IndexedFold_[I, S, T, A, B]` is constructed using the [IndexedFold_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedFold_$.html) function.</br>
For a given `IndexedFold_[I, S, T, A, B]` takes a fold function `S => (I, A)` as an argument.

```scala
object IndexedFold_ {
  def apply[I, S, T, A, B](get: S => (I, A)): IndexedFold_[I, S, T, A, B]
}
```

`IndexedFold[I, S, A]` is constructed using the [IndexedFold[I, S, A]#apply](/Proptics/api/proptics/IndexedFold$.html) function.</br>
For a given `IndexedFold[I, S, A]` it takes a fold function `S => (I, A)` as argument.

```scala
object IndexedFold {
  def apply[I, S, A](get: S => (I, A)): IndexedFold[I, S, A]
}
```

Most of the time we will be dealing with `Foldable` types. This is how we can create an `IndexedFold` for a `Foldable` type:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.IndexedFold
// import proptics.IndexedFold

val listWithIdx: List[(Int, Int)] = List.range(1, 5).zipWithIndex.map(_.swap)
// listWithIdx: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))

val optionByPredicate: ((Int, Int)) => Option[Int] = {
  case (i, a) => if (i % 2 === 0) a.some else none[Int]
}
// optionByPredicate: ((Int, Int)) => Option[Int] = $Lambda$13713/0x0000000802548840@16fd17e7

val listIndexedFold: Fold[List[Int], Int] = IndexedFold.fromFoldable[List, Int]
// listIndexedFold: proptics.Fold[List[Int],Int] = proptics.Fold_$$anon$10@6f126b09
```

## Common functions of an IndexedFold

#### viewAll
```scala
listIndexedFold.viewAll(listWithIdx)
// res0: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3, 4))
```

#### preview/first
```scala
listIndexedFold.preview(listWithIdx)
// res2: Option[(Int, Int)] = Some((0,1))

// synonym for preview
listIndexedFold.first(listWithIdx) 
// res3: Option[(Int, Int)] = Some((0,1))
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listIndexedFold.foldMap(listWithIdx)(optionByPredicate)
// res7: Option[Int] = Some(4)
```

#### foldr

```scala
listIndexedFold.foldr(listWithIdx)(List.empty[(Int, Int)])(_ :: _)
// res8: List[(Int, Int)] = List((0,1), (1,2), (2,3), (3,4))
```

#### foldl

```scala
listIndexedFold.foldl(listWithIdx)(List.empty[(Int, Int)])((xs, x) => x :: xs)
// res9: List[(Int, Int)] = List((3,4), (2,3), (1,2), (0,1))
```

#### forall

```scala
listIndexedFold.forall(_._2 < 9)(listWithIdx)
// res10: Boolean = true
```

#### exists

```scala
listIndexedFold.exists(_._2 < 9)(listWithIdx)
// res11: Boolean = true
```

#### contains

```scala
listIndexedFold.contains((0, 9))(listWithIdx)
// res12: Boolean = false

listIndexedFold.contains((1, 2))(listWithIdx)
// res13: Boolean = true
```

#### isEmpty

```scala
listIndexedFold.isEmpty(listWithIdx)
// res14: Boolean = false
```

#### find

```scala
listIndexedFold.find(_._2 === 9)(listWithIdx)
// res15: Option[Int] = None
```

#### last

```scala
listIndexedFold.last(listWithIdx)
// res16: Option[Int] = Some((3, 4))
```

#### minimum

```scala
listIndexedFold.minimum(listWithIdx)
// res17: Option[Int] = Some(1)
```

#### maximum

```scala
listIndexedFold.maximum(listWithIdx)
// res18: Option[Int] = Some(4)
```

## Laws

Since a `IndexedFold` cannot be used to write back there are no Lens laws that apply to it.

