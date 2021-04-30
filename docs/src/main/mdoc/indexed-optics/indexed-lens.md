---
id: indexed-lens
title: IndexedLens
---

An `IndexedLens` is an optic used to focus on a particular element, and its location, in a deeply nested data structure, while letting you 
view, set or modify the focus when you know it exists, that is an `IndexedLens` must never fail to get or modify the focus.<br/>
An intuition for `IndexedLens` is a getter and setter like you might have on an object, but tupled with its location/index.

## IndexedLens internal encoding

#### Polymorphic IndexedLens

```scala
IndexedLens_[I, S, T, A, B]
```

`IndexedLens_[I, S, T, A, B]` is a function `Indexed[P, I, A, B] => P[S, T]` that takes a [Strong](../profunctors/strong.md) of P[_, _].

```scala
/**
  * @tparam I the index of an IndexedLens_
  * @tparam S the source of an IndexedLens_
  * @tparam T the modified source of an IndexedLens_
  * @tparam A the focus of an IndexedLens_
  * @tparam B the modified focus of an IndexedLens_
  */
abstract class IndexedLens_[I, S, T, A, B] {
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Strong[P]): P[S, T]
```

`IndexedLens_[I, S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure</br> from `S` to `T`. </br>
An `IndexedLens` that changes its focus/structure, is called `Polymorphic IndexedLens`.

#### Monomorphic IndexedLens

```scala
IndexedLens[I, S, A]
```

`IndexedLens[I, S, A]` is a type alias for `IndexedLens_[I, S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type IndexedLens[I, S, A] = IndexedLens_[I, S, S, A, A]
``` 

`IndexedLens[I, S, A]` means that `S` is the structure or whole and `A` is the focus, or the part, and the `I` is the index or the location of the focus.<br/>
An `IndexedLens` that does not change its focus/structure, is called `Monomorphic IndexedLens`.

## Constructing IndexedLens

`IndexedLens_[S, T, A, B]` is constructed using the <a href="../../api/proptics/IndexedLens_$">IndexedLens_[I, S, T, A, B]#apply</a> function.</br>
For a given `IndexedLens_[I, S, T, A, B]` it takes two functions as arguments, `view: S => (A, I)` which is a getter function, that produces an `A` tupled with its index `I` given an `S`, 
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns a structure of `T`.

```scala
object IndexedLens_ {
  def apply[I, S, T, A, B](get: S => (A, I))(set: S => B => T): IndexedLens_[I, S, T, A, B]
}
```

`IndexedLens[I, S, A]` is constructed using the <a href="../../api/proptics/IndexedLens$">IndexedLens[I, S, A]#apply</a> function.</br> 
For a given `IndexedLens[I, S, A]` it takes two functions as arguments,`view: S => (A, I)` which is a getter function, that produces an `A` tupled with its index `I` given an `S`,
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a new structure `S`.

```scala
object IndexedLens {
  def apply[I, S, A](get: S => (A, I))(set: S => A => S): IndexedLens[I, S, A]
}
```

Consider the case of focusing on the head of an `NonEmptyList`

```scala
import proptics.IndexedLens
// import proptics.IndexedLens

import cats.data.NonEmptyList
// import cats.data.NonEmptyList

val nel = NonEmptyList.fromListUnsafe(List(1, 2, 3))
// nel: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3)

val headIndexedLens: IndexedLens[Int, NonEmptyList[Int], Int] = 
  IndexedLens[Int, NonEmptyList[Int], Int](nel => (nel.head, 0)) { nel => i =>
    NonEmptyList(i, nel.tail)
  }
// headIndexedLens: proptics.IndexedLens[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.IndexedLens_$$anon$9@635202f0 
```

## Common functions of an IndexedLens

#### view

```scala
headIndexedLens.view(nel)
// res0: (Int, Int) = (1,0)
```

#### set
```scala
headIndexedLens.set(9)(nel)
// res1: cats.data.NonEmptyList[Int] = NonEmptyList(9, 2, 3)
```

#### over
```scala
headIndexedLens.over(_._1 + 8)(nel)
// res3: cats.data.NonEmptyList[Int] = NonEmptyList(9, 2, 3)
```

#### traverse
```scala
import cats.syntax.option._
// import cats.syntax.option._

val partialTraverse = headIndexedLens.traverse(_: NonEmptyList[Int]) {
  case (1, 0) => Some(9)
  case _      => none[Int]
}
// partialTraverse: cats.data.NonEmptyList[Int] => 
//   Option[cats.data.NonEmptyList[Int]] = $Lambda$6277/0x0000000801ceb040@57f796e5

partialTraverse(nel)
// res3: Option[cats.data.NonEmptyList[Int]] = Some(NonEmptyList(9, 2, 3))

partialTraverse(NonEmptyList.fromListUnsafe(List(4, 5, 6)))
// res4: Option[cats.data.NonEmptyList[Int]] = None
```

#### exists
```scala
import cats.syntax.eq._
// import cats.syntax.eq._

headIndexedLens.exists(_._2 === 0)(nel)
// res5: Boolean = true
```

#### contains
```scala
headIndexedLens.contains((1, 1))(nel)
// res6: Boolean = false
```

#### find
```scala
import cats.syntax.eq._
// import cats.syntax.eq._

headIndexedLens.find(_._2 === 0)(nel)
// res7: Option[(Int, Int)] = Some((1,0))
```

## Laws

A `IndexedLens` must satisfy all <a href="../../api/proptics/law/IndexedLensLaws">IndexedLensLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.data.NonEmptyList
// import cats.data.NonEmptyList

import proptics.IndexedLens
// import proptics.IndexedLens

import cats.syntax.eq._
// import cats.syntax.eq._
```

#### You get back what you set

```scala
def setGet[I, S: Eq, A](indexedLens: IndexedLens[I, S, A], s: S): Boolean =
  indexedLens.set(indexedLens.view(s)._1)(s) === s

setGet[Int, NonEmptyList[Int], Int](headIndexedLens, nel)
// res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala

def getSet[I, S, A: Eq](indexedLens: IndexedLens[I, S, A], s: S, a: A): Boolean =
  indexedLens.view(indexedLens.set(a)(s))._1 === a

getSet[Int, NonEmptyList[Int], Int](headIndexedLens, nel, 9)
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[I, S: Eq, A](indexedLens: IndexedLens[I, S, A], s: S, a: A): Boolean =
  indexedLens.set(a)(indexedLens.set(a)(s)) === indexedLens.set(a)(s)

setSet[Int, NonEmptyList[Int], Int](headIndexedLens, nel, 9)
// res2: Boolean = true
```
