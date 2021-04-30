---
id: an-indexed-lens
title: AnIndexedLens
---

An `AnIndexedLens` is similar to an [IndexedLens](../indexed-optics/indexed-lens.md) but has different internal encodings, it is used to focus on a particular element, and its
location, in a deeply nested data structure, while letting you view, set or modify the focus when you know it exists, that is an `AnIndexedLens` must never fail to get or modify the focus.<br/>
An intuition for `AnIndexedLens` is a getter and setter like you might have on an object, but tupled with its location/index.

## AnIndexedLens internal encoding

#### Polymorphic AnIndexedLens

```scala
AnIndexedLens_[I, S, T, A, B]
```

`AnIndexedLens_[I, S, T, A, B]` is a function `Indexed[P, I, A, B] => P[S, T]` where's the `P[_, _]` is a data type of [Shop](../data-types/shop.md), thus making
it a function `Indexed[Shop[(A, I), B, *, *], I, A, B] => Shop[(A, I), B, S, T]`.

```scala
/**
  * @tparam I the index of an AnIndexedLens_
  * @tparam S the source of an AnIndexedLens_
  * @tparam T the modified source of an AnIndexedLens_
  * @tparam A the focus of an AnIndexedLens_
  * @tparam B the modified focus of an AnIndexedLens_
  */
abstract class AnIndexedLens_[I, S, T, A, B] {
  def apply(indexed: Indexed[Shop[(A, I), B, *, *], I, A, B]): Shop[(A, I), B, S, T]
```

`AnIndexedLens_[I, S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure</br> from `S` to `T`. </br>
An `AnIndexedLens` that changes its focus/structure, is called `Polymorphic AnIndexedLens`.

#### Monomorphic AnIndexedLens

```scala
AnIndexedLens[I, S, A]
```

`AnIndexedLens[I, S, A]` is a type alias for `AnIndexedLens_[I, S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type AnIndexedLens[I, S, A] = AnIndexedLens_[I, S, S, A, A]
``` 

`AnIndexedLens[I, S, A]` means that `S` is the structure or whole and `A` is the focus, or the part, and the `I` is the index or the location of the focus.<br/>
An `AnIndexedLens` that does not change its focus/structure, is called `Monomorphic AnIndexedLens`.

## Constructing AnIndexedLens

`AnIndexedLens_[S, T, A, B]` is constructed using the <a href="../../api/proptics/AnIndexedLens_$">AnIndexedLens_[I, S, T, A, B]#apply</a> function.</br>
For a given `AnIndexedLens_[I, S, T, A, B]` it takes two functions as arguments, `view: S => (A, I)` which is a getter function, that produces an `A` tupled with its index `I` given an `S`,
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns a structure of `T`.

```scala
object AnIndexedLens_ {
  def apply[I, S, T, A, B](get: S => (A, I))(set: S => B => T): AnIndexedLens_[I, S, T, A, B]
}
```

`AnIndexedLens[I, S, A]` is constructed using the <a href="../../api/proptics/AnIndexedLens$">AnIndexedLens[I, S, A]#apply</a> function.</br>
For a given `AnIndexedLens[I, S, A]` it takes two functions as arguments,`view: S => (A, I)` which is a getter function, that produces an `A` tupled with its index `I` given an `S`,
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a new structure `S`.

```scala
object AnIndexedLens {
  def apply[I, S, A](get: S => (A, I))(set: S => A => S): AnIndexedLens[I, S, A]
}
```

Consider the case of focusing on the head of an `NonEmptyList`

```scala
import proptics.AnIndexedLens
// import proptics.AnIndexedLens

import cats.data.NonEmptyList
// import cats.data.NonEmptyList

val nel = NonEmptyList.fromListUnsafe(List(1, 2, 3))
// nel: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3)

val headAnIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] = 
  AnIndexedLens[Int, NonEmptyList[Int], Int](nel => (nel.head, 0)) { nel => i =>
    NonEmptyList(i, nel.tail)
  }
// headAnIndexedLens: proptics.AnIndexedLens[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.AnIndexedLens_$$anon$9@635202f0 
```

## Common functions of an AnIndexedLens

#### view

```scala
headAnIndexedLens.view(nel)
// res0: (Int, Int) = (1,0)
```

#### set
```scala
headAnIndexedLens.set(9)(nel)
// res1: cats.data.NonEmptyList[Int] = NonEmptyList(9, 2, 3)
```

#### over
```scala
headAnIndexedLens.over(_._1 + 8)(nel)
// res3: cats.data.NonEmptyList[Int] = NonEmptyList(9, 2, 3)
```

#### traverse
```scala
import cats.syntax.option._
// import cats.syntax.option._

val partialTraverse = headAnIndexedLens.traverse(_: NonEmptyList[Int]) {
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

headAnIndexedLens.exists(_._2 === 0)(nel)
// res5: Boolean = true
```

#### contains
```scala
headAnIndexedLens.contains((1, 1))(nel)
// res6: Boolean = false
```

#### find
```scala
import cats.syntax.eq._
// import cats.syntax.eq._

headAnIndexedLens.find(_._2 === 0)(nel)
// res7: Option[(Int, Int)] = Some((1,0))
```

## Exporting Shop as data type of AnIndexedLens

`AnIndexedLens` allows us to export its internal construction logic to a `Shop` using the `toShop` method.

```scala
import proptics.AnIndexedLens
// import proptics.AnIndexedLens

import cats.data.NonEmptyList
// import cats.data.NonEmptyList

val nelIndexedLens: AnIndexedLens[Int, NonEmptyList[Int], Int] =
  AnIndexedLens[Int, NonEmptyList[Int], Int](ls => (ls.head, 0))(nel => i => nel.copy(head = i))
// nelIndexedLens: proptics.AnIndexedLens[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.AnIndexedLens_$$anon$23@6b60ef61

val nel = NonEmptyList.fromListUnsafe(List(1, 2, 3))
// nel: cats.data.NonEmptyList[Int] = NonEmptyList(1, 2, 3)

val shop = tupleLens.toShop
// shop: proptics.internal.Shop[(Int, Int),
//       Int,cats.data.NonEmptyList[Int],cats.data.NonEmptyList[Int]] = 
//   Shop(scala.Function1$$Lambda$6356/0x0000000801d11840@1b6fac0f,
//        proptics.internal.ShopInstances$$anon$1$$Lambda$6357/0x0000000801d11040@3244246f)

nelIndexedLens.view(nel)
// res0: (Int, Int) = (1,0)
```

We can later on create a new instance of `AnIndexedLens` or `IndexedLens` from the shop instance

```scala
import proptics.IndexedLens
// import proptics.IndexedLens

import proptics.AnIndexedLens
// import proptics.AnIndexedLens

val indexedLensFromShop: IndexedLens[Int, NonEmptyList[Int], Int] =
  IndexedLens[Int, NonEmptyList[Int], Int](shop.view)(shop.set)
// idexedLensFromShop: proptics.IndexedLens[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.IndexedLens_$$anon$23@247c18f9

val anIndexedLensFromShop: AnIndexedLens[Int, NonEmptyList[Int], Int] =
  AnIndexedLens[Int, NonEmptyList[Int], Int](shop.view)(shop.set)
// anIndexedLensFromShop: proptics.AnIndexedLens[Int,cats.data.NonEmptyList[Int],Int] = 
//   proptics.AnIndexedLens_$$anon$23@33321a90
```

## Laws

A `AnIndexedLens` must satisfy all <a href="../../api/proptics/law/AnIndexedLensLaws">AnIndexedLensLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.data.NonEmptyList
// import cats.data.NonEmptyList

import proptics.AnIndexedLens
// import proptics.AnIndexedLens

import cats.syntax.eq._
// import cats.syntax.eq._
```

#### You get back what you set

```scala
def setGet[I, S: Eq, A](AnIndexedLens: AnIndexedLens[I, S, A], s: S): Boolean =
  AnIndexedLens.set(AnIndexedLens.view(s)._1)(s) === s

setGet[Int, NonEmptyList[Int], Int](headAnIndexedLens, nel)
// res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala

def getSet[I, S, A: Eq](AnIndexedLens: AnIndexedLens[I, S, A], s: S, a: A): Boolean =
  AnIndexedLens.view(AnIndexedLens.set(a)(s))._1 === a

getSet[Int, NonEmptyList[Int], Int](headAnIndexedLens, nel, 9)
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[I, S: Eq, A](AnIndexedLens: AnIndexedLens[I, S, A], s: S, a: A): Boolean =
  AnIndexedLens.set(a)(AnIndexedLens.set(a)(s)) === AnIndexedLens.set(a)(s)

setSet[Int, NonEmptyList[Int], Int](headAnIndexedLens, nel, 9)
// res2: Boolean = true
```
