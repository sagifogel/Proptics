---
id: fold
title: Fold
---

A `Fold` describes how to retrieve multiple values. It is similar to a <a href="/Proptics/docs/optics/traversal" target="_blank">Traversal</a>, but it 
cannot modify its foci. Everything you can do with a Foldable, you can do with a Fold.


## Fold internal encoding

#### Polymorphic Fold

`Fold_[S, T, A, B]` is a function `Forget[R, A, B] => Forget[R, S, T]`. [Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets the `B` value and returns an accumulated value of type `R`.

```scala
/**
  * @tparam S the source of a Fold_
  * @tparam T the modified source of a Fold_
  * @tparam A the foci of a Fold_
  * @tparam B the modified foci of a Fold_
  */
abstract class Fold_[S, T, A, B] extends Serializable {
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]
}
```

Although `Fold_[S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic Fold`, serving as
base type from which a `Monomorphic Fold` can be obtained.
 
#### Monomorphic Traversal

`Fold[S, A]` is a type alias for `Fold_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Fold[S, A] = Fold_[S, S, A, A]
``` 

Since `Fold_` is read-only, `Fold[S, A]` is isomorphic to `Fold_[S, T, A, B]`.</br>
`Fold_[S, T, A, B]` takes `Forget[R, A, B]` which wraps a function `A => R` and  forgets the `B`, and returns `Forget[R, S, T]` 
 which wraps a function `S => R` and forgets the `T`,  and its representation can be simplified to:

```
(A => R) => S => R
```

Let's compare it to `Fold_[S, S, A, A]` which is equivalent to `Fold[S, A]`.</br> 

```scala
def fold[S, A]: Fold[S, A] = new Fold_[S, S, A, A] {
  override private[proptics] def apply[R: Monoid](forget: Forget[R, A, A]): Forget[R, S, S]
}
```

`Fold_[S, S, A, A]` takes `Forget[R, A, A]` which wraps a function `A => R` and  forgets the last type parameter `A`, and returns `Forget[R, S, S]` which wraps a function `S => R` and forgets the last parameter `S`, 
and its representation can be simplified to:

```
(A => R) => S => R
```

## Constructing Folds

`Fold_[S, T, A, B]` is constructed using the [Fold_[S, T, A, B]#apply](/Proptics/api/proptics/Fold_$.html) function.</br>
For a given `Fold_[S, T, A, B]` it takes a fold function as argument, `S => A`, which returns a `A` given an `S`.

```scala
object Fold_ {
  def apply[S, T, A, B](f: S => A): Fold[S, T, A, B]
}
```

`Fold[S, A]` is constructed using the [Fold[S, A]#apply](/Proptics/api/proptics/Fold$.html) function. For a given `Fold[S, A]` 
it takes a fold function as argument, `S => A`, which returns a `A` given an `S`.

```scala
object Fold {
  def apply[S, A](f: S => A): Fold[S, A]
}
```

Most of the time we will be dealing with `Foldable` types. This is how we can create a `Fold` for a `Foldable` type:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.Fold
// import proptics.Fold

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: Int => Option[Int] = i => if (i % 2 === 0) i.some else none[Int]
// optionByPredicate: Int => Option[Int] = $Lambda$11775/0x0000000802ea3840@2f96d5ee

val listFold: Fold[List[Int], Int] = Fold.fromFoldable[List, Int]
// listFold: proptics.Fold[List[Int],Int] = proptics.Fold_$$anon$10@6f126b09
```

## Common functions of a Fold

#### viewAll
```scala
listFold.viewAll(list)
// res0: List[Int] = List(1, 2, 3, 4)
```

#### preview/first
```scala
listFold.preview(list)
// res1: Option[Int] = Some(1)

// synonym for preview
listFold.first(list) 
// res2: Option[Int] = Some(1)
```

#### foldMap

```scala
import cats.instances.int._ // summons a Semigroup[Int] instance
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._
 
listFold.foldMap(list)(optionByPredicate)
// res3: Option[Int] = Some(6)
```

#### foldr

```scala
val cons: (Int, List[Int]) => List[Int] = _ * 2 :: _
// cons: (Int, List[Int]) => List[Int] = $Lambda$11776/0x0000000802ea7040@690c02ed

listFold.foldr(list)(List.empty[Int])(cons)
// res4: List[Int] = List(2, 4, 6, 8)
```

#### foldl

```scala
listFold.foldl(list)(List.empty[Int])((b, a) => cons(a, b))
// res5: List[Int] = List(2, 4, 6, 8)
```

#### forall

```scala
listFold.forall(_ < 9)(list)
// res6: Boolean = true
```

#### exists

```scala
listFold.exists(_ < 9)(list)
// res7: Boolean = true
```

#### contains

```scala
listFold.contains(9)(list)
// res8: Boolean = false
```

#### isEmpty

```scala
listFold.isEmpty(list)
// res9: Boolean = false
```

#### find

```scala
listFold.find(_ === 9)(list)
// res10: Option[Int] = None
```

#### last

```scala
listFold.last(list)
// res11: Option[Int] = Some(4)
```

#### minimum

```scala
listFold.minimum(list)
// res12: Option[Int] = Some(1)
```

#### maximum

```scala
listFold.maximum(list)
// res13: Option[Int] = Some(4)
```

## Laws

Since a `Fold` cannot be used to write back there are no Lens laws that apply to it.

