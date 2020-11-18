---
id: additional-functionality-of-folds
title: Folds Additional Functionality
---

This section applies to all Fold optics

```scala
Fold[S, T, A, B]

Traversal[S, T, A, B]

ATraversal[S, T, A, B]

IndexedFold[I, S, T, A, B]

IndexedTraversal[I, S, T, A, B]
```

Some of the methods of optics in `proptics` requires an implicit instance of types defined in `Cats` library, like
`foldMap` which takes an implicit instance of `Monoid[R]`

```scala
def foldMap[R: Monoid](s: S)(f: A => R): R
```

Calling this method requires us to summon an implicit instance of `Monoid[R]`.
Let's assume that we want to use the `foldMap` method of a `Traversal` in order to fold over a `List`, and we provide
a function `optionByPredicate`

```scala
// Monoid[Option[Int]] takes an implicit of `Semigroup[Int]` therefore we need to summon it also
import cats.instances.int._
// import cats.instances.int._

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._

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

Because the return type of `foldMap` is `R` and the function `optionByPredicate` returns an `Option[Int]`, we need to provide an implicit instance of `Monoid[Option[Int]]` in order to fold over the list.<br/>

## Common functions of all Folds

All Fold optics have some methods that require implicit instances which are defined in another library called
<a href="https://typelevel.org/spire/" target="_blank">Spire</a>, which deals with numeric abstractions. For example:

#### Heyting

The `Heyting[A]` or `HeytingAlgebra` is a typeclass that deals with intuitionistic logic.

```scala
trait Heyting extends {
  def and(a: A, b: A): A
  def meet(a: A, b: A): A = and(a, b)

  def or(a: A, b: A): A
  def join(a: A, b: A): A = or(a, b)

  def imp(a: A, b: A): A
  def complement(a: A): A

  def xor(a: A, b: A): A = or(and(a, complement(b)), and(complement(a), b))
  def nand(a: A, b: A): A = complement(and(a, b))
  def nor(a: A, b: A): A = complement(or(a, b))
  def nxor(a: A, b: A): A = complement(xor(a, b))
}
```

An intuition for this typeclass would be to replace the `A` with `Boolean`.<br/>
If we would create an instance of `Heyting[Boolean]` it will look something like this:

```scala
implicit def heytingBool: Heyting[Boolean] = new Heyting[Boolean] {
  override def and(a: Boolean, b: Boolean): Boolean = a && b

  override def or(a: Boolean, b: Boolean): Boolean = a || b

  override def imp(a: Boolean, b: Boolean): Boolean = !a || b

  override def complement(a: Boolean): Boolean = !a

  override def zero: Boolean = false

  override def one: Boolean = true
}
```

Fortunately `Spire` already defines such an implicit under the `spire.std.boolean` package.<br/>
Let's create a new Traversal of List[Boolean], in addition to the `listTraversal`, in order to simplify the explanation

```scala
import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import cats.instances.list._
// import cats.instances.list._

import proptics.Traversal
// import proptics.Traversal

import spire.std.boolean._
// import spire.std.boolean._

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val boolList = list.map(_ < 2)
// boolList: List[Boolean] = List(true, false, false, false)

val listTraversal: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]
// listTraversal: proptics.Traversal[List[Int],Int] = proptics.Traversal_$$anon$12@6cb0d58

val boolListTraversal: Traversal[List[Boolean], Boolean] = Traversal.fromTraverse[List, Boolean]
// boolListTraversal: proptics.Traversal[List[Boolean],Boolean] = proptics.Traversal_$$anon$12@59b0be0
```

#### and

The `and` method takes a `Heyting[A]` and uses the `Heyting#and` method in order to
perform an `AND` operation between two `A`s.

```scala
def and(s: S)(implicit ev: Heyting[A]): A
```

```scala
boolListTraversal.and(boolList)
//res0: Boolean = false

boolListTraversal.and(boolListTraversal.set(true)(boolList)) // set all to true
//res1: Boolean = true
```

#### or

```scala
def or(s: S)(implicit ev: Heyting[A]): A
```
The `or` method takes a `Heyting[A]` and uses the `Heyting#or` method in order to
perform an `OR` operation between two `A`s.

```scala
boolListTraversal.or(boolList)
//res2: Boolean = true
```

#### any

The `any` method accumulates the `A`s using a Monoid of `Disj[A]` which takes a `Heyting[A]` and uses the `Heyting#zero` method in order to
compare it to the accumulated value of `Disj[A]`. If the accumulated value equals to `zero`, any returns false, otherwise returns true.
The `any` method takes a higher order function `f` that returns an `R`. If we replace the `A` with a `Boolean` we could use the same `Heyting[Boolean]` implicit
instance defined under the `spire.std.boolean` package.

```scala
  def any[R: Heyting](s: S)(f: A => R): R
```

```scala
listTraversal.any(list)(_ > 9)
//res3: Boolean = false

listTraversal.any(list)(_ === 2)
//res4: Boolean = true
```