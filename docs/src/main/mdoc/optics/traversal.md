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

`Traversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Wander](/Proptics/docs/profunctors/wander) of P[_, _].

```scala
/** @tparam S the source of a Traversal_
  * @tparam T the modified source of a Traversal_
  * @tparam A the foci of a Traversal_
  * @tparam B the modified foci of a Traversal_
  */
abstract class Traversal_[S, T, A, B] {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
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

`Traversal_[S, T, A, B]` can be constructed using the [Traversal_[S, A]#apply](/Proptics/api/proptics/Traversal_$.html) function.</br>
For a given `Traversal_[S, T, A, B]` it takes two functions as arguments,
`view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`.

```scala
object Traversal_ {
  def apply[S, T, A, B](get: S => A)(set: S => B => T): Traversal_[S, T, A, B]
}
```

`Traversal[S, A]` can be constructed using the [Traversal[S, A]#apply](/Proptics/api/proptics/Traversal$.html) function.</br>
For a given `Traversal[S, A]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object Traversal {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A]
}
```

Most of the time you will be dealing with collections. This is the way to create a `Traversal[S, A]` for a collection:

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
listTraversal.set(list)
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

#### foldr

```scala
val cons: (Int, List[Int]) => List[Int] = _ * 2 :: _
// cons: (Int, List[Int]) => List[Int] = $Lambda$5802/0x0000000801e68040@11044fd5

listTraversal.foldr(list)(List.empty[Int])(cons)
// res8: List[Int] = List(2, 4, 6, 8)
```

#### foldl

```scala
listTraversal.foldl(list)(List.empty[Int])((b, a) => cons(a, b))
// res9: List[Int] = List(2, 4, 6, 8)
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

## Uncommon functions of a Traversal

Until now, we saw functions that sometimes use implicit types from `Cats` library, like 
`foldMap` which takes an implicit instance of `Monoid[R]`

```scala
def foldMap[R: Monoid](s: S)(f: A => R): R
```

Calling the function using `optionByPredicate` requires us to summon an implicit instance of `Monoid[R]`, 
because the return type of `foldMap` is `R` and the function `optionByPredicate` returns an `Option[Int]`, we need to provide an implicit instance of `Monoid[Option[Int]]` in order to fold over the list by
ignoring the `None`s and accumulate the `Int`s inside the `Some`s.<br/>
We can use a predefined implicit instance of `Monoid[Option[Int]]` using these line of code<br/>

```scala
// Monoid[Option[Int]] takes an implicit of `Semigroup[Int]` therefore we need to summon it also
import cats.instances.int._
// import cats.instances.int._ 

import cats.instances.option._ // summons a Monoid[Option[Int]] instance
// import cats.instances.option._

listTraversal.foldMap[Option[Int]](list)(optionByPredicate)
```

Now comes the uncommon part. Some methods require implicit instances which are defined in another library called 
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

A `Traversal` must satisfy all [TraversalLaws](/Proptics/api/proptics/law/TraversalLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

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
// respectPurity: [F[_], S, A](traversal: proptics.Traversal[S,A], s: S)
//                            (implicit evidence$1: cats.Applicative[F], 
//                            implicit ev: cats.Eq[F[S]])Boolean

val listTraversal = Traversal.fromTraverse[List, Int]
// listTraversal: proptics.Traversal[List[Int],Int] = proptics.Traversal_$$anon$12@4fb75a8c

respectPurity(listTraversal, List.range(1, 5))
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[S: Eq, A](traversal: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
    (traversal.over(f) compose traversal.over(g))(s) === traversal.over(f compose g)(s)
// consistentFoci: [S, A](traversal: proptics.Traversal[S,A], s: S, f: A => A, g: A => A)
//                       (implicit evidence$1: cats.Eq[S])Boolean

consistentFoci[List[Int], Int](listTraversal, List.range(1, 5), _ + 1, _ * 2)
// res1: Boolean = true
```