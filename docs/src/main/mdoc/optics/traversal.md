---
id: traversal
title: Traversal
---

`Traversal[S, A]` means that the type `S` might contain zero, one, or many values of type `A`. <br/>
A `Traversal` is usually used for collections like `List`, `Map`, `Array`, where the optic has zero, one, or many foci.

## Constructing Traversals

`Traversal` can be constructed using the [Traversal[S, A]#apply](/Proptics/api/proptics/Traversal$.html) function. For a given `Traversal[S, A]` it takes two functions as arguments,
`view` which is a getter function, that produces 0, one, or many elements of `A` given an `S`, and `set` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object Lens {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A]
}
```

Most of the time you will be dealing with collections. This so the way to create a `Traversal` for a collection:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.{Traversal, Traversal_}
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

Calling the function using `optionByPredicate` requires us to summon an implicit instance of Monoid[Option[Int]], 
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

Now comes the `uncommon` part. Some methods require implicit instances which are defined in another library called 
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