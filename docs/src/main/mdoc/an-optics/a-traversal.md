---
id: a-traversal
title: ATraversal
---

`ATraversal` is similar to a <a href="/Proptics/docs/optics/traversal" target="_blank">Traversal</a>, but has different internal encodings, it is used
 to focus on zero, one, or many values. `ATraversal` is usually used for collections like `List`, `Map`, `Array`.
 
 ## ATraversal internal encoding
 
 #### Polymorphic ATraversal 
 
 ```scala
 ATraversal_[S, T, A, B]
 ```
 
 `ATraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of [Bazzar](/Proptics/docs/data-types/bazzar), thus making 
 it a function `Bazaar[* => *, A, B, A, B] => Bazaar[* => *, A, B, S, T]`.
 
 ```scala
 /**
   * @tparam S the source of an ATraversal_
   * @tparam T the modified source of an ATraversal_
   * @tparam A the focus of an ATraversal_
   * @tparam B the modified focus of an ATraversal_
   */
 abstract class ATraversal_[S, T, A, B] {
   def apply(bazaar: Bazaar[Function, A, B, A, B]): Bazaar[Function, A, B, S, T]
 }
 ```
 
 `ATraversal_[S, T, A, B]` changes its foci from `A` to `B`, resulting in a change of structure from  `S` to `T`.<br/>
  An `ATraversal` that changes its foci/structure, is called `Polymorphic ATraversal`.
  
 #### Monomorphic ATraversal
  
  ```scala
 ATraversal[S, A]
  ```
 
 `ATraversal[S, A]` is a type alias for `ATraversal_[S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.
 
 ```scala
 type ATraversal[S, A] = ATraversal_[S, S, A, A]
 ``` 
 
 An `ATraversal[S, A]` means that the type `S` might contain zero or one value of type `A`. <br/>
 An `ATraversal` that does not change its foci/structure, is called `Monomorphic ATraversal`.
 
 ## Constructing ATraversal
 
 `ATraversal_[S, T, A, B]` is constructed using the [ATraversal_[S, T, A, B]#apply](/Proptics/api/proptics/ATraversal_$.html) function.<br/>
 For a given `ATraversal_[S, T, A, B]` it takes two functions as arguments,
`view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`
 
 ```scala
 object ATraversal_ {
   def apply[S, T, A, B](get: S => A)(set: S => B => T): ATraversal_[S, T, A, B]
 }
 ```
 
 `ATraversal[S, A]` is constructed using the [ATraversal[S, A]#apply](/Proptics/api/proptics/ATraversal$.html) function.<br/>
 For a given `ATraversal[S, A]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
 new structure `S` filled will all foci of that `A`.
 
 ```scala
 object ATraversal {
   def apply[S, A](get: S => A)(set: S => A => S): ATraversal[S, A]
 }
 ```

Most of the time you will be dealing with collections. This is the way to create a `ATraversal[S, A]` for a collection:

```scala
import cats.syntax.option._
// import cats.syntax.option._ // (none, some) functions

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import proptics.ATraversal
// import proptics.ATraversal 

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: Int => Option[Int] = i => if (i % 2 === 0) i.some else none[Int]
// optionByPredicate: Int => Option[Int] = $Lambda$12694/0x0000000802bfc3e0@5f962a8b

val listTraversal: ATraversal[List[Int], Int] = ATraversal.fromTraverse[List, Int]
// listTraversal: proptics.ATraversal[List[Int],Int] = proptics.ATraversal_$$anon$12@4e2da5c7
```

## Common functions of a ATraversal

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
listTraversal.foldr(list)(List.empty[Int])(_ :: _)
// res8: List[Int] = List(1, 2, 3, 4)
```

#### foldl

```scala
listTraversal.foldl(list)(List.empty[Int])((b, a) => a :: b)
// res9: List[Int] = List(4, 3, 2, 1)
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

## Syntax

We can take advantage of the `syntax` package for `ATraversal`.
If we create a polymorphic `ATraversal_[F[G[A]], F[A], G[A], A]` such that the source would be a nested structure of `F[G[A]]` and  the initial foci `G[A]` would have an `Applicative[G]` instance, and the modified foci would be an `A` , then we could
flip types `F[_]` and `G[_]` from `F[G[A]]` to `G[F[A]]` using the `sequence` method.<br/>
Consider `F[_]` to be a `List`, `G[_]` to be an `Option` and `A` to be an `Int`, the `ATraversal` would be:

```scala
ATraversal_[List[Option[Int]], List[Int], Option[Int], Int]
``` 

```scala
import cats.syntax.eq._
// import cats.syntax.eq._

import cats.syntax.option._
// import cats.syntax.option._

import cats.instances.list._
// import cats.instances.list._

import proptics.ATraversal_
// import proptics.ATraversal_

import proptics.syntax.aTraversal._
// import proptics.syntax.aTraversal._

val list: List[Int] = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val optionByPredicate: Int => Option[Int] = i => if (i % 2 === 0) i.some else none[Int]
// optionByPredicate: Int => Option[Int] = $Lambda$34954/0x0000000802317840@5d5007af

val listOfOptions: List[Option[Int]] = list.map(optionByPredicate)
// listOfOptions: List[Option[Int]] = List(None, Some(2), None, Some(4))

val listOfSomeOptions: List[Option[Int]] = list.map(_.some)
// listOfSomeOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4))

val traversal: ATraversal_[List[Option[Int]], List[Int], Option[Int], Int] = 
  ATraversal_.fromTraverse[List, Option[Int], Int]
// traversal: ATraversal_[List[Option[Int]],List[Int],Option[Int],Int] = ATraversal_$$anon$12@65892367
```

#### sequence

```scala
traversal.sequence(listOfOptions)
// res0: Option[List[Int]] = None

traversal.sequence(listOfSomeOptions)
// res1: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

## Exporting Bazaar as data type of ATraversal

`ATraversal` allows us to export its internal construction logic to a `Bazaar` using the `toBazaar` method.

```scala
val traversal: ATraversal[(Int, String), String] = 
  ATraversal[(Int, String), String](_._2) { case (i, _) => s => (i, s) }
// traversal: proptics.ATraversal[(Int, String),String] = proptics.ATraversal_$$anon$22@7218cbb6

val bazaar = traversal.toBazaar
// bazaar: internal.Bazaar[[α$11$, β$12$]α$11$ => β$12$,String,String,(Int, String),(Int, String)] = 
//   proptics.ATraversal_$$anon$22$$anon$23@5f364bc2
```

We can later on create a new instance of an `ATraversal` or a `Traversal` from the bazaar instance

```scala
import proptics.ATraversal
// import proptics.ATraversal_

import proptics.Traversal
// import proptics.Traversal

val aTraversalFromBazaar: ATraversal[(Int, String), String] = ATraversal.fromBazaar(bazaar)
// aTraversalFromBazaar: proptics.ATraversal[(Int, String),String] = 
//   proptics.ATraversal_$$anon$19@43bf1ac9

val traversalFromBazaar: Traversal[(Int, String), String] = Traversal.fromBazaar(bazaar)
// traversalFromBazaar: proptics.Traversal[(Int, String),String] = 
//   proptics.Traversal_$$anon$12@7494feef
```

## Laws

A `Traversal` must satisfy all [ATraversalLaws](/Proptics/api/proptics/law/ATraversalLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

```scala
import cats.instances.list._
// import cats.instances.list._

import cats.syntax.eq._
// import cats.syntax.eq._

import cats.{Applicative, Eq}
// import cats.{Applicative, Eq}

import proptics.ATraversal
// import proptics.ATraversal
```

#### Traversing with "empty" handler shouldn't change anything

```scala
def respectPurity[F[_]: Applicative, S, A](traversal: ATraversal[S, A], s: S)
                                          (implicit ev: Eq[F[S]]): Boolean =
  traversal.traverse[F](s)(Applicative[F].pure _) === Applicative[F].pure(s)

val listTraversal = ATraversal.fromTraverse[List, Int]
// listTraversal: proptics.ATraversal[List[Int],Int] = proptics.ATraversal_$$anon$12@4fb75a8c

respectPurity(listTraversal, List.range(1, 5))
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[S: Eq, A](traversal: ATraversal[S, A], s: S, f: A => A, g: A => A): Boolean =
    (traversal.over(f) compose traversal.over(g))(s) === traversal.over(f compose g)(s)

consistentFoci[List[Int], Int](listTraversal, List.range(1, 5), _ + 1, _ * 2)
// res1: Boolean = true
```