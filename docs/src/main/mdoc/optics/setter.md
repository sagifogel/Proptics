---
id: setter
title: Setter
---

A `Setter` is a generalization of `fmap` from `Functor`.
Setter is write-only optic, allowing us to map into a structure and change out the content, but it cannot get the content.
Everything you can do with a Functor, you can do with a Setter.<br/> 


## Setter internal encoding

#### Polymorphic Setter

```scala
Setter_[S, T, A, B]
```

`Setter_[S, T, A, B]` is a function `(A => B) => S => T`.

```scala
/**
  * @tparam S the source of a Setter_
  * @tparam T the modified source of a Setter_
  * @tparam A the focus of a Setter_
  * @tparam B the modified focus of a Setter_
  */
abstract class Setter_[S, T, A, B] {
  def apply(pab: A => B): S => T
}
```

`Setter_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`.</br>
 A `Setter` that changes its focus/structure, is called `Polymorphic Setter`.

#### Monomorphic Setter
    
```scala
Setter[S, A]
```
    
`Setter[S, A]` is a type alias for `Setter_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Setter[S, A] = Setter_[S, S, A, A]
``` 

A `Setter` that does not change its focus/structure, is called `Monomorphic Setter`.

## Constructing Setters

`Setter_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Setter_$">Setter_[S, T, A, B]#apply</a> function.</br>
For a given `Setter_[S, T, A, B]` it takes a function as argument, `(A => B) => S => T`, which is a mapping function `A => B` and a structure `S` and returns a structure of `T`.

```scala
object Setter_ {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter[S, T, A, B]
}
```

`Setter[S, A]` is constructed using the <a href="../../api/proptics/Setter$">Setter[S, A]#apply</a> function.</br>
For a given `Setter_[S, A]` it takes a function as argument, `(A => A) => S => S`, which is a mapping function `A => A` and a structure `S` and returns a new structure `S`.

```scala
object Setter {
  def apply[S, A](f: (A => A) => S => S): Setter[S, A]
}
```

```scala
import proptics.Setter
// import proptics.Setter

val list = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val listSetter: Setter[List[Int], Int] = Setter[List[Int], Int]((f: Int => Int) => ls => ls.map(f))
// listSetter: proptics.Setter[List[Int],Int] = proptics.Setter_$$anon$9@25b01dab
```

## Common functions of a Setter

#### set
```scala
listSetter.set(9)(list)
// res0: List[Int] = List(9, 9, 9, 9)
```

#### over
```scala
listSetter.over(_ + 1)(list)
// res1: List[Int] = List(2, 3, 4, 5)
```

We can achieve the same results using `fromFunctor`

```scala
import cats.instances.list
// import cats.instances.list

val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
// fromFunctor: proptics.Setter[List[Int],Int] = proptics.Setter_$$anon$9@645f1841

fromFunctor.set(9)(list)
// res2: List[Int] = List(9, 9, 9, 9)

fromFunctor.over(_ + 1)(list)
// res3: List[Int] = List(2, 3, 4, 5)
```

We can also create a `Setter` instance using `fromContravariant`

```scala
import proptics.Setter_
// import proptics.Setter_

import cats.Show
// import cats.Show

val fromContravariant: Setter_[Show[Int], Show[List[Int]], List[Int], Int] =
  Setter_.fromContravariant[Show, List[Int], Int]
// fromContravariant: Setter_[Show[Int], Show[List[Int]],List[Int],Int] = proptics.Setter_$$anon$9

val showLength = fromContravariant.over(_.length)(Show.fromToString[Int])
// showLength: cats.Show[List[Int]] = cats.Show$$anon$2@66e8df29

showLength.show(list)
// res4: String = 4
``` 

## Laws

A `Setter` must satisfy all <a href="../../api/proptics/law/SetterLaws">SetterLaws</a>. These laws reside in the <a href="../../api/proptics/law/">proptics.law</a> package.

```scala
import cats.instances.list._
// import cats.instances.list._

import cats.syntax.eq._
// import cats.syntax.eq._

import cats.Eq
// import cats.Eq

import proptics.Setter
// import proptics.Setter

val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
// fromFunctor: proptics.Setter[List[Int],Int] = proptics.Setter_$$anon$9@2361e7b8
```

#### Mapping with identity function will get you the same value

```scala
def overIdentity[S: Eq, A](setter: Setter[S, A], s: S): Boolean = 
  setter.over(identity)(s) === s

overIdentity(fromFunctor, List(1, 2, 3, 4))
// res0: Boolean = true 
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def composeOver[S: Eq, A](setter: Setter[S, A], s: S)(f: A => A)(g: A => A): Boolean = 
  setter.over(g)(setter.over(f)(s)) === setter.over(g compose f)(s)

composeOver(fromFunctor, List(1, 2, 3, 4))(_ + 1)(_ * 2)
// res1: Boolean = true 
```
#### Setting twice is the same as setting once
 
```scala
def setSet[S: Eq, A](setter: Setter[S, A], s: S, a: A): Boolean =
  setter.set(a)(setter.set(a)(s)) === setter.set(a)(s)

setSet(fromFunctor, List(1, 2, 3, 4), 9)
// res2: Boolean = true 
```
