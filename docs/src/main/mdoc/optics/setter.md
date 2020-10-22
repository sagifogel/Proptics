---
id: setter
title: Setter
---

A `Setter` is a generalization of fmap from Functor.
Setter is write-only optic, allowing us to map into a structure and change out the content, but it cannot get the content.
Everything you can do with a Functor, you can do with a Setter.<br/> 
For a given `Setter[S, A]` it takes a function `(A => A) => S => S` which takes a mapping function `A => A` and a structure `S` and returns a new structure `S`.

## Constructing Setters

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

## Setter internal encoding

`Setter[S, A]` is the monomorphic short notation version of the polymorphic one `Setter_[S, T, A, B]`.

```scala
type Setter[S, A] = Setter_[S, S, A, A]
``` 

`Setter_[S, T, A, B]` is basically a function `(A => B) => S => T`.

```scala
abstract class Setter_[S, T, A, B] extends Serializable {
  private[proptics] def apply(pab: A => B): S => T
}
```

## Laws

A `Setter` must satisfy all [SetterLaws](/Proptics/api/proptics/law/SetterLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.

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
// overIdentity: [S, A](setter: proptics.Setter[S,A], s: S)
//                      (implicit evidence$1: cats.Eq[S])Boolean

overIdentity(fromFunctor, List(1, 2, 3, 4))
// res0: Boolean = true 
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def composeOver[S: Eq, A](setter: Setter[S, A], s: S)(f: A => A)(g: A => A): Boolean = 
  setter.over(g)(setter.over(f)(s)) === setter.over(g compose f)(s)
// composeOver: [S, A](setter: proptics.Setter[S,A], s: S)
//                    (f: A => A)(g: A => A)(implicit evidence$1: cats.Eq[S])Boolean

composeOver(fromFunctor, List(1, 2, 3, 4))(_ + 1)(_ * 2)
// res1: Boolean = true 
```
#### Setting twice is the same as setting once
 
```scala
def setSet[S: Eq, A](setter: Setter[S, A], s: S, a: A): Boolean =
  setter.set(a)(setter.set(a)(s)) === setter.set(a)(s)
// setSet: [S, A](setter: proptics.Setter[S,A], s: S, a: A)
//                (implicit evidence$1: cats.Eq[S])Boolean

setSet(fromFunctor, List(1, 2, 3, 4), 9)
// res2: Boolean = true 
```
