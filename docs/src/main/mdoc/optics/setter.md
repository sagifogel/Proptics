---
id: setter
title: Setter
---

A `Setter` is a generalization of `fmap` from `Functor`.
Setter is write-only optic, allowing us to map into a structure and change out the content, but it cannot get the content.
Everything you can do with a Functor, you can do with a Setter.<br/> 

## Constructing monomorphic Setter

### Using companion object

`Setter[S, A]` is constructed using the <a href="../../api/proptics/Setter$">Setter[S, A]#apply</a> function.</br>
For a given `Setter_[S, A]` it takes a function as argument, `(A => A) => S => S`, which is a mapping function `A => A` and a structure `S` and returns a new structure `S`.

```scala
object Setter {
  def apply[S, A](f: (A => A) => S => S): Setter[S, A]
}
```

```scala
import proptics.Setter

val listSetter: Setter[List[Int], Int] = Setter[List[Int], Int](f => ls => ls.map(f))

listSetter.over(_ + 1)(List.range(1, 6))
// val res0: List[Int] = List(2, 3, 4, 5, 6)
```

### Using fromFunctor method

```scala
import proptics.Setter
import cats.instances.list

val listSetter: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]

listSetter.over(_ + 1)(List.range(1, 6))
// val res1: List[Int] = List(2, 3, 4, 5, 6)
```

## Constructing polymorphic Setter

### Using companion object

`Setter_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Setter_$">Setter_[S, T, A, B]#apply</a> function.</br>
For a given `Setter_[S, T, A, B]` it takes a function as argument, `(A => B) => S => T`, which is a mapping function `A => B` and a structure `S` and returns a structure of `T`.

```scala
object Setter_ {
  def apply[S, T, A, B](f: (A => B) => S => T): Setter_[S, T, A, B]
}
```

Consider a Person data structure

```scala
case class Person(id: String, name: String, yearOfBirth: Int)
```

We have a person instance of type `Person`, and we want to compare it to another instance using `cats.Eq`.<br/>
In order to do it we need to use the `id` property of `Person`. We can use a `Setter_` in order to change `Eq[String]` to `Eq[Person]`


```scala
import cats.Eq
import cats.syntax.eq._
import proptics.Setter_

final case class Person(id: String, name: String, yearOfBirth: Int)

val personEqSetter: Setter_[Eq[String], Eq[Person], Person, String] =
  Setter_[Eq[String], Eq[Person], Person, String]((f: Person => String) => (eqString: Eq[String]) => {
    Eq.instance[Person]((p1, p2) => eqString.eqv(f(p1), f(p2)))
  })

val eqString: Eq[String] = Eq.fromUniversalEquals[String]
implicit val personEq: Eq[Person] = personEqSetter.over(_.id)(eqString)

Person("123", "Samuel Eilenberg", 1913) === Person("123", "Samuel Eilenberg", 1913)
// val res0: Boolean = True
```

We can also use the `contramap` method of `cats.Eq` in order to make the code more concise

```scala
import cats.Eq
import cats.syntax.eq._
import proptics.Setter_
import cats.syntax.contravariant._

final case class Person(id: String, name: String, yearOfBirth: Int)

val personEqSetter: Setter_[Eq[String], Eq[Person], Person, String] =
  Setter_[Eq[String], Eq[Person], Person, String]((f: Person => String) => (eqString: Eq[String]) => {
    eqString.contramap[Person](f)
  })

val eqString: Eq[String] = Eq.fromUniversalEquals[String]
implicit val personEq: Eq[Person] = personEqSetter.over(_.id)(eqString)

Person("123", "Samuel Eilenberg", 1913) === Person("123", "Samuel Eilenberg", 1913)
// val res1: Boolean = True
```

### Using fromContravariant method

```scala
import cats.Eq
import cats.syntax.eq._
import proptics.Setter_

final case class Person(id: String, name: String, yearOfBirth: Int)

val personEqSetter: Setter_[Eq[String], Eq[Person], Person, String] =
  Setter_.fromContravariant[Eq, Person, String]

implicit val eqString: Eq[String] = Eq.fromUniversalEquals[String]
implicit val personEq: Eq[Person] = personEqSetter.over(_.id)(eqString)

Person("123", "Samuel Eilenberg", 1913) === Person("123", "Samuel Eilenberg", 1913)
// val res2: Boolean = True
```

## Methods

#### [set](../../api/proptics/Setter_.html#set(b:B):S=>T)

```scala
/** set the modified focus of a Setter */
def set(a: A): S => S
```

```scala
import proptics.Setter

val listSetter: Setter[List[Int], Int] = Setter[List[Int], Int](f => ls => ls.map(f))

listSetter.set(9)(List.range(1, 6))
// res0: List[Int] = List(9, 9, 9, 9, 9)
```

#### [over](../../api/proptics/Setter_.html#over(f:A=>B):S=>T)

```scala
/** modify the focus of a Setter using a function */
def over(f: A => A): S => S
```

```scala
import proptics.Setter

val listSetter: Setter[List[Int], Int] = Setter[List[Int], Int](f => ls => ls.map(f))

listSetter.over(_ + 1)(List.range(1, 6))
// res1: List[Int] = List(2, 3, 4, 5, 6)
```

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

## Laws

A `Setter` must satisfy all <a href="../../api/proptics/law/SetterLaws">SetterLaws</a>. These laws reside in the <a href="../../api/proptics/law/">proptics.law</a> package.

```scala
import cats.instances.list._
import cats.syntax.eq._
import cats.Eq
import proptics.Setter

val fromFunctor: Setter[List[Int], Int] = Setter.fromFunctor[List, Int]
```

#### Mapping with identity function will get you the same value

```scala
def overIdentity[S: Eq, A](setter: Setter[S, A], s: S): Boolean = 
  setter.over(identity)(s) === s

overIdentity(fromFunctor, List(1, 2, 3, 4))
// val res0: Boolean = true 
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def composeOver[S: Eq, A](setter: Setter[S, A], s: S)(f: A => A)(g: A => A): Boolean = 
  setter.over(g)(setter.over(f)(s)) === setter.over(g compose f)(s)

composeOver(fromFunctor, List(1, 2, 3, 4))(_ + 1)(_ * 2)
// val res1: Boolean = true 
```
#### Setting twice is the same as setting once
 
```scala
def setSet[S: Eq, A](setter: Setter[S, A], s: S, a: A): Boolean =
  setter.set(a)(setter.set(a)(s)) === setter.set(a)(s)

setSet(fromFunctor, List(1, 2, 3, 4), 9)
// val res2: Boolean = true 
```
