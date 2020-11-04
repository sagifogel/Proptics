---
id: closed
title: Closed
---

The `Closed` class extends [Profunctor](/Proptics/docs/profunctors/profunctor) class to work with functions.

```scala
trait Closed[P[_, _]] extends Profunctor[P] {
  def closed[A, B, C](pab: P[A, B]): P[C => A, C => B]
}
```

In order to understand the `Closed` profunctor, we will specialize the `P[_, _]` type constructor to the `Function` type. Now the type signature
of the `closed` method is:

```scala
def closed[A, B, C](pab: A => B): (C => A) => C => B
```  

Consider the `A` an input and the `B` an output. We can think of the `closed` method as if we get an argument that can map from `A` to `B`, 
then we would return a function given a mapping from another input `C` to the initial input `A`, returns a function from `C` to `B`, meaning
the `closed` method lets us pre compose the<br/>  `A -> B` with another function. Another way to think about it is, if we consider the `A -> B` as our 
main business logic the `closed` method lets us run some initialization logic before our main function.

## Function as Closed profunctor

Let's implement an instance of `Closed` for `Function`

```scala
import proptics.profunctor.Closed
// import proptics.profunctor.Closed

implicit def closedFunction: Closed[* => *] = new Closed[* => *] {
  override def closed[A, B, C](pab: A => B): (C => A) => C => B = c2a => pab compose c2a

  override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
}
// closedFunction: proptics.profunctor.Closed[[α$0$, β$1$]α$0$ => β$1$]

val currentYear: Int = 2020 
// currentYear: Int = 2020

def calculatePassedYears(year: Int): Int = currentYear - year
// calculatePassedYears: (year: Int)Int
```

Consider the `calculatePassedYears` our main business logic. The method takes an `Int` input, which represent a year, and returns how many years 
have passed since the current one. Using the `closed` we can add an initialization logic in order to extract an `Int` from another type.

```scala
final case class Person(id: String, name: String, yearOfBirth: Int)
// defined class Person

val calculateAge: Person => Int = 
  closedFunction.closed[Int, Int, Person](calculatePassedYears)(_.yearOfBirth)
// calculateAge: Person => Int = scala.Function1$$Lambda$11142/0x00000008028678e8@49f4dc14

calculateAge(Person("123", "Samuel Eilenberg", 1913))
// res0: Int = 107
```

We extracted an `Int` using `(_.yearOfBirth)` which is our initialization function and then calculate the age 
using our main logic `calculatePassedYears`.<br/>

Consider a `UserRegistration` class that represents a domain entity for a registration form, we can use `closed` method
to add an initialization logic that converts the raw data into a domain user object using pre composition, but we can also
use post composition in order to return a type different from the return type of our main business logic

```scala
import java.util.UUID
// import java.util.UUID

final case class User(id: String, name: String, age: Int)
// defined class User

final case class UserRegistration(name: String, yearOfBirth: Int)
// defined class UserRegistration

def generateUniqueId: String = UUID.randomUUID.toString
// generateUniqueId: String

def registerUser(registration: UserRegistration): User = {
  val user = User(generateUniqueId, registration.name, calculatePassedYears(registration.yearOfBirth))
  // ... persist to db
  user
}
// registerUser: (registration: UserRegistration)User

val submitForm: ((String, Int)) => User = 
  closedFunction.closed[UserRegistration, User, (String, Int)](registerUser)(UserRegistration.tupled)
// submitForm: ((String, Int)) => User = scala.Function1$$Lambda$11142/0x00000008028678e8@9590db8

submitForm(("Samuel Eilenberg", 1913))
res1: User = User(1b016d65-bbe8-419e-8cfa-7f26c82e01e3,Samuel Eilenberg,107)
```  


