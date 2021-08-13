---
id: lens
title: Lens
---

A `Lens` is an optic used to focus on a particular element in a deeply nested data structure, while letting you 
view, set or modify the focus when you know it exists, that is a `Lens` must never fail to get or modify the focus.<br/>
An intuition for `Lens` is a getter and setter like you might have on an object.

## Lens internal encoding

#### Polymorphic Lens

```scala
Lens_[S, T, A, B]
```

`Lens_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Strong](../profunctors/strong.md) of P[_, _].

 ```scala
/**
  * @tparam S the source of a Lens_
  * @tparam T the modified source of a Lens_
  * @tparam A the focus of a Lens_
  * @tparam B the modified focus of a Lens_
  */
abstract class Lens_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]
}
```

`Lens_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`. </br>
A `Lens` that changes its focus/structure, is called `Polymorphic Lens`.

#### Monomorphic Lens

```scala
Lens[S, A]
```

`Lens[S, A]` is a type alias for `Lens_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Lens[S, A] = Lens_[S, S, A, A]
``` 

`Lens[S, A]` means that `S` is the structure or whole and `A` is the focus, or the part.<br/>
A `Lens` that does not change its focus/structure, is called `Monomorphic Lens`.

## Constructing Lenses

`Lens_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Lens_$">Lens_[S, T, A, B]#apply</a> function.</br>
For a given `Lens_[S, T, A, B]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces an `A` given an `S`, 
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns a structure of `T`.

```scala
object Lens_ {
  def apply[S, T, A, B](view: S => A)(set: S => B => T): Lens_[S, T, A, B]
}
```

`Lens[S, A]` is constructed using the <a href="../../api/proptics/Lens$">Lens[S, A]#apply</a> function.</br> 
For a given `Lens[S, A]` it takes two functions as arguments,`view: S => A` which is a getter function, that produces an `A` given an `S`,
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a new structure `S`.

```scala
object Lens {
  def apply[S, A](view: S => A)(set: S => A => S): Lens[S, A]
}
```

Consider a User data structure

```scala
case class User(userName: String, email: String)
// defined class User  

val user = User("user99", "user@email.com")
// user: User = User(user99,user@email.com)
```

We have user instance of type `User`, and we want to focus on the email field, so we could
interact with it.

```scala
import proptics.Lens
// import proptics.Lens

val emailLens = Lens[User, String](_.email)(person => email => person.copy(email = email))
// emailLens: proptics.Lens[User,String] = proptics.Lens_$$anon$11@35a0773a
```

## Common functions of a Lens

#### view
```scala
emailLens.view(user)
// res0: String = user@email.com
```

#### set
```scala
emailLens.set("user@email.it")(user)
// res1: User = User(user99,user@email.it,1)
```

#### over
```scala
emailLens.over(_.replace("com", "it"))(user)
// res2: User = User(user99,user@email.it)
```

#### traverse
```scala
val partialTraverse = emailLens.traverse(_: User) {
  case email if email.endsWith("com") => Some(email)
  case _                              => None
}
// partialTraverse: User => Option[User] = $Lambda$7357/476326355@2100263b

partialTraverse(user)
// res3: Option[User] = Some(User(user99,user@email.com))

partialTraverse(User("user99", "user@email.it"))
// res4: Option[User] = None
```

#### exists
```scala
emailLens.exists(_.endsWith("com"))(user)
// res5: Boolean = true
```

#### contains
```scala
emailLens.contains("user@email.it")(user)
// res6: Boolean = false
```

#### find
```scala
emailLens.find(_.endsWith("com"))(user)
// res7: Option[String] = Some(user@email.com)
```

## Composability

Let's change the User to be a nested data structure

```scala
case class AccountSecurity(password: String, mfaEnabled: Boolean)
// defined class AccountSecurity  

case class User(userName: String, email: String, accountSecurity: AccountSecurity)
// defined class User  

val user = User("user99", "user@email.com", AccountSecurity("123456!", mfaEnabled = true))
// user: User = User(user99,user@email.com,AccountSecurity(123456!,true))
```

`Lens` can focus on the top-level fields in a nested structure, which means that we cannot create a lens of <br/> 
`Lens[User, String]`, which can modify the password of the user like this
  
```scala
val inapplicableLens = Lens[User, String](_.accountSecurity.password) { user => password =>
  user.accountSecurity.copy(password = password)
}
// error: type mismatch;
// found   : AccountSecurity
// required: User  
```

In order to be able to focus on a deeply nested field, we need to define multiple lenses, and to compose them into a new `Lens`  

```scala
val accountSecurityLens = Lens[User, AccountSecurity](_.accountSecurity) { person => security => 
  person.copy(accountSecurity = security)
}
// accountSecurityLens: proptics.Lens[User,AccountSecurity] = proptics.Lens_$$anon$11@67fcf75c

val passwordLens = Lens[AccountSecurity, String](_.password) { security => password => 
  security.copy(password = password)
}
// passwordLens: proptics.Lens[AccountSecurity,String] = proptics.Lens_$$anon$11@73c60f21 

val userPasswordLens = accountSecurityLens andThen passwordLens
// userPasswordLens: proptics.Lens[User,String] = proptics.Lens_$$anon$2@27ae8f48

userPasswordLens.view(user)
// res0: String = 123456!
    
userPasswordLens.set("!654321")(user) 
// res1: User = User(user99,user@email.com,AccountSecurity(!654321,true))

userPasswordLens.over(_.reverse)(user)
// res2: User = User(user99,user@email.com,AccountSecurity(!654321,true))  
``` 

We can also use an inline composition

```scala
(accountSecurityLens andThen passwordLens).over(_.reverse)(user)
// res3: User = User(user99,user@email.com,AccountSecurity(!654321,true))  
``` 

## Laws

A `Lens` must satisfy all <a href="../../api/proptics/law/LensLaws">LensLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.Eq
// import cats.Eq

import cats.syntax.eq._
// import cats.syntax.eq._

implicit val eqUser: Eq[User] = Eq.fromUniversalEquals[User] // triple equals operator (===)
// eqUser: cats.Eq[User] = cats.kernel.Eq$$anon$6@52e0e22c
```

#### You get back what you set

```scala
def setGet[S: Eq, A](lens: Lens[S, A], s: S): Boolean =
  lens.set(lens.view(s))(s) === s

setGet[User, String](userPasswordLens, user)
// res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala
def getSet[S, A: Eq](lens: Lens[S, A], s: S, a: A): Boolean = 
  lens.view(lens.set(a)(s)) === a

getSet[User, String](userPasswordLens, user, "123456!")
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](lens: Lens[S, A], s: S, a: A): Boolean =
  lens.set(a)(lens.set(a)(s)) === lens.set(a)(s)

setSet[User, String](userPasswordLens, user, "123456!")
// res2: Boolean = true
```
