---
id: lens
title: Lens
---

A `Lens` is an optic used to focus on a particular element in a deeply nested data structure, while letting you 
view, set or modify the focus when you know it exists, that is a `Lens` must never fail to get or modify the focus.<br/>
`Lens[S, A]` means that `S` is the structure or whole and `A` is the focus, or the part.<br/>
An intuition for `Lens` is a getter and setter like you might have on an object.

## Constructing Lenses

`Lens` is constructed using the [Lens[S, A]#apply](/Proptics/api/proptics/Lens$.html) function. For a given `Lens[S, A]` it takes two functions as arguments,
`view` which is a getter function, that produces an `A` given an `S`, and `set` function which takes a structure `S` and a focus `A` and returns a
new structure `S`.

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

val userPasswordLens = accountSecurityLens compose passwordLens
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
(accountSecurityLens compose passwordLens).over(_.reverse)(user)
// res3: User = User(user99,user@email.com,AccountSecurity(!654321,true))  
``` 

## Lens internal encoding

`Lens[S, A]` is the monomorphic short notation version (does not change the type of the structure) of the polymorphic one `Lens_[S, T, A, B]`

```scala
type Lens[S, A] = Lens_[S, S, A, A]
``` 

`Lens_[S, T, A, B]` is basically a function `P[A, B] => P[S, T]` that takes a [Strong](/Proptics/docs/profunctors/strong) of P[_, _].

```scala
abstract class Lens_[S, T, A, B] extends Serializable {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Strong[P]): P[S, T]
}
```

## Laws

A `Lens` must satisfy all [LensLaws](/Proptics/api/proptics/law/LensLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

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
// setGet: [S, A](lens: proptics.Lens[S,A], s: S)(implicit evidence$1: cats.Eq[S])Boolean

setGet[User, String](userPasswordLens, user)
// res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala
def getSet[S, A: Eq](lens: Lens[S, A], s: S, a: A): Boolean = 
  lens.view(lens.set(a)(s)) === a
// getSet: [S, A](lens: proptics.Lens[S,A], s: S, a: A)(implicit evidence$1: cats.Eq[A])Boolean

getSet[User, String](userPasswordLens, user, "123456!")
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](lens: Lens[S, A], s: S, a: A): Boolean =
  lens.set(a)(lens.set(a)(s)) === lens.set(a)(s)
// setSet: [S, A](lens: proptics.Lens[S,A], s: S, a: A)(implicit evidence$1: cats.Eq[S])Boolean

setSet[User, String](userPasswordLens, user, "123456!")
// res2: Boolean = true
```
