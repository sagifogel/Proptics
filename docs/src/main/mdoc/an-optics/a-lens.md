---
id: a-lens
title: ALens
---

`ALens` is similar to <a href="/Proptics/docs/optics/lens" target="_blank">Lens</a>, but has different internal encodings, it is used
to focus on a particular element in a deeply nested data structure, while letting you view, set or modify the focus when you know it exists, that is a `ALens` must never fail to get or modify the focus.
An intuition for `ALens` is a getter and setter like you might have on an object.


## ALens internal encoding

#### Polymorphic ALens

```scala
ALens_[S, T, A, B]
```

`ALens_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of [Shop](/Proptics/docs/data-types/shop), thus making 
it a function `Shop[A, B, A, B] => Shop[A, B, S, T]`.

```scala
/**
  * @tparam S the source of a ALens_
  * @tparam T the modified source of a ALens_
  * @tparam A the focus of a ALens_
  * @tparam B the modified focus of a ALens_
  */  
abstract class ALens_[S, T, A, B] {
  def apply(shop: Shop[A, B, A, B]): Shop[A, B, S, T]
}
```

`ALens_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`. </br>
An `ALens` that changes its focus/structure, is called `Polymorphic ALens`.

#### Monomorphic Lens

```scala
ALens[S, A]
```


`ALens[S, A]` is a type alias for `ALens_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type ALens[S, A] = ALens_[S, S, A, A]
``` 

`ALens[S, A]` means that `S` is the structure or whole and `A` is the focus, or the part.<br/>
An `ALens` that does not change its focus/structure, is called `Monomorphic ALens`.

## Constructing ALenses

`ALens_[S, T, A, B]` is constructed using the [ALens_[S, T, A, B]#apply](/Proptics/api/proptics/ALens_$.html) function.</br>
For a given `Lens[S, A]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces an `A` given an `S`, 
and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns a structure of `T`.

```scala
object ALens_ {
  def apply[S, T, A, B](view: S => A)(set: S => B => T): ALens_[S, T, A, B]
}
```

`ALens[S, A]` is constructed using the [ALens[S, A]#apply](/Proptics/api/proptics/ALens$.html) function. For a given `ALens[S, A]` it takes two functions as arguments,
`view: S => A` which is a getter function, that produces an `A` given an `S`, and<br/>  `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S`.

```scala
object ALens {
  def apply[S, A](view: S => A)(set: S => A => S): ALens[S, A]
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
import proptics.ALens
// import proptics.ALens

val emailLens = ALens[User, String](_.email)(person => email => person.copy(email = email))
// emailLens: proptics.ALens[User,String] = proptics.ALens_$$anon$11@35a0773a
```

## Common functions of a ALens

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

`ALens` can focus on the top-level fields in a nested structure, which means that we cannot create a lens of <br/> 
`ALens[User, String]`, which can modify the password of the user like this
  
```scala
val inapplicableLens = ALens[User, String](_.accountSecurity.password) { user => password =>
  user.accountSecurity.copy(password = password)
}
// error: type mismatch;
// found   : AccountSecurity
// required: User  
```

In order to be able to focus on a deeply nested field, we need to define multiple lenses, and to compose them into a new `ALens`  

```scala
val accountSecurityLens = ALens[User, AccountSecurity](_.accountSecurity) { person => security => 
  person.copy(accountSecurity = security)
}
// accountSecurityLens: proptics.ALens[User,AccountSecurity] = proptics.ALens_$$anon$11@67fcf75c

val passwordLens = ALens[AccountSecurity, String](_.password) { security => password => 
  security.copy(password = password)
}
// passwordLens: proptics.ALens[AccountSecurity,String] = proptics.ALens_$$anon$11@73c60f21 

val userPasswordLens = accountSecurityLens compose passwordLens
// userPasswordLens: proptics.ALens[User,String] = proptics.ALens_$$anon$2@27ae8f48

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

## Exporting Shop as data type of ALens

`ALens` allows us to export its internal construction logic to a `Shop` using the `toShop` method.

```scala
import proptics.ALens
// import proptics.ALens

val tupleLens: ALens[(Int, String), Int] = ALens[(Int, String), Int](_._1) { 
  case(_, s) => i => (i, s) 
}
// tupleLens: proptics.ALens[(Int, String),Int] = proptics.ALens_$$anon$12@28eb4316

val shop = tupleLens.toShop
//shop: proptics.internal.Shop[Int,Int,(Int, String),(Int, String)] = 
//  Shop(scala.Function1$$Lambda$32794/0x000000080398f840@51b6fb0e,
//       proptics.ALens_$$$Lambda$32795/0x000000080398d840@64eeb60e)

shop.view((9, "Hello"))
// res0: Int = 9

shop.set((1, "Hello"))(9)
// res1: (Int, String) = (9,Hello)
```

We can later on create a new instance of `ALens` or `Lens` from the shop instance

```scala
import proptics.Lens
// import proptics.Lens

import proptics.ALens
// import proptics.ALens

val aLensFromShop: ALens[(Int, String), Int] = ALens[(Int, String), Int](shop.view)(shop.set)
// aLensFromShop: proptics.ALens[(Int, String),Int] = proptics.ALens_$$anon$12@1e797afb

val lensFromShop: Lens[(Int, String), Int] = Lens[(Int, String), Int](shop.view)(shop.set)
// lensFromShop: proptics.Lens[(Int, String),Int] = proptics.Lens_$$anon$11@7f2ed0a1
```

## Laws

A `ALens` must satisfy all [ALensLaws](/Proptics/api/proptics/law/ALensLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>

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
def setGet[S: Eq, A](lens: ALens[S, A], s: S): Boolean =
  lens.set(lens.view(s))(s) === s

setGet[User, String](userPasswordLens, user)
// res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala
def getSet[S, A: Eq](lens: ALens[S, A], s: S, a: A): Boolean = 
  lens.view(lens.set(a)(s)) === a

getSet[User, String](userPasswordLens, user, "123456!")
// res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](lens: ALens[S, A], s: S, a: A): Boolean =
  lens.set(a)(lens.set(a)(s)) === lens.set(a)(s)

setSet[User, String](userPasswordLens, user, "123456!")
// res2: Boolean = true
```



