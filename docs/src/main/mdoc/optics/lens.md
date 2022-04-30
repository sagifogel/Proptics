---
id: lens
title: Lens
---

A `Lens` is an optic used to focus on a particular element in a deeply nested data structure, while letting you 
view, set or modify the focus when you know it exists, that is a `Lens` must never fail to get or modify the focus.<br/>
An intuition for `Lens` is a getter and setter like you might have on an object.

## Constructing a Lens

Consider a User data structure

```scala
case class AccountSecurity(password: String, mfaEnabled: Boolean)
case class User(userName: String, email: String, accountSecurity: AccountSecurity)

val user = User("user99", "user@email.com", AccountSecurity("123456!", mfaEnabled = true))
```

### Using companion object

`Lens[S, A]` is constructed using the <a href="../../api/proptics/Lens$#apply[S,A](view:S=>A)(set:S=>(A=>S)):proptics.Lens[S,A]">Lens[S, A]#apply</a> method.</br> 
For a given `Lens[S, A]` it takes two functions as arguments,`view: S => A` which is a getter function, that produces an `A` given an `S`,
and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a new structure `S`.

```scala
object Lens {
  def apply[S, A](view: S => A)(set: S => A => S): Lens[S, A]
}
```

We have user instance, and we want to focus on the email field, so we could
interact with it.

```scala
import proptics.Lens

val emailLens = Lens[User, String](_.email)(person => email => person.copy(email = email))

emailLens.view(user)
// val res0: String = user@email.com

emailLens.set("user@email.it")(user)
// val res1: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

### Using macros

Macros provide a convent way to create a Lens by removing the repetitive boilerplate code

```scala
import proptics.macros._

val emailLens = GLens[User](_.email)

emailLens.view(user)
// val res0: String = user@email.com

emailLens.set("user@email.it")(user)
// val res1: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

### Using Lens syntax

Syntax is probably the most intuitive way to work with Lenses, you just need to call the `lens`
extension method on the object to create a lens

```scala
import proptics.syntax.all._

val emailLens = 
  User("user99", "user@email.com", AccountSecurity("123456!", mfaEnabled = true))
    .lens(_.email)

emailLens.view
// val res0: String = user@email.com

emailLens.set("user@email.it")
// val res1: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

## Composability

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

### Using companion object

```scala
val accountSecurityLens =
  Lens[User, AccountSecurity](_.accountSecurity){ user => accountSecurity =>
    user.copy(accountSecurity = accountSecurity )
  }

val passwordLens = 
  Lens[AccountSecurity, String](_.password) { security => password => 
    security.copy(password = password)
  }

val userPasswordLens = accountSecurityLens andThen passwordLens

userPasswordLens.view(user)
// val res0: String = 123456!
    
userPasswordLens.set("!111111")(user) 
// val res1: User = User(user99,user@email.com,AccountSecurity(!111111,true))

userPasswordLens.over(_.reverse)(user)
// val res2: User = User(user99,user@email.com,AccountSecurity(!654321,true))
``` 
### Using macros

```scala
import proptics.macros._
 
val userPasswordLens = GLens[User](_.accountSecurity) andThen GLens[AccountSecurity](_.password)

userPasswordLens.view(user)
// val res0: String = 123456!

userPasswordLens.set("!111111")(user)
// val res1: User = User(user99,user@email.com,AccountSecurity(!111111,true))
```

In fact `GLens` can take nested path in order to create a lens

```scala
import proptics.macros._
 
val userPasswordLens = GLens[User](_.accountSecurity.password)

userPasswordLens.view(user)
// val res0: String = 123456!

userPasswordLens.set("!111111")(user)
// val res1: User = User(user99,user@email.com,AccountSecurity(!111111,true))
```

### Using Lens syntax

```scala
import proptics.syntax.all._

val userPasswordLens = 
  User("user99", "user@email.com", AccountSecurity("123456!", mfaEnabled = true))
    .lens(_.accountSecurity.password)

userPasswordLens.view
// val res0: String = 123456!

userPasswordLens.set("!111111")
// val res1: User = User(user99,user@email.com,AccountSecurity(!111111,true))
```

## Methods

#### [view](../../api/proptics/Lens_.html#view(s:S):A) 
```scala
/** view the focus of a Lens */
def view(s: S): A
```

```scala
emailLens.view(user)
// val res0: String = user@email.com
```

#### [set](../../api/proptics/Lens_.html#set(b:B):S=>T)
```scala
/** set the focus of a Lens */
def set(a: A): S => S
```

```scala
emailLens.set("user@email.it")(user)
// val res1: User =  User(user99,user@email.it,AccountSecurity(123456!,true))
```

#### [over](../../api/proptics/Lens_.html#over(f:A=>B):S=>T)
```scala
/** modify the focus of a Lens using a function */
def over(f: A => A): S => S
```

```scala
emailLens.over(_.replace("com", "it"))(user)
// val res2: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

#### [traverse](../../api/proptics/Lens_.html#traverse[F[_]](s:S)(f:A=>F[B])(implicitevidence$1:cats.Applicative[F]):F[T])
```scala
/** modify the focus of a Lens using a Functor */
def traverse[F[_]](s: S)(f: A => F[A])(implicit arg0: Applicative[F]): F[S]
```

```scala
val user2 = emailLens.set("user@email.it")(user)

def isComPostfix(email: String): Option[String] =
  Option.when(email.endsWith("com"))(email)

emailLens.traverse(user)(isComPostfix)
// val res3: Option[User] = Some(User(user99,user@email.com,AccountSecurity(123456!,true)))

emailLens.traverse(user2)(isComPostfix)
// val res4: Option[User] = None
```

#### [overF](../../api/proptics/Lens_.html#overF[F[_]](f:A=>F[B])(s:S)(implicitevidence$2:cats.Applicative[F]):F[T])
```scala
/** synonym for [[traverse]], flipped */
def overF[F[_]](f: A => F[B])(s: S)(implicit arg0: Applicative[F]): F[T]
```

```scala
val user2 = emailLens.set("user@email.it")(user)

def isComPostfix(email: String): Option[String] =
  Option.when(email.endsWith("com"))(email)

val partialLens = emailLens.overF(isComPostfix) _

partialLens(user)
// val res5: Option[User] = Some(User(user99,user@email.com,AccountSecurity(123456!,true)))

partialLens(user2)
// val res6: Option[User] = None
```

#### [exists](../../api/proptics/Lens_.html#exists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate holds for the focus of a Lens */
def exists(f: A => Boolean): S => Boolean
```

```scala
emailLens.exists(_.endsWith("com"))(user)
// val res7: Boolean = true
```

#### [notExists](../../api/proptics/Lens_.html#notExists(f:A=>Boolean):S=>Boolean)
```scala
/** test whether a predicate does not hold for the focus of a Lens */
def notExists(f: A => Boolean): S => Boolean
```

```scala
emailLens.notExists(_.endsWith("com"))(user)
// val res8: Boolean = false
```

#### [contains](../../api/proptics/Lens_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of a Lens contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
emailLens.contains("user@email.it")(user)
// val res9: Boolean = false
```

#### [notContains](../../api/proptics/Lens_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** test whether the focus of a Lens does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
emailLens.notContains("user@email.it")(user)
// val res10: Boolean = true
```

#### [find](../../api/proptics/Lens_.html#find(f:A=>Boolean):S=>Option[A])
```scala
/** find the first focus of a Lens that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
emailLens.find(_.endsWith("com"))(user)
// val res11: Option[String] = Some(user@email.com)
```

#### [cotraverse](../../api/proptics/Lens_.html#cotraverse[F[_]](fs:F[S])(f:F[A]=>B)(implicitevidence$2:cats.Comonad[F]):T)
```scala
/** modify an effectual focus of a Lens into the modified focus */
def cotraverse[F[_]](fs: F[S])(f: F[A] => A)(implicit arg0: Comonad[F]): S
```

```scala
import cats.Id

emailLens.cotraverse(Id(user))(_.replace("com", "it"))
// val res12: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

#### [zipWithF](../../api/proptics/Lens_.html#zipWithF[F[_]](f:F[A]=>B)(fs:F[S])(implicitevidence$3:cats.Comonad[F]):T)
```scala
/** synonym for [[cotraverse]], flipped */
def zipWithF[F[_]](fs: F[S])(f: F[A] => A)(implicit arg0: Comonad[F]): S
```

```scala
import cats.Id

emailLens.zipWithF[Id](identity)(user)
// val res13: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

#### <a href="../../api/proptics/Lens_.html#zipWith(s1:S,s2:S)(f:(A,A)=>B):T">zipWith</a>
```scala
/** zip two sources of a Lens together provided a binary operation */
def zipWith(s1: S, s2: S)(f: (A, A) => A): S
```

```scala
val user2 = User("user100", "user100@email.it", AccountSecurity("!654321", mfaEnabled = false))
emailLens.zipWith(user, user2) { (s1, s2) =>
  val Array(user, _) = s1.split('@')
  val Array(_, domain) = s2.split('@')

  s"$user@$domain"
}
// val res14: User = User(user99,user@email.it,AccountSecurity(123456!,true))
```

#### [use](../../api/proptics/Lens_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)
```scala
/** view the focus of a Lens in the state of a monad */
def use(implicit ev: State[S, A]): State[S, A]
```

```scala
implicit val state: State[User, String] = State.set(user).inspect(_.email)

emailLens.use.run(user).value
// val res15: (User, String) = 
//   (User(user99,user@email.com,AccountSecurity(123456!,true)),user@email.com)
```

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

`Lens_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`. <br />
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

## Laws

A `Lens` must satisfy all <a href="../../api/proptics/law/LensLaws">LensLaws</a>. These laws reside in the <a href="../../api/proptics/law">proptics.law</a> package.<br/>

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
// val res0: Boolean = true
```

#### Setting back what you got doesn't change anything

```scala
def getSet[S, A: Eq](lens: Lens[S, A], s: S, a: A): Boolean = 
  lens.view(lens.set(a)(s)) === a

getSet[User, String](userPasswordLens, user, "123456!")
// val res1: Boolean = true
```

#### Setting twice is the same as setting once

```scala
def setSet[S: Eq, A](lens: Lens[S, A], s: S, a: A): Boolean =
  lens.set(a)(lens.set(a)(s)) === lens.set(a)(s)

setSet[User, String](userPasswordLens, user, "123456!")
// val res2: Boolean = true
```