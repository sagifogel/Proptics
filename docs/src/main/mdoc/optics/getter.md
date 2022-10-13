---
id: getter
title: Getter
---

A `Getter` describes how to retrieve a single value. It is similar to a [Fold](fold.md), but it
focuses on a single value.<br/> A `Getter[S, A]` is just any function `S => A`, everything you can do with a function, you can do with a `Getter`.

## Constructing a monomorphic Getter

`Getter[S, A]` is constructed using the <a href="../../api/proptics/Getter$">Getter[S, A]#apply</a> function.</br>
For a given `Getter[S, A]` it takes a function as argument `S => A`.


```scala
object Getter {
  def apply[S, A](f: S => A): Getter[S, A]
}
```

```scala
import proptics.Getter

val headOptionGetter: Getter[List[Int], Option[Int]] = Getter[List[Int]](_.headOption)

headOptionGetter.view(List.range(1, 6))
// val res0: Option[Int] = Some(1)
```

## Constructing a polymorphic Getter

`Getter_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Getter_$">Getter_[S, T, A, B]#apply</a> function.</br>
For a given `Getter_[S, T, A, B]` it takes a function as argument `S => A`.

```scala
object Getter_ {
  def apply[S, T, A, B](f: S => A): Getter[S, T, A, B]
}
```

## Methods

```scala
import proptics.Getter

val list = List.range(1, 6)
val headOptionGetter: Getter[List[Int], Option[Int]] = Getter[List[Int]](_.headOption)
```

#### [view](../../api/proptics/Getter_.html#view(s:S):A)

```scala
/** view the focus of a Getter */
def view(s: S): A
```

```scala
headOptionGetter.view(list)
// res0: Option[Int] = Some(1)
```

#### [exists](../../api/proptics/Getter_.html#exists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate holds for the focus of a Getter */
def exists(f: A => Boolean): S => Boolean
```

```scala
import cats.syntax.eq._
import cats.syntax.option._

headOptionGetter.exists(_ === 1.some)(list)
// res1: Boolean = true
```

#### [notExists](../../api/proptics/Getter_.html#notExists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate does not hold for the focus of a Getter */
def notExists(f: A => Boolean): S => Boolean
```

```scala
import cats.syntax.eq._
import cats.syntax.option._

headOptionGetter.notExists(_ === 9.some)(list)
// res2: Boolean = true
```

#### [contains](../../api/proptics/Getter_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Getter contains a given value */
 def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
headOptionGetter.contains(1.some)(list)
// res3: Boolean = true
```

#### [notContains](../../api/proptics/Getter_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Getter does not contain a given value */ 
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
headOptionGetter.notContains(9.some)(list)
// res4: Boolean = true
```

#### [find](../../api/proptics/Getter_.html#find(f:A=>Boolean):S=>Option[A])

```scala
/** find the first focus of a Getter that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
import cats.syntax.eq._
import cats.syntax.option._

headOptionGetter.find(_ === 1.some)(list).flatten
// res5: Option[Int] = Some(1)
```

#### [use](../../api/proptics/Getter_.html#use(implicitev:cats.data.State[S,A]):cats.data.State[S,A])

```scala
/** collect all the foci of a Getter in the state of a monad */
def use(implicit ev: State[S, A]): State[S, A]
```

```scala
import cats.syntax.option._
import cats.data.State

implicit val state: State[List[Int], Option[Int]] = State.pure[List[Int], Option[Int]](0.some)

headOptionGetter.use.runA(list.reverse).value
// val res6: Option[Int] = Some(5)
```

#### [focus](../../api/proptics/Getter_.html#focus[C,D](f:A=>C):proptics.Getter_[S,T,C,D])

```scala
/** compose this Getter with a function lifted to a Getter */
def focus[C](f: A => C): Getter[S, C]
```

```scala
val focusedGetter = 
  headOptionGetter.focus((headOption: Option[Int]) => headOption.map(_ + 1))

focusedGetter.view(list)
val res7: Option[Int] = Some(2)
```


## Getter internal encoding

#### Polymorphic Getter

```scala
Getter_[S, T, A, B]
```

`Getter_[S, T, A, B]` is a function `Forget[A, A, B] => Forget[A, S, T]`. [Forget](../data-types/forget.md) is a data type shaped like a profunctor, which forgets its last type parameter.

```scala
/**
  * @tparam S the source of a Getter_
  * @tparam T the modified source of a Getter_
  * @tparam A the focus of a Getter_
  * @tparam B the modified focus of anGetter_
  */
abstract class Getter_[S, T, A, B] {
  def apply(forget: Forget[A, A, B]): Forget[A, S, T]
}
```

Although `Getter_[S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic Getter`, serving as
a base type from which a `Monomorphic Fold` can be obtained.

#### Monomorphic Getter

```scala
Getter[S, A]
```

`Getter[S, A]` is a type alias for `Getter_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Getter[S, A] = Getter_[S, S, A, A]
``` 

Since `Getter` is read-only, `Getter[S, A]` is isomorphic to `Getter_[S, T, A, B]`.</br>
`Getter_[S, T, A, B]` takes `Forget[A, A, B]` which wraps a function `A => A` and  forgets the `B`, and returns `Forget[A, S, T]`
which wraps a function `S => A` and forgets the `T`,  and its representation can be simplified to:

```
(A => A) => S => A
```

Let's compare it to `Getter_[S, S, A, A]` which is equivalent to `Getter[S, A]`.</br>

```scala
def getter[S, A]: Getter[S, A] = new Getter_[S, S, A, A] {
  def apply(forget: Forget[A, A, A]): Forget[A, S, S]
}
```

`Getter_[S, S, A, A]` takes `Forget[A, A, A]` which wraps a function `A => A` and  forgets the last type parameter `A`, and returns `Forget[A, S, S]` which wraps a function `S => A` and forgets the last parameter `S`,
and its representation can be simplified to:

```
(A => A) => S => A
```

## Laws

Since a `Getter` cannot be used to write back there are no Lens laws that apply to it.
