---
id: getter
title: Getter
---

A `Getter` describes how to retrieve a single value. It is similar to a <a href="/Proptics/docs/optics/fold" target="_blank">Fold</a>, but it 
a focuses on a single value.<br/> A `Getter[S, A]` is just any function `S => A`, everything you can do with a function, you can do with a Getter.

## Getter internal encoding

#### Polymorphic Getter

`Getter_[S, T, A, B]` is a function `Forget[A, A, B] => Forget[A, S, T]`. [Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets its last type parameter.

```scala
/**
  * @tparam S the source of a Getter_
  * @tparam T the modified source of a Getter_
  * @tparam A the focus of a Getter_
  * @tparam B the modified focus of anGetter_
  */
abstract class Getter_[S, T, A, B] extends Serializable {
  private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T]
}
```

Although `Getter_[S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic Getter`, serving as
base type from which a `Monomorphic Fold` can be obtained.

#### Monomorphic Traversal

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
  override private[proptics] def apply(forget: Forget[A, A, A]): Forget[A, S, S]
}
```

`Getter_[S, S, A, A]` takes `Forget[A, A, A]` which wraps a function `A => A` and  forgets the last type parameter `A`, and returns `Forget[A, S, S]` which wraps a function `S => A` and forgets the last parameter `S`, 
and its representation can be simplified to:

```
(A => A) => S => A
```

## Constructing Getters

`Getter_[S, T, A, B]` is constructed using the [Getter_[S, T, A, B]#apply](/Proptics/api/proptics/Getter_$.html) function.</br>
For a given `Getter_[S, T, A, B]` it takes a function as argument `S => A`.

```scala
object Getter_ {
  def apply[S, T, A, B](f: S => A): Getter[S, T, A, B]
}
```

`Getter[S, A]` is constructed using the [Getter[S, T, A, B]#apply](/Proptics/api/proptics/Getter$.html) function.</br>
For a given `Getter[S, A]` it takes a function as argument `S => A`.


```scala
object Getter {
  def apply[S, A](f: S => A): Getter[S, A]
}
```

```scala
import proptics.Getter
// import proptics.Getter

import cats.instances.option._ // instance of Eq[Option]
// import cats.instances.option._ 

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import cats.syntax.option._ // some function
// import cats.syntax.option._

val list = List.range(1, 5)
// list: List[Int] = List(1, 2, 3, 4)

val headOptionGetter: Getter[List[Int], Option[Int]] = Getter[List[Int], Option[Int]](_.headOption)
// headOptionGetter: proptics.Getter[List[Int],Option[Int]] = proptics.Getter_$$anon$10@32638083
```

## Common functions of a Getter

#### view
```scala
headOptionGetter.view(list)
// res0: Option[Int] = Some(1)
```

#### exists
```scala
headOptionGetter.exists(_ === 1.some)(list)
// res1: Boolean = true
```

#### contains
```scala
headOptionGetter.contains(1.some)(list)
// res2: Boolean = true
```

#### find

```scala
headOptionGetter.find(_ === 1.some)(list).flatten
// res3: Option[Int] = Some(1)
```

## Laws

Since a `Getter` cannot be used to write back there are no Lens laws that apply to it.
