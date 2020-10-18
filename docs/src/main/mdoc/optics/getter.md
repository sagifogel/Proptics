---
id: getter
title: Getter
---

A `Getter` describes how to retrieve a single value. It is similar to a <a href="/Proptics/docs/optics/fold" target="_blank">Fold</a>, but it 
a focuses on a single value.<br/> A `Getter[S, A]` is just any function `S => A`, everything you can do with a function, you can do with a Getter.
For a given `Getter[S, A]` it takes a function which takes a structure `S` and returns an `A`.

## Constructing Getters

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

## Getter internal encoding

`Getter[S, A]` is the monomorphic short notation version of the polymorphic one `Getter_[S, T, A, B]`.

```scala
type Getter[S, A] = Getter_[S, S, A, A]
``` 

`Getter_[S, T, A, B]` is basically a function `Forget[A, A, B] => Forget[A, S, T]`. [Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets its last type parameter.

```scala
abstract class Getter_[S, T, A, B] extends Serializable {
  private[proptics] def apply(forget: Forget[A, A, B]): Forget[A, S, T]
}
```

Since `Getter` is read only, `Getter[S, A]` is isomorphic to `Getter_[S, T, A, B]`.</br>
`Getter_[S, T, A, B]` takes `Forget[A, A, B]` which wraps a function `A => A` and  forgets the `B`, and returns `Forget[A, S, T]` 
 which wraps a function `S => A` and forgets the `T`,  and its representation can be simplified to:

```
(A => A) => S => A
```

Let's compare it to `Getter[S, S, A, A]` which is equivalent to `Getter[S, A]`.</br> 

```scala
def getter[S, A]: Getter[S, A] = new Getter_[S, S, A, A] {
  override private[proptics] def apply(forget: Forget[A, A, A]): Forget[A, S, S]
}
```

`Getter[S, S, A, A]` takes `Forget[A, A, A]` which wraps a function `A => A` and  forgets the last type parameter `A`, and returns `Forget[A, S, S]` which wraps a function `S => A` and forgets the last parameter `S`, 
and its representation can be simplified to:

```
(A => A) => S => A
```

## Laws

Since a `Getter` cannot be used to write back there are no Lens laws that apply to it.
