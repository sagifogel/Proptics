---
id: review
title: Review
---

A `Review` is a write-only [Prism](prism.md), It
describes how to construct a single value. It's a dual of [Getter](getter.md).<br/>

## Constructing a monomorphic Review

`Review[S, A]` is constructed using the <a href="../../api/proptics/Review$">Review[S, A]#apply</a> function.</br>
For a given `Review[S, A]` it takes a function `review: A => S` as argument.

```scala
object Review {
  def apply[S, A](f: A => S): Review[S, A]
}
```
```scala
import proptics.Review

def fibonacci(a: Int, b: Int): LazyList[Int] = a #:: fibonacci(b, a  + b)

val review = Review[List[Int], Int](n => fibonacci(0, 1).take(n).toList)

review.review(7)
// val res0: List[Int] = List(0, 1, 1, 2, 3, 5, 8)
```

## Constructing a polymorphic Review

`Review_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Review_$">Review_[S, T, A, B]#apply</a> function.</br>
For a given `Review_[S, T, A, B]` it takes a function `review: B => T` as argument.

```scala
object Review_ {
  def apply[S, T, A, B](f: B => T): Review[S, T, A, B]
}
```

## Methods

#### [review](../../api/proptics/Review_.html#review(b:B):T)

```scala
/** view the modified source of a Review */
def review(a: A): S
```

```scala
import proptics.Review

final case class Whole(part: Int)

val wholeReview = Review[Whole, Int](Whole.apply)

wholeReview.review(9)
// val res0: Whole = Whole(9)
```

#### [use](../../api/proptics/Review_.html#use(implicitev:cats.data.State[B,T]):cats.data.State[B,T])

```scala
/** view the modified focus of a Review in the state of a monad */
def use(implicit ev: cats.data.State[A, S]): State[A, S]
```

```scala
import cats.data.State
import proptics.Review

final case class Whole(part: Int)

implicit val state: State[Int, Whole] = State.pure[Int, Whole](Whole(1))
val wholeReview = Review[Whole, Int](Whole.apply)

wholeReview.use.runA(9).value
// val res1: Whole = Whole(9)
```

## Review internal encoding

#### Polymorphic Review

```scala
Review_[S, T, A, B]
```

`Review_[S, T, A, B]` is a function `Tagged[A, B] => Tagged[S, T]`. [Tagged](../data-types/tagged.md) is a data type shaped like a profunctor, which ignores it's first type parameter.

```scala
/**
  * @tparam S the source of a Review_
  * @tparam T the modified source of a Review_
  * @tparam A the focus of a Review_
  * @tparam B the modified focus of a Review_
  */
abstract class Review_[S, T, A, B] {
  def apply(tagged: Tagged[A, B]): Tagged[S, T]
}
```

`Review_[S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure from `S` to `T`.</br>
 A `Review` that changes its focus/structure, is called `Polymorphic Review`.
 
 #### Monomorphic Review
 
```scala
Review[S, A]
```

`Review[S, A]` is a type alias for `Review_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Review[S, A] = Review_[S, S, A, A]
``` 

A `Review` that does not change its focus/structure, is called `Monomorphic Review`.

Since `Review` is write-only, `Review[T, B]` is isomorphic to `Review_[S, T, A, B]`.</br>
`Review_[S, T, A, B]` takes `Tagged[A, B]` which wraps a value of `B` and  ignores the `A`, and returns `Tagged[S, T]` 
 which wraps a value `T` and ignores the `S`,  and its representation can be simplified to:

```scala
B => T
```
Let's compare it to `Review[T, T, B, B]` which is equivalent to `Review[T, B]`.</br> 

```scala
def review[T, B]: Review[T, B] = new Review_[T, T, B, B] {
  def apply(tagged: Tagged[B, B]): Tagged[T, T] 
}
```

`Review[T, T, B, B]` takes `Tagged[B, B]` which wraps a value `B` and  ignores the first type parameter `B`, and returns `Tagged[T, T]` which wraps a value `T` and ignores the first type parameter `T`, 
and its representation can be simplified to:

```scala
B => T
```

## Laws

Since a `Review` cannot be used to read back there are no laws that can be applied to it.
