---
id: review
title: Review
---

A `Review` is a write-only <a href="/Proptics/docs/optics/prism" target="_blank">Prism</a>, It
describes how to construct a single value. It's a dual of <a href="/Proptics/docs/optics/getter" target="_blank">Getter</a>.<br/>

## Review internal encoding

#### Polymorphic Review

```scala
Review_[S, T, A, B]
```

`Review_[S, T, A, B]` is a function `Tagged[A, B] => Tagged[S, T]`. [Tagged](/Proptics/docs/data-types/tagged) is a data type shaped like a profunctor, which ignores it's first type parameter.

```scala
/**
  * @tparam S the source of a Review_
  * @tparam T the modified source of a Review_
  * @tparam A the focus of a Review_
  * @tparam B the modified focus of a Review_
  */
abstract class Review_[S, T, A, B] {
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]
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
  private[proptics] def apply(tagged: Tagged[B, B]): Tagged[T, T] 
}
```

`Review[T, T, B, B]` takes `Tagged[B, B]` which wraps a value `B` and  ignores the first type parameter `B`, and returns `Tagged[T, T]` which wraps a value `T` and ignores the first type parameter `T`, 
and its representation can be simplified to:

```scala
B => T
```

## Constructing Reviews

`Review_[S, T, A, B]` is constructed using the [Review_[S, T, A, B]#apply](/Proptics/api/proptics/Review_$.html) function.</br>
For a given `Review_[S, T, A, B]` it takes a function `review: B => T` as argument.

```scala
object Review_ {
  def apply[S, T, A, B](f: B => T): Review[S, T, A, B]
}
```

`Review[S, A]` is constructed using the [Review[S, T, A, B]#apply](/Proptics/api/proptics/Review$.html) function.</br>
For a given `Review_[S, A]` it takes a function `review: A => S` as argument.

```scala
object Review {
  def apply[S, A](f: A => S): Review[S, A]
}
```

```scala
import proptics.Review
// import proptics.Review 

val listReview: Review[List[Int], Int] = Review[List[Int], Int](List(_))
// listReview: proptics.Review[List[Int],Int] = proptics.Review_$$anon$5@a6255a
```

## Common functions of a Setter

#### review
```scala
listReview.review(9)
// res0: List[Int] = List(9)
```

## Laws

Since a `Review` cannot be used to read back there are no laws that can be applied to it.
