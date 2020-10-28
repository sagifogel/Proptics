---
id: review
title: Review
---

A `Review` is a write-only <a href="/Proptics/docs/optics/prism" target="_blank">Prism</a>, It
describes how to construct a single value. It's a dual of <a href="/Proptics/docs/optics/getter" target="_blank">Getter</a>.<br/>
For a given `Review[S, A]` it takes a function `A => S`, which takes an `A` and returns a structure `S`.

## Constructing Reviews

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

## Review internal encoding

`Review[S, A]` is the monomorphic short notation version of the polymorphic one `Review_[S, T, A, B]`.

```scala
type Review[S, A] = Review_[S, S, A, A]
``` 

`Review_[S, T, A, B]` is a function `Tagged[A, B] => Tagged[S, T]`. [Tagged](/Proptics/docs/data-types/tagged) is a data type shaped like a profunctor, which ignores it's first type parameter.

```scala
abstract class Review_[S, T, A, B] extends Serializable {
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]
}
```

Since `Review` is write-only, `Review[T, B]` is isomorphic to `Review_[S, T, A, B]`.</br>
`Review_[S, T, A, B]` takes `Tagged[A, B]` which wraps a value of `B` and  ignores the `A`, and returns `Tagged[S, T]` 
 which wraps a value `T` and ignores the `S`,  and its representation can be simplified to:

```
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

```
B => T
```

## Laws

Since a `Review` cannot be used to read back there are no laws that can be applied to it.
