---
id: tagged
title: Tagged
---

`Tagged[A, B]` is a data type shaped like a `Profunctor`, that ignores it's first type parameter `A` and holds a value of type `B`.

```scala
case class Tagged[A, B](runTag: B)
```

The type argument `A` is used here as a phantom type, which means that it exists just to
satisfy some type constraints, but it does not have any effect on the runtime. in this example it serves as the first type argument for the `Profunctor`.

## Tagged as a review encoding

`Tagged` is used to construct a `Review_` optic.

```scala
abstract class Review_[S, T, A, B] {
  private[proptics] def apply(tagged: Tagged[A, B]): Tagged[S, T]
}
```

Let's see how the`review` method, is actually implemented in `Review_`.

```scala
def review(b: B): T = self(Tagged[A, B](b)).runTag
```

An optic is a function `P[A, B] => P[S, T]`. `Tagged` takes two type parameters like a `Profunctor`. I we replace the `P[_, _]` with `Tagged`
we would end up with a function `Tagged[A, B] => Tagged[S, T]`.<br/>Because `Tagged` ignores the first type parameter, the apply 
method can be represented as:

```scala
B => T
```

Which is equivalent to the `review` method.</br>

The `apply` function of `Review_[S, T, A, B]` takes a `Tagged[A, B]` and returns a new `Tagged[S, T]`.<br/> 
In `review` we call the `apply` function with a `Tagged[A, B]` instance that wraps a value `B` and ignores an `A`, which gives us a new instance of `Tagged[S, T]`.
The `Tagged[S, T]`  wraps a value `T` and ignores the `S`. In order to get a `T` for the return type of `review`, we just need to 
unwrap the `Tagged[S, T]` using `runTag`.
