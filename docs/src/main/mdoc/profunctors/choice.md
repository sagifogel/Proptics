---
id: choice
title: Choice
---

The `Chocie` class extends [Profunctor](profunctor.md) with combinators for working with sum types, and has two lifting operations, `left` and `right`.

```scala
trait Choice[P[_, _]] extends Profunctor[P] {
  // Create a new `P` that takes an either, and maps over the left element of the either
  def left[A, B, C](pab: P[A, B]): P[Either[A, C], Either[B, C]]
  
  // Create a new `P` that takes an either, and maps over the right element of the either
  def right[A, B, C](pab: P[A, B]): P[Either[C, A], Either[C, B]]
}
```

In order to understand the `Choice` profunctor, we will specialize the `P[_, _]` type constructor to the `Function` type. Now the type signatures 
of the `left` and `right` methods are:

```scala
  def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C]
  
  def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B]
```  

Consider the `A` an input and the `B` an output, the `left` and the `right` methods lets us choose between the input `A`, 
or another argument of type `C` embedded within an `Either`. If the argument sent to the function is an `A` embedded within an `Either`,
then the input will be mapped to an output embedded within an `Either`, or else the `C` embedded within an `Either` will be the return value.
The only difference between the methods is that `left` takes an `Either[A, C]` which has its input `A` on the left side, and will use `leftMap` function 
to transform the input to an output of `B`, while the `right` method takes an `Either[C, A]` which has its input `A` on the right side, and will use `map`
to transform the input to an output of `B`

## Function as Choice profunctor

Let's implement an instance of `Choice` for `Function`

```scala
import cats.syntax.either._ // for the leftMap, asRight, asLeft
// import cats.syntax.either._

import proptics.profunctor.Choice
// import proptics.profunctor.Choice

implicit val choiceFunction: Choice[Function] = new Choice[Function] {
  override def left[A, B, C](pab: A => B): Either[A, C] => Either[B, C] = _.leftMap(pab)

  override def right[A, B, C](pab: A => B): Either[C, A] => Either[C, B] = _.map(pab)

  override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
}
// choiceFunction: proptics.profunctor.Choice[Function] = $anon$1@61b256e1

val f: Int => Int = _ * 2
// f: Int => Int = $Lambda$11374/934306524@6d1a9a1f

val leftFn = Choice[Function].left[Int, Int, Int](f)
// leftFn: Function[Either[Int,Int],Either[Int,Int]] = $anon$1$$Lambda$11375/127097346@43714523

val rightFn = Choice[Function].right[Int, Int, Int](f)
// rightFn: Function[Either[Int,Int],Either[Int,Int]] = $anon$1$$Lambda$11376/361603221@c2d7e79

leftFn(2.asLeft[Int])
// res0: Either[Int,Int] = Left(4)

leftFn(3.asRight[Int])
// res1: Either[Int,Int] = Right(3)

rightFn(2.asLeft[Int])
// res2: Either[Int,Int] = Left(2)

rightFn(3.asRight[Int])
// res3: Either[Int,Int] = Right(6)
``` 

