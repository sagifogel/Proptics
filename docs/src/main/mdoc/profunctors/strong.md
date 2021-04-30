---
id: strong
title: Strong
---

The `Strong` class extends [Profunctor](profunctor.md) with combinators for working with product types, and has two lifting operations, `first` and `second`.

```scala
trait Strong[P[_, _]] extends Profunctor[P] {
  // Create a new `P` that takes two inputs, but only modifies the first input
  def first[A, B, C](fa: P[A, B]): P[(A, C), (B, C)]
  
  // Create a new `P` that takes two inputs, but only modifies the second input
  def second[A, B, C](fa: P[A, B]): P[(C, A), (C, B)]
}
```

In order to understand the `Strong` profunctor, we will specialize the `P[_, _]` type constructor to the `Function` type. Now the type signatures 
of the `first` and `second` methods are:

```scala
  def first[A, B, C](fa: A => B): (A, C) => (B, C)
  
  def second[A, B, C](fa: A => B): (C, A) => (C, B)
```  

Consider the `A` an input and the `B` an output, the `first` and the `second` methods lets us add another argument of type `C`
to the input `A`, which will be passed to the return type without modifications. The only difference is that in `first` method the `C` argument 
is tupled with the input as the right element, and in `second` method the `C` argument is tupled with the input as the left element.<br/>
Basically each method lets you add extra information to a computation and returns this information at the end of the computation, 
while preserving its value. 

## Function as Strong profunctor

Let's implement an instance of `Strong` for `Function`

```scala
import cats.arrow.Strong
// import cats.arrow.Strong

implicit val strongFunction: Strong[Function] = new Strong[Function] {
  override def first[A, B, C](fa: A => B): ((A, C)) => (B, C) = { case (a, c) => (fa(a), c) }

  override def second[A, B, C](fa: A => B): ((C, A)) => (C, B) = { case (c, a) => (c, fa(a)) }

  override def dimap[A, B, C, D](fab: A => B)(f: C => A)(g: B => D): C => D = g compose fab compose f
}
// strongFunction: cats.arrow.Strong[Function] = $anon$1@5c6db24b

val f: Int => Int = _ * 2
// f: Int => Int = $Lambda$10087/262289294@587b0af2

val firstFn = Strong[Function].first[Int, Int, Int](f)
// firstFn: Function[(Int, Int),(Int, Int)] = $anon$1$$Lambda$10086/1934538552@762ceea9

val secondFn = Strong[Function].second[Int, Int, Int](f)
// secondFn: Function[(Int, Int),(Int, Int)] = $anon$1$$Lambda$10088/1789117242@41df804d

firstFn((2, 3))
// res0: (Int, Int) = (4,3)

secondFn((2, 3))
// res0: (Int, Int) = (2,6)
``` 