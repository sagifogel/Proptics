---
id: fold
title: Fold
---

A `Fold` describes how to retrieve multiple values. It is similar to a [Traversal](traversal.md), but it 
cannot modify its foci. Everything you can do with a Foldable, you can do with a `Fold`.

## Constructing a monomorphic Fold

### Using companion object

`Fold[S, A]` is constructed using the <a href="../../api/proptics/Fold$">Fold[S, A]#apply</a> function. For a given `Fold[S, A]`
it takes a fold function as argument, `S => A`.

```scala
object Fold {
  def apply[S, A](f: S => A): Fold[S, A]
}
```

```scala
import proptics.Fold

val fold = Fold[List[Int], Int](_.sum)

fold.foldLeft(List(1, 2, 3))(10)(_ + _)
// val res0: Int = 6
```

### Using fromFoldable method

Using the companion object of a `Fold` creates just a [Getter](getter.md).<br />
Most of the time you will be dealing with `Foldable` types. 

```scala
/** create a monomorphic Fold from cats.Foldable */
def fromFoldable[F[_]: Foldable, A]: Fold[F[A], A]
```

```scala
import cats.instances.all._

val list = List(1, 2, 3, 4)
val listFoldable = Fold.fromFoldable[List, Int]
val tupleFoldable = Fold.fromFoldable[(String, *), Int]

listFoldable.foldLeft(List(1, 2, 3))(0)(_ + _)
// val res0: Int = 6

tupleFoldable.foldMap(("Proptics", 8))((i: Int) => i + 1)
// val res1: Int = 9
```

### Using foldable syntax

```scala
import proptics.syntax.all._

List(1, 2, 3)
  .foldable
  .foldLeft(0)(_ + _)
// val res0: Int = 6

("Proptics", 8)
  .foldable
  .foldMap((i: Int) => i + 1)
// val res1: Int = 9
```

### Using both method

```scala
/** fold both parts of a cats.Bifoldable with matching types */
def both[G[_, _]: Bifoldable, A]: Fold[G[A, A], A]
```

```scala
import cats.instances.all._

val tupleBifoldable = Fold.both[(*, *), Int]

tupleBifoldable.foldMap((1, 8))(identity[Int])
// val res0: Int = 9
```

### Using bifold syntax

```scala
import proptics.syntax.all._

(1, 8).bifold.foldMap(identity[Int])
// val res0: Int = 9
```

### Using single method

```scala
/** create a monomorphic Fold that narrows the focus to a single element */
final def single[F[_]: Foldable, A](i: Int): Fold[F[A], A] 
```

```scala
val singletonlistFoldable = Fold.single[List, Int](1)

singletonListFoldable.foldLeft(List(1, 2, 3))(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2)
```

### Using single syntax

```scala
import proptics.syntax.all._

List(1, 2, 3)
 .foldable
 .single(1)
 .foldLeft(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2)
```

### Using take method

```scala
/** create a monomorphic Fold that selects the first n elements of a Foldable */
def take[G[_]: Foldable, A](i: Int): Fold[G[A], A]
```

```scala
val listFoldable = Fold.take[List, Int](2)

listFoldable.foldLeft(List(1, 2, 3))(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
```

### Using take syntax

```scala
import proptics.syntax.all._

List(1, 2, 3)
 .foldable
 .take(2)
 .foldLeft(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
```

## Using drop method

```scala
/** create a monomorphic Fold that selects all elements of a Foldable except the first n ones */
final def drop[G[_]: Foldable, A](i: Int): Fold[G[A], A]
```

```scala
val listFoldable = Fold.drop[List, Int](1)

listFoldable.foldLeft(List(1, 2, 3))(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2, 3)
```

### Using drop syntax

```scala
import proptics.syntax.all._

List(1, 2, 3)
 .foldable
 .drop(1)
 .foldLeft(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2, 3)
```

## Using takeWhile method

```scala
/** 
 * create a monomorphic Fold that takes the longest prefix of 
 * elements of a Foldable that satisfy a predicate 
 */
def takeWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A]
```

```scala
val listFoldable = Fold.takeWhile[List, Int](_ < 3)

listFoldable.foldLeft(List(1, 2, 3))(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
```

### Using takeWhile syntax

```scala
import proptics.syntax.all._

List(1, 2, 3)
 .foldable
 .takeWhile(_ < 3)
 .foldLeft(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(1, 2)
```

## Using dropWhile method

```scala
/** 
 * create a monomorphic Fold that drop longest prefix of 
 * elements of a Foldable that satisfy a predicate 
 */
final def dropWhile[G[_]: Foldable, A](predicate: A => Boolean): Fold[G[A], A]
```

```scala
import cats.syntax.eq._

val listFoldable = Fold.dropWhile[List, Int](_ === 1)

listFoldable.foldLeft(List(1, 2, 3))(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2, 3)
```

### Using dropWhile syntax

```scala
import proptics.syntax.all._
import cats.syntax.eq._

List(1, 2, 3)
 .foldable
 .dropWhile(_ === 1)
 .foldLeft(Vector.empty[Int])(_ :+ _)
// val res0: scala.collection.immutable.Vector[Int] = Vector(2, 3)
```

## Constructing a polymorphic Fold

`Fold_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Fold_$">Fold_[S, T, A, B]#apply</a> function.</br>
For a given `Fold_[S, T, A, B]` it takes a fold function `S => A` as an argument.

```scala
object Fold_ {
  def apply[S, T, A, B](f: S => A): Fold_[S, T, A, B]
}
```

## Methods

#### [view](../../api/proptics/Fold_.html#view(s:S)(implicitev:cats.Monoid[A]):A)

```scala
/** fold the foci of a Fold using a cats.Monoid */
def view(s: S)(implicit ev: Monoid[A]): A
```

```scala
listFoldable.view(list)
// val res0: Int = 10
```

#### [viewAll](../../api/proptics/Fold_.html#viewAll(s:S):List[A])

```scala
/** collect all the foci of a Fold into aList */
def viewAll(s: S): List[A]
```

```scala
val vector = Vector(1, 2, 3, 4)
val vectorFoldable = Fold.fromFoldable[Vector, Int]

vectorFoldable.viewAll(vector)
// val res1: List[Int] = List(1, 2, 3, 4)
```

#### [preview](../../api/proptics/Fold_.html#preview(s:S):Option[A])

```scala
/** view the first focus of a Fold, if there is any */
def preview(s: S): Option[A]
```

```scala
listFoldable.preview(list)
// val res2: Option[Int] = Some(1)
```

#### [fold](../../api/proptics/Fold_.html#fold(s:S)(implicitev:cats.Monoid[A]):A)

```scala
/** synonym for view */
def fold(s: S): Option[A]
```

```scala
listFoldable.fold(list)
// val res3: Int = 10
```

#### [foldMap](../../api/proptics/Fold_.html#foldMap[R](s:S)(f:A=>R)(implicitevidence$2:cats.Monoid[R]):R)

```scala
/** map each focus of a Fold to a cats.Monoid, and combine the results */
def foldMap[R](s: S)(f: A => R)(implicit arg0: Monoid[R]): R
```

```scala
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.option._

def isEven(i: Int): Option[Int] =
  Option.when(i % 2 === 0)(i)

listFoldable.foldMap(list)(isEven)
// val res4: Option[Int] = Some(6)
```

#### <a href="../../api/proptics/Fold_.html#foldRight[R](s:S)(r:R)(f:(A,R)=>R):R">foldRight</a>

```scala
/** fold the foci of a Fold using a binary operator, going right to left */
def foldRight[R](s: S)(r: R)(f: (A, R) => R): R
```

```scala
listFoldable.foldRight(list)(Vector.empty[Int])(_ +: _)
// val res5: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4)
```

#### <a href="../../api/proptics/Fold_.html#foldLeft[R](s:S)(r:R)(f:(R,A)=>R):R">foldLeft</a>

```scala
/** fold the foci of a Fold using a binary operator, going left to right */
def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R
```

```scala
listFoldable.foldLeft(list)(Vector.empty[Int])(_ :+ _)
// val res6: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4)
```

#### [exists](../../api/proptics/Fold_.html#exists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate holds for the focus of a Fold */
def exists(f: A => Boolean): S => Boolean
```
```scala
listFoldable.exists(_ < 9)(list)
// val res7: Boolean = true
```

#### [notExists](../../api/proptics/Fold_.html#notExists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate does not hold for the focus of a Fold */
def notExists(f: A => Boolean): S => Boolean
```

```scala
listFoldable.notExists(_ < 9)(list)
// val res8: Boolean = false
```

#### [contains](../../api/proptics/Fold_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Fold contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
listFoldable.contains(9)(list)
// val res9: Boolean = false
```

#### [notContains](../../api/proptics/Fold_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Fold does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
listFoldable.notContains(9)(list)
// val res10: Boolean = true
```

#### [isEmpty](../../api/proptics/Fold_.html#isEmpty(s:S):Boolean)

```scala
/** check if the Fold does not contain a focus */
def isEmpty(s: S): Boolean
```

```scala
listFoldable.isEmpty(list)
// val res11: Boolean = false
```

#### [nonEmpty](../../api/proptics/Fold_.html#nonEmpty(s:S):Boolean)

```scala
/** check if the Fold contains a focus */
def nonEmpty(s: S): Boolean
```

```scala
listFoldable.nonEmpty(List.empty[Int])
// val res12: Boolean = false
```

#### [find](../../api/proptics/Fold_.html#find(f:A=>Boolean):S=>Option[A])

```scala
/** find the focus of an Fold that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
listFoldable.find(_ === 9)(list)
// val res13: Option[Int] = None
```

#### [first](../../api/proptics/Fold_.html#first(s:S):Option[A])

```scala
/** synonym for preview */
def first(s: S): Option[A]
```

```scala
listFoldable.first(list)
// val res14: Option[Int] = Some(1)
```
#### [last](../../api/proptics/Fold_.html#last(s:S):Option[A])

```scala
/** find the last focus of a Fold, if there is any */
def last(s: S): Option[A]
```

```scala
listFoldable.last(list)
// val res15: Option[Int] = Some(4)
```

#### [length](../../api/proptics/Fold_.html#length(s:S):Int)

```scala
/** the number of foci of a Fold */
def length(s: S): Int
```

```scala
listFoldable.length(list)
// val res16: Int = 4
```

#### [maximum](../../api/proptics/Fold_.html#maximum(s:S)(implicitev:cats.kernel.Order[A]):Option[A])

```scala
/** the maximum of all foci of a Fold, if there is any /*
def maximum(s: S)(implicit ev: Order[A]): Option[A]
```

```scala
import cats.instances.int._

listFoldable.maximum(list)
// val res17: Option[Int] = Some(4)
```

#### [minimum](../../api/proptics/Fold_.html#minimum(s:S)(implicitev:cats.kernel.Order[A]):Option[A])

```scala
/** the minimum of all foci of a Fold, if there is any /*
def minimum(s: S)(implicit ev: Order[A]): Option[A]
```

```scala
import cats.instances.int._

listFoldable.minimum(list)
// val res18: Option[Int] = Some(1)
```

#### [mkString](../../api/proptics/Fold_.html#mkString(s:S)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Fold in a string */
def mkString(s: S)(implicit ev: S <:< Iterable[A]): String
```

```scala
listFoldable.mkString(list)
// val res19: String = 1234
```

#### [mkString](../../api/proptics/Fold_.html#mkString(s:S,sep:String)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Fold in a string using a separator */
def mkString(s: S, sep: String)(implicit ev: S <:< Iterable[A]): String
```

```scala
listFoldable.mkString(list, ", ")
// val res20: String = 1, 2, 3, 4
```

#### [mkString](../../api/proptics/Fold_.html#mkString(s:S,start:String,sep:String,end:String)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Fold in a string using a start, end and a separator */
def mkString(s: S, start: String, sep: String, end: String)(implicit ev: S <:< Iterable[A]): String
```

```scala
listFoldable.mkString(list, "[", ", ", "]")
// val res21: String = [1, 2, 3, 4]
```

#### [intercalate](../../api/proptics/Fold_.html#intercalate(s:S,a:A)(implicitev0:cats.Monoid[A],implicitev1:S<:<Iterable[A]):A)

```scala
/** intercalate/insert an element between the existing elements while folding */
final def intercalate(s: S, a: A)(implicit ev0: Monoid[A], ev1: S <:< Iterable[A]): A
```

```scala
listFoldable.intercalate(list, 0)
// val res22: Int = 10

listFoldable.intercalate(list, 2)
// val res23: Int = 16
```

#### [forall](../../api/proptics/Fold_.html#forall(f:A=>Boolean):S=>Boolean)

```scala
/** test whether there is no focus or a predicate holds for the focus of a Fold */
def forall(f: A => Boolean): S => Boolean
```

```scala
listFoldable.forall(_ < 9)(list)
// val res24: Boolean = true
```

#### [forall](../../api/proptics/Fold_.html#forall[R](s:S)(f:A=>R)(implicitevidence$1:spire.algebra.lattice.Heyting[R]):R)

```scala
/**
 * test whether there is no focus or a predicate holds for the focus of 
 * a Fold, using Heyting algebra
 */
def forall[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```

```scala
import spire.std.boolean._

listFoldable.forall(list)(_ < 9)
// val res25: Boolean = true
```

#### [any](../../api/proptics/Fold_.html#any[R](s:S)(f:A=>R)(implicitevidence$1:spire.algebra.lattice.Heyting[R]):R)

```scala
/** test whether a predicate holds for any focus of a Fold, using a Heyting algebra */
def any[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```

```scala
listFoldable.any(list)(_ < 2)
// val res26: Boolean = true
```

#### [or](../../api/proptics/Fold_.html#or(s:S)(implicitev:spire.algebra.lattice.Heyting[A]):A)

```scala
/** return the result of a disjunction of all foci of a Fold, using a algebra */
def or(s: S)(implicit ev: Heyting[A]): A
```

```scala
listFoldable.any(list)(_ < 2)
// val res27: Boolean = true
```

#### [and](../../api/proptics/Fold_.html#and(s:S)(implicitev:spire.algebra.lattice.Heyting[A]):A)

```scala
/** return the result of a conjunction of all foci of a Fold, using a Heyting algebra */
def and(s: S)(implicit ev: Heyting[A]): A
```

```scala
val boolFoldable = Fold.fromFoldable[List, Boolean]
val boolList = List(true, false, true)

boolFoldable.and(boolList)
// val res28: Boolean = false
```

#### [product](../../api/proptics/Fold_.html#product(s:S)(implicitev:spire.algebra.MultiplicativeMonoid[A]):A)

```scala
/** the product of all foci of a Fold */
def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A
```

```scala
import spire.std.int._

listFoldable.product(list)
// val res29: Int = 24
```

#### [sum](../../api/proptics/Fold_.html#sum(s:S)(implicitev:spire.algebra.AdditiveMonoid[A]):A)

```scala
/** the sum of all foci of a Fold */
def sum(s: S)(implicit ev: AdditiveMonoid[A]): A
```

```scala
import spire.std.int._

listFoldable.sum(list)
// val res30: Int = 10
```

#### [toList](../../api/proptics/Fold_.html#toList(s:S):List[A])

```scala
/** synonym for viewAll */
def toList(s: S): List[A]
```

```scala
val vector = Vector(1, 2, 3, 4)
val vectorFoldable = Fold.fromFoldable[Vector, Int]

vectorFoldable.toList(vector)
// val res31: List[Int] = List(1, 2, 3, 4)
```

#### [toArray](../../api/proptics/Fold_.html#toArray[AA>:A](s:S)(implicitev:scala.reflect.ClassTag[AA]):Array[AA])

```scala
/** collect all the foci of a Fold into an Array */
def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA]
```

```scala
val vector = Vector(1, 2, 3, 4)
val vectorFoldable = Fold.fromFoldable[Vector, Int]

vectorFoldable.toArray(vector)
// val res32: Array[Int] = Array(1, 2, 3, 4)
```

#### [use](../../api/proptics/Fold_.html#use(implicitev:cats.data.State[S,A]):cats.data.State[S,List[A]])

```scala
/** collect all the foci of a Fold in the state of a monad */
def use(implicit ev: State[S, A]): State[S, List[A]]
```

```scala
implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](0)

state.runS(list).value
// val res33: List[Int] = List(1, 2, 3, 4)
```

## Fold internal encoding

#### Polymorphic Fold

```scala
Fold_[S, T, A, B]
```

`Fold_[S, T, A, B]` is a function `Forget[R, A, B] => Forget[R, S, T]`. [Forget](../data-types/forget.md) is a data type shaped like a profunctor, which forgets the `B` value and returns an accumulated value of type `R`.

```scala
/**
  * @tparam S the source of a Fold_
  * @tparam T the modified source of a Fold_
  * @tparam A the foci of a Fold_
  * @tparam B the modified foci of a Fold_
  */
abstract class Fold_[S, T, A, B] {
  def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]
}
```

Although `Fold_[S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic Fold`, serving as
a base type from which a `Monomorphic Fold` can be obtained.
 
#### Monomorphic Fold

```scala
Fold[S, A]
```

`Fold[S, A]` is a type alias for `Fold_[S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type Fold[S, A] = Fold_[S, S, A, A]
``` 

Since `Fold_` is read-only, `Fold[S, A]` is isomorphic to `Fold_[S, T, A, B]`.</br>
`Fold_[S, T, A, B]` takes `Forget[R, A, B]` which wraps a function `A => R` and  forgets the `B`, and returns `Forget[R, S, T]` 
 which wraps a function `S => R` and forgets the `T`,  and its representation can be simplified to:

```
(A => R) => S => R
```

Let's compare it to `Fold_[S, S, A, A]` which is equivalent to `Fold[S, A]`.</br> 

```scala
def fold[S, A]: Fold[S, A] = new Fold_[S, S, A, A] {
  override def apply[R: Monoid](forget: Forget[R, A, A]): Forget[R, S, S]
}
```

`Fold_[S, S, A, A]` takes `Forget[R, A, A]` which wraps a function `A => R` and  forgets the last type parameter `A`, and returns `Forget[R, S, S]` which wraps a function `S => R` and forgets the last parameter `S`, 
and its representation can be simplified to:

```
(A => R) => S => R
```

## Laws

Since a `Fold` cannot be used to write back there are no Lens laws that apply to it.

