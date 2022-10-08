---
id: traversal
title: Traversal
---

A `Traversal` is an optic used to focus on zero, one, or many values.</br>
`Traversal` is usually used for collections like `List`, `Map`, `Array`.

## Constructing a monomorphic Traversal

### Using companion object

`Traversal[S, A]` is constructed using the <a href="../../api/proptics/Traversal$">Traversal[S, A]#apply</a> function.</br>
For a given `Traversal[S, A]` it takes two functions as arguments, `view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => A => S` function which takes a structure `S` and a focus `A` and returns a
new structure `S` filled will all foci of that `A`.

```scala
object Traversal {
  def apply[S, A](get: S => A)(set: S => A => S): Traversal[S, A]
}
```

```scala
import proptics.Traversal

val list: List[Int] = List.range(1, 6)
val traversal = Traversal[List[Int], Int](_.sum)(ls => i => ls.map(_ + i))

traversal.set(1)(list)
// val res0: List[Int] = List(2, 3, 4, 5, 6)
```

### Using fromTraverse method

Most of the time you will be dealing with `cats.Traverse` types.

```scala
/** create a monomorphic Traversal from cats.Traverse */
def fromTraverse[F[_]: Traverse, A]: Traversal[F[A], A]
```

```scala
import cats.syntax.option._
import cats.syntax.eq._ // triple equals (===)
import proptics.Traversal

val list: List[Int] = List.range(1, 6)
val listTraversal: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]

listTraversal.viewAll(list)
// val res0: List[Int] = List(1, 2, 3, 4, 5)

listTraversal.first(list)
// val res1: Option[Int] = Some(1)

listTraversal.over(_ + 1)(list)
// val res2: List[Int] = List(2, 3, 4, 5, 6)
```

### Using traverse syntax

```scala
import proptics.syntax.all._

List.range(1, 6)
  .traversal
  .take(2)
  .over(_ * 10)
// val res0: List[Int] = List(10, 20, 3, 4, 5) 
```

### Using each syntax

You can also use the `each` extension method which is similar, but works also on tuples</ br></ br>

works the same on collections

```scala
import proptics.instances.each._
import proptics.syntax.all._

val list = List.range(1, 6)

list.traversal.over(_ * 10)
// val res0: List[Int] = List(10, 20, 30, 40, 50)

list.each.over(_ * 10)
// val res1: List[Int] = List(10, 20, 30, 40, 50)
```

works differently on tuples

```scala
import proptics.instances.each._
import proptics.syntax.all._

val tuple = ("abcd", "efgh")

tuple.traversal.over(_.toUpperCase)
// val res0: (String, String) = (abcd,EFGH)

tuple.each.over(_.toUpperCase)
// val res1: (String, String) = (ABCD,EFGH)
```

### Using both method

```scala
/** fold both parts of a cats.Bitraverse with matching types */
def both[G[_, _]: Bitraverse, A]: Traversal[G[A, A], A]
```

```scala
import cats.instances.option._
import cats.syntax.eq._
import proptics.Traversal

val tupleBoth = Traversal.both[(*, *), Int]

tupleBoth.over(_ + 1)((1, 2))
val res0: (Int, Int) = (2,3)
```

### Using single method

```scala
/** create a monomorphic Traversal that narrows the focus to a single element */
def single[F[_]: Traverse, A](i: Int): Traversal[F[A], A] 
```

```scala
import proptics.Traversal

val list = List.range(1, 6)
val singleTraverse =  Traversal.single[List, Int](1)

singleTraverse.over(_ + 1)(list)
// val res0: List[Int] = List(1, 3, 3, 4, 5)
```

### Using single syntax

```scala
import proptics.syntax.all._
import proptics.instances.each._

List.range(1, 6)
 .each
 .single(1)
 .over(_ + 1)
// val res0: List[Int] = List(1, 3, 3, 4, 5)
```

### Using take method

```scala
/** create a monomorphic Traversal that selects the first n elements of a Traverse */
def take[F[_]: Traverse, A](i: Int): Traversal[F[A], A]
```

```scala
import proptics.Traversal

val list = List.range(1, 6)
val traversal = Traversal.take[List, Int](2)

traversal.over(_ * 10)(list)
// val res0: List[Int] = List(10, 20, 3, 4, 5)
```

### Using take syntax

```scala
import proptics.instances.each._
import proptics.syntax.all._

List.range(1, 6)
 .each
 .take(2)
 .over(_ * 10)
// val res0: List[Int] = List(10, 20, 3, 4, 5)
```

## Using drop method

```scala
  /** create a monomorphic [[Traversal]] that selects all elements of a Traverse except the first n ones */
def drop[F[_]: Traverse, A](i: Int): Traversal[F[A], A] 
```

```scala
import proptics.Traversal

val list = List.range(1, 6)
val traversal = Traversal.drop[List, Int](2)

traversal.over(_ * 10)(list)
// val res0: List[Int] = List(1, 2, 30, 40, 50)
```

### Using drop syntax

```scala
import proptics.instances.each._
import proptics.syntax.all._

List.range(1, 6)
 .each
 .drop(1)
 .over(_ * 10)
// val res0: ist[Int] = List(1, 20, 30, 40, 50)
```

## Using takeWhile method

```scala
/** create a monomorphic Traversal that takes the longest prefix of 
 * elements of a Traverse that satisfy a predicate 
 */
def takeWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A]
```

```scala
import proptics.Traversal

val list = List(1, 2, 3, 4, 5, 2)
val traversal = Traversal.takeWhile[List, Int](_ < 3)

traversal.over(_ * 10)(list)
// val res0: List[Int] = List(10, 20, 3, 4, 5, 2)
```

### Using takeWhile syntax

```scala
import proptics.instances.each._
import proptics.syntax.all._

List(1, 2, 3, 4, 5, 2)
 .each
 .takeWhile(_ < 3)
 .over(_ * 10)
// val res0: List[Int] = List(10, 20, 3, 4, 5, 2)
```

## Using dropWhile method

```scala
/** 
 * create a monomorphic Traversal that drop longest prefix of 
 * elements of a Foldable that satisfy a predicate 
 */
def dropWhile[G[_]: Traverse, A](predicate: A => Boolean): Traversal[G[A], A]
```

```scala
import proptics.Traversal

val list = List(1, 2, 3, 4, 5, 2)
val traversal = Traversal.dropWhile[List, Int](_ < 3)

traversal.over(_ * 10)(list)
// val res0: List[Int] = List(1, 2, 30, 40, 50, 20)
```

### Using dropWhile syntax

```scala
import proptics.instances.each._
import proptics.syntax.all._

List(1, 2, 3, 4, 5, 2)
 .each
 .dropWhile(_ < 3)
 .over(_ * 10)
// val res0: List[Int] = List(1, 2, 30, 40, 50, 20)
```


## Constructing a polymorphic Traversal 

`Traversal_[S, T, A, B]` is constructed using the <a href="../../api/proptics/Traversal_$">Traversal_[S, T, A, B]#apply</a> function.</br>
For a given `Traversal_[S, T, A, B]` it takes two functions as arguments,
`view: S => A` which is a getter function, that produces zero, one, or many elements of `A` given an `S`, and `set: S => B => T` function which takes a structure `S` and a new focus `B` and returns
a structure of `T` filled will all foci of that `B`.

```scala
object Traversal_ {
  def apply[S, T, A, B](get: S => A)(set: S => B => T): Traversal_[S, T, A, B]
}
```

## Methods

```scala
import proptics.Traversal

val list = List.range(1, 6)
val listTraversal: Traversal[List[Int], Int] = Traversal.fromTraverse[List, Int]
```

#### [view](../../api/proptics/Traversal_.html#view(s:S)(implicitev:cats.Monoid[A]):A)

```scala
/** fold the foci of a Traversal using a cats.Monoid */
def view(s: S)(implicit ev: Monoid[A]): A
```

```scala
listTraversal.view(list)
// val res0: Int = 15
```

#### [viewAll](../../api/proptics/Traversal_.html#viewAll(s:S):List[A])

```scala
/** collect all the foci of a Traversal into a List */
def viewAll(s: S): List[A]
```

```scala
listTraversal.viewAll(list)
// val res1: List[Int] = List(1, 2, 3, 4, 5)
```

#### [preview](../../api/proptics/Traversal_.html#preview(s:S):Option[A])

```scala
/** view the first focus of a Traverse, if there is any */
def preview(s: S): Option[A]
```

```scala
listTraversal.preview(list)
// val res2: Option[Int] = Some(1)
```

#### [fold](../../api/proptics/Traversal_.html#fold(s:S)(implicitev:cats.Monoid[A]):A)

```scala
/** synonym for view */
def fold(s: S): Option[A]
```

```scala
listTraversal.fold(list)
// val res3: Int = 15
```

#### <a href="../../api/proptics/Traversal_.html#foldMap[R](s:S)(f:A=>R)(implicitevidence$1:cats.Monoid[R]):R">foldMap</a>

```scala
/** map each focus of a Traverse to a cats.Monoid, and combine the results */
def foldMap[R](s: S)(f: A => R)(implicit arg0: Monoid[R]): R
```

```scala
import cats.syntax.eq._
import cats.instances.int._
import cats.instances.option._

def isEven(i: Int): Option[Int] =
  Option.when(i % 2 === 0)(i)

listTraversal.foldMap(list)(isEven)
// val res4: Option[Int] = Some(6)
```

#### <a href="../../api/proptics/Traversal_.html#foldRight[R](s:S)(r:R)(f:(A,R)=>R):R">foldRight</a>

```scala
/** fold the foci of a Traverse using a binary operator, going right to left */
def foldRight[R](s: S)(r: R)(f: (A, R) => R): R
```

```scala
listTraversal.foldRight(list)(Vector.empty[Int])(_ +: _)
// val res5: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4, 5)
```

#### <a href="../../api/proptics/Traversal_.html#foldLeft[R](s:S)(r:R)(f:(R,A)=>R):R">foldLeft</a>

```scala
/** fold the foci of a Traversal using a binary operator, going left to right */
def foldLeft[R](s: S)(r: R)(f: (R, A) => R): R
```

```scala
listTraversal.foldLeft(list)(Vector.empty[Int])(_ :+ _)
// val res6: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4, 5)
```

#### [set](../../api/proptics/Traversal_.html#set(b:B):S=>T)

```scala
/** set the focus of a Traversal */
def set(a: A): S => S
```

```scala
listTraversal.set(9)(list)
// val res7: List[Int] = List(9, 9, 9, 9, 9)
```

#### [over](../../api/proptics/Traversal_.html#over(f:A=>B):S=>T)

```scala
/** modify the focus of a Traversal using a function */
def over(f: A => A): S => S
```

```scala
listTraversal.over(_ + 1)(list)
// val res8: List[Int] = List(2, 3, 4, 5, 6)
```

#### [traverse](../../api/proptics/Traversal_.html#traverse[F[_]](s:S)(f:A=>F[B])(implicitevidence$1:cats.Applicative[F]):F[T])
```scala
/** modify the focus of a Traversal using a Functor */
def traverse[F[_]](s: S)(f: A => F[A])(implicit arg0: Applicative[F]): F[S]
```

```scala
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.option._

def isEven(i: Int): Option[Int] =
 Option.when(i % 2 === 0)(i)
 
listTraversal.traverse(list)(isEven)
// val res9: Option[List[Int]] = None

listTraversal.traverse(list)(_.some)
// val res10: Option[List[Int]] = Some(List(1, 2, 3, 4))
```

#### [overF](../../api/proptics/Traversal_.html#overF[F[_]](f:A=>F[B])(s:S)(implicitevidence$2:cats.Applicative[F]):F[T])
```scala
/** synonym for traverse, flipped */
def overF[F[_]](f: A => F[B])(s: S)(implicit arg0: Applicative[F]): F[T]
```

```scala
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.option._

def isEven(i: Int): Option[Int] =
 Option.when(i % 2 === 0)(i)

listTraversal.overF(isEven)(list)
// val res11: Option[List[Int]] = None

listTraversal.overF(_.some)(list)
// val res12: Option[List[Int]] = Some(List(1, 2, 3, 4, 5))
```

#### [exists](../../api/proptics/Traversal_.html#exists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate holds for the focus of a Traversal */
def exists(f: A => Boolean): S => Boolean
```

```scala
listTraversal.exists(_ < 9)(list)
// val res13: Boolean = true
```

#### [notExists](../../api/proptics/Traversal_.html#notExists(f:A=>Boolean):S=>Boolean)

```scala
/** test whether a predicate does not hold for the focus of a Traversal */
def notExists(f: A => Boolean): S => Boolean
```

```scala
listTraversal .notExists(_ < 9)(list)
// val res14: Boolean = false
```

#### [contains](../../api/proptics/Traversal_.html#contains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Traversal contains a given value */
def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
listTraversal .contains(9)(list)
// val res15: Boolean = false
```

#### [notContains](../../api/proptics/Traversal_.html#notContains(a:A)(s:S)(implicitev:cats.Eq[A]):Boolean)

```scala
/** test whether the focus of a Traversal does not contain a given value */
def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean
```

```scala
listTraversal .notContains(9)(list)
// val res16: Boolean = true
```

#### [isEmpty](../../api/proptics/Traversal_.html#isEmpty(s:S):Boolean)

```scala
/** check if the Fold does not contain a focus */
def isEmpty(s: S): Boolean
```

```scala
listTraversal .isEmpty(list)
// val res17: Boolean = false
```

#### [nonEmpty](../../api/proptics/Traversal_.html#nonEmpty(s:S):Boolean)

```scala
/** check if the Fold contains a focus */
def nonEmpty(s: S): Boolean
```

```scala
listTraversal .nonEmpty(List.empty[Int])
// val res18: Boolean = false
```

#### [find](../../api/proptics/Traversal_.html#find(f:A=>Boolean):S=>Option[A])

```scala
/** find the focus of an Fold that satisfies a predicate, if there is any */
def find(f: A => Boolean): S => Option[A]
```

```scala
import cats.syntax.eq._

listTraversal .find(_ === 9)(list)
// val res19: Option[Int] = None
```

#### [first](../../api/proptics/Traversal_.html#first(s:S):Option[A])

```scala
/** synonym for preview */
def first(s: S): Option[A]
```

```scala
listTraversal.first(list)
// val res20: Option[Int] = Some(1)
```

#### [last](../../api/proptics/Traversal_.html#last(s:S):Option[A])


```scala
/** find the last focus of a Traversal, if there is any */
def last(s: S): Option[A]
```

```scala
listTraversal.last(list)
// val res21: Option[Int] = Some(5)
```

#### [length](../../api/proptics/Traversal_.html#length(s:S):Int)

```scala
/** the number of foci of a Traversal */
def length(s: S): Int
```

```scala
listTraversal.length(list)
// val res22: Int = 5
```

#### [maximum](../../api/proptics/Traversal_.html#maximum(s:S)(implicitev:cats.kernel.Order[A]):Option[A])

```scala
/** the maximum of all foci of a Traversal, if there is any /*
def maximum(s: S)(implicit ev: Order[A]): Option[A]
```

```scala
import cats.instances.int._

listTraversal.maximum(list)
// val res23: Option[Int] = Some(5)
```

#### [minimum](../../api/proptics/Traversal_.html#minimum(s:S)(implicitev:cats.kernel.Order[A]):Option[A])

```scala
/** the minimum of all foci of a Fold, if there is any /*
def minimum(s: S)(implicit ev: Order[A]): Option[A]
```

```scala
listTraversal.minimum(list)
// val res24: Option[Int] = Some(1)
```

#### [mkString](../../api/proptics/Traversal_.html#mkString(s:S)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Traversal in a string */
def mkString(s: S)(implicit ev: S <:< Iterable[A]): String
```


```scala
listTraversal.mkString(list)
// val res25: String = 12345
```

#### [mkString](../../api/proptics/Traversal_.html#mkString(s:S,sep:String)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Traversal in a string using a separator */
def mkString(s: S, sep: String)(implicit ev: S <:< Iterable[A]): String
```

```scala
listTraversal.mkString(list, ", ")
// val res26: String = 1, 2, 3, 4, 5
```


#### [mkString](../../api/proptics/Traversal_.html#mkString(s:S,start:String,sep:String,end:String)(implicitev:S<:<Iterable[A]):String)

```scala
/** displays all foci of a Traversal in a string using a start, end and a separator */
def mkString(s: S, start: String, sep: String, end: String)(implicit ev: S <:< Iterable[A]): String
```

```scala
listTraversal.mkString(list, "[", ", ", "]")
// val res27: String = [1, 2, 3, 4, 5]
```

#### [intercalate](../../api/proptics/Traversal_.html#intercalate(s:S,a:A)(implicitev0:cats.Monoid[A],implicitev1:S<:<Iterable[A]):A)

```scala
/** intercalate/insert an element between the existing elements while folding */ 
def intercalate(s: S, a: A)(implicit ev0: Monoid[A], ev1: S <:< Iterable[A]): A
```

```scala
listTraversal.intercalate(list, 0)
// val res28: Int = 15

listTraversal.intercalate(list, 2)
// val res29: Int = 23
```

#### [forall](../../api/proptics/Traversal_.html#forall(f:A=>Boolean):S=>Boolean)

```scala
/** test whether there is no focus or a predicate holds for the focus of a Traversal */
def forall(f: A => Boolean): S => Boolean
```

```scala
listTraversal.forall(_ < 9)(list)
// val res30: Boolean = true
```

#### [forall](../../api/proptics/Traversal_.html#forall[R](s:S)(f:A=>R)(implicitevidence$2:spire.algebra.lattice.Heyting[R]):R)

```scala
/**
 * test whether there is no focus or a predicate holds for the focus of 
 * a Traversal, using Heyting algebra
 */
def forall[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```

```scala
import spire.std.boolean._

listTraversal.forall(list)(_ < 9)
// val res31: Boolean = true
```

#### [any](../../api/proptics/Traversal_.html#any[R](s:S)(f:A=>R)(implicitevidence$2:spire.algebra.lattice.Heyting[R]):R)

```scala
/** test whether a predicate holds for any focus of a Traversal, using a Heyting algebra */
def any[R](s: S)(f: A => R)(implicit arg0: Heyting[R]): R
```

```scala
import spire.std.boolean._

listTraversal.any(list)(_ < 2)
// val res32: Boolean = true
```

#### [or](../../api/proptics/Traversal_.html#or(s:S)(implicitev:spire.algebra.lattice.Heyting[A]):A)

```scala
/** return the result of a disjunction of all foci of a Traversal, using a algebra */
def or(s: S)(implicit ev: Heyting[A]): A
```

```scala
import spire.std.boolean._

listTraversal.any(list)(_ < 2)
// val res33: Boolean = true
```

#### [and](../../api/proptics/Traversal_.html#and(s:S)(implicitev:spire.algebra.lattice.Heyting[A]):A)

```scala
/** return the result of a conjunction of all foci of a Fold, using a Heyting algebra */
def and(s: S)(implicit ev: Heyting[A]): A
```

```scala
import spire.std.boolean._

val boolTraversal = Traversal.fromTraverse[List, Boolean]
val boolList = List(true, false, true)

boolTraversal.and(boolList)
// val res34: Boolean = false
```

#### [product](../../api/proptics/Traversal_.html#product(s:S)(implicitev:spire.algebra.MultiplicativeMonoid[A]):A)

```scala
/** the product of all foci of a Traversal */
def product(s: S)(implicit ev: MultiplicativeMonoid[A]): A
```

```scala
import spire.std.int._

listTraversal.product(list)
// val res35: Int = 120
```

#### [sum](../../api/proptics/Traversal_.html#sum(s:S)(implicitev:spire.algebra.AdditiveMonoid[A]):A)

```scala
/** the sum of all foci of a Fold */
def sum(s: S)(implicit ev: AdditiveMonoid[A]): A
```

```scala
import spire.std.int._

listTraversal.sum(list)
// val res36: Int = 15
```

#### [toList](../../api/proptics/Traversal_.html#toList(s:S):List[A])

```scala
/** synonym for viewAll */
def toList(s: S): List[A]
```

```scala
val vector = Vector(1, 2, 3, 4, 5)
val vectorTraversal = Traversal.fromTraverse[Vector, Int]

vectorTraversal.toList(vector)
// val res37: List[Int] = List(1, 2, 3, 4, 5)
```

#### [toArray](../../api/proptics/Traversal_.html#toArray[AA>:A](s:S)(implicitev:scala.reflect.ClassTag[AA]):Array[AA])

```scala
/** collect all the foci of a Traversal into an Array */
def toArray[AA >: A](s: S)(implicit ev: ClassTag[AA]): Array[AA]
```

```scala
val vector = Vector(1, 2, 3, 4, 5)
val vectorTraversal = Traversal.fromTraverse[Vector, Int]

vectorTraversal.toArray(vector)
// val res38: Array[Int] = Array(1, 2, 3, 4, 5)
```

#### [use](../../api/proptics/Traversal_.html#use(implicitev:cats.data.State[S,A]):cats.data.State[S,List[A]])

```scala
/** collect all the foci of a Traversal in the state of a monad */
def use(implicit ev: State[S, A]): State[S, List[A]]
```

```scala
import cats.data.State

implicit val state: State[List[Int], Int] = State.pure[List[Int], Int](0)

listTraversal.use.runS(list).value
// val res39: List[Int] = List(1, 2, 3, 4, 5)
```

## Traversal internal encoding

```scala
Traversal_[S, T, A, B]
```

`Traversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a [Wander](../profunctors/wander.md) of P[_, _].

```scala
/** @tparam S the source of a Traversal_
  * @tparam T the modified source of a Traversal_
  * @tparam A the foci of a Traversal_
  * @tparam B the modified foci of a Traversal_
  */
abstract class Traversal_[S, T, A, B] {
  def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
}
```

`Traversal_[S, T, A, B]` changes its foci from `A` to `B`, resulting in a change of structure from `S` to `T`.<br/>
 A `Traversal` that changes its foci/structure, is called `Polymorphic Traversal`.

#### Monomorphic Traversal

```scala
Traversal[S, A]
```

`Traversal[S, A]` is a type alias for `Traversal_[S, S, A, A]`, which has the same type of foci `A`, thus preserving the same type of structure `S`.

```scala
type Traversal[S, A] = Traversal_[S, S, A, A]
``` 

`Traversal[S, A]` means that the type `S` might contain zero, one, or many values of type `A`.</br>
A `Traversal` that does not change its foci/structure, is called `Monomorphic Traversal`.

Most of the time we will be dealing with collections. This is the way to create a `Traversal[S, A]` for a collection:

## Laws

A `Traversal` must satisfy all <a href="../../api/proptics/law/TraversalLaws">TraversalLaws</a>. These laws reside in the <a href="../../api/proptics/law/>proptics.law</a> package.<br/>

```scala
import cats.instances.list._
import cats.syntax.eq._
import cats.{Applicative, Eq}
import proptics.Traversal
```

#### Traversing with "empty" handler shouldn't change anything

```scala
def respectPurity[F[_]: Applicative, S, A](traversal: Traversal[S, A], s: S)
                                          (implicit ev: Eq[F[S]]): Boolean =
  traversal.traverse[F](s)(Applicative[F].pure _) === Applicative[F].pure(s)

val listTraversal = Traversal.fromTraverse[List, Int]
respectPurity(listTraversal, List.range(1, 5))
// res0: Boolean = true
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def consistentFoci[S: Eq, A](traversal: Traversal[S, A], s: S, f: A => A, g: A => A): Boolean =
  (traversal.over(f) compose traversal.over(g))(s) === traversal.over(f compose g)(s)

consistentFoci[List[Int], Int](listTraversal, List.range(1, 5), _ + 1, _ * 2)
// res1: Boolean = true
```