---
id: indexed-setter
title: IndexedSetter
---

An `IndexedSetter` is a generalization of `map` from `IxFunctor`.</br>
`IndexedSetter` is write-only optic, allowing us to map into a structure and change out the content, but it cannot get the content.

## IndexedSetter internal encoding

#### Polymorphic IndexedSetter

```scala
IndexedSetter_[I, S, T, A, B]
```

`IndexedSetter_[I, S, T, A, B]` is a function `Indexed[Function, I, A, B] => S => T`.

```scala
/**
  * @tparam I the index of an IndexedSetter_
  * @tparam S the source of an IndexedSetter_
  * @tparam T the modified source of an IndexedSetter_—
  * @tparam A the focus an IndexedSetter_
  * @tparam B the modified focus of an IndexedSetter_
  */
abstract class IndexedSetter_[I, S, T, A, B] extends Serializable { self =>
  def apply(indexed: Indexed[Function, I, A, B]): S => T
}
```

`IndexedSetter_[I, S, T, A, B]` changes its focus from `A` to `B`, resulting in a change of structure</br> from `S` to `T`.</br>
 A `IndexedSetter` that changes its focus/structure, is called `Polymorphic IndexedSetter`.

#### Monomorphic IndexedSetter
    
```scala
IndexedSetter[I, S, A]
```
    
`IndexedSetter[I, S, A]` is a type alias for `IndexedSetter_[I, S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type IndexedSetter[I, S, A] = IndexedSetter_[I, S, S, A, A]
``` 

An `IndexedSetter` that does not change its focus/structure, is called `Monomorphic IndexedSetter`.

## Constructing IndexedSetters

`IndexedSetter_[I, S, T, A, B]` is constructed using the [IndexedSetter_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedSetter_$.html) function.</br>
For a given `IndexedSetter_[I, S, T, A, B]` it takes a function as argument, `((I, A) => B) => S => T`, which is a mapping function `((I, A) => B)` from a focus `A` and its index `I` to the modified focus `B`
and a structure `S` and returns a structure of `T`.

```scala
object IndexedSetter_ {
  def apply[I, S, T, A, B](f: ((I, A) => B) => S => T): IndexedSetter[I, S, T, A, B]
}
```

`IndexedSetter[I, S, A]` is constructed using the [IndexedSetter[I, S, A]#apply](/Proptics/api/proptics/IndexedSetter$.html) function.</br>
For a given `IndexedSetter_[I, S, A]` it takes a function as argument, `((I, A) => A) => S => S`,  which is a mapping function `(I, A) => A` from a focus `A` and its index `I` a new focus `A` and a structure `S` and returns a new structure `S`.

```scala
object IndexedSetter {
  def apply[I, S, A](f: (A => A) => S => S): IndexedSetter[I, S, A]
}
```

Consider a `Map[String, List[String]]` of series recommendations, and we want to update 
an entry by an index.

```scala
import proptics.IndexedSetter
// import proptics.IndexedSetter

import cats.syntax.eq._ // triple equals (===) 
// import cats.syntax.eq._

val seriesMap: Map[String, List[String]] = Map[String, List[String]](
  "tt0903747" -> List("True Detective", "Fargo", "Dexter"),
  "tt2356777" -> List("Breaking Bad", "Fargo", "Dexter"),
  "tt2802850" -> List("Breaking Bad", "True Detective", "Dexter"),
  "tt0773262" -> List("Breaking Bad", "True Detective", "Fargo")
)
// seriesMap: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))

val newRecommendations = List("True Detective", "Fargo", "Dexter", "The Mandalorian")
// newRecommendations: List[String] = List(True Detective, Fargo, Dexter, The Mandalorian)

val indexedSetter = IndexedSetter[String, Map[String, List[String]], List[String]] { f => aMap =>
  aMap.map { case (i, a) => if (i === "tt0903747") i -> f(i, a) else i -> a }
}
// indexedSetter: proptics.IndexedSetter[String,Map[String,List[String]],List[String]] = 
//   proptics.IndexedSetter_$$anon$5@3bcbfc2d
```

## Common functions of a IndexedSetter

#### set
```scala
indexedSetter.set(newRecommendations)(seriesMap)
// res1: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter, The Mandalorian), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))
```

#### over
```scala
indexedSetter.over(_._2 :+ "The Mandalorian")(seriesMap)
// res1: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter, The Mandalorian), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))
```

## Laws

A `IndexedSetter` must satisfy all [IndexedSetterLaws](/Proptics/api/proptics/law/IndexedSetterLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.

#### Mapping with identity function will get you the same value

```scala
def overIdentity[I, S: Eq, A](s: S, indexedSetter: IndexedSetter[I, S, A]): Boolean =
  indexedSetter.over(_._2)(s) === s

overIdentity(seriesMap, indexedSetter)
// res0: Boolean = true 
```

#### Running twice with different handlers is equivalent to running it once with the composition of those handlers

```scala
def composeOver[I, S: Eq, A](s: S, indexedSetter: IndexedSetter[I, S, A])
                            (f: (I, A) => A)
                            (g: (I, A) => A): Boolean = {
  val overTwice = indexedSetter.over(g.tupled)(indexedSetter.over { case (i, a) => f(i, a) }(s))
  val composedOver = indexedSetter.over({ case (i, a) => g(i, f(i, a)) })(s)

  overTwice === composedOver
}

composeOver(seriesMap, indexedSetter)((_, a) => a)((_, _) => List.empty[String])
// res1: Boolean = true 
```
#### Setting twice is the same as setting once
 
```scala
def setSet[I, S: Eq, A](s: S, a: A, indexedSetter: IndexedSetter[I, S, A]): Boolean =
  indexedSetter.set(a)(indexedSetter.set(a)(s)) === indexedSetter.set(a)(s)

setSet(seriesMap, List("The Mandalorian"), indexedSetter)
// res2: Boolean = true 
```