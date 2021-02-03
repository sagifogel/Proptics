---
id: indexed-getter
title: IndexedGetter
---

An `IndexedGetter` describes how to retrieve a single value with its index. It is similar to a <a href="/Proptics/docs/indexed-optics/indexed-fold" target="_blank">IndexedFold</a>, but it 
focuses on a single pair of element and index. An `IndexedGetter[I, S, A]` is just any function `S => (A, I)`, everything you can do with a function, you can do with a `IndexedGetter`.

## IndexedGetter internal encoding

#### Polymorphic IndexedGetter

```scala
IndexedGetter_[I, S, T, A, B]
```

`IndexedGetter_[I, S, T, A, B]` is a function

```scala
Indexed[Forget[(A, I), *, *], I, A, B] => Forget[(A, I), S, T]
```

[Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets its last type parameter.

```scala
/**
  * @tparam I the index of an IndexedGetter_
  * @tparam S the source of a IndexedGetter_
  * @tparam T the modified source of a IndexedGetter_
  * @tparam A the focus of a IndexedGetter_
  * @tparam B the modified focus of anIndexedGetter_
  */
abstract class IndexedGetter_[I, S, T, A, B] {
  def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, B]): Forget[(A, I), S, T]
}
```

Although `IndexedGetter_[I, S, T, A, B]` is read-only, and cannot change its foci, it has a definition of `Polymorphic IndexedGetter`, serving as
a base type from which a `Monomorphic IndexedGetter` can be obtained.

#### Monomorphic IndexedGetter

```scala
IndexedGetter[I, S, A]
```

`IndexedGetter[I, S, A]` is a type alias for `IndexedGetter_[I, S, S, A, A]`, which has the same type of focus `A`, thus preserving the same type of structure `S`.

```scala
type IndexedGetter[I, S, A] = IndexedGetter_[I, S, S, A, A]
``` 

Since `IndexedGetter` is read-only, `IndexedGetter[I, S, A]` is isomorphic to</br> `IndexedGetter_[I, S, T, A, B]`.</br>
`IndexedGetter_[I, S, T, A, B]` takes `Indexed[Forget[(A, I), *, *], I, A, B]` and returns</br> `Forget[(A, I), S, T]`.
`Indexed[Forget[(A, I), *, *], I, A, B]` wraps a value of </br> `Forget[(A, I), (A, I), B]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[(A, I), (A, I), B] => Forget[(A, I), S, T]
``` 

`Forget[(A, I), (A, I), B]` wraps a function `(A, I) => (A, I)` and forgets its last type parameter `B`.</br>
`Forget[(A, I), S, T]` wraps a function `S => (A, I)` and forgets its last type parameter `T`, 
so the representation of can be even more simplified to:

```scala
((A, I) => (A, I)) => S => (A, I)
```

Let's compare it to `IndexedGetter_[I, S, S, A, A]` which is equivalent to `IndexedGetter[I, S, A]`.</br> 

```scala
def IndexedGetter[I, S, A]: IndexedGetter[I, S, A] = new IndexedGetter_[I, S, S, A, A] {
  def apply(indexed: Indexed[Forget[(A, I), *, *], I, A, A]): Forget[(A, I), S, S]
}
```

`IndexedGetter_[I, S, S, A, A]` takes `Forget[(A, I), *, *], I, A, A]` and returns </br> `Forget[(A, I), S, S]`.
`Indexed[Forget[(A, I), *, *], I, A, A]` wraps a value of </br>`Forget[(A, I), (A, I), A]`, so the representation of the `apply` method can be simplified to:
 
```scala
Forget[(A, I), (A, I), A] => Forget[(A, I), S, S]
``` 

`Forget[(A, I), (A, I), A]` wraps a function `(A, I) => (A, I)` and forgets its last type parameter `A`.</br>
`Forget[(A, I), S, S]` wraps a function `S => (A, I)` and forgets its last type parameter `S`, 
so the representation of can be even more simplified to:

```scala
((A, I) => (A, I)) => S => (A, I)
```

## Constructing IndexedGetters

`IndexedGetter_[I, S, T, A, B]` is constructed using the [IndexedGetter_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedGetter_$.html) function.</br>
For a given `IndexedGetter_[I, S, T, A, B]` it takes a function as argument `S => (A, I)`.

```scala
object IndexedGetter_ {
  def apply[I, S, T, A, B](get: S => (A, I)): IndexedGetter[I, S, T, A, B]
}
```

`IndexedGetter[I, S, A]` is constructed using the [IndexedGetter[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedGetter$.html) function.</br>
For a given `IndexedGetter[I, S, A]` it takes a function as argument `S => (A, I)`.


```scala
object IndexedGetter {
  def apply[I, S, A](get: S => (A, I)): IndexedGetter[I, S, A]
}
```

Consider a `Map[String, List[String]]` of series recommendations, and we want to get 
an entry by a specific index.

```scala
import proptics.IndexedGetter
// import proptics.IndexedGetter

import cats.instances.option._ // instance of Eq[Option]
// import cats.instances.option._ 

import cats.syntax.eq._ // triple equals (===)
// import cats.syntax.eq._

import cats.syntax.option._ // some function
// import cats.syntax.option._

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

val indexedGetter: IndexedGetter[String, Map[String, List[String]], Option[List[String]]] =
  IndexedGetter[String, Map[String, List[String]], Option[List[String]]] { map =>
    (map.get("tt0903747"), "tt0903747")
  }
// indexedGetter: proptics.IndexedGetter[String,Map[String,List[String]],Option[List[String]]] = 
//   proptics.IndexedGetter_$$anon$14@33ddee76
```

## Common functions of a IndexedGetter

#### view
```scala
indexedGetter.view(seriesMap)
// res0: (Option[List[String]], String) = (Some(List(True Detective, Fargo, Dexter)),tt0903747)
```

#### exists
```scala
indexedGetter.exists(_._2 === "tt0903747")(seriesMap)
// res1: Boolean = true
```

#### contains
```scala
val series = List("Breaking Bad", "True Detective", "Fargo")
// series: List[String] = List(Breaking Bad, True Detective, Fargo)

indexedGetter.contains((series.some, "tt0903747"))(seriesMap)
// res2: Boolean = false
```

#### find

```scala
indexedGetter.find(_._2 === "tt0903747")(seriesMap)
// res4: Option[(Option[List[String]], String)] = Some((Some(List(True Detective, Fargo, Dexter)),tt0903747))
```

## Laws

Since a `IndexedGetter` cannot be used to write back there are no Lens laws that apply to it.




