---
id: indexed-getter
title: IndexedGetter
---

An `IndexedGetter` describes how to retrieve a single value with its index. It is similar to a <a href="/Proptics/docs/indexed-optics/indexed-fold" target="_blank">IndexedFold</a>, but it 
focuses on a single pair of element and index. An `IndexedGetter[I, S, A]` is just any function `S => (I, A)`, everything you can do with a function, you can do with a `IndexedGetter`.

## IndexedGetter internal encoding

#### Polymorphic IndexedGetter

```scala
IndexedGetter_[I, S, T, A, B]
```

`IndexedGetter_[I, S, T, A, B]` is a function

```scala
Indexed[Forget[(I, A), *, *], I, A, B] => Forget[(I, A), S, T]`
```

[Forget](/Proptics/docs/data-types/forget) is a data type shaped like a profunctor, which forgets its last type parameter.

```scala
/**
  * @tparam S the source of a IndexedGetter_
  * @tparam T the modified source of a IndexedGetter_
  * @tparam A the focus of a IndexedGetter_
  * @tparam B the modified focus of anIndexedGetter_
  */
abstract class IndexedGetter_[I, S, T, A, B] {
  def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, B]): Forget[(I, A), S, T]
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
`IndexedGetter_[I, S, T, A, B]` takes `Indexed[Forget[(I, A), *, *], I, A, B]` and returns</br> `Forget[(I, A), S, T]`.
`Indexed[Forget[(I, A), *, *], I, A, B]` wraps a value of </br> `Forget[(I, A), (I, A), B]`, so the representation of the `apply` method can be simplified to:

```scala
Forget[(I, A), (I, A), B] => Forget[(I, A), S, T]
``` 

`Forget[(I, A), (I, A), B]` wraps a function `(I, A) => (I, A)` and forgets its last type parameter `B`.</br>
`Forget[(I, A), S, T]` wraps a function `S => (I, A)` and forgets its last type parameter `T`, 
so the representation of can be even more simplified to:

```scala
((I, A) => (I, A)) => S => (I, A)
```

Let's compare it to `IndexedGetter_[I, S, S, A, A]` which is equivalent to `IndexedGetter[I, S, A]`.</br> 

```scala
def IndexedGetter[I, S, A]: IndexedGetter[I, S, A] = new IndexedGetter_[I, S, S, A, A] {
  def apply(indexed: Indexed[Forget[(I, A), *, *], I, A, A]): Forget[(I, A), S, S]
}
```

`IndexedGetter_[I, S, S, A, A]` takes `Forget[(I, A), *, *], I, A, A]` and returns </br> `Forget[(I, A), S, S]`.
`Indexed[Forget[(I, A), *, *], I, A, A]` wraps a value of </br>`Forget[(I, A), (I, A), A]`, so the representation of the `apply` method can be simplified to:
 
```scala
Forget[(I, A), (I, A), A] => Forget[(I, A), S, S]
``` 

`Forget[(I, A), (I, A), A]` wraps a function `(I, A) => (I, A)` and forgets its last type parameter `A`.</br>
`Forget[(I, A), S, S]` wraps a function `S => (I, A)` and forgets its last type parameter `S`, 
so the representation of can be even more simplified to:

```scala
((I, A) => (I, A)) => S => (I, A)
```

## Constructing IndexedGetters

`IndexedGetter_[I, S, T, A, B]` is constructed using the [IndexedGetter_[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedGetter_$.html) function.</br>
For a given `IndexedGetter_[I, S, T, A, B]` it takes a function as argument `S => (I, A)`.

```scala
object IndexedGetter_ {
  def apply[I, S, T, A, B](get: S => (I, A)): IndexedGetter[I, S, T, A, B]
}
```

`IndexedGetter[I, S, A]` is constructed using the [IndexedGetter[I, S, T, A, B]#apply](/Proptics/api/proptics/IndexedGetter$.html) function.</br>
For a given `IndexedGetter[I, S, A]` it takes a function as argument `S => (I, A)`.


```scala
object IndexedGetter {
  def apply[I, S, A](get: S => (I, A)): IndexedGetter[I, S, A]
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
    ("tt0903747", map.get("tt0903747"))
  }
// indexedGetter: proptics.IndexedGetter[String,Map[String,List[String]],Option[List[String]]] = 
//   proptics.IndexedGetter_$$anon$8@7b6764cd
```

## Common functions of a IndexedGetter

#### view
```scala
indexedGetter.exists(_._1 === "tt0903747")(seriesMap)
// res0: (String, Option[List[String]]) = (tt0903747,Some(List(True Detective, Fargo, Dexter)))
```

#### exists
```scala
indexedGetter.exists(_._1 === "tt0903747")(seriesMap)
// res1: Boolean = true
```

#### contains
```scala
val series = List("Breaking Bad", "True Detective", "Fargo")
// series: List[String] = List(Breaking Bad, True Detective, Fargo)

indexedGetter.contains(("tt0903747", series.some))(seriesMap)
// res2: Boolean = false
```

#### find

```scala
indexedGetter.find(_._1 === "tt0903747")(seriesMap)
// res4: Option[Option[List[String]]] = Some(Some(List(True Detective, Fargo, Dexter)))
```

## Laws

Since a `IndexedGetter` cannot be used to write back there are no Lens laws that apply to it.




