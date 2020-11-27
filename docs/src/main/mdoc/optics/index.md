---
id: index
title: Index
---

`Index` is a typeclass that provides an <a href ="/Proptics/docs/optics/affine-traversal" target="_blank">`AffineTraversal`</a> which allows reading, writing or deleting the value associated with a key in a Map-like containers.

```scala
/**
  * @tparam S source of an AffineTraversal
  * @tparam I index an AffineTraversal
  * @tparam A target of an AffineTraversal
  */
trait Index[S, I, A] {
  def ix(i: I):  AffineTraversal[S, A]
}
```

It defines one method `ix`, which given an index of `I` returns an `AffineTraversal`.

## Constructing an Index

`Index[S, I, A]` is constructed using the `index` function.</br>
For a given `Index[S, I, A]` it takes two functions as arguments, `viewOrModify: I => S => Either[T, A]` which is a matching function that produces an `Either[T, A]` given an an index of `I` and a structure of `S`,
and `set: I => S => A => S`, a function which takes an index of `I`, a structure `S` and a focus `A` and returns a new structure of `S`.

```scala
object Index {
  def index[S, I, A](viewOrModify: I => S => Either[S, A])(set: I => S => A => S): Index[S, I, A]
}
```

A more concise version would be using the `fromPreview` method

```scala
object Index {
  def fromPreview[S, I, A](preview: I => S => Option[A])(set: I => S => A => S): Index[S, I, A]
}
```

An even more concise version would be using the `fromPartial` method

```scala
object Index {
  def fromPartial[S, I, A](preview: I => PartialFunction[S, A])(set: I => S => A => S): Index[S, I, A]
}
```

Most of the time we will be using the predefined implicit instances of `Index`, for example an implicit instance of `Map`.
We can use the `apply` method to summon an instance of `Map`.

```scala
val indexMap = Index[Map[String, List[String]], String, List[String]]
``` 

```scala
import proptics.Index
// import proptics.Index

import proptics.instances.index._ // summoner of the instance
// import proptics.instances.index._

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

// summon of the instance
val indexMap: Index[Map[String, List[String]], String, List[String]] =
  Index[Map[String, List[String]], String, List[String]]  
// proptics.Index[Map[String,List[String]],String,List[String]] = 
//   proptics.instances.IndexInstances$$anon$8@46398364

atMap.at("tt0903747").set(List())(seriesMap)
// res0: Option[List[String]] = Some(List(True Detective, Fargo, Dexter))
```




