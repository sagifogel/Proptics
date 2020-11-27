---
id: at
title: At
---

`At` is a typeclass that provides an <a href ="/Proptics/docs/optics/affine-traversal" target="_blank">`AffineTraversal`</a> which allows reading, writing or deleting the value associated with a key in a Map-like containers.

```scala
/**
  * @tparam S source of a Lens
  * @tparam I index of a Lens
  * @tparam A target of a Lens
  */
trait At[S, I, A] extends Index[S, I, A] {
  def at(i: I): Lens[S, Option[A]]
}
```

It defines one method `at`, which given an index of `I` returns a `Lens[S, Option[A]]`, and inherits Index's 
`ix` method.

## Constructing an At

`At[S, I, A]` is constructed using the `at` function.</br>
For a given `At[S, I, A]` it takes two functions as arguments, `view: I => S => Option[A]` which is a matching function that produces an `Option[A]` given an index of `I` and a structure of `S`, and an index of `I`,
and `set: I => S => Option[A] => S`, a function which takes an index of `I`, a structure of `S` and an optional focus `Option[A]` and returns a structure of `S`.

```scala
object At {
  def at[S, I, A](view: I => S => Option[A])(set: I => S => Option[A] => S): At[S, I, A]
}
```

Most of the time we will be using the predefined implicit instances of `At`, for example an implicit instance of `Map`. We can use the `apply` method to summon an instance of `Map`.

```scala
val atMap = At[Map[String, List[String]], String, List[String]]
``` 

```scala
import proptics.At
// import proptics.At

import cats.syntax.option._ // some method
// import cats.syntax.option._

import proptics.instances.at._ // summoner of the instance
// import proptics.instances.at._

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
val atMap: At[Map[String, List[String]], String, List[String]] =
  At[Map[String, List[String]], String, List[String]]  
// proptics.At[Map[String,List[String]],String,List[String]] = 
//   proptics.instances.AtInstances$$anon$4@5016596e

val newRecommendation = List("The Mandalorian", "Fargo", "Dexter").some
// newRecommendation: Option[List[String]] = Some(List(The Mandalorian, Fargo, Dexter))

atMap.at("tt0903747").set(newRecommendation.some)(seriesMap)
// res0: Map[String,List[String]] = 
//   Map(tt0903747 -> List(The Mandalorian, Fargo, Dexter), 
//       tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))


// setting to None removes the item from the Map
val newSeriesMap = atMap.at("tt0903747").set(None)(seriesMap)
//  newSeriesMap: Map[String,List[String]] = 
//    Map(tt2356777 -> List(Breaking Bad, Fargo, Dexter), 
//        tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//        tt0773262 -> List(Breaking Bad, True Detective, Fargo))

newSeriesMap.contains("tt0903747")
// res1 : Boolean = false
```

We can also use the syntax method `remove`

```scala
import proptics.syntax.at._

atMap.remove("tt2356777")(seriesMap)
// res2: Map[String,List[String]] = 
//   Map(tt0903747 -> List(True Detective, Fargo, Dexter), 
//       tt2802850 -> List(Breaking Bad, True Detective, Dexter), 
//       tt0773262 -> List(Breaking Bad, True Detective, Fargo))
```




