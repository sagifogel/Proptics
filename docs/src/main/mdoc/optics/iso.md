---
id: iso
title: Iso
---

An `Iso` enables you to transform back and forth between two types without losing information.<br/>
`Iso[S, A]` means that `S` and `A` are isomorphic – the two types represent the same information.<br/>
Iso is useful when you need to convert between types, the most obvious example is to transform a`List` to an `Array`.

## Constructing Isos

Isos are constructed using the [Iso[S, A]#apply](/Proptics/api/proptics/Iso$.html#apply[S,A](view:S=%3EA)(review:A=%3ES):proptics.Iso[S,A]) function. For a given `Iso[S, A]` it expects two conversion functions as arguments,
`view` which produces an `A` given an `S`, and `review` which produces an `S` given an `A`.

```scala
  object Iso {
    def apply[S, A](view: S => A)(review: A => S): Iso[S, A]
  }
```

```scala
  import proptics.Iso
  
  val isoListToArray = Iso[List[Int], Array[Int]](_.toArray)(_.toList)
// isoListToArray: proptics.Iso[List[Int],Array[Int]] = proptics.Iso_$$anon$16@1051f78b  

  isoListToArray.view(List(1, 2, 3))
// res0: Array[Int] = Array(1, 2, 3)

  isoListToArray.review(Array(1, 2, 3, 4, 5))
// res1: List[Int] = List(1, 2, 3, 4, 5)
``` 

## Iso under the hood

`Iso[S, A]` is the monomorphic short notation (does not change the type of the structure) of the polymorphic version `Iso_[S, T, A, B]`

```scala
type Iso[S, A] = Iso_[S, S, A, A]
``` 

`Iso_[S, T, A, B]` is basically a function `P[A, B] => P[S, T]` that expects a [Profunctor](/Proptics/docs/profunctors/profunctor) of P[_, _].

```scala
  abstract class Iso_[S, T, A, B] extends Serializable {
    private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Profunctor[P]): P[S, T]
  }
```

## Laws

An Iso must satisfy all [IsoLaws](/Proptics/api/proptics/law/IsoLaws.html). These laws reside in the [proptics.law](/Proptics/api/proptics/law/index.html) package.<br/>
All laws constructed from the reversibility law, which says that we can completely reverse the transformation.<br/>
```scala
  def sourceReversibility(s: S): IsEq[S]
 
  def focusReversibility(a: A): IsEq[A]
```



