---
id: indexed-optic
title: IndexedOptic
---

`IndexedOptic` is a type that can be used to focus on a particular element, and it's location (or index) in a deeply nested data structure.
It describes a relationship between a structure `S` and zero, one, or many values of type `(I, A)`, called the indexed focus (or foci) of the optic.

## Definition

This is a general definition of an `IndexedOptic`:

```scala
/**
  * @tparam I the index of an IndexedOptic_
  * @tparam S the source of an IndexedOptic_
  * @tparam T the modified source of an IndexedOptic_
  * @tparam A the focus of an IndexedOptic_
  * @tparam B the modified focus of an IndexedOptic_
  */
trait IndexedOptic_[I, S, T, A, B] {
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Profunctor[P]): P[S, T]   
}
```

## Indexed

`Indexed` is a data type shaped like a `Profunctor`, that has is `P[(I, A), B]`, which is similar to a `P[A, B]` of an `Optic`, but 
has also a notion of an index.

```scala
case class Indexed[P[_, _], I, A, B](runIndex: P[(I, A), B])
```

## Understanding the types of an IndexedOptic

Let's assume that we want to change a `NonEmptyList` to `NonEmptyChain`. The concrete types for this `IndexedOptic` would be:

```scala
/**
  * Int the index of an IndexedOptic_
  * NonEmptyList[Int] the source of an IndexedOptic_
  * NonEmptyChain[Int] the modified source of an IndexedOptic_
  * Int the focus of an IndexedOptic_
  * Int the modified focus of an IndexedOptic_
  */
trait IndexedOptic_ {
  apply[P[_, _]](indexed: Indexed[P, Int, Int, Int])
                (implicit ev: Strong[P]): P[NonEmptyList[Int], NonEmptyChain[Int]]]   
}
```

An `IndexedOptic` that changes its focus/structure, is called `Polymorphic IndexedOptic`.

Often times you will just change the value of the focus, which means to the type of the structure will remain the same, for this specific scenario
instead of using an `IndexedOptic_[I, S, S, A, A]` which has repeating type parameters, there is a short notation of:

```scala
type IndexedOptic[I, S, A] = IndexedOptic_[I, S, S, A, A]
```

An `IndexedOptic` that does not change its focus/structure, is called `Monomorphic IndexedOptic`.

While `IndexedOptic_[I, S, T, A, B]` is not really used for the encoding of optics in `Proptics` (does not serve as a base class for all optics, and it is only shown for explanation purposes), 
all optics are functions from `Indexed[P, I, A, B]` to `P[S, T]`, where's the `P[_, _]` is a typeclass derived from profunctor.

## List of all IndexedOptics

This table shows all pairs of indexed optics and their profunctor:

|                                                                   |  Profunctor                                  | Data Type                                                                                  |
| ----------------------------------------------------------------- |:-------------------------------------------- |:------------------------------------------------------------------------------------------:|
| [IndexedLens](/Proptics/docs/indexed-optics/indexed-lens)                         | [Strong](/Proptics/docs/profunctors/strong)  |                                                                                            |
| [IndexedTraversal](/Proptics/docs/indexed-optics/indexed-traversal)               | [Wander](/Proptics/docs/profunctors/wander)  |                                                                                            |
| [IndexedFold](/Proptics/docs/indexed-optics/indexed-fold)                         |                                              | [Indexed](/Proptics/docs/data-types/indexed) of [Forget](/Proptics/docs/data-types/forget) |
| [IndexedGetter](/Proptics/docs/indexed-optics/indexed-getter)                     |                                              | [Indexed](/Proptics/docs/data-types/indexed) of [Forget](/Proptics/docs/data-types/forget) |
| [IndexedSetter](/Proptics/docs/indexed-optics/indexed-setter)                     |                                              | [Indexed](/Proptics/docs/data-types/indexed) of Function            |