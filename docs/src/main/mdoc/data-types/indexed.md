---
id: indexed
title: Indexed
---

`Indexed[P, I, S, T]` is a data type shaped like a `Profunctor`, that has `P[(I, S), T]`, which is similar to a `P[S, T]` of an `Optic`, but 
has also a notion of an index. 

```scala
case class Indexed[P[_, _], I, S, T](runIndex: P[(I, S), T])
```

## Understanding the types of an Indexed

This is a general definition of an `IndexedOptic`, which uses the `Indexed` type for its internal encoding.

```scala
trait IndexedOptic_[I, S, T, A, B] {
  def apply[P[_, _]](indexed: Indexed[P, I, A, B])(implicit ev: Profunctor[P]): P[S, T]   
}
``` 

An `IndexedOptic_` is a function `Indexed[P, I, A, B] => P[S, T]` and the `P[_, _]` is of some kind of `Profunctor`.
The underlying function of `IndexedOptic_` is:

```scala
P[(I, A), B] => P[S, T]
``` 

If we replace the `P[_, _]` with the `Function` type we would get

```scala
((I, A) => B) => S => T
```

That is, given a structure `S` and a function `(I, A) => B`, which can extract a new focus of type `B` out of a tuple `(I, A)`, of a focus and 
its index/location, we would get a new structure `T`.

## IndexedOptics that takes a Profunctor

`IndexedLens` and `IndexedTraversal` each takes some kind of `Profunctor`

IndexedLens takes a `Strong[P[_, _]]` profunctor therefore an instance of `Strong` of `Indexed` has to be introduced

```scala
import cats.arrow.Strong
import proptics.internal.Indexed

implicit def strongIndexed[P[_, _], I](implicit ev: Strong[P]): 
  Strong[({ type F[A, B] = Indexed[P, I, A, B] })#F] =
    new Strong[({ type F[A, B] = Indexed[P, I, A, B] })#F] {
      override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] = {
        val first: P[((I, A), C), (B, C)] = ev.first(fa.runIndex)
        Indexed(ev.lmap[((I, A), C), (B, C), (I, (A, C))](first) { case (i, (a, c)) => ((i, a), c) })
      }

      override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] = {
        val second: P[(C, (I, A)), (C, B)] = ev.second(fa.runIndex)
        Indexed(ev.lmap[(C, (I, A)), (C, B), (I, (C, A))](second) { case (c, (i, a)) => (i, (c, a)) })
      }

      override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])
                                    (f: C => A)
                                    (g: B => D): Indexed[P, I, C, D] =
        Indexed(ev.dimap[(I, A), B, (I, C), D](fab.runIndex) { case (i, c) => (i, f(c)) }(g))
    }
```

IndexedTraversal takes a `Wander[P[_, _]]` profunctor therefore an instance of `Wander` of `Indexed` has to be introduced

```scala
import cats.syntax.either._
import cats.Applicative
import proptics.profunctor.{Traversing, Wander}
import proptics.internal.Indexed

implicit def wanderIndexed[P[_, _], I](implicit ev: Wander[P]): 
  Wander[({ type F[A, B] = Indexed[P, I, A, B] })#F] =
  new Wander[({ type F[A, B] = Indexed[P, I, A, B] })#F] {
  override def wander[S, T, A, B](traversing: Traversing[S, T, A, B])
                                 (indexed: Indexed[P, I, A, B]): Indexed[P, I, S, T] = {
    val traversal = new Traversing[(I, S), T, (I, A), B] {
      override def apply[F[_]](f: ((I, A)) => F[B])(s: (I, S))(implicit ev: Applicative[F]): F[T] =
        traversing(a => f((s._1, a)))(s._2)
    }

    Indexed(ev.wander(traversal)(indexed.runIndex))
  }

  override def first[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (A, C), (B, C)] = {
    val first: P[((I, A), C), (B, C)] = ev.first(fa.runIndex)
    Indexed(ev.lmap[((I, A), C), (B, C), (I, (A, C))](first) { case (i, (a, c)) => ((i, a), c) })
  }

  override def second[A, B, C](fa: Indexed[P, I, A, B]): Indexed[P, I, (C, A), (C, B)] = {
    val second: P[(C, (I, A)), (C, B)] = ev.second(fa.runIndex)
    Indexed(ev.lmap[(C, (I, A)), (C, B), (I, (C, A))](second) { case (c, (i, a)) => (i, (c, a)) })
  }

  override def left[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[A, C], Either[B, C]] = {
    val left: P[Either[(I, A), C], Either[B, C]] = ev.left(pab.runIndex)

    Indexed(ev.lmap(left) { case (i, ac) =>
      ac.fold(a => (i, a).asLeft[C], _.asRight[(I, A)])
    })
  }

  override def right[A, B, C](pab: Indexed[P, I, A, B]): Indexed[P, I, Either[C, A], Either[C, B]] = {
    val right: P[Either[C, (I, A)], Either[C, B]] = ev.right(pab.runIndex)

    Indexed(ev.lmap(right) { case (i, ca) =>
      ca.fold(_.asLeft[(I, A)], a => (i, a).asRight[C])
    })
  }

  override def dimap[A, B, C, D](fab: Indexed[P, I, A, B])
                                (f: C => A)
                                (g: B => D): Indexed[P, I, C, D] =
    Indexed(ev.dimap[(I, A), B, (I, C), D](fab.runIndex) { case (i, c) => (i, f(c)) }(g))
}
```