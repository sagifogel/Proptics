---
id: bazaar
title: Bazaar
---

`Bazaar[P[_, _], A, B, S, T]` is a data type shaped like a `Profunctor`, which characterizes the construction of a <a href="/Proptics/docs/optics/traversal" target="_blank">Traversal</a> and <a href="/Proptics/docs/an-optics/a-traversal" target="_blank">ATraversal</a>.

## Traversal encoding

`Traversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` that takes a `Wander` of P[_, _]. 

```scala
abstract class Traversal_[S, T, A, B] {
  private[proptics] def apply[P[_, _]](pab: P[A, B])(implicit ev: Wander[P]): P[S, T]
}
```

The <a href="/Proptics/docs/profunctors/wander" target="_blank">Wander[_, _]</a> typeclass defines a `wander` method which takes a `Traversing[S, T, A, B]` and `P[A, B]` and returns `P[S, T]` 

```scala
trait Wander[P[_, _]] extends Strong[P] with Choice[P] {
  def wander[S, T, A, B](traversal: Traversing[S, T, A, B])(pab: P[A, B]): P[S, T]
}
```

`Traversing[S, T, A, B]` is a traversal function that is encoded within a type.
 
 ```scala
 A => F[B] => S => Applicative[F] => F[T]
 ```

```scala
import cats.Applicative

trait Traversing[S, T, A, B] {
  def apply[F[_]](f: A => F[B])(s: S)(implicit ev: Applicative[F]): F[T]
}
```

## ATraversal encoding

 `ATraversal_[S, T, A, B]` is a function `P[A, B] => P[S, T]` Where's the `P[_, _]` is a data type of `Bazaar`, thus making 
 it a function `Bazaar[Function, A, B, A, B] => Bazaar[Function, A, B, S, T]`.
 
 ```scala
abstract class ATraversal_[S, T, A, B] {
  private[proptics] def apply(bazaar: Bazaar[Function, A, B, A, B]): Bazaar[Function, A, B, S, T]
}
``` 

`Bazaar[P[_, _], A, B, S, T]` defines a `runBazaar` method which returns a </br> `RunBazaar[P[_, _], A, B, S, T]` type.

```scala
trait Bazaar[P[_, _], A, B, S, T] {
  def runBazaar: RunBazaar[P, A, B, S, T]
}
``` 

`RunBazaar[P[_, _], A, B, S, T]` is a traversal function that is encoded within a type.

```scala
P[A, F[B]] => S => Applicative[F] => F[T]
```

```scala
trait RunBazaar[P[_, _], A, B, S, T] {
  def apply[F[_]](pafb: P[A, F[B]])(s: S)(implicit ev: Applicative[F]): F[T]
}
```

`ATraversal_` specialize the `P[_, _]` of  `Bazaar` and `RunBazaar` to the `Function` type, thus making it a traversal function equivalent 
to the one defined in `Traversing[S, T, A, B]`.

```scala
A => F[B] => S => Applicative[F] => F[T]
```

In order for `ATraversal_[S, T, A, B]` to be compatible with `Traversal_[S, T, A, B]`, an instance of `Wander` of `Bazaar` has been
introduced.

`Wander[_, _]` is a type constructor that takes 2 type parameters. `Bazaar[P[_, _], A, B, S, T]` is a type that has 5 type parameters, so we need
to fix three of the type parameters of `Bazaar` in order to create an instance of `Wander` of `Bazaar`.

```scala
implicit def wanderBazaar[P[_, _], G, H]: Wander[({ type B[S, T] = Bazaar[P, G, H, S, T] })#B] =
  new Wander[({ type B[S, T] = Bazaar[P, G, H, S, T] })#B] {
    override def wander[S, T, A, B](traversal: Traversing[S, T, A, B])
                                   (pab: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, S, T] =
      new Bazaar[P, G, H, S, T] {
        override def runBazaar: RunBazaar[P, G, H, S, T] = new RunBazaar[P, G, H, S, T] {
          override def apply[F[_]](pafb: P[G, F[H]])(s: S)(implicit ev: Applicative[F]): F[T] =
            traversal(pab.runBazaar(pafb))(s)
        }
      }
      
    override def left[A, B, C](pab: Bazaar[P, G, H, A, B]): 
      Bazaar[P, G, H, Either[A, C], Either[B, C]] =  
        new Bazaar[P, G, H, Either[A, C], Either[B, C]] {
          override def runBazaar: RunBazaar[P, G, H, Either[A, C], Either[B, C]] = 
            new RunBazaar[P, G, H, Either[A, C], Either[B, C]] {
              override def apply[F[_]](pafb: P[G, F[H]])
                                      (s: Either[A, C])
                                      (implicit ev: Applicative[F]): F[Either[B, C]] =
                Bitraverse[Either].bitraverse(s)(pab.runBazaar(pafb), ev.pure)
            }
        }

    override def right[A, B, C](pab: Bazaar[P, G, H, A, B]): 
      Bazaar[P, G, H, Either[C, A], Either[C, B]] = 
        new Bazaar[P, G, H, Either[C, A], Either[C, B]] {
          override def runBazaar: RunBazaar[P, G, H, Either[C, A], Either[C, B]] = 
            new RunBazaar[P, G, H, Either[C, A], Either[C, B]] {
              override def apply[F[_]](pafb: P[G, F[H]])
                                      (s: Either[C, A])
                                      (implicit ev: Applicative[F]): F[Either[C, B]] =
                s.traverse(pab.runBazaar(pafb))
          }
        }
    
    override def first[A, B, C](fa: Bazaar[P, G, H, A, B]): 
      Bazaar[P, G, H, (A, C), (B, C)] = 
         new Bazaar[P, G, H, (A, C), (B, C)] {
           override def runBazaar: RunBazaar[P, G, H, (A, C), (B, C)] = 
             new RunBazaar[P, G, H, (A, C), (B, C)] {
                override def apply[F[_]](pafb: P[G, F[H]])
                                        (s: (A, C))
                                        (implicit ev: Applicative[F]): F[(B, C)] =
                  ev.map(fa.runBazaar(pafb)(s._1))((_, s._2))
              }
        }
    
    override def second[A, B, C](fa: Bazaar[P, G, H, A, B]): Bazaar[P, G, H, (C, A), (C, B)] = 
      new Bazaar[P, G, H, (C, A), (C, B)] {
        override def runBazaar: RunBazaar[P, G, H, (C, A), (C, B)] = 
          new RunBazaar[P, G, H, (C, A), (C, B)] {
            override def apply[F[_]](pafb: P[G, F[H]])
                                    (s: (C, A))
                                    (implicit ev: Applicative[F]): F[(C, B)] =
              ev.map(fa.runBazaar(pafb)(s._2))((s._1, _))
          }    
        }
      
    override def dimap[A, B, C, D](fab: Bazaar[P, G, H, A, B])
                                  (f: C => A)
                                  (g: B => D): Bazaar[P, G, H, C, D] = new Bazaar[P, G, H, C, D] {
      override def runBazaar: RunBazaar[P, G, H, C, D] = new RunBazaar[P, G, H, C, D] {
        override def apply[F[_]](pafb: P[G, F[H]])(s: C)(implicit ev: Applicative[F]): F[D] =
          ev.map(fab.runBazaar(pafb)(f(s)))(g)
        }
     }
}
```

`ATraversal` allows us to export its internal construction logic to a `Bazaar` using the `toBazaar` method.

```scala
val traversal: ATraversal[(Int, String), String] = 
  ATraversal[(Int, String), String](_._2) { case (i, _) => s => (i, s) }
// traversal: proptics.ATraversal[(Int, String),String] = proptics.ATraversal_$$anon$22@7218cbb6

val bazaar = traversal.toBazaar
// bazaar: internal.Bazaar[[α$11$, β$12$]α$11$ => β$12$,String,String,(Int, String),(Int, String)] = 
//   proptics.ATraversal_$$anon$22$$anon$23@5f364bc2
```

We can later on create a new instance of an `ATraversal` or a `Traversal` from the bazaar instance

```scala
import proptics.ATraversal
// import proptics.ATraversal_

import proptics.Traversal
// import proptics.Traversal

val aTraversalFromBazaar: ATraversal[(Int, String), String] = ATraversal.fromBazaar(bazaar)
// aTraversalFromBazaar: proptics.ATraversal[(Int, String),String] = 
//   proptics.ATraversal_$$anon$19@43bf1ac9

val traversalFromBazaar: Traversal[(Int, String), String] = Traversal.fromBazaar(bazaar)
// traversalFromBazaar: proptics.Traversal[(Int, String),String] = 
//   proptics.Traversal_$$anon$12@7494feef
```