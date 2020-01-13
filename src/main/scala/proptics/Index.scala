package proptics

import cats.syntax.either._
import cats.syntax.eq._
import cats.{Applicative, Eq, Id}
import proptics.internal.{Traversing, Wander}

import scala.Function.const
import scala.reflect.ClassTag

trait Index[P[_, _], M, A, B] {
  def ix(a: A): Traversal_[P, M, B]
}

abstract class IndexInstances {
  implicit final def indexArr[P[_, _], I, A](implicit ev: Wander[P], ev2: Eq[I]): Index[P, I => A, I, A] = new Index[P, I => A, I, A] {
    override def ix(i: I): Traversal_[P, I => A, A] = new Traversal_[P, I => A, A] {
      override def apply(pab: P[A, A]): P[I => A, I => A] = {
        val traversing: Traversing[I => A, I => A, A, A] = new Traversing[I => A, I => A, A, A] {
          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): (I => A) => F[I => A] = f =>
            ev.map(coalg(f(i)))(a => j => if (i === j) a else f(j))
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexOption[P[_, _], A](implicit ev: Wander[P]): Index[P, Option[A], Unit, A] = new Index[P, Option[A], Unit, A] {
    override def ix(f: Unit): Traversal_[P, Option[A], A] = new Traversal_[P, Option[A], A] {
      override def apply(pab: P[A, A]): P[Option[A], Option[A]] = {
        val traversing: Traversing[Option[A], Option[A], A, A] = new Traversing[Option[A], Option[A], A, A] {
          override def apply[F[_]](f: A => F[A])(implicit ev: Applicative[F]): Option[A] => F[Option[A]] = ev.pure
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexIdentity[P[_, _], A](implicit ev: Wander[P]): Index[P, Id[A], Unit, A] = new Index[P, Id[A], Unit, A] {
    override def ix(a: Unit): Traversal_[P, Id[A], A] = Traversal_[P, Id[A], A](identity)(const(identity))
  }

  implicit final def indexArray[P[_, _], A](implicit ev: Wander[P], ev2: ClassTag[A]): Index[P, Array[A], Int, A] = new Index[P, Array[A], Int, A] {
    override def ix(i: Int): Traversal_[P, Array[A], A] = new Traversal_[P, Array[A], A] {
      override def apply(pab: P[A, A]): P[Array[A], Array[A]] = {
        val traversing: Traversing[Array[A], Array[A], A, A] = new Traversing[Array[A], Array[A], A, A] {
          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): Array[A] => F[Array[A]] = xs =>
            Either.catchNonFatal(xs(i)).fold(const(ev.pure(xs)), x => {
              ev.map(coalg(x))(v => Either.catchNonFatal(xs.updated(i, v)).fold(const(xs), identity))
            })
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexList[P[_, _], A](implicit ev: Wander[P]): Index[P, List[A], Int, A] = new Index[P, List[A], Int, A] {
    override def ix(i: Int): Traversal_[P, List[A], A] = new Traversal_[P, List[A], A] {
      override def apply(pab: P[A, A]): P[List[A], List[A]] = {
        val traversing: Traversing[List[A], List[A], A, A] = new Traversing[List[A], List[A], A, A] {
          def go[F[_]](list: List[A], i: Int)(coalg: A => F[A])(implicit ev: Applicative[F]): F[List[A]] = (list, i) match {
            case (Nil, _) => ev.pure(Nil)
            case (x :: xs, 0) => ev.map(coalg(x))(_ :: xs)
            case (x :: xs, i) => ev.map(go(xs, i - 1)(coalg))(x :: _)
          }

          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): List[A] => F[List[A]] = list => {
            if (i < 0) ev.pure(list)
            else go(list, i)(coalg)
          }
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexSet[P[_, _], A](implicit ev: Wander[P]): Index[P, Set[A], A, Unit] = new Index[P, Set[A], A, Unit] {
    override def ix(a: A): Traversal_[P, Set[A], Unit] = new Traversal_[P, Set[A], Unit] {
      override def apply(pab: P[Unit, Unit]): P[Set[A], Set[A]] = {
        val traversing: Traversing[Set[A], Set[A], Unit, Unit] = new Traversing[Set[A], Set[A], Unit, Unit] {
          override def apply[F[_]](coalg: Unit => F[Unit])(implicit ev: Applicative[F]): Set[A] => F[Set[A]] =
            set => ev.pure(set + a)
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexMap[P[_, _], K, V](implicit ev: Wander[P]): Index[P, Map[K, V], K, V] = new Index[P, Map[K, V], K, V] {
    override def ix(k: K): Traversal_[P, Map[K, V], V] = new Traversal_[P, Map[K, V], V] {
      override def apply(pab: P[V, V]): P[Map[K, V], Map[K, V]] = {
        val traversing = new Traversing[Map[K, V], Map[K, V], V, V] {
          override def apply[F[_]](coalg: V => F[V])(implicit ev: Applicative[F]): Map[K, V] => F[Map[K, V]] =
            map => map.get(k).fold(ev.pure(map))(ev.lift[V, Map[K, V]](map.updated(k, _)) compose coalg)
        }

        ev.wander(traversing)(pab)
      }
    }
  }
}

object Index extends IndexInstances
