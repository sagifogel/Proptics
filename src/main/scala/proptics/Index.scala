package proptics

import cats.syntax.either._
import cats.syntax.eq._
import cats.{Applicative, Eq, Id}
import proptics.internal.{Traversing, Wander}

import scala.Function.const
import scala.reflect.ClassTag

trait Index[M, A, B] {
  def ix(a: A): Traversal_[M, B]
}

abstract class IndexInstances {
  implicit final def indexArr[I, A](implicit ev0: Eq[I]): Index[I => A, I, A] = new Index[I => A, I, A] {
    override def ix(i: I): Traversal_[I => A, A] = new Traversal_[I => A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev1: Wander[P]): P[I => A, I => A] = {
        val traversing: Traversing[I => A, I => A, A, A] = new Traversing[I => A, I => A, A, A] {
          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): (I => A) => F[I => A] = f =>
            ev.map(coalg(f(i)))(a => j => if (i === j) a else f(j))
        }

        ev1.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexOption[A]: Index[Option[A], Unit, A] = new Index[Option[A], Unit, A] {
    override def ix(f: Unit): Traversal_[Option[A], A] = new Traversal_[Option[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[Option[A], Option[A]] = {
        val traversing: Traversing[Option[A], Option[A], A, A] = new Traversing[Option[A], Option[A], A, A] {
          override def apply[F[_]](f: A => F[A])(implicit ev: Applicative[F]): Option[A] => F[Option[A]] = ev.pure
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexIdentity[A]: Index[Id[A], Unit, A] = new Index[Id[A], Unit, A] {
    override def ix(a: Unit): Traversal_[Id[A], A] = Traversal_[Id[A], A](identity)(const(identity))
  }

  implicit final def indexArray[A](implicit ev0: ClassTag[A]): Index[Array[A], Int, A] = new Index[Array[A], Int, A] {
    override def ix(i: Int): Traversal_[Array[A], A] = new Traversal_[Array[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev1: Wander[P]): P[Array[A], Array[A]] = {
        val traversing: Traversing[Array[A], Array[A], A, A] = new Traversing[Array[A], Array[A], A, A] {
          override def apply[F[_]](coalg: A => F[A])(implicit ev: Applicative[F]): Array[A] => F[Array[A]] = xs =>
            Either.catchNonFatal(xs(i)).fold(const(ev.pure(xs)), x => {
              ev.map(coalg(x))(v => Either.catchNonFatal(xs.updated(i, v)).fold(const(xs), identity))
            })
        }

        ev1.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexList[A]: Index[List[A], Int, A] = new Index[List[A], Int, A] {
    override def ix(i: Int): Traversal_[List[A], A] = new Traversal_[List[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[List[A], List[A]] = {
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

  implicit final def indexSet[A]: Index[Set[A], A, Unit] = new Index[Set[A], A, Unit] {
    override def ix(a: A): Traversal_[Set[A], Unit] = new Traversal_[Set[A], Unit] {
      override def apply[P[_, _]](pab: P[Unit, Unit])(implicit ev: Wander[P]): P[Set[A], Set[A]] = {
        val traversing: Traversing[Set[A], Set[A], Unit, Unit] = new Traversing[Set[A], Set[A], Unit, Unit] {
          override def apply[F[_]](coalg: Unit => F[Unit])(implicit ev: Applicative[F]): Set[A] => F[Set[A]] =
            set => ev.pure(set + a)
        }

        ev.wander(traversing)(pab)
      }
    }
  }

  implicit final def indexMap[K, V]: Index[Map[K, V], K, V] = new Index[Map[K, V], K, V] {
    override def ix(k: K): Traversal_[Map[K, V], V] = new Traversal_[Map[K, V], V] {
      override def apply[P[_, _]](pab: P[V, V])(implicit ev: Wander[P]): P[Map[K, V], Map[K, V]] = {
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
