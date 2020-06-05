package proptics

import cats.instances.option._
import cats.syntax.either._
import cats.syntax.eq._
import cats.{Applicative, Eq, Id, Traverse}
import proptics.internal.Wander
import proptics.rank2types.Traversing

import scala.Function.const
import scala.reflect.ClassTag

/**
  * [[Index]] provides a [[Traversal]] that can be used to read the value associated with a key in a Map-like container
  */
trait Index[M, A, B] {
  def ix(a: A): Traversal[M, B]
}

abstract class IndexInstances {
  implicit final def indexArr[I: Eq, A]: Index[I => A, I, A] = new Index[I => A, I, A] {
    override def ix(i: I): Traversal[I => A, A] = new Traversal[I => A, A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[I => A, I => A] = {
        val traversing: Traversing[I => A, I => A, A, A] = new Traversing[I => A, I => A, A, A] {
          override def apply[F[_]](coalg: A => F[A])(s: I => A)(implicit ev: Applicative[F]): F[I => A] = overF(coalg)(s)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](coalg: A => F[A])(s: I => A)(implicit ev: Applicative[F]): F[I => A] =
        ev.map(coalg(s(i)))(a => j => if (i === j) a else s(j))
    }
  }

  implicit final def indexOption[A]: Index[Option[A], Unit, A] = new Index[Option[A], Unit, A] {
    override def ix(f: Unit): Traversal[Option[A], A] = new Traversal[Option[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[Option[A], Option[A]] = {
        val traversing: Traversing[Option[A], Option[A], A, A] = new Traversing[Option[A], Option[A], A, A] {
          override def apply[F[_]](f: A => F[A])(s: Option[A])(implicit ev: Applicative[F]): F[Option[A]] = overF(f)(s)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](f: A => F[A])(s: Option[A])(implicit ev: Applicative[F]): F[Option[A]] =
        Traverse[Option].traverse(s)(f)
    }
  }

  implicit final def indexIdentity[A]: Index[Id[A], Unit, A] = new Index[Id[A], Unit, A] {
    override def ix(a: Unit): Traversal[Id[A], A] = Traversal { a: A => a }(_ => identity[A])
  }

  implicit final def indexArray[A: ClassTag]: Index[Array[A], Int, A] = new Index[Array[A], Int, A] {
    override def ix(i: Int): Traversal[Array[A], A] = new Traversal[Array[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[Array[A], Array[A]] = {
        val traversing: Traversing[Array[A], Array[A], A, A] = new Traversing[Array[A], Array[A], A, A] {
          override def apply[F[_]](coalg: A => F[A])(s: Array[A])(implicit ev: Applicative[F]): F[Array[A]] = overF(coalg)(s)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](coalg: A => F[A])(s: Array[A])(implicit ev: Applicative[F]): F[Array[A]] =
        Either.catchNonFatal(s(i)).fold(const(ev.pure(s)), x => ev.map(coalg(x))(v => Either.catchNonFatal(s.updated(i, v)).fold(const(s), identity)))
    }
  }

  implicit final def indexList[A]: Index[List[A], Int, A] = new Index[List[A], Int, A] {
    override def ix(i: Int): Traversal[List[A], A] = new Traversal[List[A], A] {
      override def apply[P[_, _]](pab: P[A, A])(implicit ev: Wander[P]): P[List[A], List[A]] = {
        val traversing: Traversing[List[A], List[A], A, A] = new Traversing[List[A], List[A], A, A] {
          override def apply[F[_]](coalg: A => F[A])(s: List[A])(implicit ev: Applicative[F]): F[List[A]] = overF(coalg)(s)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](f: A => F[A])(s: List[A])(implicit ev: Applicative[F]): F[List[A]] = {
        def go(list: List[A], i: Int)(coalg: A => F[A])(implicit ev: Applicative[F]): F[List[A]] = (list, i) match {
          case (Nil, _)     => ev.pure(Nil)
          case (x :: xs, 0) => ev.map(coalg(x))(_ :: xs)
          case (x :: xs, i) => ev.map(go(xs, i - 1)(coalg))(x :: _)
        }

        if (i < 0) ev.pure(s)
        else go(s, i)(f)
      }
    }
  }

  implicit final def indexSet[A]: Index[Set[A], A, Unit] = new Index[Set[A], A, Unit] {
    override def ix(a: A): Traversal[Set[A], Unit] = new Traversal[Set[A], Unit] {
      override def apply[P[_, _]](pab: P[Unit, Unit])(implicit ev: Wander[P]): P[Set[A], Set[A]] = {
        val traversing: Traversing[Set[A], Set[A], Unit, Unit] = new Traversing[Set[A], Set[A], Unit, Unit] {
          override def apply[F[_]](coalg: Unit => F[Unit])(s: Set[A])(implicit ev: Applicative[F]): F[Set[A]] = ev.pure(s + a)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](f: Unit => F[Unit])(s: Set[A])(implicit ev: Applicative[F]): F[Set[A]] =
        ev.pure(s)
    }
  }

  implicit final def indexMap[K, V]: Index[Map[K, V], K, V] = new Index[Map[K, V], K, V] {
    override def ix(k: K): Traversal[Map[K, V], V] = new Traversal[Map[K, V], V] {
      override def apply[P[_, _]](pab: P[V, V])(implicit ev: Wander[P]): P[Map[K, V], Map[K, V]] = {
        val traversing = new Traversing[Map[K, V], Map[K, V], V, V] {
          override def apply[F[_]](coalg: V => F[V])(s: Map[K, V])(implicit ev: Applicative[F]): F[Map[K, V]] =
            s.get(k).fold(ev.pure(s))(ev.lift[V, Map[K, V]](s.updated(k, _)) compose coalg)
        }

        ev.wander(traversing)(pab)
      }

      override def overF[F[_]](coalg: V => F[V])(s: Map[K, V])(implicit ev: Applicative[F]): F[Map[K, V]] =
        s.get(k).fold(ev.pure(s))(ev.lift[V, Map[K, V]](s.updated(k, _)) compose coalg)
    }
  }
}

object Index extends IndexInstances
