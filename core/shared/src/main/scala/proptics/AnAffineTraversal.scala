package proptics

import scala.Function.const

import cats.data.Const
import cats.syntax.either._
import cats.syntax.eq._
import cats.syntax.option._
import cats.{Applicative, Eq, Id, Monoid}
import spire.algebra.lattice.Heyting
import spire.std.boolean._

import proptics.IndexedTraversal_.wander
import proptics.internal.{Forget, Indexed, RunBazaar, Stall}
import proptics.newtype.{Conj, Disj, First, Newtype}
import proptics.profunctor.{Traversing, Wander}
import proptics.rank2types.LensLikeWithIndex

abstract class AnAffineTraversal_[S, T, A, B] extends Serializable { self =>
  private[proptics] def apply(pab: Stall[A, B, A, B]): Stall[A, B, S, T]

  /** view the focus of an [[AnAffineTraversal_]] or return the modified source of an [[AnAffineTraversal_]] */
  def viewOrModify(s: S): Either[T, A]

  /** view an optional focus of an [[AnAffineTraversal_]] */
  def preview(s: S): Option[A] = foldMapNewtype[First[A], Option[A]](s)(_.some)

  /** set the modified focus of an [[AnAffineTraversal_]] */
  def set(b: B): S => T = over(const(b))

  /** set the focus of an [[AnAffineTraversal_]] conditionally if it is not None */
  def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[AnAffineTraversal_]] using a function, resulting in a change of type to the full structure */
  def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus of a [[AnAffineTraversal_]] using a function conditionally if it is not None, resulting in a change of type to the full structure */
  def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** synonym for [[traverse]], flipped */
  def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnAffineTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = {
    val stall: Stall[A, B, S, T] = toStall

    stall
      .viewOrModify(s)
      .fold(Applicative[F].pure, a => Applicative[F].map(f(a))(stall.set(s)(_)))
  }

  /** test whether there is no focus or a predicate holds for the focus of a [[Prism_]] */
  def forall(f: A => Boolean): S => Boolean = forall(_)(f)

  /** test whether there is no focus or a predicate holds for the focus of an [[AnAffineTraversal_]], using a [[Heyting]] algebra */
  def forall[R: Heyting](s: S)(f: A => R): R = foldMapNewtype[Conj[R], R](s)(f)

  /** test whether a predicate holds for the focus of an [[AnAffineTraversal_]] */
  def exists(f: A => Boolean): S => Boolean = foldMapNewtype[Disj[Boolean], Boolean](_)(f)

  /** test whether a predicate does not hold for the focus of an [[AnAffineTraversal_]] */
  def notExists(f: A => Boolean): S => Boolean = s => !exists(f)(s)

  /** test whether the focus of an [[AnAffineTraversal_]] contains a given value */
  def contains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = exists(_ === a)(s)

  /** test whether the focus of an [[AnAffineTraversal_]] does not contain a given value */
  def notContains(a: A)(s: S)(implicit ev: Eq[A]): Boolean = !contains(a)(s)

  /** check if the [[AnAffineTraversal_]] does not contain a focus */
  def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[AnAffineTraversal_]] contains a focus */
  def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of an [[AnAffineTraversal_]] is satisfying a predicate. */
  def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** convert an [[AnAffineTraversal_]] to the pair of functions that characterize it */
  def withAffineTraversal[R](f: (S => Either[T, A]) => (S => B => T) => R): R = {
    val stall: Stall[A, B, S, T] = toStall

    f(stall.viewOrModify)(stall.set)
  }

  /** convert an [[AnAffineTraversal_]] to an Stall[A, B, S, T] */
  def toStall: Stall[A, B, S, T] = self(Stall(_.asRight[B], const(identity[B])))

  /** transform an [[AnAffineTraversal_]] to an [[AffineTraversal_]] */
  def asAffineTraversal: AffineTraversal_[S, T, A, B] = withAffineTraversal(AffineTraversal_[S, T, A, B])

  /** transform an [[AnAffineTraversal_]] to a [[Fold_]] */
  def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** compose a [[AnAffineTraversal_]] with a function lifted to a [[Getter_]] */
  def to[C, D](f: A => C): Fold_[S, T, C, D] = compose(Getter_[A, B, C, D](f))

  /** compose an [[AnAffineTraversal_]] with an [[Iso_]] */
  def compose[C, D](other: Iso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).map(other.view)
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with an [[Iso_]] */
  def compose[C, D](other: AnIso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).map(other.view)
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with a [[Lens_]] */
  def compose[C, D](other: Lens_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).map(other.view)
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with an [[ALens_]] */
  def compose[C, D](other: ALens_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).map(other.view)
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with a [[Prism_]] */
  def compose[C, D](other: Prism_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with an [[APrism_]] */
  def compose[C, D](other: APrism_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with an [[AffineTraversal_]] */
  def compose[C, D](other: AffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with an [[AnAffineTraversal_]] */
  def compose[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_ { s: S =>
      self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s)))
    }(s => d => self.over(other.set(d))(s))

  /** compose an [[AnAffineTraversal_]] with a [[Traversal_]] */
  def compose[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] = new Traversal_[S, T, C, D] {
    override private[proptics] def apply[P[_, _]](pab: P[C, D])(implicit ev: Wander[P]): P[S, T] = {
      val traversing = new Traversing[S, T, C, D] {
        override def apply[F[_]](f: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
          self.traverse(s)(other.traverse(_)(f))
      }

      ev.wander(traversing)(pab)
    }
  }

  /** compose an [[AnAffineTraversal_]] with an [[ATraversal_]] */
  def compose[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.traverse(_)(pafb))
    })

  /** compose an [[AnAffineTraversal_]] with a [[Setter_]] */
  def compose[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.over(other.over(pab))
  }

  /** compose an [[AnAffineTraversal_]] with a [[Getter_]] */
  def compose[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = self compose other.asFold

  /** compose an [[AnAffineTraversal_]] with a [[Fold_]] */
  def compose[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose an [[AnAffineTraversal_]] with an [[IndexedLens_]] */
  def compose[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose an [[AnAffineTraversal_]] with an [[AnIndexedLens_]] */
  def compose[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose an [[AnAffineTraversal_]] with an [[IndexedTraversal_]] */
  def compose[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose an [[AnAffineTraversal_]] with an [[IndexedFold_]] */
  def compose[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  private def foldMapNewtype[F: Monoid, R](s: S)(f: A => R)(implicit ev: Newtype.Aux[F, R]): R =
    ev.unwrap(foldMap(s)(ev.wrap _ compose f))

  private def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object AnAffineTraversal_ {
  /** create a polymorphic [[AnAffineTraversal_]] from an [[AnAffineTraversal_]] encoded in Stall */
  private[proptics] def apply[S, T, A, B](f: Stall[A, B, A, B] => Stall[A, B, S, T]): AnAffineTraversal_[S, T, A, B] = new AnAffineTraversal_[S, T, A, B] { self =>
    override def apply(stall: Stall[A, B, A, B]): Stall[A, B, S, T] = f(stall)

    /** view the focus of an [[AnAffineTraversal_]] or return the modified source of an [[AnAffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, A] = f(Stall(_.asRight[B], const(identity[B]))).viewOrModify(s)
  }

  /** create a polymorphic [[AnAffineTraversal_]] from a getter/setter pair */
  def apply[S, T, A, B](viewOrModify: S => Either[T, A])(set: S => B => T): AnAffineTraversal_[S, T, A, B] =
    AnAffineTraversal_ { stall: Stall[A, B, A, B] =>
      Stall(
        s => viewOrModify(s).fold(_.asLeft[A], stall.viewOrModify(_).leftMap(set(s)(_))),
        s => b => viewOrModify(s).fold(identity, a => set(s)(stall.set(a)(b)))
      )
    }

  /** polymorphic identity of an [[AnAffineTraversal_]] */
  def id[S, T]: AnAffineTraversal_[S, T, S, T] = AnAffineTraversal_[S, T, S, T] { s: S => s.asRight[T] }(const(identity[T]))
}

object AnAffineTraversal {
  /** create a monomorphic [[AnAffineTraversal]], using preview and setter functions */
  def fromOption[S, A](preview: S => Option[A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal { s: S => preview(s).fold(s.asLeft[A])(_.asRight[S]) }(set)

  /** create a monomorphic [[AnAffineTraversal]], using a partial function and a setter function */
  def fromPartial[S, A](preview: PartialFunction[S, A])(set: S => A => S): AnAffineTraversal[S, A] =
    fromOption(preview.lift)(set)

  /** create a monomorphic [[AnAffineTraversal]] from a matcher function that produces an [[Either]] and a setter function
    * <p>
    * the matcher function returns an [[Either]] to allow for type-changing prisms in the case where the input does not match.
    * </p>
    */
  def apply[S, A](viewOrModify: S => Either[S, A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal_(viewOrModify)(set)

  /** monomorphic identity of an [[AnAffineTraversal]] */
  def id[S]: AnAffineTraversal[S, S] = AnAffineTraversal_.id[S, S]
}
