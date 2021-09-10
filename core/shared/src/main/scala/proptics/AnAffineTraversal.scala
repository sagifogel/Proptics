package proptics

import scala.Function.const

import cats.data.Const
import cats.syntax.either._
import cats.syntax.option._
import cats.{Applicative, Id, Monoid, catsInstancesForId}

import proptics.IndexedTraversal_.wander
import proptics.data.First
import proptics.internal.{Forget, Indexed, RunBazaar, Stall}
import proptics.rank2types.{LensLike, LensLikeWithIndex}

/** [[AnAffineTraversal_]] has at most one focus, but is not a [[Prism_]].
  *
  * [[AnAffineTraversal_]] is an [[AffineTraversal_]] with fixed type [[proptics.internal.Stall]] [[cats.arrow.Profunctor]]
  *
  * @tparam S
  *   the source of an [[AnAffineTraversal_]]
  * @tparam T
  *   the modified source of an [[AnAffineTraversal_]]
  * @tparam A
  *   the focus of an [[AnAffineTraversal_]]
  * @tparam B
  *   the modified focus of an [[AnAffineTraversal_]]
  */
abstract class AnAffineTraversal_[S, T, A, B] extends FoldCompat0[S, A] { self =>
  private[proptics] def apply(pab: Stall[A, B, A, B]): Stall[A, B, S, T]

  /** view the focus of an [[AnAffineTraversal_]] or return the modified source of an [[AnAffineTraversal_]] */
  def viewOrModify(s: S): Either[T, A]

  /** view an optional focus of an [[AnAffineTraversal_]] */
  final def preview(s: S): Option[A] = foldMap(s)(a => First(a.some)).runFirst

  /** set the modified focus of an [[AnAffineTraversal_]] */
  final def set(b: B): S => T = over(const(b))

  /** set the focus of an [[AnAffineTraversal_]] conditionally if it is not None */
  final def setOption(b: B): S => Option[T] = overOption(const(b))

  /** modify the focus type of an [[AnAffineTraversal_]] using a function, resulting in a change of type to the full structure */
  final def over(f: A => B): S => T = overF[Id](f)

  /** modify the focus of a [[AnAffineTraversal_]] using a function conditionally if it is not None, resulting in a change of type to the full structure */
  final def overOption(f: A => B): S => Option[T] = s => preview(s).map(a => set(f(a))(s))

  /** synonym for [[traverse]], flipped */
  final def overF[F[_]: Applicative](f: A => F[B])(s: S): F[T] = traverse(s)(f)

  /** modify the focus type of an [[AnAffineTraversal_]] using a [[cats.Functor]], resulting in a change of type to the full structure */
  final def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] = {
    val stall: Stall[A, B, S, T] = toStall

    stall
      .viewOrModify(s)
      .fold(Applicative[F].pure, a => Applicative[F].map(f(a))(stall.set(s)(_)))
  }

  /** check if the [[AnAffineTraversal_]] does not contain a focus */
  final def isEmpty(s: S): Boolean = preview(s).isEmpty

  /** check if the [[AnAffineTraversal_]] contains a focus */
  final def nonEmpty(s: S): Boolean = !isEmpty(s)

  /** find if the focus of an [[AnAffineTraversal_]] is satisfying a predicate. */
  final def find(p: A => Boolean): S => Option[A] = preview(_).filter(p)

  /** convert an [[AnAffineTraversal_]] to the pair of functions that characterize it */
  final def withAffineTraversal[R](f: (S => Either[T, A]) => (S => B => T) => R): R = {
    val stall: Stall[A, B, S, T] = toStall

    f(stall.viewOrModify)(stall.set)
  }

  /** convert an [[AnAffineTraversal_]] to an [[proptics.internal.Stall]] */
  final def toStall: Stall[A, B, S, T] = self(Stall(_.asRight[B], const(identity[B])))

  /** transform an [[AnAffineTraversal_]] to an [[AffineTraversal_]] */
  final def asAffineTraversal: AffineTraversal_[S, T, A, B] = withAffineTraversal(AffineTraversal_[S, T, A, B])

  /** transform an [[AnAffineTraversal_]] to a [[Fold_]] */
  final def asFold: Fold_[S, T, A, B] = new Fold_[S, T, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget))
  }

  /** compose this [[AnAffineTraversal_]] with a function lifted to a [[Getter_]], having this [[AnAffineTraversal_]] applied first */
  final def to[C, D](f: A => C): Fold_[S, T, C, D] = andThen(Getter_[A, B, C, D](f))

  /** compose this [[AnAffineTraversal_]] with an [[Iso_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Iso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[Iso_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Iso_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.review))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with an [[AnIso_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AnIso_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[AnIso_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: AnIso_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.review))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with a [[Lens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Lens_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with a [[Lens_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Lens_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.set(_)(c)))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with an [[ALens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: ALens_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).map(other.view))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[ALens_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: ALens_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => self.viewOrModify(other.view(c)).leftMap(other.set(_)(c)))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with a [[Prism_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Prism_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with a [[Prism_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Prism_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with an [[APrism_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: APrism_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[APrism_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: APrism_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with an [[AffineTraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[AffineTraversal_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: AffineTraversal_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: AnAffineTraversal_[A, B, C, D]): AnAffineTraversal_[S, T, C, D] =
    AnAffineTraversal_((s: S) => self.viewOrModify(s).flatMap(other.viewOrModify(_).leftMap(self.set(_)(s))))(s => d => self.over(other.set(d))(s))

  /** compose this [[AnAffineTraversal_]] with an [[AnAffineTraversal_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: AnAffineTraversal_[C, D, S, T]): AnAffineTraversal_[C, D, A, B] =
    AnAffineTraversal_((c: C) => other.viewOrModify(c).flatMap(self.viewOrModify(_).leftMap(other.set(_)(c))))(c => b => other.over(s => self.set(b)(s))(c))

  /** compose this [[AnAffineTraversal_]] with a [[Traversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Traversal_[A, B, C, D]): Traversal_[S, T, C, D] =
    Traversal_.wander(new LensLike[S, T, C, D] {
      override def apply[F[_]](f: C => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AnAffineTraversal_]] with a [[Traversal_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Traversal_[C, D, S, T]): Traversal_[C, D, A, B] =
    Traversal_.wander(new LensLike[C, D, A, B] {
      override def apply[F[_]](f: A => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF(self.overF(f))
    })

  /** compose this [[AnAffineTraversal_]] with an [[ATraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: ATraversal_[A, B, C, D]): ATraversal_[S, T, C, D] =
    ATraversal_(new RunBazaar[* => *, C, D, S, T] {
      override def apply[F[_]](pafb: C => F[D])(s: S)(implicit ev: Applicative[F]): F[T] =
        self.traverse(s)(other.overF(pafb))
    })

  /** compose this [[AnAffineTraversal_]] with an [[ATraversal_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: ATraversal_[C, D, S, T]): ATraversal_[C, D, A, B] =
    ATraversal_(new RunBazaar[* => *, A, B, C, D] {
      override def apply[F[_]](pafb: A => F[B])(c: C)(implicit ev: Applicative[F]): F[D] =
        other.traverse(c)(self.overF(pafb))
    })

  /** compose this [[AnAffineTraversal_]] with a [[Setter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Setter_[A, B, C, D]): Setter_[S, T, C, D] = new Setter_[S, T, C, D] {
    override private[proptics] def apply(pab: C => D): S => T = self.over(other.over(pab))
  }

  /** compose this [[AnAffineTraversal_]] with a [[Setter_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Setter_[C, D, S, T]): Setter_[C, D, A, B] = new Setter_[C, D, A, B] {
    override private[proptics] def apply(pab: A => B): C => D = other(self.over(pab))
  }

  /** compose an [[AnAffineTraversal_]] with a [[Getter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Getter_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(forget.runForget compose other.view))
  }

  /** compose an [[AnAffineTraversal_]] with a [[Getter_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Getter_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(c => self.foldMap(other.view(c))(forget.runForget))
  }

  /** compose an [[AnAffineTraversal_]] with a [[Fold_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[C, D](other: Fold_[A, B, C, D]): Fold_[S, T, C, D] = new Fold_[S, T, C, D] {
    override def apply[R: Monoid](forget: Forget[R, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(forget.runForget)))
  }

  /** compose this [[AnAffineTraversal_]] with a [[Fold_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[C, D](other: Fold_[C, D, S, T]): Fold_[C, D, A, B] = new Fold_[C, D, A, B] {
    override private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_)(self.foldMap(_)(forget.runForget)))
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedLens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AnAffineTraversal_]] with an [[IndexedLens_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AnAffineTraversal_]] with an [[AnIndexedLens_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: AnIndexedLens_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AnAffineTraversal_]] with an [[AnIndexedLens_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: AnIndexedLens_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AnAffineTraversal_]] with an [[IndexedTraversal_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedTraversal_[I, A, B, C, D]): IndexedTraversal_[I, S, T, C, D] =
    wander(new LensLikeWithIndex[I, S, T, C, D] {
      override def apply[F[_]](f: ((C, I)) => F[D])(implicit ev: Applicative[F]): S => F[T] =
        self.overF(other.overF(f))
    })

  /** compose this [[AnAffineTraversal_]] with an [[IndexedTraversal_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedTraversal_[I, C, D, S, T]): IndexedTraversal_[I, C, D, A, B] =
    wander(new LensLikeWithIndex[I, C, D, A, B] {
      override def apply[F[_]](f: ((A, I)) => F[B])(implicit ev: Applicative[F]): C => F[D] =
        other.overF { case (s, i) => self.traverse(s)(a => f((a, i))) }
    })

  /** compose this [[AnAffineTraversal_]] with an [[IndexedSetter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedSetter_[I, A, B, C, D]): IndexedSetter_[I, S, T, C, D] = new IndexedSetter_[I, S, T, C, D] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, C, D]): S => T =
      self.over(other.over(indexed.runIndex))
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedSetter_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedSetter_[I, C, D, S, T]): IndexedSetter_[I, C, D, A, B] = new IndexedSetter_[I, C, D, A, B] {
    override private[proptics] def apply(indexed: Indexed[* => *, I, A, B]): C => D =
      other.over { case (s, i) => self.over(a => indexed.runIndex((a, i)))(s) }
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedGetter_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedGetter_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(indexed.runIndex.runForget compose other.view))
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedGetter_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedGetter_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget { c =>
        val (s, i) = other.view(c)
        self.foldMap(s)(a => indexed.runIndex.runForget((a, i)))
      }
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedFold_]], having this [[AnAffineTraversal_]] applied first */
  final def andThen[I, C, D](other: IndexedFold_[I, A, B, C, D]): IndexedFold_[I, S, T, C, D] = new IndexedFold_[I, S, T, C, D] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, C, D]): Forget[R, S, T] =
      Forget(self.foldMap(_)(other.foldMap(_)(indexed.runIndex.runForget)))
  }

  /** compose this [[AnAffineTraversal_]] with an [[IndexedFold_]], having this [[AnAffineTraversal_]] applied last */
  final def compose[I, C, D](other: IndexedFold_[I, C, D, S, T]): IndexedFold_[I, C, D, A, B] = new IndexedFold_[I, C, D, A, B] {
    override private[proptics] def apply[R: Monoid](indexed: Indexed[Forget[R, *, *], I, A, B]): Forget[R, C, D] =
      Forget(other.foldMap(_) { case (s, i) => self.foldMap(s)(a => indexed.runIndex.runForget((a, i))) })
  }

  override protected[proptics] def foldMap[R: Monoid](s: S)(f: A => R): R = overF[Const[R, *]](Const[R, B] _ compose f)(s).getConst
}

object AnAffineTraversal_ {
  /** create a polymorphic [[AnAffineTraversal_]] from an [[AnAffineTraversal_]] encoded in [[proptics.internal.Stall]] */
  private[proptics] def apply[S, T, A, B](f: Stall[A, B, A, B] => Stall[A, B, S, T]): AnAffineTraversal_[S, T, A, B] = new AnAffineTraversal_[S, T, A, B] { self =>
    override def apply(stall: Stall[A, B, A, B]): Stall[A, B, S, T] = f(stall)

    /** view the focus of an [[AnAffineTraversal_]] or return the modified source of an [[AnAffineTraversal_]] */
    override def viewOrModify(s: S): Either[T, A] = f(Stall(_.asRight[B], const(identity[B]))).viewOrModify(s)
  }

  /** create a polymorphic [[AnAffineTraversal_]] from a getter/setter pair */
  final def apply[S, T, A, B](viewOrModify: S => Either[T, A])(set: S => B => T): AnAffineTraversal_[S, T, A, B] =
    AnAffineTraversal_[S, T, A, B] { (stall: Stall[A, B, A, B]) =>
      Stall[A, B, S, T](
        s => viewOrModify(s).fold(_.asLeft[A], stall.viewOrModify(_).leftMap(set(s)(_))),
        s => b => viewOrModify(s).fold(identity, a => set(s)(stall.set(a)(b)))
      )
    }

  /** polymorphic identity of an [[AnAffineTraversal_]] */
  final def id[S, T]: AnAffineTraversal_[S, T, S, T] = AnAffineTraversal_((s: S) => s.asRight[T])(const(identity[T]))
}

object AnAffineTraversal {
  /** create a monomorphic [[AnAffineTraversal]], using preview and setter functions */
  final def fromOption[S, A](preview: S => Option[A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal((s: S) => preview(s).fold(s.asLeft[A])(_.asRight[S]))(set)

  /** create a monomorphic [[AnAffineTraversal]], using a partial function and a setter function */
  final def fromPartial[S, A](preview: PartialFunction[S, A])(set: S => A => S): AnAffineTraversal[S, A] =
    fromOption(preview.lift)(set)

  /** create a monomorphic [[AnAffineTraversal]] from a matcher function that produces an Either and a setter function
    *
    * the matcher function returns an Either to allow for type-changing prisms in the case where the input does not match.
    */
  final def apply[S, A](viewOrModify: S => Either[S, A])(set: S => A => S): AnAffineTraversal[S, A] =
    AnAffineTraversal_(viewOrModify)(set)

  /** monomorphic identity of an [[AnAffineTraversal]] */
  final def id[S]: AnAffineTraversal[S, S] = AnAffineTraversal_.id[S, S]
}
