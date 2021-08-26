package proptics.internal

import cats.catsInstancesForId
import cats.arrow.Profunctor
import cats.data.State
import cats.instances.function._
import cats.{Applicative, Id}

import proptics.profunctor.{Corepresentable => Corep}

trait CorepresentableInstances {
  implicit def corepresentableBazaar[C, D]: Corep.Aux[* => *, Bazaar[* => *, List[C], List[D], Unit, *]] = new Corep[* => *] {
    override def P: Profunctor[* => *] = Profunctor[* => *]

    override type Corepresentation[x] = Bazaar[* => *, List[C], List[D], Unit, x]

    override def cotabulate[A, B](f: Bazaar[* => *, List[C], List[D], Unit, A] => B): A => B = a => {
      f(Applicative[Bazaar[* => *, List[C], List[D], Unit, *]].pure(a))
    }

    override def cosieve[A, B](pab: A => B)(fa: Bazaar[* => *, List[C], List[D], Unit, A]): B = {
      val res = fa.runBazaar.apply[Id](list => list.map(c => pab(c.asInstanceOf[A]).asInstanceOf[D]))(())
      pab(res)
    }
  }

  implicit def corepresentableState[C]: Corep.Aux[* => *, State[List[C], *]] = new Corep[* => *] {
    override def P: Profunctor[* => *] = Profunctor[* => *]

    override type Corepresentation[x] = State[List[C], x]

    override def cotabulate[A, B](f: State[List[C], A] => B): A => B = a => f(State.pure[List[C], A](a))

    override def cosieve[A, B](pab: A => B)(fa: State[List[C], A]): B =
      fa.map(pab).runEmptyA.value
  }
}

object Corepresentable extends CorepresentableInstances
