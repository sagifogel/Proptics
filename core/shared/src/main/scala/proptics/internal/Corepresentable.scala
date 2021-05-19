package proptics.internal

import cats.arrow.Profunctor
import cats.instances.function._
import cats.{Applicative, Id}

import proptics.profunctor.{Corepresentable => Corep}

trait CorepresentableInstances {
  implicit def corepresentableBazaar[C, D]: Corep.Aux[* => *, Bazaar[* => *, C, D, Unit, *]] = new Corep[* => *] {
    override def P: Profunctor[* => *] = Profunctor[* => *]

    override type Corepresentation[x] = Bazaar[* => *, C, D, Unit, x]

    override def cotabulate[A, B](f: Bazaar[* => *, C, D, Unit, A] => B): A => B = a => {
      f(Applicative[Bazaar[* => *, C, D, Unit, *]].pure(a))
    }

    override def cosieve[A, B](pab: A => B)(fa: Bazaar[* => *, C, D, Unit, A]): B = {
      val res = fa.runBazaar.apply[Id](c => pab(c.asInstanceOf[A]).asInstanceOf[D])(())
      pab(res)
    }
  }
}

object Corepresentable extends CorepresentableInstances
