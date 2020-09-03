package proptics.specs

import cats.Eq
import cats.arrow.Profunctor
import cats.instances.function._
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests}
import proptics.internal.{Forget, Re}
import org.scalacheck.ScalacheckShapeless._
import proptics.law.ChoiceTests
import proptics.profunctor.Choice

class ReSpecs extends PropticsSuite {
  implicit val profunctorRe: Profunctor[Re[* => *, Int, Int, *, *]] = Re.profunctorRe[* => *, Int, Int]
  implicit val choiceRe: Choice[Re[Forget[Int, *, *], Int, Int, *, *]] = Re.choiceRe[Forget[Int, *, *], Int, Int]
  implicit def eqRe0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[* => *, Int, Int, Int, Int]] = Eq.instance[Re[* => *, Int, Int, Int, Int]] { (re1, re2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt
      re1.runRe(identity[Int])(int) === re2.runRe(identity[Int])(int)
    }
  }
  
  implicit def eqRe6(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Int, Int]] = Eq.instance[Re[Forget[Int, *, *], Int, Int, Int, Int]] {
    (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Int, Int](identity)

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
  }

  implicit def eqRe7(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Int, Either[Int, Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Int, Either[Int, Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Int], Int](_.fold(identity, identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe8(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Int], Either[Int, Int]](_.fold(identity, identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe9(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]](_.fold(identity, _.fold(identity, identity)))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe10(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]](_.fold(_.fold(identity, identity), identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  checkAll("Profunctor Re[* -> *, Int, Int, Int, Int]", ProfunctorTests[Re[* => *, Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Re[Forget[Int, *, *], Int, Int, Int, Int]", ChoiceTests[Re[Forget[Int, *, *], Int, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
}
