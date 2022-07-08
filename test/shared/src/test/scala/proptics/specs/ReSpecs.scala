package proptics.specs

import cats.Eq
import cats.arrow.Profunctor
import cats.laws.discipline.{ExhaustiveCheck, MiniInt, ProfunctorTests}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}

import proptics.internal.{Forget, Re}
import proptics.law.discipline.{ChoiceTests, CochoiceTests}
import proptics.profunctor.{Choice, Cochoice}

class ReSpecs extends PropticsSuite {
  implicit val profunctorRe: Profunctor[Re[* => *, Int, Int, *, *]] = Re.profunctorRe[* => *, Int, Int]
  implicit val choiceRe: Choice[Re[Forget[Int, *, *], Int, Int, *, *]] = Re.choiceRe[Forget[Int, *, *], Int, Int]
  implicit val cochoiceRe: Cochoice[Re[Forget[Int, *, *], Int, Int, *, *]] = Re.cochoiceRe[Forget[Int, *, *], Int, Int]

  implicit def eqRe0(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[* => *, Int, Int, Int, Int]] = Eq.instance[Re[* => *, Int, Int, Int, Int]] { (re1, re2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt
      re1.runRe(identity[Int])(int) === re2.runRe(identity[Int])(int)
    }
  }

  implicit def eqRe1(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Int, Int]] = Eq.instance[Re[Forget[Int, *, *], Int, Int, Int, Int]] { (re1, re2) =>
    ev.allValues.forall { miniInt =>
      val int = miniInt.toInt
      val forget = Forget[Int, Int, Int](identity)

      re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
    }
  }

  implicit def eqRe2(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Int, Either[Int, Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Int, Either[Int, Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Int], Int](_.fold(identity, identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe3(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Int], Either[Int, Int]](_.fold(identity, identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe4(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]](_.fold(identity, _.fold(identity, identity)))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def eqRe5(implicit ev: ExhaustiveCheck[MiniInt]): Eq[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Eq.instance[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] { (re1, re2) =>
      ev.allValues.forall { miniInt =>
        val int = miniInt.toInt
        val forget = Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]](_.fold(_.fold(identity, identity), identity))

        re1.runRe(forget).runForget(int) === re2.runRe(forget).runForget(int)
      }
    }

  implicit def cogenForget0: Cogen[Forget[Int, Either[Int, Int], Either[Int, Int]]] =
    Cogen.function1[Either[Int, Int], Int].contramap(_.runForget)

  implicit def cogenForget1: Cogen[Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Cogen.function1[Either[Either[Int, Int], Int], Int].contramap(_.runForget)

  implicit def cogenForget2: Cogen[Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Cogen.function1[Either[Int, Either[Int, Int]], Int].contramap(_.runForget)

  implicit def arbRe: Arbitrary[Re[* => *, Int, Int, Int, Int]] = Arbitrary[Re[* => *, Int, Int, Int, Int]] {
    for {
      runRe <- Gen.function1[Int => Int, Int => Int](Gen.function1[Int, Int](Arbitrary.arbInt.arbitrary))
    } yield Re[* => *, Int, Int, Int, Int](runRe)
  }

  implicit def arbReForget0: Arbitrary[Re[Forget[Int, *, *], Int, Int, Int, Int]] = Arbitrary[Re[Forget[Int, *, *], Int, Int, Int, Int]] {
    for {
      runForget <- Gen.function1[Forget[Int, Int, Int], Forget[Int, Int, Int]](arbForget.arbitrary)
    } yield Re[Forget[Int, *, *], Int, Int, Int, Int](runForget)
  }

  implicit def argReForget1: Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] =
    Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]]] {
      for {
        runForget <- Gen.function1[Forget[Int, Either[Int, Int], Either[Int, Int]], Forget[Int, Int, Int]](arbForget.arbitrary)
      } yield Re[Forget[Int, *, *], Int, Int, Either[Int, Int], Either[Int, Int]](runForget)
    }

  implicit def argReForget2: Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] =
    Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]]] {
      for {
        runForget <- Gen.function1[Forget[Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]], Forget[Int, Int, Int]](arbForget.arbitrary)
      } yield Re[Forget[Int, *, *], Int, Int, Either[Either[Int, Int], Int], Either[Either[Int, Int], Int]](runForget)
    }

  implicit def argReForge3: Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] =
    Arbitrary[Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]]] {
      for {
        runForget <- Gen.function1[Forget[Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]], Forget[Int, Int, Int]](arbForget.arbitrary)
      } yield Re[Forget[Int, *, *], Int, Int, Either[Int, Either[Int, Int]], Either[Int, Either[Int, Int]]](runForget)
    }

  checkAll("Profunctor Re[* -> *, Int, Int, Int, Int]", ProfunctorTests[Re[* => *, Int, Int, *, *]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Choice Re[Forget[Int, *, *], Int, Int, Int, Int]", ChoiceTests[Re[Forget[Int, *, *], Int, Int, *, *]].choice[Int, Int, Int, Int, Int, Int])
  checkAll("Cochoice Re[Forget[Int, *, *], Int, Int, Int, Int]", CochoiceTests[Re[Forget[Int, *, *], Int, Int, *, *]].cochoice[Int, Int, Int, Int, Int, Int])
}
