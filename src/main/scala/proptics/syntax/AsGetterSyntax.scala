package proptics.syntax

import cats.Monoid
import proptics.{AGetter_, Getter}

object AsGetterSyntax {
  implicit class AsGetterOps[S, A](val grate: AGetter_[S, A]) extends AnyVal {
    def asGetter(implicit ev: Monoid[A]): Getter[S, A] = grate.asGetter_
  }
}
