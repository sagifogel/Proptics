package proptics.syntax

import cats.Monoid
import proptics.{AGetter, Getter, IndexedFold}

object AsGetterSyntax {
  implicit class AsGetterOps[S, A](val grate: AGetter[S, A]) extends AnyVal {
    def asGetter(implicit ev: Monoid[A]): Getter[S, A] = grate.asGetter_
  }

  implicit class AsAGetterOps[I, S, A](val indexedFold: IndexedFold[I, S, A]) extends AnyVal {
    def asGetter(implicit ev: Monoid[A]): AGetter[S, A] = indexedFold.asAGetter_
  }
}
