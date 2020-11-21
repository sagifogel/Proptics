package proptics.syntax

import proptics.At
import proptics.instances.at.{remove => rm}

trait AtSyntax {
  implicit def atRemoveOps[S, I, A](at: At[S, I, A]): AtRemoveOps[S, I, A] = AtRemoveOps[S, I, A](at)
}

final case class AtRemoveOps[S, I, A](at: At[S, I, A]) extends AnyVal {
  /** remove a value associated with a key in a Map-like container */
  def remove(i: I)(s: S): S = rm(i)(s)(at)
}
