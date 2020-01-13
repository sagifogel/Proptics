package proptics

import proptics.profunctor.Closed

/**
 * <a href="http://r6research.livejournal.com/28050.html">A [[Grate]]</a>
 * @tparam P an evidence of [[Closed]] [[cats.arrow.Profunctor]]
 * @tparam S the source of an [[Grate]]
 * @tparam T the modified source of an [[Grate]]
 * @tparam A the target of an [[Grate]]
 * @tparam B the modified target of an [[Grate]]
 */
abstract class Grate[P[_, _]: Closed, S, T, A, B] extends Optic[P, S, T, A, B] {
}
