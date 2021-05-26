package proptics.instances

import proptics.internal.{Bazaar, CorepresentableInstances, Sellable, SellableInstances}
import proptics.profunctor.Corepresentable.Aux

trait PartsOf extends SellableInstances with CorepresentableInstances {
  implicit def partsOfSellable[A](implicit ev: Aux[* => *, Bazaar[* => *, List[A], List[A], Unit, *]]): Sellable[* => *, Bazaar[* => *, *, *, Unit, *]] =
    sellableBazaar[* => *, Bazaar[* => *, List[A], List[A], Unit, *]]
}
