package proptics

import proptics.std.{CoproductOptics, TuplesOptics}

package object instances {
  object all extends AllInstances
  object at extends AtInstances
  object index extends IndexInstances
}
