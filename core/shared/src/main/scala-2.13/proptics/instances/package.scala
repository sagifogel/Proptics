package proptics

package object instances {
  object all extends AllInstances
  object tuple extends TuplesInstances
  object list extends ListInstances
  object product extends ProductInstances
  object coproduct extends CoproductInstances
  object option extends OptionInstances
  object either extends EitherInstances
  object newtype extends NewtypeInstances
  object unit extends UnitInstances
  object at extends AtInstances
  object index extends IndexInstances with ScalaVersionSpecificIndexInstances
}
