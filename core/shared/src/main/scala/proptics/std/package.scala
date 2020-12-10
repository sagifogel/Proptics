package proptics

package object std {
  object all extends AllOptics
  object eitherK extends CoproductOptics
  object either extends EitherOptics
  object function extends FunctionOptics
  object list extends ListOptics
  object newtype extends NewtypeOptics
  object option extends OptionOptics
  object tuple2K extends ProductOptics
  object string extends StringOptics
  object tuple extends TuplesOptics
  object unit extends UnitOptics
}
