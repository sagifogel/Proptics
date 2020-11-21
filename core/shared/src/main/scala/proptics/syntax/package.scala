package proptics

package object syntax {
  object all extends AllSyntax
  object iso extends IsoSyntax
  object anIso extends AnIsoSyntax
  object lens extends LensSyntax
  object aLens extends ALensSyntax
  object prism extends PrismSyntax
  object aPrism extends APrismSyntax
  object traversal extends TraversalSyntax
  object aTraversal extends ATraversalSyntax
  object setter extends SetterSyntax
  object indexedLens extends IndexedLensSyntax
  object anIndexedLens extends AnIndexedLensSyntax
  object indexedTraversal extends IndexedTraversalSyntax
  object function extends FunctionSyntax
  object tuple extends TupleSyntax
  object star extends StarSyntax
  object costar extends CostarSyntax
  object at extends AtSyntax
}
