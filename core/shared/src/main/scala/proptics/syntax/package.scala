package proptics

package object syntax {
  object iso extends IsoSyntax
  object anIso extends AnIsoSyntax
  object lens extends LensSyntax
  object aLens extends ALensSyntax
  object prism extends PrismSyntax
  object aPrism extends APrismSyntax
  object traversal extends TraversalSyntax
  object setter extends SetterSyntax
  object fold extends FoldSyntax
}