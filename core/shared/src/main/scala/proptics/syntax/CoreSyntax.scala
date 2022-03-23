package proptics.syntax

trait CoreSyntax
    extends AtSyntax
    with IsoSyntax
    with FoldSyntax
    with LensSyntax
    with StarSyntax
    with AnIsoSyntax
    with ALensSyntax
    with PrismSyntax
    with TupleSyntax
    with IndexSyntax
    with CostarSyntax
    with APrismSyntax
    with SetterSyntax
    with FunctionSyntax
    with TraversalSyntax
    with ATraversalSyntax
    with IndexedLensSyntax
    with IndexedFoldSyntax
    with AnIndexedLensSyntax
    with IndexedTraversalSyntax
    with FunctorWithIndexSyntax
    with FoldableWithIndexSyntax
    with TraverseWithIndexSyntax

object core {
  object all extends CoreSyntax
}
