package proptics.syntax

package object applied {
  object all extends AppliedSyntax
  object at extends AppliedAtSyntax
  object index extends AllIndexSyntax
  object fold extends AppliedFoldSyntax
  object each extends AppliedEachSyntax
  object lens extends AppliedLensSyntax
  object cons extends AppliedConsSyntax
  object empty extends AppliedEmptySyntax
  object tuple extends AppliedTupleSyntax
  object either extends AppliedEitherSyntax
  object fields extends AppliedFieldsSyntax
  object string extends AppliedStringSyntax
  object reversed extends AppliedReverseSyntax
  object suffixed extends AppliedSuffixedSyntax
  object prefixed extends AppliedPrefixedSyntax
  object nonEmptyCons extends AppliedNonEmptyCons
  object traversal extends AppliedTraversalSyntax
  object affineTraversal extends AppliedAffineTraversalSyntax
}
