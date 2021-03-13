package proptics

package object instances {
  object at extends AtInstances
  object all extends AllInstances
  object cons extends ConsInstances
  object index extends IndexInstances
  object nonEmptyCons extends NonEmptyConsInstances
  object functorWithIndex extends FunctorWithIndexInstances
  object foldableWithIndex extends FoldableWithIndexInstances
  object traverseWithIndex extends TraverseWithIndexInstances
}
