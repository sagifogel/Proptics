package proptics

package object instances {
  object all extends AllInstances
  object at extends AtInstances
  object cons extends ConsInstances with ScalaVersionSpecificConsInstances
  object index extends IndexInstances with ScalaVersionSpecificIndexInstances
  object functorWithIndex extends FunctorWithIndexInstances with ScalaVersionSpecificFunctorWithIndexInstances
  object foldableWithIndex extends FoldableWithIndexInstances with ScalaVersionSpecificFoldableWithIndexInstances
  object traverseWithIndex extends TraverseWithIndexInstances with ScalaVersionSpecificTraverseWithIndexInstances
}
