package proptics

package object instances {
  object all extends AllInstances
  object at extends AtInstances
  object cons extends ConsInstances
  object index extends IndexInstances with ScalaVersionSpecificIndexInstances
  object functorWithIndex extends FunctorWithIndexInstances
  object foldableWithIndex extends FoldableWithIndexInstances
  object traverseWithIndex extends TraverseWithIndexInstances
}
