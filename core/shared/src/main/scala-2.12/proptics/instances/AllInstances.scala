package proptics.instances

trait AllInstances
  extends ConsInstances
  with ScalaVersionSpecificConsInstances
  with AtInstances
  with IndexInstances
  with ScalaVersionSpecificIndexInstances
  with FunctorWithIndexInstances
  with FoldableWithIndexInstances
  with TraverseWithIndexInstances
