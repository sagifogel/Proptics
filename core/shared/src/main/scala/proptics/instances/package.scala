package proptics

package object instances {
  object at extends AtInstances
  object all extends AllInstances
  object cons extends ConsInstances
  object each extends EachInstances
  object index extends IndexInstances
  object empty extends EmptyInstances
  object field1 extends Field1Instances
  object field2 extends Field2Instances
  object field3 extends Field3Instances
  object field4 extends Field4Instances
  object field5 extends Field5Instances
  object fields extends FieldsInstances
  object reverse extends ReverseInstances
  object prefixed extends PrefixedInstances
  object suffixed extends SuffixedInstances
  object nonEmptyCons extends NonEmptyConsInstances
  object functorWithIndex extends FunctorWithIndexInstances
  object foldableWithIndex extends FoldableWithIndexInstances
  object traverseWithIndex extends TraverseWithIndexInstances
}
