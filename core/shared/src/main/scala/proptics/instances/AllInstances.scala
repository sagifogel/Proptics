package proptics.instances

import proptics.std.AllStdOptics

trait AllInstances
    extends PartsOf
    with AtInstances
    with AllStdOptics
    with ConsInstances
    with EachInstances
    with EmptyInstances
    with IndexInstances
    with Field1Instances
    with Field2Instances
    with Field3Instances
    with Field4Instances
    with Field5Instances
    with FieldsInstances
    with ReverseInstances
    with PrefixedInstances
    with SuffixedInstances
    with NonEmptyConsInstances
    with FunctorWithIndexInstances
    with FoldableWithIndexInstances
    with TraverseWithIndexInstances
