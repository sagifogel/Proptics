package object proptics {
  /**
   * [[Lens]] is a specialization of [[Lens_]]. An optic of type [[Lens]]
   * can change only the value of its focus, not its type.
   */
  type Lens[S, A] = Lens_[S, S, A, A]

  /** [[Prism]] is a specialization of [[Prism_]]. An optic of type [[Prism]] */
  type Prism[S, A] = Prism_[S, S, A, A]

  /** [[Iso]] is a specialization of [[Iso_]]. An optic of type [[Iso]] */
  type Iso[S, A] = Iso_[S, S, A, A]

  /** [[Traversal]] is a specialization of [[Traversal_]]. An optic type [[Traversal]] */
  type Traversal[S, A] = Traversal_[S, S, A, A]

  /** [[ATraversal]] is a specialization of [[ATraversal_]]. An optic type [[ATraversal]] */
  type ATraversal[S, A] = ATraversal_[S, S, A, A]

  /** [[AnIso]] is a specialization of [[AnIso_]]. An optic type [[AnIso]] */
  type AnIso[S, A] = AnIso_[S, S, A, A]

  /** [[ALens]] is a specialization of [[ALens_]]. An optic type [[ALens]] */
  type ALens[S, A] = ALens_[S, S, A, A]

  /** [[IndexedLens]] is a specialization of [[IndexedLens_]]. An optic type [[IndexedLens]] */
  type IndexedLens[I, S, A] = IndexedLens_[I, S, S, A, A]

  /** [[AnIndexedLens_]] is a specialization of [[AnIndexedLens]]. An optic type [[AnIndexedLens_]] */
  type AnIndexedLens_[I, S, A] = AnIndexedLens[I, S, S, A, A]

  /** [[APrism_]] is a specialization of [[APrism]]. An optic type [[APrism_]] */
  type APrism_[S, A] = APrism[S, S, A, A]

  /** [[Grate_]] is a specialization of [[Grate]]. An optic type [[Grate_]] */
  type Grate_[S, A] = Grate[S, S, A, A]

  /** [[AGrate_]] is a specialization of [[AGrate]]. An optic type [[AGrate_]] */
  type AGrate_[S, A] = AGrate[S, S, A, A]

  /** [[Getter_]] is a specialization of [[Getter]]. An optic type [[Getter_]] */
  type Getter_[S, A] = Getter[S, S, A, A]

  /** [[AGetter_]] is a specialization of [[AGetter]]. An optic type [[AGetter_]] */
  type AGetter_[S, A] = AGetter[S, S, A, A]

  /** [[Setter_]] is a specialization of [[Setter]]. An optic type [[Setter_]] */
  type Setter_[S, A] = Setter[S, S, A, A]

  /** [[Review_]] is a specialization of [[Review]]. An optic type [[Review_]] */
  type Review_[S, A] = Review[S, S, A, A]

  /** [[Fold_]] is a specialization of [[Fold]]. An optic type [[Fold_]] */
  type Fold_[S, A] = Fold[S, S, A, A]

  /** [[IndexedOptic_]] is a specialization of [[IndexedOptic]] */
  type IndexedOptic_[P[_, _], I, S, A] = IndexedOptic[P, I, S, S, A, A]

  /** [[IndexedTraversal_]] is a specialization of [[IndexedTraversal]]. An optic type [[IndexedTraversal_]] */
  type IndexedTraversal_[I, S, A] = IndexedTraversal[I, S, S, A, A]

  /** [[IndexedFold_]] is a specialization of [[IndexedFold]]. An optic type [[IndexedFold_]] */
  type IndexedFold_[I, S, A] = IndexedFold[I, S, S, A, A]

  /** [[IndexedGetter_]] is a specialization of [[IndexedGetter]]. An optic type [[IndexedGetter_]] */
  type IndexedGetter_[I, S, A] = IndexedGetter[I, S, S, A, A]

  /** [[IndexedSetter_]] is a specialization of [[IndexedSetter]]. An optic type [[IndexedSetter_]] */
  type IndexedSetter_[I, S, A] = IndexedSetter[I, S, S, A, A]
}
