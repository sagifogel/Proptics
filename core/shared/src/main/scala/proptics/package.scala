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

  /** [[AnIndexedLens]] is a specialization of [[AnIndexedLens_]]. An optic type [[AnIndexedLens]] */
  type AnIndexedLens[I, S, A] = AnIndexedLens_[I, S, S, A, A]

  /** [[APrism]] is a specialization of [[APrism_]]. An optic type [[APrism]] */
  type APrism[S, A] = APrism_[S, S, A, A]

  /** [[Grate]] is a specialization of [[Grate_]]. An optic type [[Grate]] */
  type Grate[S, A] = Grate_[S, S, A, A]

  /** [[AGrate]] is a specialization of [[AGrate_]]. An optic type [[AGrate]] */
  type AGrate[S, A] = AGrate_[S, S, A, A]

  /** [[Getter]] is a specialization of [[Getter_]]. An optic type [[Getter]] */
  type Getter[S, A] = Getter_[S, S, A, A]

  /** [[Setter]] is a specialization of [[Setter_]]. An optic type [[Setter]] */
  type Setter[S, A] = Setter_[S, S, A, A]

  /** [[Review]] is a specialization of [[Review_]]. An optic type [[Review]] */
  type Review[S, A] = Review_[S, S, A, A]

  /** [[Fold]] is a specialization of [[Fold_]]. An optic type [[Fold]] */
  type Fold[S, A] = Fold_[S, S, A, A]

  /** [[IndexedTraversal]] is a specialization of [[IndexedTraversal_]]. An optic type [[IndexedTraversal]] */
  type IndexedTraversal[I, S, A] = IndexedTraversal_[I, S, S, A, A]

  /** [[IndexedFold]] is a specialization of [[IndexedFold_]]. An optic type [[IndexedFold]] */
  type IndexedFold[I, S, A] = IndexedFold_[I, S, S, A, A]

  /** [[IndexedGetter]] is a specialization of [[IndexedGetter_]]. An optic type [[IndexedGetter]] */
  type IndexedGetter[I, S, A] = IndexedGetter_[I, S, S, A, A]

  /** [[IndexedSetter]] is a specialization of [[IndexedSetter_]]. An optic type [[IndexedSetter]] */
  type IndexedSetter[I, S, A] = IndexedSetter_[I, S, S, A, A]
}
