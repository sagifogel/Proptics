package object proptics {

  /** [[Iso]] is a specialization of [[Iso_]] */
  type Iso[S, A] = Iso_[S, S, A, A]

  /** [[AnIso]] is a specialization of [[AnIso_]] */
  type AnIso[S, A] = AnIso_[S, S, A, A]

  /** [[Lens]] is a specialization of [[Lens_]] */
  type Lens[S, A] = Lens_[S, S, A, A]

  /** [[ALens]] is a specialization of [[ALens_]] */
  type ALens[S, A] = ALens_[S, S, A, A]

  /** [[Prism]] is a specialization of [[Prism_]] */
  type Prism[S, A] = Prism_[S, S, A, A]

  /** [[APrism]] is a specialization of [[APrism_]] */
  type APrism[S, A] = APrism_[S, S, A, A]

  /** [[AffineTraversal]] is a specialization of [[AffineTraversal_]] */
  type AffineTraversal[S, A] = AffineTraversal_[S, S, A, A]

  /** [[AffineTraversal]] is a specialization of [[AffineTraversal_]] */
  type AnAffineTraversal[S, A] = AnAffineTraversal_[S, S, A, A]

  /** [[Traversal]] is a specialization of [[Traversal_]] */
  type Traversal[S, A] = Traversal_[S, S, A, A]

  /** [[ATraversal]] is a specialization of [[ATraversal_]] */
  type ATraversal[S, A] = ATraversal_[S, S, A, A]

  /** [[Fold]] is a specialization of [[Fold_]] */
  type Fold[S, A] = Fold_[S, S, A, A]

  /** [[Getter]] is a specialization of [[Getter_]] */
  type Getter[S, A] = Getter_[S, S, A, A]

  /** [[Setter]] is a specialization of [[Setter_]] */
  type Setter[S, A] = Setter_[S, S, A, A]

  /** [[Review]] is a specialization of [[Review_]] */
  type Review[S, A] = Review_[S, S, A, A]

  /** [[Grate]] is a specialization of [[Grate_]] */
  type Grate[S, A] = Grate_[S, S, A, A]

  /** [[IndexedLens]] is a specialization of [[IndexedLens_]] */
  type IndexedLens[I, S, A] = IndexedLens_[I, S, S, A, A]

  /** [[AnIndexedLens]] is a specialization of [[AnIndexedLens_]] */
  type AnIndexedLens[I, S, A] = AnIndexedLens_[I, S, S, A, A]

  /** [[IndexedTraversal]] is a specialization of [[IndexedTraversal_]] */
  type IndexedTraversal[I, S, A] = IndexedTraversal_[I, S, S, A, A]

  /** [[IndexedFold]] is a specialization of [[IndexedFold_]] */
  type IndexedFold[I, S, A] = IndexedFold_[I, S, S, A, A]

  /** [[IndexedGetter]] is a specialization of [[IndexedGetter_]] */
  type IndexedGetter[I, S, A] = IndexedGetter_[I, S, S, A, A]

  /** [[IndexedSetter]] is a specialization of [[IndexedSetter_]] */
  type IndexedSetter[I, S, A] = IndexedSetter_[I, S, S, A, A]
}
