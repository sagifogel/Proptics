package object optics {
  /**
   * [[Lens_]] is a specialization of [[Lens]]. An optic of type [[Lens_]]
   * can change only the value of its focus, not its type.
   */
  type Lens_[P[_, _], S, A] = Lens[P, S, S, A, A]

  /** [[Prism_]] is a specialization of [[Prism]]. An optic of type [[Prism_]] */
  type Prism_[P[_, _], S, A] = Prism[P, S, S, A, A]

  /** [[Iso_]] is a specialization of [[Iso]]. An optic of type [[Iso_]] */
  type Iso_[P[_, _], S, A] = Iso[P, S, S, A, A]

  /** [[Optic_]] is a specialization of [[Optic]] */
  type Optic_[P[_, _], S, A] = Optic[P, S, S, A, A]

  /** [[Traversal_]] is a specialization of [[Traversal]]. An optic type [[Traversal_]] */
  type Traversal_[P[_, _], S, A] = Traversal[P, S, S, A, A]

  /** [[AnIso_]] is a specialization of [[AnIso]]. An optic type [[AnIso_]] */
  type AnIso_[S, A] = AnIso[S, A, A, A]

  /** [[ALens_]] is a specialization of [[ALens]]. An optic type [[ALens_]] */
  type ALens_[S, A] = ALens[S, A, A, A]

  /** [[ATraversal_]] is a specialization of [[ATraversal]] */
  type ATraversal_ [S, A] = ATraversal[S, S, A, A]
}
