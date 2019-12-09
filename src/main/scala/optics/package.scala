package object optics {
  /**
   * [[Lens_]] is a specialization of [[Lens]]. An optic of type [[Lens_]]
   * can change only the value of its focus, not its type.
   */
  type Lens_[P[_, _], S, A] = Lens[P, S, S, A, A]

  /**
   * [[Prism_]] is a specialization of [[Prism]]. An optic of type [[Prism_]]
   * can change only the value of its focus, not its type.
   */
  type Prism_[P[_, _], S, A] = Prism[P, S, S, A, A]

  /** [[Iso_]] is a specialization of [[Iso]]. An optic of type [[Iso_]] */
  type Iso_[P[_, _], S, A] = Iso[P, S, S, A, A]

  /** [[Optic_]] is a specialization of [[Optic]] */
  type Optic_[P[_, _], S, A] = Optic[P, S, S, A, A]
}
