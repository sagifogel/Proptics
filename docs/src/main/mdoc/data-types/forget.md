---
id: forget
title: Forget
---

`Forget[R, A, B]` is a data type shaped like a profunctor, that forgets the `B` value and returns an accumulated value of type `R`

```scala
case class Forget[R, A, B](runForget: A => R)
```

`Profunctor[_, _]` is a type constructor that takes 2 type parameters. `Forget[R, A, B]` is a type that has 3 type parameters, so we need
to fix one of the type parameters of `Forget` in order to create an instance of `Profunctor` of `Forget`. We can use Scala's type lambda syntax:

```scala
implicit def profunctorForget[R]: Profunctor[({ type P[A, B] = Forget[R, A, B] })#P] = 
  new Profunctor[({ type P[A, B] = Forget[R, A, B] })#P] {
    override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
      Forget(fab.runForget compose f)
  }
```

or we can use the <a href="https://github.com/typelevel/kind-projector" target="_blank">kind projector</a> compiler plugin:

```scala
implicit def profunctorForget[R]: Profunctor[Forget[R, *, *]] = new Profunctor[Forget[R, *, *]] {
  override def dimap[A, B, C, D](fab: Forget[R, A, B])(f: C => A)(g: B => D): Forget[R, C, D] =
    Forget(fab.runForget compose f)
}
```

Now that the `R` of the `Forget` is fixed only `A` and `B` can vary, but the `Forget` type does not use the type `B` or rather, 
forgets it. The type argument `B` is used here as a phantom type, which means that it exists just to
satisfy some type constraints, but it does not have any effect on the runtime. in this example it serves as the second type argument for the Profunctor.

 
```
runForget: A => R
```

So a function `A => R` where you can vary the `A` forms a Profunctor. 




