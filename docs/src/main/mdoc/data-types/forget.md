---
id: forget
title: Forget
---

`Forget[R, A, B]` is a data type shaped like a `Profunctor`, that forgets the `B` and returns value of type `R`

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

## Forget as a fold encoding

Forget is a type that is used to implement folds. In order to encode a fold within a Profunctor, we need to come up with a type 
that takes two type parameters, and encapsulates the notion of fold.<br/>
After fixing the `R` in `Forget[R, A, B]`, we got a type that takes two type parameters, and met the first requirement. Now we need 
to address the second requirement.<br/> 

#### foldMap

`foldMap` is a function that takes a foldable structure `S` and a mapping function `f` from `A` into `R`, and then
combining them using the given `Monoid[R]` instance.
                                
```scala
def foldMap[S, A, R: Monoid](s: S)(f: A => R): R
```

We can implement all fold functions in terms of `foldMap` 

```scala
def fold[S, A: Monoid](s: S): A = foldMap[S, A, A](s)(identity)

def foldl[S, A, R: Monoid](s: S)(r: R)(f: (R, A) => R): R = foldMap[S, A, R](s)(f(r, _))

def foldr[S, A, R: Monoid](s: S)(r: R)(f: (A, R) => R): R = foldMap[S, A, R](s)(f(_, r))
```

The `Forget` type wraps a `foldMap` function `runForget: A => R` within, thus making itself an appropriate
type that can form a Profunctor. The `Monoid[R]` can be found in the `apply ` signature of `Fold_[S, T, A, B]`:

```scala
abstract class Fold_[S, T, A, B] {
  private[proptics] def apply[R: Monoid](forget: Forget[R, A, B]): Forget[R, S, T]
}
```

Let's see how `foldMap` is actually implemented in `Fold`

```scala
def foldMap[R: Monoid](s: S)(f: A => R): R = self(Forget(f)).runForget(s)
```

The `apply` function of `Fold_[S, T, A, B]` takes a `Forget[R, A, B]` and an implicit instance of `Monoid[R]` and returns
a new `Forget[R, S, T]`. In `foldMap` we call the `apply` function with a `Forget` instance that wraps our `fold` function `R => A`, which gives us a new instance of `Forget[R, S, T]`.<br/>
The `Forget[R, S, T]` wraps a function `S => A` and forgets the `T`. In order to get an `R` for the return type of `foldMap`, we just need to 
unwrap the `Forget[R, S, T]` using `runForget` and invoke the function with the supplied argument of `S`.




