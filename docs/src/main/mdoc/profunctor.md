---
id: profunctor
title: Profunctor
---


>Profunctors are bifunctors that are contravariant in their first type argument <br/> and covariant in their second one

In order to understand the meaning of the sentence, we need to know the foundations of a Profunctor

## Type Constructor
 
Type constructor is basically a type level function (function that acts on types rather on values), when given a type creates a new type.

`List` is a type constructor. By itself it’s not a valid type.<br/>
It takes a type (often called type parameter) in order to create an actual type of List.<br/> 
For example in order to create a `List[String]`, we need to supply the List type constructor a type argument of `String`.
   
 ```scala
  List[+A] -> String -> List[String]
```
    
 ```scala
val list: List[String] = List[String]("A", "B", "C")
//                        ^     ^^   
//            type constructor  expected type
```

## Functor
 
Functor (a.k.a Covariant Functor) is a type class for type constructors that define a map function
```scala
  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
```

In order to create an instance of a `Functor` you must provide it a type constructor that takes one type parameter, 
Where's the `F[_]` is the underlying type constructor  

Example implementation for an instance of a Functor for `List`
```scala
  import cats.Functor
  
  implicit val functorForList: Functor[List] = new Functor[List] {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.map(f)
  }

  def usageOfFunctorForList(list: List[Int])(implicit F: Functor[List]): List[Int] =
     F.map(list)(_ + 1)
    
  usageOfFunctorForList(List(1, 2, 3))
// res0: List[Int] = List(2,3,4)  
```

The intuition for the map function is, if you have a context of `F[A]`, and a function from `A -> B`, 
and you also can unwrap the value `A` from its content/wrap a value `A` with a context of `F`,<br/>
then you could unwrap it, feed it to the `f` function to create a `B`, and then wrap it again with an `F`.<br/>
You can think of the `map` function as a producer of `B`s.

## Contravariant
Contravariant (a.k.a Contravariant Functor) is a type class for type constructors that define a contramap function

```scala
  trait Contravariant[F[_]] {
    def contramap[A, B](fa: F[A])(f: B => A): F[B]
  }
```

It looks like regular (Covariant) Functor’s map, but the `f` function is from `B -> A` instead of `A -> B`.<br/>
You'll be confused if you try to come up with an intuition like the one for `map`,<br/>
because now you have a function from `B -> A` but the context is `F[A]`.<br/> if you extract the `A` from `F[_]` you will not be able to feed it to the `f` function.<br/>
The best way to understand it is by using an example. The example will use the `Eq` type class, which is a type constructor.<br/>  

Let's say we have a case class of Person

 ```scala
  final case class Person(id: String, name: String, yearOfBirth: Int) 
```

and we want to compare between two persons using the id field.<br/>
One possible way to do it is to create a new instance of `Eq[Person]`

 ```scala
  import cats.Eq
  import cats.syntax.eq._ // triple equals operator (===)

  implicit val eqPerson: Eq[Person] = Eq.instance[Person] { (person1, person2) =>
    person1.id === person2.id
  }

  Person("123", "Samuel Eilenberg", 1913) === Person("123", "Samuel Eilenberg", 1913)
// res0: Boolean = true
``` 

Another way is to use Contravariant.<br/>
Basically we are trying to compare strings, so we can reuse an instance of `Eq[String]` provided by cats, 
and to transform it to an instance of `Eq[Person]` using the `contramap` function.

 ```scala
  import cats.Eq
  import cats.syntax.eq._ // triple equals operator (===)
  import cats.syntax.contravariant._ // contramap syntax for Eq
  
  implicit val eqPerson: Eq[Person] =  Eq[String].contramap[Person](_.id)

  Person("123", "Samuel Eilenberg", 1913) === Person("123", "Samuel Eilenberg", 1913)
// res0: Boolean = true
``` 

That is, if you have a context of `F[A] | Eq[String]`, which is a context of type `F[_] | Eq[_]`<br/> for a type `A | String`, and you have another type
`B | Person` that you can extract an `A | String` out of it, then you can get a context of type `F[B] | Eq[Person]`

```scala 
  def contramap[A,      B     ](fa: F[A]     )(f: B       => A     ): F[B]    
  def contramap[String, Person](fa: Eq[String])(f: Person => String): Eq[Person]                      
```  
You can think of the `contramap` function as a consumer of `B`s.

## Bifunctor

Bifunctor takes two type parameters instead of one (`F[_, _]`), and is a (Covariant) Functor on both sides.

```scala
  trait Bifunctor[F[_, _]] {
    def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
  }
```

It defines a `bimap` method, which enables to map on both sides.

Example implementation for an instance of a Bifunctor for `Either`
```scala
  import cats.Bifunctor
  import cats.syntax.either._

  implicit val bifunctorForEither: Bifunctor[Either] = new Bifunctor[Either] {
    override def bimap[A, B, C, D](fab: Either[A, B])(f: A => C, g: B => D): Either[C, D] =
      fab.fold(a => f(a).asLeft[D], b => g(b).asRight[C])
  }

  def usageOfBiFunctorForEither(either: Either[Throwable, Int])
                               (implicit F: Bifunctor[Either]): Either[String, String] =
    F.bimap(either)(_.getMessage, _.toString)
  
  usageOfBifunctorForEither(Right(10))
// res0: Either[String,String] = Right(10)  
```

## Profunctor

Now we can understand the opening quote.<br/>
Profunctors are bifunctors (takes two type parameters instead of one (`F[_, _]`)<br/>
that are contravariant in their first type argument and covariant in their second one.<br/>


```scala
trait Profunctor[F[_, _]] {
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D]
}
```

It defines a `dimap` method, which enables `contramap` on the first type parameter and `map` on the second type parameter .

Example implementation for an instance of a Profunctor for `Function`
```scala
  import cats.arrow.Profunctor

  implicit val profunctorForFunction: Profunctor[Function] = new Profunctor[Function] {
    override def dimap[A, B, C, D](fab: Function[A, B])(f: C => A)(g: B => D): Function[C, D] =
      g compose fab compose f
  }
 
  def usageOfProfunctorForFunction(f: Function[Int, Int])
                                  (implicit F: Profunctor[Function]): Function[Person, String] =
    F.dimap[Int, Int, Person, String](f)(_.yearOfBirth)(_.toString)

  usageOfProfunctorForFunction(_ / 100).apply(Person("123", "Samuel Eilenberg", 1913))
// res0: String = 19  
```



