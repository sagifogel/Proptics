package proptics.example
import cats.data.NonEmptyList

import proptics.instances.nonEmptyCons._
import proptics.syntax.all._

trait Award
case object Emmy extends Award
case object None_ extends Award
case object GoldenGlobe extends Award

final case class Language(name: String, designer: String)
final case class Actor(name: String, birthYear: Int, nomination: Award, awards: List[Award])
final case class TVShow(title: String, numEpisodes: Int, numSeasons: Int, criticScore: Int, actors: List[Actor])
final case class Person(name: String, address: Address)
final case class Address(city: String, street: Street)
final case class Street(name: String, number: Int)

object Main {
  val rj: Actor = Actor("RJ Mitte", 1992, None_, Nil)
  val norris: Actor = Actor("Dean Norris", 1963, None_, Nil)
  val brandt: Actor = Actor("Betsy Brandt", 1973, None_, Nil)
  val banks: Actor = Actor("Jonathan Banks", 1947, Emmy, Nil)
  val gunn: Actor = Actor("Anna Gunn", 1968, Emmy, List(Emmy, Emmy))
  val seehorn: Actor = Actor("Rhea Seehorn", 1972, None_, Nil)
  val esposito: Actor = Actor("Giancarlo Esposito", 1958, Emmy, Nil)
  val paul: Actor = Actor("Aaron Paul", 1979, GoldenGlobe, List(Emmy, Emmy, Emmy))
  val odenkirk: Actor = Actor("Bob Odenkirk", 1962, GoldenGlobe, List(Emmy, Emmy))
  val cranston: Actor = Actor("Bryan Cranston", 1956, GoldenGlobe, List(GoldenGlobe, Emmy, Emmy, Emmy))
  val breakingBadActors: List[Actor] = List(cranston, paul, gunn, norris, brandt, rj, odenkirk, esposito, banks)
  val betterCallSaulActors: List[Actor] = List(odenkirk, banks, esposito)
  val breakingBad: TVShow = TVShow("Breaking Bad", 62, 5, 96, breakingBadActors)
  val betterCallSaul: TVShow = TVShow("Better Call Saul", 63, 6, 97, betterCallSaulActors)
  val tvShows: List[TVShow] = List(breakingBad, betterCallSaul)
  val mrWhite: Person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
  //implicit val eqActor = Eq.fromUniversalEquals[Actor]

  sealed trait Json
  case class JNull() extends Json
  case class JString(value: String) extends Json
  case class JNumber(value: Double) extends Json

  def main(args: Array[String]): Unit = {
    val list = List((1, "First"), (2, "Second"))
    val nel = NonEmptyList.fromList(list)
    val foldable = nel.foldable.head.swap
    println(foldable.viewAll)
  }
}
