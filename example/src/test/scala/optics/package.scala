import java.awt.Color

package object optics {
  sealed trait Fill
  final case class Solid(color: Color) extends Fill
  final case class LinearGradient(color1: Color, color2: Color) extends Fill
  final case class RadialGradient(color1: Color, color2: Color, color3: Color) extends Fill
  case object NoFill extends Fill

  final case class Person(name: String, address: Address)
  final case class Address(city: String, street: Street)
  final case class Street(name: String, number: Int)

  trait Award
  case object Emmy extends Award
  case object None_ extends Award
  case object GoldenGlobe extends Award

  final case class Language(name: String, designer: String)
  final case class Actor(name: String, birthYear: Int, nomiation: Award, awards: List[Award])
  final case class TVShow(title: String, numEpisodes: Int, numSeasons: Int, criticScore: Int, actors: List[Actor])

  final case class UserRegistration(userName: String, password: String, yearOfBirth: Int)

  type Path = List[String]
  type Body = String

  sealed trait Request
  final case class GET(path: Path) extends Request
  final case class POST(path: Path, body: Body) extends Request
  final case class DELETE(path: Path) extends Request

  sealed trait Response
  case object OK extends Response
  case object Forbidden extends Response

  val rj: Actor = Actor("RJ Mitte", 1992, None_, List.empty)
  val norris: Actor = Actor("Dean Norris", 1963, None_, List.empty)
  val brandt: Actor = Actor("Betsy Brandt", 1973, None_, List.empty)
  val banks: Actor = Actor("Jonathan Banks", 1947, Emmy, List.empty)
  val gunn: Actor = Actor("Anna Gunn", 1968, Emmy, List(Emmy, Emmy))
  val seehorn: Actor = Actor("Rhea Seehorn", 1972, None_, List.empty)
  val esposito: Actor = Actor("Giancarlo Esposito", 1958, Emmy, List.empty)
  val paul: Actor = Actor("Aaron Paul", 1979, GoldenGlobe, List(Emmy, Emmy, Emmy))
  val odenkirk: Actor = Actor("Bob Odenkirk", 1962, GoldenGlobe, List(Emmy, Emmy))
  val cranston: Actor = Actor("Bryan Cranston", 1956, GoldenGlobe, List(GoldenGlobe, Emmy, Emmy, Emmy))
  val breakingBadActors: List[Actor] = List(cranston, paul, gunn, norris, brandt, rj, odenkirk, esposito, banks)
  val betterCallSaulActors: List[Actor] = List(odenkirk, banks, esposito)
  val breakingBad: TVShow = TVShow("Breaking Bad", 62, 5, 96, breakingBadActors)
  val betterCallSaul: TVShow = TVShow("Better Call Saul", 63, 6, 97, betterCallSaulActors)
  val tvShows: List[TVShow] = List(breakingBad, betterCallSaul)
  val mrWhite: Person = Person("Walter White", Address("Albuquerque", Street("Negra Arroyo Lane", 308)))
}
