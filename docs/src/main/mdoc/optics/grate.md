---
id: grate
title: Grate
---

```scala
case class Request(url: String)
sealed trait Response
case class Ok[A](value: A) extends Response
case object Forbidden extends Response

def extractSeriesId(req: Request): Option[String] =
  req.url.split('?').toList match {
    case _ :: qs :: Nil =>
      val qsMap = qsToMap(qs)
      if (qsMap.get("password").exists(_ === "123456")) qsMap.get("id") orElse "tt0903747".some
      else None
    case _ => None
  }

def qsToMap(qs: String): Map[String, String] =
  qs.split('&').toList match {
    case ls @ _ :: _ :: Nil =>
      ls.foldLeft(Map.empty[String, String]) { (map, current) =>
        current.split('=') match {
          case Array(key, value) => map.updated(key, value)
          case _                 => map
        }
      }
    case _ => Map.empty[String, String]
  }

def mkRequest(f: (Request => Option[String]) => Option[List[String]]): Response =
  f(extractSeriesId) match {
    case Some(list) => Ok(list)
    case _          => Forbidden
  }

val seriesMap: Map[String, List[String]] = Map[String, List[String]](
  "tt0903747" -> List("True Detective", "Fargo", "Dexter"),
  "tt2356777" -> List("Breaking Bad", "Fargo", "Dexter"),
  "tt2802850" -> List("Breaking Bad", "True Detective", "Dexter"),
  "tt0773262" -> List("Breaking Bad", "True Detective", "Fargo")
)

def recommendSeries(seriesId: Option[String]): Option[List[String]] = seriesId.flatMap(seriesMap.get)

val grate: Grate_[Request, Response, Option[String], Option[List[String]]] = Grate_(mkRequest)
val recommendedSeries: Request => Response = grate.over(recommendSeries)
recommendedSeries(Request("/recommend?password=123456&id=tt2356777"))
```