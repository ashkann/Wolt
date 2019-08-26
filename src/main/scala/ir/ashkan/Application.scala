package ir.ashkan

import ir.ashkan.Application.input

import scala.collection.SortedMap

object Application extends App {
  val input =
    """
{
  "monday" : [],
  "tuesday" : [
  {
    "type" : "open",
    "value" : 36000
  },
  {
    "type" : "close",
    "value" : 64800
  }
  ],
  "wednesday" : [],
  "thursday" : [
  {
    "type" : "open",
    "value" : 36000
  },
  {
    "type" : "close",
    "value" : 64800
  }
  ],
  "friday" : [
  {
    "type" : "open",
    "value" : 36000
  }
  ],
  "saturday" : [
  {
    "type" : "close",
    "value" : 3600
  },
  {
    "type" : "open",
    "value" : 36000
  }
  ],
  "sunday" : [
  {
    "type" : "close",
    "value" : 3600
  },
  {
    "type" : "open",
    "value" : 43200
  },
  {
    "type" : "close",
    "value" : 75600
  }
  ]
}
  """.stripMargin

  val input2 =
    """
{
  "monday" : [
  {
    "type" : "close",
    "value" : 75600
  }
  ],
  "tuesday" : [
  {
    "type" : "open",
    "value" : 36000
  },
  {
    "type" : "close",
    "value" : 64800
  }
  ],
  "wednesday" : [],
  "thursday" : [
  {
    "type" : "open",
    "value" : 36000
  },
  {
    "type" : "close",
    "value" : 64800
  }
  ],
  "friday" : [
  {
    "type" : "open",
    "value" : 36000
  }
  ],
  "saturday" : [
  {
    "type" : "close",
    "value" : 3600
  },
  {
    "type" : "open",
    "value" : 36000
  }
  ],
  "sunday" : [
  {
    "type" : "close",
    "value" : 3600
  },
  {
    "type" : "open",
    "value" : 43200
  }
  ]
}
  """.stripMargin

  println(api(input))
  println(api(input2))

  def api(input:String): String = {
    import spray.json._
    import DefaultJsonProtocol._

    trait Input
    case class Open(hour: Int) extends Input
    case class Close(hour: Int) extends Input

    implicit object InputFormat extends JsonFormat[Input] {
      override def read(value: JsValue): Input = {
        value.asJsObject.getFields("type", "value") match {
          case Seq(JsString(name), JsNumber(hour)) if (hour >= 0 && hour <= 24*3600) =>
            if(name == "open")
              Open(hour.toInt / 3600)
            else if (name == "close")
              Close(hour.toInt / 3600)
            else
              throw DeserializationException("Bad input")
          case _ => throw DeserializationException("Bad input")
        }
      }

      override def write(obj: Input) = ???
    }

    implicit object WeekOrdering extends Ordering[String] {
      val week = Seq("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
      private val order: Map[String,Int] = week.zipWithIndex.toMap

      override def compare(x: String, y: String) = order(x) - order(y)
    }

    val is = for {
      (dayOfWeek, is) <- SortedMap(input.parseJson.convertTo[Map[String, List[Input]]].toSeq: _* ).toList
      i <- is
    } yield (dayOfWeek,i)

    case class Shift(dayOfWeek: String, open: Int, close: Option[Int]) {
      lazy val shift = s"${hour(open)} - ${hour(close.get)}"

      private def hour(h: Int): String = if(h < 12) s"$h AM" else if (h==12) "12 PM" else s"${h-12} PM"
    }

    val os = {
      val (o, _) = is.foldLeft[(List[Shift], Option[Shift])]((List.empty, None)) { (out, i) =>
        (out, i) match {
          case ((os, Some(o@Shift(_, _, None))), (_, Close(hour))) => (o.copy(close = Option(hour)) :: os, None)
          case ((os, None), (dayOfWeek, Open(hour))) => (os, Option(Shift(dayOfWeek, hour, None)))
        }
      }
      o.groupBy(_.dayOfWeek)
    }

    println(is)

    (for {
      dayOfWeek <- WeekOrdering.week
      o = os.getOrElse(dayOfWeek, Seq.empty)
    } yield dayOfWeek.capitalize + ": " + (if (o.isEmpty) "Closed" else o.map(_.shift).mkString(","))
      ).mkString("\n")
  }
}
