import java.io.{BufferedReader, InputStreamReader}
import scala.io.Source

object CSVParse

case class Data(date: String, time: String, longitude: String, latitude: String):
    def getDate(): java.util.Date =
      val format = new java.text.SimpleDateFormat("yyyy/MM/dd");
      format.parse(date)

def parseCsvLine(line: String): Option[Data] =
    line.split(";").toVector.map(_.trim) match
      case Vector(date, time, longitude, latitude) => Some(Data(date, time, longitude, latitude))
      case _ => println(s"WARNING UNKNOWN DATA FORMAT FOR LINE: $line")
        None
end parseCsvLine

@ main def readData(): Vector[Data] =
  for
    line <- Source.fromFile(BufferedReader(InputStreamReader(System.in)).readLine()).getLines().toVector
    data <- parseCsvLine(line)
  yield data





