package observatory

import java.time.LocalDate

/**
  * 1st milestone: data extraction
  */
trait Extraction {

  type LocatedTemperature = (LocalDate, Location, Temperature)
  type StnId = Int
  type WbanId = Int
  type StationId = (Option[StnId], Option[WbanId])

  private def asTemperature(double: Temperature): Temperature =
    math.round(double * 10) / 10d

  protected def toCelsius(fahrenheit: Temperature): Temperature =
    asTemperature((fahrenheit - 32) * 5 / 9)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)]

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    def yearlyAvgTemperature(temperatures: Iterable[Double]): Double =
      asTemperature(temperatures.sum / temperatures.size)

    records
      .groupBy(_._2)
      .toSeq
      .sortBy(x => (x._1.lat, x._1.lon))
      .map { case ((location , records)) => (location, yearlyAvgTemperature(records map (_._3))) }
  }
}

trait SparkExtraction extends Extraction with Serializable { sparkExtraction =>
  import java.nio.file.Paths
  import org.apache.spark.sql._
  import org.apache.spark.sql.types._

  private def withSparkSession[T](f: SparkSession => T): T = {
    val sparkSession: SparkSession =
      SparkSession
        .builder()
        .appName("Time Usage")
        .config("spark.master", "local")
        .getOrCreate()
    try { f(sparkSession) } finally { sparkSession.close }
  }

  import scala.reflect.ClassTag
  import org.apache.spark.rdd.RDD

  override def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    def parse[T](row: Row, i: Int, f: String => T): Option[T] =
      Option(row.getString(i)) filter (_.nonEmpty) map f
    def parseInt(row: Row, i: Int): Option[Int] = parse(row, i, _.toInt)
    def parseDouble(row: Row, i: Int): Option[Double] = parse(row, i, _.toDouble)
    def parseStationId(row: Row): StationId = (parseInt(row, 0), parseInt(row, 1))

    def toLocation(row: Row): (StationId, Option[Location]) = {
//      println(s"toLocation: ${row.mkString(",")}")
      val statitionId = parseStationId(row)
      val location = for {
        lat <- parseDouble(row, 2)
        lon <- parseDouble(row, 3)
      } yield Location(lat, lon)

      (statitionId, location)
    }

    def toTemperature(row: Row): (StationId, LocalDate, Temperature) = {
//      println(s"toTemperature: ${row.mkString(",")}")
      val statitionId = parseStationId(row)
      (statitionId, LocalDate.of(year, parseInt(row, 2).get, parseInt(row, 3).get),
          toCelsius(parseDouble(row, 4).get))
    }

    withSparkSession { implicit spark =>
      import spark.implicits._

      val locationsByStation: RDD[(StationId, Location)] =
        lines(stationsFile)
          .transformLines(4, toLocation)
          .collect { case ((stationId, Some(location))) => (stationId, location) }
          .keyBy(_._1).mapValues(_._2)
      val temperaturesByStation: RDD[(StationId, (LocalDate, Temperature))] =
        lines(temperaturesFile)
          .transformLines(5, toTemperature)
          .keyBy(_._1).mapValues { case ((_, localDate, temperature)) => (localDate, temperature) }

      val locatedTemperatures: RDD[(LocalDate, Location, Temperature)] =
        locationsByStation.join(temperaturesByStation)
          .map { case ((_, (location, (localDate, temperature)))) => (localDate, location, temperature) }

      locatedTemperatures.collect
    }
  }

  def lines(resource: String)(implicit spark: SparkSession): RDD[String] =
    spark.sparkContext.textFile(fsPath(resource))

  private def transformLines[T: ClassTag](lines: RDD[String], numCols: Int, f: Row => T): RDD[T] = {
    lines
      .map(_.split(",", numCols).toSeq)
      .map(f compose Row.fromSeq)
  }
  implicit class TransformLines(lines: org.apache.spark.rdd.RDD[String]) {
    def transformLines[T: ClassTag](numCols: Int, f: Row => T): RDD[T] =
      sparkExtraction.transformLines(lines, numCols, f)
  }

  /** @return The filesystem path of the given resource */
  private def fsPath(resource: String): String =
    Paths.get(getClass.getResource(resource).toURI).toString
}

object Extraction extends SparkExtraction {
}
