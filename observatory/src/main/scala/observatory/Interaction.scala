package observatory

import java.io.File

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps
import scala.math.Pi
import scala.math.atan
import scala.math.sinh
import scala.math.toDegrees

import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.Pixel

import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.schedulers.ExecutionModel.AlwaysAsyncExecution


/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  def main(args: Array[String]): Unit = {
    (1975 to 2015) foreach (year =>
      Interaction.generateTiles(Seq(Interaction.yearlyData(year)), Interaction.generateImage))
  }

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
import monix.execution.schedulers.ExecutionModel.AlwaysAsyncExecution
    * 
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location =
    tileLocationDouble(zoom, x, y)

  private def tileLocationDouble(zoom: Int, x: Double, y: Double): Location = {
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y / (1<<zoom)))))
    val lon = x / (1<<zoom) * 360.0 - 180.0

    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Zoom, x: Int, y: Int): Image = {
    import Visualization._

    val scale = 4
    val tileSize = 256 / scale
    val alpha = 127
    val delta = 1.0 / tileSize

    def interpolate(a1: Int, a2: Int): Double = a1.toDouble + a2 * delta

    val pixels: Seq[Pixel] = for {
      yt <- 0 until tileSize
      xt <- 0 until tileSize
      color = interpolateColor(colors, predictTemperature(temperatures, tileLocationDouble(zoom, interpolate(x, xt), interpolate(y, yt))))
    } yield color.toPixel
 
    val image = Image(tileSize, tileSize, pixels.toArray)
    image.scale(scale)
  }

  private type YearlyData = Iterable[(Location, Temperature)]

  private[observatory] def generateImage(year: Year, zoom: Zoom, x: Int, y: Int, data: YearlyData): Unit = {
    val outputFile = new File(s"target/temperatures/$year/$zoom/$x-$y.png”")
    outputFile.getParentFile.mkdirs()
    val image = time(s"Generating tile $year $x $y (level $zoom)") {
      tile(data, ColorPalette, zoom, x, y)
    }
    image.output(outputFile)
    ()
  }

  private[observatory] def yearlyData(year: Year): (Year, YearlyData) = {
    val stationsFile = "/stations.csv"
    val temperaturesFile = s"/$year.csv"
    val records = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
    val temperatures: YearlyData = Extraction.locationYearlyAverageRecords(records)
    (year, temperatures)
  }

  private implicit val scheduler = Scheduler.computation(
        parallelism = Runtime.getRuntime().availableProcessors(),
        executionModel = AlwaysAsyncExecution)

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Zoom, Int, Int, Data) => Unit): Unit =
  {
    val imageTasks: Iterable[Task[Unit]] =
      for {
          (year, data) <- yearlyData
          zoom <- 0 to 3
          maxCoord = math.pow(2, zoom).toInt
          x <- 0 until maxCoord
          y <- 0 until maxCoord
        } yield Task(generateImage(year, zoom, x, y, data))

    val future = Task.gatherUnordered(imageTasks).runAsync
    Await.result(future, 4 hours)
    ()
  }
}
