package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._
import java.io.File

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
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

    val width = 256
    val height = 256
    val step = 1d / width
    def interpolate(a1: Int, a2: Int): Double = a1.toDouble + a2 * step

    val pixels: Seq[Pixel] = for {
      yt <- 0 until height
      xt <- 0 until width
      color = interpolateColor(colors, predictTemperature(temperatures, tileLocationDouble(zoom, interpolate(x, xt), interpolate(y, yt))))
    } yield color.toPixel
 
    Image(width, height, pixels.toArray)
  }

  private type YearlyData = Iterable[(Location, Temperature)]

  private[observatory] def generateImage(year: Year, zoom: Zoom, x: Int, y: Int, data: YearlyData): Unit = {
    val outputFile = new File(s"target/temperatures/$year/$zoom/$x-$y.png”")
    outputFile.getParentFile.mkdirs()
    val image = tile(data, ColorPalette, zoom, x, y)
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
    for {
      zoom <- 0 to 3
      (year, data) <- yearlyData
      maxCoord = math.pow(2, zoom).toInt - 1
      x <- 0 to maxCoord
      y <- 0 to maxCoord
    } generateImage(year, zoom, x, y, data)
  }
}
