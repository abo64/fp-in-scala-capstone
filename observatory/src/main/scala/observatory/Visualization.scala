package observatory

import scala.language.implicitConversions
import scala.language.postfixOps
import math._
import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  type Distance = Double
  type Temperature = Double
  type Weight = Double

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val p = 3

    def dist(loc: Location): Distance = {
      val R = 6372.8 // Earth radius in km

      def haversine(lat1: Double, lon1: Double, lat2: Double, lon2: Double) = {
        val dLat = (lat2 - lat1).toRadians
        val dLon = (lon2 - lon1).toRadians

        val a = pow(sin(dLat / 2), 2) + pow(sin(dLon / 2), 2) * cos(lat1.toRadians) * cos(lat2.toRadians)
        val c = 2 * asin(sqrt(a))
        R * c
      }

      haversine(loc.lat, loc.lon, location.lat, location.lon)
    }

    def weight(dist: Distance): Weight = 1 / pow(dist, p)

    val distances: Iterable[(Distance, Temperature)] =
      temperatures map { x => (dist(x._1), x._2) }

    distances.find(_._1 == 0) map (_._2) getOrElse {
      val weights: Iterable[(Distance, Temperature)] =
        distances map { x => (weight(x._1), x._2) }

      weights.map(x => x._1 * x._2).sum / weights.map(_._1).sum
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def linearInterpolation(x0: (Temperature, Color), x1: (Temperature, Color)): Color = {
      val ((t0, Color(r0, g0, b0)), (t1, Color(r1, g1, b1))) = (x0, x1)
      val t = (value - t0) / (t1 - t0)

      def liChromaticity(y0: Int, y1: Int): Int =
        round(y0 + (y1 - y0) * t).toInt

      Color(liChromaticity(r0, r1), liChromaticity(g0, g1), liChromaticity(b0, b1))
    }

    points find(_._1 == value) map (_._2) getOrElse {
      val (lower, upper) = points.toSeq sortBy(_._1) partition(_._1 < value)
      (lower.lastOption, upper.headOption) match {
        case (None, None) => Color(0, 0, 0)
        case (Some((_, c)), None) => c
        case (None, Some((_, c))) => c
        case (Some(x0), Some(x1)) => linearInterpolation(x0, x1)
      }
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    implicit def toRGBColor(c: Color): RGBColor = RGBColor(c.red, c.green, c.blue)

    val width = 360
    val height = 180

    def pointToLocation(x: Int, y: Int): Location =
      Location(90 - y, -180 + x)

    val futurePixels: Seq[Future[Pixel]] = for {
      y <- 0 until height
      x <- 0 until width
      pixel = Future {
        interpolateColor(colors, predictTemperature(temperatures, pointToLocation(x, y)))
      } map (_.toPixel)
    } yield pixel

    val pixels: Seq[Pixel] =
      Await.result(Future.sequence(futurePixels), 10 minutes)

    Image(width, height, pixels.toArray)
  }
}

