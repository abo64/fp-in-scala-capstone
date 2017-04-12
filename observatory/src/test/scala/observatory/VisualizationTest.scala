package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {

  val year = 2015
  val stationsPath: String = s"/stations.csv"
  val temperaturePath: String = s"/$year.csv"
  lazy val locateTemperatures = Extraction.locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locateAverage = Extraction.locationYearlyAverageRecords(locateTemperatures)

  test("visualize") {
    val palette = List(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0)))

    val img = Visualization.visualize(locateAverage, palette)

    val outputFile = new java.io.File(s"$year-out.png")
    img.output(outputFile)
    println(s"outputFile: ${outputFile.getAbsolutePath}")

    assert(img.pixels.length === 360 * 180)
  }
}
