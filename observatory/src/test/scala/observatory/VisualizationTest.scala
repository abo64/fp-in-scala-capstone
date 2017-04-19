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
    val img = Visualization.visualize(locateAverage, ColorPalette)

    val outputFile = new java.io.File(s"$year-out.png")
    img.output(outputFile)
    println(s"outputFile: ${outputFile.getAbsolutePath}")

    assert(img.pixels.length === 360 * 180)
  }
}
