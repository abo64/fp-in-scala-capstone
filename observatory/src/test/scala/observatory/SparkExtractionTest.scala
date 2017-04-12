package observatory

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import java.time.LocalDate

@RunWith(classOf[JUnitRunner])
class SparkExtractionTest extends FlatSpec {

  private def sparkExtraction(f: String => String): SparkExtraction =
    new SparkExtraction {
      override def lines(resource: String)(implicit spark: SparkSession): RDD[String] =
        spark.sparkContext.parallelize(f(resource) split("\n"))
    }

  behavior of "SparkExtraction"

  it should "work for the example data" in {
    val stations =
"""010013,,,
724017,03707,+37.358,-078.438
724017,,+37.350,-078.433"""

    val temperatures =
"""010013,,11,25,39.2
724017,,08,11,81.14
724017,03707,12,06,32
724017,03707,01,29,35.6"""

    val sparkExtract =
      sparkExtraction(Map("stations" -> stations, "temperatures" -> temperatures))

    val locTemperatures =
      Seq((LocalDate.of(2015, 8, 11), Location(37.35, -78.433d), 27.3d),
          (LocalDate.of(2015, 12, 6), Location(37.358, -78.438d), 0.0d),
          (LocalDate.of(2015, 1, 29), Location(37.358, -78.438d), 2.0d))

    val locatedTemperatures = sparkExtract.locateTemperatures(2015, "stations", "temperatures")
    assert(locatedTemperatures == locTemperatures)

    val locYearlyAvgs =
      Seq((Location(37.35, -78.433d), 27.3d),
          (Location(37.358, -78.438d), 1.0d))
    assert(sparkExtract.locationYearlyAverageRecords(locatedTemperatures) == locYearlyAvgs)
  }
}
