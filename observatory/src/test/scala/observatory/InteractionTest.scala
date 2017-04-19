package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("create image files for year 2015") {
    Interaction.generateTiles(Seq(Interaction.yearlyData(2015)), Interaction.generateImage)
  }
}
