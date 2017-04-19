package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {

  test("create a file") {
   for {
     year <- Seq(2015) //1975 to 2015
     zoom <- Seq(0) //0 to 3
   } Interaction.generateTiles(Seq(Interaction.yearlyData(year)), Interaction.generateImage)
  }
}
