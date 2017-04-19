import com.sksamuel.scrimage.RGBColor
import scala.language.implicitConversions
import java.text.SimpleDateFormat
import java.util.Date

package object observatory {

  type Distance = Double
  type Temperature = Double
  type Weight = Double
  type Year = Int
  type Zoom = Int

  implicit def toRGBColor(c: Color): RGBColor = RGBColor(c.red, c.green, c.blue)

  val ColorPalette = List(
    (60.0, Color(255, 255, 255)),
    (32.0, Color(255, 0, 0)),
    (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)),
    (-15.0, Color(0, 0, 255)),
    (-27.0, Color(255, 0, 255)),
    (-50.0, Color(33, 0, 107)),
    (-60.0, Color(0, 0, 0)))

  def time[R](message: String)(block: => R): R = {
    println(message + " ...")
    val t0 = System.currentTimeMillis
    val result = block
    val t1 = System.currentTimeMillis
    println(message + " completed in " + formatter.format(new Date(t1 - t0)))
    result
  }

  private val formatter = new SimpleDateFormat("HH:mm:ss")
}
