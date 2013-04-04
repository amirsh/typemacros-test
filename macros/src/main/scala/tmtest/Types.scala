package tmtest

object Types {
  import scala.reflect.runtime.universe._

  def getType(tpeName: String): Type =
    tpeName match {
      case "Int" => typeOf[Int]
      case "String" => typeOf[String]
      case _ => typeOf[Double]
    }
}