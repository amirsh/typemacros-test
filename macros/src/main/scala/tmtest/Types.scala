package tmtest

import scala.reflect.api.Universe

object Types {
  import scala.reflect.runtime.universe._

  def getType(tpeName: String): Type =
    tpeName match {
      case "Int" => typeOf[Int]
      case "String" => typeOf[String]
      case "Tuple2[Int,String]" => typeOf[Tuple2[Int, String]]
      case _ => typeOf[Double]
    }

  def getTypeOfUniverse(universe: Universe)(tpeName: String): universe.Type =
    tpeName match {
      case "Int" => universe.typeOf[Int]
      case "String" => universe.typeOf[String]
      case "Tuple2[Int,String]" => universe.typeOf[Tuple2[Int, String]]
      case _ => universe.typeOf[Double]
    }

}