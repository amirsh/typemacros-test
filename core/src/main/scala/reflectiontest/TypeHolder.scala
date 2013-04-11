package reflectiontest

import scala.reflect.api.Universe

object TypeHolder {
  trait TypeA

  def giveType(universe: Universe): universe.Type =
    universe.typeOf[TypeA]
}