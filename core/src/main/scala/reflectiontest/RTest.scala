package reflectiontest

object RTest {

  def main(args: Array[String]): Unit = {
    val tpe = TypeHolder.giveType(scala.reflect.runtime.universe)
  }

}