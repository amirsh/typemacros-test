package tmtest

object TMMain extends App {
//  object TMTest extends MacroUse.TypeMacro("Int")
  object TMTest extends MacroUse.TypeMacro("Tuple2[Int,String]")
  println(TMTest.j)
}