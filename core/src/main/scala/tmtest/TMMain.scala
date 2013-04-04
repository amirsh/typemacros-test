package tmtest

object TMMain extends App {
  object TMTest extends MacroUse.TypeMacro("Int")
  println(TMTest.j)
}