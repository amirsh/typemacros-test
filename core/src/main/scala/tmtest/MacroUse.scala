package tmtest

import language.experimental.macros

object MacroUse {
  type TypeMacro(typeName: String = "String") = macro Macros.typeMacro
}