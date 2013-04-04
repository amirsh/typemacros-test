package tmtest

import scala.reflect.macros.Context

object Macros {
  def typeMacro(ctx: Context)(typeName: ctx.Expr[String]) = {
    import ctx.universe._

    val Expr(Literal(Constant(tpeName: String))) = typeName

    val tpe = Types.getType(tpeName).asInstanceOf[Type]
    
    def generateCode(): List[Tree] = {
      val Expr(Block(List(ValDef(mods, lhs, tpt, rhs)), Literal(Constant(())))) = reify {
        val j: Int = 4
      }
      val valDef = ValDef(mods, lhs, tpt, rhs)
//      val valDef = ValDef(mods, lhs, TypeTree(tpe.asInstanceOf[Type]), rhs)
      val typeDef = TypeDef(NoMods, TypeName("MyInt"), List(), TypeTree(tpe))
      List(valDef, typeDef)
    }

    val completeExpr = reify {
      class CONTAINER {
        def a(i: Int): Int = i * 2
        // generated code will be spliced here
      }
    }
    val Expr(Block(List(ClassDef(_, containerType, _, Template(parents, self, body))), _)) = completeExpr

    val packageName = ctx.enclosingPackage.pid.toString
    val className = ctx.freshName(ctx.enclosingImpl.name).toTypeName

    ctx.introduceTopLevel(packageName, ClassDef(NoMods, className, Nil, Template(parents, self, body ++ generateCode())))
  }
}