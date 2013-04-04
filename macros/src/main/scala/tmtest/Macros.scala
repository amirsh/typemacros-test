package tmtest

import scala.reflect.macros.Context

object Macros {
  def typeMacro(ctx: Context)(typeName: ctx.Expr[String]) = {
    import ctx.universe._

    val STRATEGY = 2

    val Expr(Literal(Constant(tpeName: String))) = typeName

    //    val tpe = Types.getType(tpeName).asInstanceOf[Type]
    val tpe = Types.getTypeOfUniverse(ctx.universe)(tpeName).asInstanceOf[Type]

    def treeOfType2(tpe: Type): Tree = {
      tpe.asInstanceOf[TypeRef].args match {
        case Nil => Ident(tpe.typeSymbol)
        case args => AppliedTypeTree(Ident(tpe.typeSymbol), args.map(t => treeOfType2(t)))
      }
      /*
      println(tpe.asInstanceOf[TypeRef].args)
      if (tpe.takesTypeArgs) {
        println("takes type args")
        println(tpe.asInstanceOf[TypeRef].args.map(t => treeOfType2(t)))
        println(AppliedTypeTree(Ident(tpe.typeSymbol), tpe.asInstanceOf[TypeRef].args.map(t => treeOfType2(t))))
        AppliedTypeTree(Ident(tpe.typeSymbol), tpe.asInstanceOf[TypeRef].args.map(t => treeOfType2(t)))
      }
      else {
        println("doesn't takes type args")
        Ident(tpe.typeSymbol)
      }
      */
    }

    def treeOfType(tpe: Type): Tree =
      if (STRATEGY == 1)
        // STRATEGY 1
        TypeTree(tpe)
      else
        // STRATEGY 2
        treeOfType2(tpe)

    // some general methods
    def createObjectOrClass(generator: String => Name)(name: String, packages: scala.List[String]): Tree = {
      val classType = generator(name)
      val packagesType = packages map (TermName(_))
      if (packagesType.isEmpty)
        Ident(classType)
      else {
        val firstPackage = packages.head match {
          case "_root_" => Ident(nme.ROOTPKG)
          case _ => Ident(packagesType.head)
        }
        val others = (packagesType.tail :+ classType)
        others.foldLeft[Tree](firstPackage)((prev, elem) => {
          Select(prev, elem)
        })
      }
    }

    def scalaProductClass: Tree = createClass("Product", List("scala"))
    def scalaSerializableClass: Tree = createClass("Serializable", List("scala"))

    def createClass(className: String, packages: scala.List[String]): Tree =
      createObjectOrClass(x => TypeName(x))(className, packages)

    object CaseClassCreator {
      import Flag._

      val CASEACCESSOR = scala.reflect.internal.Flags.CASEACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]
      val PARAMACCESSOR = scala.reflect.internal.Flags.PARAMACCESSOR.asInstanceOf[Long].asInstanceOf[FlagSet]

      def fieldCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
      def ctorParamCreate(name: String, tpt: Tree): ValDef = ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName(name), tpt, EmptyTree)
      def ctorCreate(ctorParams: List[ValDef]): DefDef = DefDef(NoMods, nme.CONSTRUCTOR, List(), List(ctorParams), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
      def caseClassCreate(name: String, fields: List[ValDef], ctor: DefDef): ClassDef =
        ClassDef(Modifiers(CASE), TypeName(name), Nil, Template(
          List(scalaProductClass, scalaSerializableClass),
          emptyValDef,
          fields :+ ctor))
    }

    def caseClassGenerate(): ClassDef = {
      import CaseClassCreator._
      val schema = List(("field1", treeOfType(tpe)))
      val caseClassName = "MyCaseClass"
      val fields: List[ValDef] = schema map { case (name, tpe) => fieldCreate(name, tpe) }
      val ctorParams: List[ValDef] = schema map { case (name, tpe) => ctorParamCreate(name, tpe) }
      val ctor: DefDef = ctorCreate(ctorParams)

      caseClassCreate(caseClassName, fields, ctor)
    }

    def generateCode(): List[Tree] = {
      val Expr(Block(List(ValDef(mods, lhs, tpt, rhs)), Literal(Constant(())))) = reify {
        val j: Int = 4
      }
      val valDef = ValDef(mods, lhs, tpt, rhs)
      //      val valDef = ValDef(mods, lhs, TypeTree(tpe.asInstanceOf[Type]), rhs)
      //      val typeDef = TypeDef(NoMods, TypeName("MyType"), List(), Ident(tpe.typeSymbol))
      val typeDef = TypeDef(NoMods, TypeName("MyType"), List(), treeOfType(tpe))
      //      val typeDef = TypeDef(NoMods, TypeName("MyInt"), List(), TypeTree(definitions.IntTpe))
      val caseClassDef = caseClassGenerate()
      List(valDef, typeDef, caseClassDef)
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