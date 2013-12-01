package scala.tools.refactoring
package implementations

import scala.tools.nsc.io.AbstractFile
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.common.TextChange
import common.InteractiveScalaCompiler
import scala.reflect.internal.Flags

abstract class AddVariable extends Refactoring with InteractiveScalaCompiler {

  val global: tools.nsc.interactive.Global
  import global._

  //TODO implement isVal
  def addVariable(file: AbstractFile, className: String, varName: String, isVar: Boolean, returnTypeOpt: Option[String], target: AddMethodTarget): List[TextChange] = {
    val astRoot = abstractFileToTree(file)

    val classOrObjectDef = target match {
      case AddToClosest(offset: Int) => {
        case class UnknownDef(tree: Tree, offset: Int)

        val classAndObjectDefs = astRoot.collect {
          case classDef: ClassDef if classDef.name.decode == className =>
            UnknownDef(classDef, classDef.namePosition.point)
          case moduleDef: ModuleDef if moduleDef.name.decode == className =>
            UnknownDef(moduleDef, moduleDef.namePosition.point)
        }

        //the class/object definition just before the given offset
        classAndObjectDefs.sortBy(_.offset).reverse.find(_.offset < offset).map(_.tree)
      }
      case _ => {
        astRoot.find {
          case classDef: ClassDef if target == AddToClass => classDef.name.decode == className
          case moduleDef: ModuleDef if target == AddToObject => moduleDef.name.decode == className
          case _ => false
        }
      }
    }

    addVariable(varName, isVar, returnTypeOpt, classOrObjectDef.get)
  }

  private def addVariable(varName: String, isVar: Boolean, returnTypeOpt: Option[String], classOrObjectDef: Tree): List[TextChange] = {

//    val returnStatement = Ident("???") :: Nil
//    val newValOrVar = mkValDef(varName, mkBlock(returnStatement), returnTypeOpt.map(name => TypeTree(newType(name))).getOrElse(TypeTree(newType("Any"))))
    
    val returnStatement = Ident("???")
//    val mutFlag = if (isVar) Flags.MUTABLE else 0L
    val newValOrVar = mkValDef(varName, returnStatement, returnTypeOpt.map(name => TypeTree(newType(name))).getOrElse(TypeTree(newType("Any")))) //.mods | mutFlag
   
    def addVariableToTemplate(tpl: Template) = tpl copy (body = tpl.body ::: newValOrVar :: Nil) replaces tpl

    val insertVariable = transform {
      case ClassDef(_, _, _, tpl) => addVariableToTemplate(tpl)
      case ModuleDef(_, _, tpl) => addVariableToTemplate(tpl)
    }

    refactor((insertVariable apply classOrObjectDef).toList)
  }

  private def newType(name: String) = new Type {
    override def safeToString: String = name
  }
}