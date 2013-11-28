package scala.tools.refactoring
package implementations

import common.Change
import scala.tools.refactoring.common.PimpedTrees
import scala.tools.refactoring.transformation.TreeFactory
import scala.sprinter.printers.TypePrinters

/**
 * Refactoring that implements the ProductN trait for a class.
 * Given N selected class parameters this refactoring generates
 * the methods needed to implement the ProductN trait. This includes
 * implementations for hashCode and equals.
 * @see GenerateHashcodeAndEquals
 */
abstract class IntroduceProductNTrait extends GenerateHashcodeAndEquals {

  import global._


  override def sourceGeneration(selectedParams: List[ValDef], preparationResult: PreparationResult, refactoringParams: RefactoringParameters) = {
    val superGeneration = super.sourceGeneration(selectedParams, preparationResult, refactoringParams)

    val projections = {
      def makeElemProjection(elem: ValDef, pos: Int) = {
        val body = List(Ident(elem.name))
        mkDefDef(name = "_" + pos, body = body)
      }

      selectedParams.zipWithIndex.map(t => makeElemProjection(t._1, t._2 + 1))
    }

    def addProductTrait = transform ({
      case t @ Template(_, _, body) => t.copy(body = projections:::body) replaces t
    })

    superGeneration &> addProductTrait
  }

  override def newParentNames(classDef: ClassDef, selectedParams: List[ValDef]) = {
    val typePrinter = TypePrinters(global)
    val response = global.askForResponse { () =>
      val typePrinterContext = global.doLocateContext(global.rangePos(classDef.pos.source, classDef.pos.start, classDef.pos.start, classDef.pos.start))
      selectedParams.map(v => typePrinter.showType(v.tpt, typePrinterContext)) //v.tpt.nameString
    }
    val paramsTypenames = response.get.left.toOption flatMap {
          case list: List[String] => Option(list)
          case _ => None
        } getOrElse (selectedParams.map(v => v.tpt.nameString))
    val arity = selectedParams.length
    val productParent = "Product" + arity + "[" + paramsTypenames.mkString(", ") + "]"
    productParent::Nil
  }

}