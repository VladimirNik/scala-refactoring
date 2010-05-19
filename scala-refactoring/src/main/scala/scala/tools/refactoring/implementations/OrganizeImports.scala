/*
 * Copyright 2005-2010 LAMP/EPFL
 */
// $Id$

package scala.tools.refactoring
package implementations

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.interactive.Global
import analysis.FullIndexes
import common.Change
import sourcegen.Transformations

abstract class OrganizeImports extends MultiStageRefactoring {
  
  import global._
  import Transformations._
  
  class PreparationResult
  
  class RefactoringParameters
  
  def prepare(s: Selection): Either[PreparationError, PreparationResult] = Right(new PreparationResult)
    
  def perform(selection: Selection, prepared: PreparationResult, params: RefactoringParameters): Either[RefactoringError, List[Tree]] = {
    
    val organizeImports = Transformations.transform[Tree, Tree] {
       case p @ PackageDef(_, stats) =>
        
          p copy (stats = stats partition {
              case _: Import => true
              case _ => false
            } match {
              case (imports, others) => 
              
                val sortImports: List[Tree] => List[Tree] = _.sortBy {
                  case t: Import => t.expr.toString
                }
                  
                val collapseImports: List[Tree] => List[Tree] = _.foldRight(Nil: List[Import]) { 
                  case (imp: Import, x :: xs) if imp.expr.toString == x.expr.toString => 
                    x.copy(selectors = x.selectors ::: imp.selectors).setPos(x.pos) :: xs
                  case (imp: Import, xs) => 
                    imp :: xs
                }
                
                val simplifyWildcards: List[Tree] => List[Tree] = {
                  def importsAll(i: ImportSelector) = i.name == nme.WILDCARD
                  def renames(i: ImportSelector) = i.name != i.rename
                  
                  _ map {
                    case imp @ Import(_, selectors) if selectors.exists(importsAll) && !selectors.exists(renames) => 
                      imp.copy(selectors = selectors.filter(importsAll)).setPos(imp.pos)
                    case imp =>
                      imp
                  }
                }
                ((sortImports andThen collapseImports andThen simplifyWildcards) apply imports) ::: others
            }
          ) setPos p.pos
    }
    
    val changes = organizeImports apply abstractFileToTree(selection.file)

    Right(changes toList)
  }
}