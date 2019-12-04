package tip.analysis

import tip.ast._
import tip.cfg.CfgOps._
import tip.ast.AstOps._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData

import tip.solvers._
import tip.cfg._

/**
  * Base class for live variables analysis.
  */
abstract class LiveVarsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  import AstNodeData._

  val allVars: Set[ADeclaration] = cfg.nodes.flatMap(_.appearingIds)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allVars))

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunExitNode => lattice.sublattice.bottom
      case r: CfgStmtNode =>
        r.data match {
          case cond: AExpr => s union cond.appearingIds
          case ass: AAssignStmt =>
            ass.left match {
              case id: AIdentifier => (s - declData(id)) union ass.right.appearingIds
              case _ => ???
            }
          case varr: AVarStmt => s -- varr.appearingIds
          case ret: AReturnStmt => s union ret.appearingIds
          case out: AOutputStmt => s union out.appearingIds
          case _ => s
        }
      case _ => s
    }
}

/**
  * Live variables analysis that uses the simple fixpoint solver.
  */
class LiveVarsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with BackwardDependencies

/**
  * Live variables analysis that uses the worklist solver.
  */
class LiveVarsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends LiveVarsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with BackwardDependencies
