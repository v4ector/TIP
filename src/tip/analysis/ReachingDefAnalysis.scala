package tip.analysis

import tip.ast._
import tip.ast.AstNodeData.DeclarationData
import tip.cfg._
import tip.lattices._
import tip.solvers._


/**
  * Base class for the reaching definitions analysis
  */
abstract class ReachingDefAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis[CfgNode](cfg) {

  import tip.cfg.CfgOps._
  import tip.ast.AstOps._

  val allAssgnmnts: Set[AAssignStmt] = cfg.nodes.flatMap(_.appearingAssignments)

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  val lattice = new MapLattice(cfg.nodes, new PowersetLattice(allAssgnmnts))

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case _: CfgFunEntryNode => Set()
      case r: CfgStmtNode =>
        r.data match{
          case ass: AAssignStmt =>
            ass.left match{
              case id: AIdentifier =>
                (s.filter { e => 
                  !(id.appearingIds subsetOf e.n.appearingIds)
                 }) + ass
              case _ => ???
            }
          case _ => s
        }
      case _ => s
    }
}

/**
  * Reaching definitions analysis that uses the simple fixpoint solver.
  */
class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

/**
  * Reaching definitions analysis that uses the worklist solver.
  */
class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
