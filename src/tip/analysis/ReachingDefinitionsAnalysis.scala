package tip.analysis

import tip.ast._
import tip.lattices._
import tip.ast.AstNodeData.DeclarationData
import tip.solvers._
import tip.cfg._

import scala.collection.immutable.Set
import tip.ast.AstOps.AstOp

abstract class ReachingDefinitionsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {
  val lattice: MapLattice[CfgNode, PowersetLattice[AStmt]] = new MapLattice(new PowersetLattice())
  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def removedefs(s: lattice.sublattice.Element, id: AIdentifier): lattice.sublattice.Element = s.filter(stm =>
    stm match {
      case as: AAssignStmt => as.left match {
        case id2: AIdentifier => id2.name != id.name
        case _ => ???
      }
      case varr: AVarStmt => !varr.declIds.exists(id2 => id2.name == id.name)
      case _ => ???
    }
  )

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case as: AAssignStmt =>
            as.left match {
              case id: AIdentifier => removedefs(s, id) + as
              case _ => ???
            }
          case varr: AVarStmt => s + varr
          case _ => s
        }
      case _ => s
    }
}

class ReachingDefinitionsAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

class ReachingDefinitionsAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies
