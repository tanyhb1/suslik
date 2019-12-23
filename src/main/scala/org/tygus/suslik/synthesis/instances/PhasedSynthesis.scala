package org.tygus.suslik.synthesis.instances

import org.tygus.suslik.logic.Specifications.Goal
import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.logic.smt.SMTSolving.sat
import org.tygus.suslik.synthesis._
import org.tygus.suslik.synthesis.rules.Rules.SynthesisRule
import org.tygus.suslik.synthesis.rules._
import org.tygus.suslik.util.SynLogging

class PhasedSynthesis(implicit val log: SynLogging) extends Synthesis {

  {
    // Warm-up the SMT solver on start-up to avoid future delays
    for (i <- 1 to 5; j <- 1 to 2; k <- 1 to 3) {
      sat(BinaryExpr(OpLeq, IntConst(i), BinaryExpr(OpPlus, IntConst(j), IntConst(k))))
    }
  }

  def nextRules(goal: Goal, depth: Int): List[SynthesisRule] = {
    val config = goal.env.config
    if (!config.phased)
    // Phase distinction is disabled: use all non top-level rules
      anyPhaseRules(config) ++ unfoldingPhaseRules(config) ++
        blockPhaseRules(config) ++ pointerPhaseRules(config) ++ purePhaseRules(config)
    else if (goal.hasPredicates)
      // Unfolding phase: get rid of predicates
      anyPhaseRules(config) ++ unfoldingPhaseRules(config)
    else if (goal.hasBlocks)
      // Block phase: get rid of blocks
      anyPhaseRules(config) ++ blockPhaseRules(config)
    else if (goal.hasExistentialPointers)
      // Pointer phase: match all existential pointers
      anyPhaseRules(config) ++ pointerPhaseRules(config)
    else
      // Pure phase: get rid of all the heap
      anyPhaseRules(config) ++ purePhaseRules(config)
  }


  def anyPhaseRules(config: SynConfig):  List[SynthesisRule] = List(
    LogicalRules.StarPartial,
    LogicalRules.NilNotLval,
    LogicalRules.Inconsistency,
    if (!config.fail) FailRules.Noop else FailRules.PostInconsistent,
//    LogicalRules.SubstLeftVar,
    OperationalRules.ReadRule,
  )

  def unfoldingPhaseRules(config: SynConfig):  List[SynthesisRule] = List(
    LogicalRules.SubstLeftVar,
//    LogicalRules.SubstRightVar,
    LogicalRules.FrameUnfolding,
    UnfoldingRules.CallRule,
    UnfoldingRules.Open,
    UnificationRules.HeapUnifyUnfolding,
    UnfoldingRules.AbduceCall,
    UnfoldingRules.Close,
  )

  def blockPhaseRules(config: SynConfig): List[SynthesisRule] = List(
    if (config.branchAbduction) FailRules.AbduceBranch else if (!config.fail) FailRules.Noop else FailRules.PostInvalid,
    LogicalRules.FrameBlock,
    UnificationRules.HeapUnifyBlock,
    OperationalRules.AllocRule,
    OperationalRules.FreeRule
  )

  def pointerPhaseRules(config: SynConfig): List[SynthesisRule] = List(
    if (config.branchAbduction) FailRules.AbduceBranch else if (!config.fail) FailRules.Noop else FailRules.PostInvalid,
    if (!config.fail) FailRules.Noop else FailRules.HeapUnreachable,
    LogicalRules.SubstLeft,
    UnificationRules.SubstRight,
    LogicalRules.FrameFlat,
    OperationalRules.WriteRuleOld,
    UnificationRules.HeapUnifyPointer,
  )

  def purePhaseRules(config: SynConfig): List[SynthesisRule] = List(
    if (config.branchAbduction) FailRules.AbduceBranch else if (!config.fail) FailRules.Noop else FailRules.PostInvalid,
    LogicalRules.EmpRule,
    if (!config.fail) FailRules.Noop else FailRules.HeapUnreachable,
    LogicalRules.SubstLeft,
    UnificationRules.SubstRight,
    LogicalRules.FrameFlat,
    OperationalRules.WriteRuleOld,
    //    UnificationRules.PureUnify,
    UnificationRules.HeapUnifyPure,
    UnificationRules.Pick,
  )

}
