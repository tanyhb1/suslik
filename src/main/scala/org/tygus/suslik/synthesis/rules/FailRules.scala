package org.tygus.suslik.synthesis.rules

import org.tygus.suslik.language.Expressions.{Expr, Var}
import org.tygus.suslik.language.IntType
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic.smt.SMTSolving
import org.tygus.suslik.logic._
import org.tygus.suslik.synthesis.Evaluator.Examples
import org.tygus.suslik.synthesis.Termination.Transition
import org.tygus.suslik.synthesis._
import org.tygus.suslik.synthesis.rules.Rules._

/**
  * Rules for short-circuiting failure;
  * do not affect completeness, they are simply an optimization.
  *
  * @author Nadia Polikarpova, Ilya Sergey
  */

object FailRules extends PureLogicUtils with SepLogicUtils with RuleUtils {

  val exceptionQualifier: String = "rule-fail"

  // Short-circuits failure if pure post is inconsistent with the pre
  object PostInconsistent extends SynthesisRule with InvertibleRule {
    override def toString: String = "PostInconsistent"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre.phi
      val post = goal.post.phi

      if (!SMTSolving.sat((pre && post).toExpr))
        // post inconsistent with pre
        List(RuleResult(List(goal.unsolvableChild), IdProducer, this, goal))
      else
        Nil
    }
  }

  // Short-circuits failure if universal part of post is too strong
  object CheckPost extends SynthesisRule with InvertibleRule {
    override def toString: String = "CheckPost"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def filterOutValidPost(goal: Goal, exPost: PFormula, uniPost: PFormula): Seq[RuleResult] = {
      val validExConjuncts = exPost.conjuncts.filter(c => SMTSolving.valid(goal.pre.phi ==> c))
      if (validExConjuncts.isEmpty && uniPost.conjuncts.isEmpty) Nil
      else {
        val newPost = Assertion(exPost - PFormula(validExConjuncts), goal.post.sigma)
        val newGoal = goal.spawnChild(post = newPost)
        List(RuleResult(List(newGoal), IdProducer, this, goal))
      }
    }

    def apply(goal: Goal): Seq[RuleResult] = {
      val (uniPost, exPost) = goal.splitPost
      // If precondition does not contain predicates, we can't get new facts from anywhere
      if (!SMTSolving.valid(goal.pre.phi ==> uniPost))
        // universal post not implied by pre
        List(RuleResult(List(goal.unsolvableChild), IdProducer, this, goal))
      else filterOutValidPost(goal, exPost, uniPost)
    }
  }

  object AbduceBranch extends SynthesisRule with GeneratesCode with InvertibleRule {
    override def toString: String = "AbduceBranch"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def atomCandidates(goal: Goal): Seq[Expr] =
      for {
        lhs <- goal.programVars.filter(goal.post.phi.vars.contains)
        rhs <- goal.programVars.filter(goal.post.phi.vars.contains)
        if lhs != rhs
        if goal.getType(lhs) == IntType && goal.getType(rhs) == IntType
      } yield lhs |<=| rhs

    def condCandidates(goal: Goal): Seq[Expr] = {
      val atoms = atomCandidates(goal)
      // Toggle this to enable abduction of conjunctions
      // (without branch pruning, produces too many branches)
//      atoms
      for {
        subset <- atoms.toSet.subsets.toSeq.sortBy(_.size)
        if subset.nonEmpty && subset.size <= goal.env.config.maxGuardConjuncts
      } yield PFormula(subset).toExpr
    }

    /**
      * Find the earliest ancestor of goal
      * that is still valid and has all variables from vars
      */
    def findBranchPoint(vars: Set[Var], goal: Goal): Option[Goal] = {
      def valid(g: Goal) = SMTSolving.valid(g.pre.phi ==> g.universalPost)

      goal.parent match {
        case None => Some(goal).filter(valid) // goal is root: return itself if valid
        case Some(pGoal) =>
          if (vars.subsetOf(pGoal.programVars.toSet)) {
            // Parent goal has all variables from vars: recurse
            findBranchPoint(vars, pGoal)
          } else Some(goal).filter(valid) // one of vars undefined in the goal: return itself if valid
      }
    }

    def guardedCandidates(goal: Goal): Seq[RuleResult] =
      for {
        cond <- condCandidates(goal)
        pre = goal.pre.phi
        if SMTSolving.valid((pre && cond) ==> goal.universalPost)
        if SMTSolving.sat((pre && cond).toExpr)
        bGoal <- findBranchPoint(cond.vars, goal)
        thenGoal = goal.spawnChild(goal.pre.copy(phi = goal.pre.phi && cond), childId = Some(0))
        elseGoal = bGoal.spawnChild(
          pre = bGoal.pre.copy(phi = bGoal.pre.phi && cond.not),
          childId = Some(1))
        thenTransition = Transition(goal, thenGoal)
        elseTransition = Transition(bGoal, elseGoal)
      } yield RuleResult(List(thenGoal, elseGoal), GuardedProducer(cond, bGoal), this, List(thenTransition, elseTransition))

    def apply(goal: Goal): Seq[RuleResult] = {
      val (uniPost, exPost) = goal.splitPost
      if (SMTSolving.valid(goal.pre.phi ==> uniPost))
        CheckPost.filterOutValidPost(goal, exPost, uniPost)
      else {
        val guarded = guardedCandidates(goal)
        if (guarded.isEmpty)
          // Abduction failed
          List(RuleResult(List(goal.unsolvableChild), IdProducer, this, goal)) // pre doesn't imply post: goal is unsolvable
        else guarded.take(1) // TODO: try several incomparable conditions, but filter out subsumed ones?
      }
    }
  }


  // Short-circuits failure if spatial post doesn't match pre
  // This rule is only applicable when only points-to heaplets are left
  object HeapUnreachable extends SynthesisRule with InvertibleRule {
    override def toString: String = "HeapUnreachable"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      assert(!(goal.hasPredicates() || goal.hasBlocks))
      if ((goal.pre.sigma.profile == goal.post.sigma.profile) && // profiles must match
        goal.post.sigma.chunks.forall { case pts@PointsTo(v@Var(_), _, _) => goal.isExistential(v) || // each post heaplet is either existential pointer
          findHeaplet(sameLhs(pts), goal.pre.sigma).isDefined
        }) // or has a heaplet in pre with the same LHS
        Nil
      else
        List(RuleResult(List(goal.unsolvableChild), IdProducer, this, goal)) // spatial parts do not match: only magic can save us
    }
  }

}
