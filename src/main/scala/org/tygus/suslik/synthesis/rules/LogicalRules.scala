package org.tygus.suslik.synthesis.rules

import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.language.Ident
import org.tygus.suslik.language.Statements._
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic._
import org.tygus.suslik.logic.smt.SMTSolving
import org.tygus.suslik.synthesis.Evaluator.Examples
import org.tygus.suslik.synthesis._
import org.tygus.suslik.synthesis.rules.Rules._

/**
  * Logical rules simplify specs and terminate the derivation;
  * they do not eliminate existentials.
  * @author Nadia Polikarpova, Ilya Sergey
  */

object LogicalRules extends PureLogicUtils with SepLogicUtils with RuleUtils {

  val exceptionQualifier: String = "rule-logical"

  /*

    -------------------------------- [emp]
    Γ ; {φ ; emp} ; {emp} ---> skip

    Axiom: heaps are empty and pure spec is valid -> emit skip

  */
  object EmpRule extends SynthesisRule with InvertibleRule {

    override def toString: Ident = "Emp"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre
      val post = goal.post

      if (pre.sigma.isEmp && post.sigma.isEmp && // heaps are empty
        goal.existentials.isEmpty &&             // no existentials
        SMTSolving.valid(pre.phi ==> post.phi))  // pre implies post
        List(RuleResult(Nil, ConstProducer(Skip), this, goal))      // we are done
      else Nil
    }
  }

  /*
  --------------------------------------- [inconsistency]
  Γ ; {φ ∧ l ≠ l ; P} ; {ψ ; Q} ---> emp

  The other axiom: pre is inconsistent -> emit error
  */
  object Inconsistency extends SynthesisRule with InvertibleRule {
    override def toString: String = "Inconsistency"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre.phi.toExpr

      if (!SMTSolving.sat(pre))
        List(RuleResult(Nil, ConstProducer(Error), this, goal)) // pre inconsistent: return error
      else
        Nil
    }
  }

  /*
   Remove conjuncts from the pure pre if their variables do not occur anywhere else
  */
  object WeakenPre extends SynthesisRule with InvertibleRule {
    override def toString: String = "WeakenPre"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val unused = goal.pre.phi.indepedentOf(goal.pre.sigma.vars ++ goal.post.vars)
      if (unused.conjuncts.isEmpty) Nil
      else {
        val newPre = Assertion(goal.pre.phi - unused, goal.pre.sigma)
        val newGoal = goal.spawnChild(pre = newPre)
        val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)
        List(RuleResult(List(newGoal), kont, this, goal))
      }
    }
  }

  object SimplifyConditional extends SynthesisRule with InvertibleRule {
    override def toString: String = "Simplify"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)
      goal.post.sigma.chunks.find {
        case h@PointsTo(_, _, IfThenElse(_, _, _)) => h.vars.forall(v => !goal.isGhost(v))
        case _ => false
      } match {
        case None => Nil
        case Some(h@PointsTo(l, o, IfThenElse(c, t, e))) => {
          if (SMTSolving.valid(goal.pre.phi ==> (c || (t |=| e)))) {
            val thenSigma = (goal.post.sigma - h) ** PointsTo(l, o, t)
            val thenPhi = goal.post.phi // && c
            val thenGoal = goal.spawnChild(post = Assertion(thenPhi, thenSigma))
            List(RuleResult(List(thenGoal), kont, this, goal))
          } else if (SMTSolving.valid(goal.pre.phi ==> (c.not || (t |=| e)))) {
            val elseSigma = (goal.post.sigma - h) ** PointsTo(l, o, e)
            val elsePhi = goal.post.phi // && c.not
            val elseGoal = goal.spawnChild(post = Assertion(elsePhi, elseSigma))
            List(RuleResult(List(elseGoal), kont, this, goal))
          } else Nil
        }
      }
    }
  }

  /*
   Remove an equivalent heaplet from pre and post
   */
  abstract class Frame extends SynthesisRule {
    def heapletFilter(h: Heaplet): Boolean

    // Do we have a chance to get rid of the relevant kind of heaplets by only unification and framing?
    def profilesMatch(pre: SFormula, post: SFormula, exact: Boolean): Boolean

    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre
      val post = goal.post
      if (!profilesMatch(pre.sigma, post.sigma, goal.callGoal.isEmpty)) return Nil

      def isMatch(hPre: Heaplet, hPost: Heaplet): Boolean = hPre.eqModTags(hPost) && heapletFilter(hPost)

      findMatchingHeaplets(_ => true, isMatch, pre.sigma, post.sigma) match {
        case None => Nil
        case Some((hPre, hPost)) => {
          val newPreSigma = pre.sigma - hPre
          val newPostSigma = post.sigma - hPost
          val newPre = Assertion(pre.phi, newPreSigma)
          val newPost = Assertion(post.phi, newPostSigma)
          val newGoal = goal.spawnChild(newPre, newPost)
          val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)
          List(RuleResult(List(newGoal), kont, this, goal))
        }
      }
    }
  }

  object FrameUnfolding extends Frame with UnfoldingPhase {
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    override def toString: String = "FrameUnfold"
  }

  object FrameUnfoldingFinal extends Frame with UnfoldingPhase with InvertibleRule {
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    override def toString: String = "FrameUnfold"
  }

  object FrameBlock extends Frame with BlockPhase with InvertibleRule {
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    override def toString: String = "FrameBlock"
  }

  object FrameFlat extends Frame with FlatPhase with InvertibleRule {
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    override def toString: String = "FrameFlat"
  }


  /*
  x ≠ nil ∉ φ
  Γ ; {φ ∧ x ≠ nil ; x.f -> l * P} ; {ψ ; Q} ---> S
  -------------------------------------------------- [nil-not-lval]
  Γ ; {φ ; x.f -> l * P} ; {ψ ; Q} ---> S
  */

  object NilNotLval extends SynthesisRule with InvertibleRule {
    override def toString: String = "NilNotLval"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      if (goal.pre.phi == pFalse) return Nil

      // Find pointers in `a` that are not yet known to be non-null
      def findPointers(p: PFormula, s: SFormula): Set[Expr] = {
        // All pointers
        val allPointers = (for (PointsTo(l, _, _) <- s.chunks) yield l).toSet
        allPointers.filter(
          x => !p.conjuncts.contains(x |/=| NilPtr) && !p.conjuncts.contains(NilPtr |/=| x)
        )
      }

      def addToAssertion(a: Assertion, ptrs: Set[Expr]): Assertion = {
        Assertion(a.phi && PFormula(ptrs.map { x => x |/=| NilPtr }), a.sigma)
      }

      val pre = goal.pre
      val post = goal.post

      val prePointers = findPointers(pre.phi, pre.sigma)
      val postPointers = findPointers(pre.phi && post.phi, post.sigma)

      if (prePointers.isEmpty && postPointers.isEmpty)
        Nil // no pointers to insert
      else {
        val newPre = addToAssertion(pre, prePointers)
        val newPost = addToAssertion(post, postPointers)
        val newGoal = goal.spawnChild(newPre, newPost)
        val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)
        List(RuleResult(List(newGoal), kont, this, goal))
      }
    }
  }

  /*
  x ≠ y ∉ φ
  Γ ; {φ ∧ x ≠ y ; x.f -> l * y.f -> l' * P} ; {ψ ; Q} ---> S
  ------------------------------------------------------------ [*-partial]
  Γ ; {φ ; x.f -> l * y.f -> l' * P} ; {ψ ; Q} ---> S
   */
  object StarPartial extends SynthesisRule with InvertibleRule {
    override def toString: String = "*Partial"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def extendPure(p: PFormula, s: SFormula): PFormula = {
      val ptrs = (for (PointsTo(x, o, _) <- s.chunks) yield (o, x)).groupBy(_._1).mapValues(_.map(_._2))
      // All pairs of pointers
      val pairs = for (o <- ptrs.keySet; x <- ptrs(o); y <- ptrs(o) if x != y) yield (x, y)
      val newPairs = pairs.filter {
        case (x, y) => !p.conjuncts.contains(x |/=| y) && !p.conjuncts.contains(y |/=| x)
      }
      PFormula(newPairs.map { case (x, y) => x |/=| y })
    }

    def apply(goal: Goal): Seq[RuleResult] = {
      if (goal.pre.phi == pFalse) return Nil
      val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)

      val newPrePhi = extendPure(goal.pre.phi, goal.pre.sigma)
      val newPostPhi = extendPure(goal.pre.phi && goal.post.phi, goal.post.sigma)

      if (newPrePhi.conjuncts.isEmpty && newPostPhi.conjuncts.isEmpty) return Nil
      val newPre = goal.pre.copy(phi = goal.pre.phi && newPrePhi)
      val newPost = goal.post.copy(phi = goal.post.phi && newPostPhi)
      val newGoal = goal.spawnChild(newPre, newPost)
      List(RuleResult(List(newGoal), kont, this, goal))
    }
  }


  /*
  Γ ; {[l/x]φ ; [l/x]P} ; {[l/x]ψ ; [l/x]Q} ---> S
  ------------------------------------------------ [subst-L]
  Γ ; {φ ∧ x = l ; P} ; {ψ ; Q} ---> S
  */
  object SubstLeft extends SynthesisRule with InvertibleRule {
    override def toString: String = "SubstL"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val p1 = goal.pre.phi
      val s1 = goal.pre.sigma

      // Should only substitute for a ghost
      def isGhostVar(e: Expr): Boolean = e.isInstanceOf[Var] && goal.universalGhosts.contains(e.asInstanceOf[Var])

      findConjunctAndRest({
        case BinaryExpr(OpEq, l, r) => (isGhostVar(l) || isGhostVar(r)) && l.vars.intersect(r.vars).isEmpty
        case BinaryExpr(OpBoolEq, l, r) => (isGhostVar(l) || isGhostVar(r)) && l.vars.intersect(r.vars).isEmpty
        case BinaryExpr(OpSetEq, l, r) => (isGhostVar(l) || isGhostVar(r)) && l.vars.intersect(r.vars).isEmpty
        case _ => false
      }, p1) match {
        case Some((BinaryExpr(_, l, r), rest1)) => {
          val (x, e) = if (isGhostVar(l)) {
            (l.asInstanceOf[Var], r)
          } else {
            (r.asInstanceOf[Var], l)
          }
          val _p1 = rest1.subst(x, e)
          val _s1 = s1.subst(x, e)
          val newGoal = goal.spawnChild(Assertion(_p1, _s1), goal.post.subst(x, e))
          val kont = IdProducer >> HandleGuard(goal) >> ExtractHelper(goal)
          assert(goal.callGoal.isEmpty)
          List(RuleResult(List(newGoal), kont, this, goal))
        }
        case _ => Nil
      }
    }
  }
}
