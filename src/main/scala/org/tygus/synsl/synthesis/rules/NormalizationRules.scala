package org.tygus.synsl.synthesis.rules

import org.tygus.synsl.language.Expressions._
import org.tygus.synsl.language.Statements.Skip
import org.tygus.synsl.logic._
import org.tygus.synsl.synthesis._

import scala.collection.Set

/**
  * @author Ilya Sergey
  */

object NormalizationRules extends PureLogicUtils with SepLogicUtils with RuleUtils {

  val exceptionQualifier: String = "rule-normalization"

  /*
  x ≠ nil ∉ φ
  Γ ; {φ ∧ x ≠ nil ; x.f -> l * P} ; {ψ ; Q} ---> S
  -------------------------------------------------- [nil-not-lval]
  Γ ; {φ ; x.f -> l * P} ; {ψ ; Q} ---> S
  */

  object NilNotLval extends SynthesisRule {
    override def toString: String = "[Norm: nil-not-lval]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      val Spec(Assertion(p1, s1), post, g) = spec
      conjuncts(p1) match {
        case None => SynFail
        case Some(cs) =>
          // All pointers
          val ptrs = (for (PointsTo(x, _, _) <- s1.chunks) yield x).toSet.filter(
            x => !cs.contains(PNeg(PEq(x, NilPtr))) && !cs.contains(PNeg(PEq(NilPtr, x)))
          )
          if (ptrs.isEmpty) return SynFail
          // The implementation immediately adds _all_ inequalities
          val _p1 = mkConjunction(cs ++ ptrs.map { x => PNeg(PEq(x, NilPtr)) })
          val newSpec = Spec(Assertion(_p1, s1), post, g)
          SynAndGoals(List(newSpec), pureKont(toString))
      }
    }
  }

  /*
  x ≠ y ∉ φ
  Γ ; {φ ∧ x ≠ y ; x.f -> l * y.f -> l' * P} ; {ψ ; Q} ---> S
  ------------------------------------------------------------ [*-partial]
  Γ ; {φ ; x.f -> l * y.f -> l' * P} ; {ψ ; Q} ---> S
   */
  object StarPartial extends SynthesisRule {
    override def toString: String = "[Norm: *-partial]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      val Spec(Assertion(p1, s1), post, g) = spec
      conjuncts(p1) match {
        case None => SynFail
        case Some(cs) =>
          val ptrs = (for (PointsTo(x, _, _) <- s1.chunks) yield x).toSet
          // All pairs of pointers
          val pairs = (for (x <- ptrs; y <- ptrs if x != y) yield Set(x, y)).
              map { s =>
                val elems = s.toList
                ruleAssert(elems.size == 2, "Wrong number of elements in a pair")
                (elems.head, elems.tail.head)
              }.toList
          val newPairs = pairs.filter {
            case (x, y) => !cs.contains(PNeg(PEq(x, y))) && !cs.contains(PNeg(PEq(y, x)))
          }
          if (newPairs.isEmpty) return SynFail

          // The implementation immediately adds _all_ inequalities
          val _p1 = mkConjunction(cs ++ newPairs.map { case (x, y) => PNeg(PEq(x, y)) })
          val newSpec = Spec(Assertion(_p1, s1), post, g)
          SynAndGoals(List(newSpec), pureKont(toString))
      }
    }
  }


  /*
  Γ ; {[l/x]φ ; [l/x]P} ; {[l/x]ψ ; [l/x]Q} ---> S
  ------------------------------------------------ [subst-L]
  Γ ; {φ ∧ x = l ; P} ; {ψ ; Q} ---> S
  */
  object SubstLeft extends SynthesisRule {
    override def toString: String = "[Norm: subst-L]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      val Spec(Assertion(p1, s1), Assertion(p2, s2), g) = spec

      findConjunctAndRest({
        case PEq(v1@Var(_), v2) => v1 != v2
        case _ => false
      }, simplify(p1)) match {
        case Some((PEq(x@Var(_), l), rest1)) =>
          val _p1 = mkConjunction(rest1).subst(x, l)
          val _s1 = s1.subst(x, l)
          val _p2 = p2.subst(x, l)
          val _s2 = s2.subst(x, l)
          val newSpec = Spec(
            Assertion(_p1, _s1),
            Assertion(_p2, _s2),
            g.filter { case (t, w) => w != x })
          SynAndGoals(List(newSpec), pureKont(toString))
        case _ => SynFail
      }
    }
  }

  /*
    X ∈ GV(post) / GV (pre)
    Γ ; {φ ; P} ; {[l/X]ψ ; [l/X]Q} ---> S
    --------------------------------------- [subst-R]
    Γ ; {φ ; P} ; {ψ ∧ X = l; Q} ---> S
  */
  object SubstRight extends SynthesisRule {
    override def toString: String = "[Norm: subst-R]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      val Spec(pre, Assertion(p2, s2), g) = spec

      findConjunctAndRest({
        case PEq(x@Var(_), _) => spec.existentials.contains(x)
        case _ => false
      }, simplify(p2)) match {
        case Some((PEq(x@Var(_), l), rest2)) =>
          val _p2 = mkConjunction(rest2).subst(x, l)
          val _s2 = s2.subst(x, l)
          val newSpec = Spec(pre, Assertion(_p2, _s2), g)
          SynAndGoals(List(newSpec), pureKont(toString))
        case _ => SynFail
      }
    }
  }


  /*
  Γ ; {φ ; P} ; {ψ ; Q} ---> S
  -------------------------------------- [=-L]
  Γ ; {φ ∧ l = l ; P} ; {ψ ; Q} ---> S
  */
  object StripEqPre extends SynthesisRule {
    override def toString: String = "[Norm: =-L]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      findConjunctAndRest({
        case PEq(x, y) => x == y
        case _ => false
      }, simplify(spec.pre.phi)) match {
        case None => SynFail
        case Some((_, rest)) =>
          val newPre = Assertion(mkConjunction(rest), spec.pre.sigma)
          val newSpec = Spec(newPre, spec.post, spec.gamma)
          SynAndGoals(List(newSpec), pureKont(toString))
      }
    }
  }

  /*
  --------------------------------------- [inconsistency]
  Γ ; {φ ∧ l ≠ l ; P} ; {ψ ; Q} ---> emp
  */
  object Inconsistency extends SynthesisRule {
    override def toString: String = "[Norm: Inconsistency]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      val Spec(Assertion(p1, _), _, g) = spec
      val res = findConjunctAndRest({
        case PNeg(PEq(x, y)) => x == y
        case _ => false
      }, simplify(p1))
      res match {
        case Some((PNeg(PEq(x, y)), rest1)) if x == y =>
          SynAndGoals(Nil, _ => Skip)
        case _ => SynFail
      }
    }
  }


  /*
  Γ ; {φ ∧ φ' ; P} ; {ψ ; Q} ---> S
  --------------------------------------- [Hypothesis]
  Γ ; {φ ∧ φ' ; P} ; {ψ ∧ φ' ; Q} ---> S
  */
  object Hypothesis extends SynthesisRule {
    override def toString: String = "[Norm: Hypothesis]"
    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      (conjuncts(spec.pre.phi), conjuncts(spec.post.phi)) match {
        case (Some(cs1), Some(cs2)) =>
          findCommon((p: PFormula) => true, cs1, cs2) match {
            case Some((p, ps1, ps2)) =>
              val newPost = Assertion(mkConjunction(ps2), spec.post.sigma)
              val newSpec = Spec(spec.pre, newPost, spec.gamma)
              SynAndGoals(List(newSpec), pureKont(toString))
            case None => SynFail
          }
        case _ => SynFail
      }
    }
  }


  /*

  Γ ; {φ ; P} ; {ψ ; Q} ---> S
  ------------------------------------- [=-R]
  Γ ; {φ ; P} ; {ψ ∧ l = l ; Q} ---> S

   */
  object StripEqPost extends SynthesisRule {
    override def toString: String = "[Sub: =-R]"

    def apply(spec: Spec, env: Environment): SynthesisRuleResult = {
      findConjunctAndRest({
        case PEq(x, y) => x == y
        case _ => false
      }, simplify(spec.post.phi)) match {
        case None => SynFail
        case Some((_, rest)) =>
          val newPost = Assertion(mkConjunction(rest), spec.post.sigma)
          val newSpec = Spec(spec.pre, newPost, spec.gamma)
          SynAndGoals(List(newSpec), pureKont(toString))
      }
    }
  }


}