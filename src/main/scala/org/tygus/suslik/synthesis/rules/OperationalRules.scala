package org.tygus.suslik.synthesis.rules

import org.tygus.suslik.LanguageUtils.generateFreshVar
import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.language.{Statements, _}
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic._
import org.tygus.suslik.synthesis.{PrependProducer, _}
import org.tygus.suslik.synthesis.Evaluator._
import org.tygus.suslik.synthesis.rules.Rules._

import scala.collection.mutable.ListBuffer

/**
  * Operational rules emit statement that operate of flat parts of the heap.
  * @author Nadia Polikarpova, Ilya Sergey
  */

object OperationalRules extends SepLogicUtils with RuleUtils {

  val exceptionQualifier: String = "rule-operational"

  import Statements._

  /*
  Write rule: create a new write from where it's possible

  Γ ; {φ ; x.f -> l' * P} ; {ψ ; x.f -> l' * Q} ---> S   GV(l) = GV(l') = Ø
  ------------------------------------------------------------------------- [write]
  Γ ; {φ ; x.f -> l * P} ; {ψ ; x.f -> l' * Q} ---> *x.f := l' ; S

  */
  object WriteRule extends SynthesisRule with GeneratesCode with InvertibleRule {

    override def toString: Ident = "Write"
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre
      val post = goal.post

      // Heaplets have no ghosts
      def noGhosts: Heaplet => Boolean = {
        case PointsTo(x@Var(_), _, e) => !goal.isGhost(x) && e.vars.forall(v => !goal.isGhost(v))
        case _ => false
      }

      // When do two heaplets match
      def isMatch(hl: Heaplet, hr: Heaplet) = sameLhs(hl)(hr) && noGhosts(hr)

      findMatchingHeaplets(noGhosts, isMatch, goal.pre.sigma, goal.post.sigma) match {
        case None => Nil
        case Some((hl@PointsTo(x@Var(_), offset, e1), hr@PointsTo(_, _, e2))) =>
          if (e1 == e2) {
            return Nil
          } // Do not write if RHSs are the same

          val newPre = Assertion(pre.phi, goal.pre.sigma - hl)
          val newPost = Assertion(post.phi, goal.post.sigma - hr)
          val subGoal = goal.spawnChild(newPre, newPost)
          val kont: StmtProducer = PrependProducer(Store(x, offset, e2)) >> HandleGuard(goal) >> ExtractHelper(goal)

          List(RuleResult(List(subGoal), kont, this, goal))
        case Some((hl, hr)) =>
          ruleAssert(assertion = false, s"Write rule matched unexpected heaplets ${hl.pp} and ${hr.pp}")
          Nil
      }
    }

  }

  /*
  Read rule: create a fresh typed read

        y is fresh   Γ,y ; [y/A]{φ ; x -> A * P} ; [y/A]{ψ ; Q} ---> S
      ---------------------------------------------------------------- [read]
             Γ ; {φ ; x.f -> A * P} ; {ψ ; Q} ---> let y := *x.f ; S
  */
  object ReadRule extends SynthesisRule with GeneratesCode with InvertibleRule {

    override def toString: Ident = "Read"
    def apply(goal: Goal, examples: Option[Examples]): Seq[RuleResult] = {
      val pre = goal.pre
      val post = goal.post
      val init_pre = pre.sigma.chunks.toSet
      def isGhostPoints: Heaplet => Boolean = {
        case PointsTo(x@Var(_), _, a@Var(_)) =>
          !goal.isGhost(x) && goal.isGhost(a)
        case _ => false
      }
      val ans = apply(goal)
      ans match {
        case Nil =>
          examples match {
            case None => Nil
            case Some(eg) =>
              var ls = ListBuffer[RuleResult]()
              for (e <- eg){
                val store = e._1
                val heap_fst = e._2
                val resolved_heap = resolveHeapLHS(heap_fst, store)
                val exampleAsSFormula = exampleResolvedHeapToSFormula(resolved_heap)
                val exampleAsHeaplets = exampleAsSFormula.chunks.toSet
                val final_heap = SFormula(exampleAsHeaplets.diff(init_pre).toList)

                findHeaplet(isGhostPoints, final_heap) match{
                  case None => ()
                  case Some(PointsTo(x@Var(_), offset, a@Var(_))) =>
                    val y = a
                    val tpy = goal.getType(a)
                    val subGoal = goal.spawnChild(pre = pre.subst(a, y),
                      post = post.subst(a, y),
                      gamma = goal.gamma + (y -> tpy),
                      programVars = y :: goal.programVars)
                    var kont: StmtProducer = PrependProducer(Load(y, tpy, x, offset)) >> HandleGuard(goal) >> ExtractHelper(goal)
                    // if we haven't read in a variable yet, then we search for it in our user-provided Heap as well
                    if (!goal.programVars.contains(x)) {
                      var from : Option[Expr] = None
                      for ((k,v) <- resolved_heap){
                        if (x == v) {
                          from = Some(k)
                        }
                      }
                      from match {
                        case None => ()
                        case Some(s) =>
                          kont  = PrependProducer(SeqComp(Load(x, tpy, s.asInstanceOf[Var], offset), Load(a, tpy, x, offset))) >> HandleGuard(goal) >> ExtractHelper(goal)
                      }
                    }
                    ls +=  RuleResult(List(subGoal), kont, this, goal)
                  case Some(h) =>
                    ruleAssert(false, s"Read rule matched unexpected heaplet ${h.pp}")
                }
              }
              if (ls.isEmpty) {
                Nil
              } else {
                ls
              }
          }
        case s => s
      }

    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre
      val post = goal.post

      def isGhostPoints: Heaplet => Boolean = {
        case PointsTo(x@Var(_), _, a@Var(_)) =>
          !goal.isGhost(x) && goal.isGhost(a)
        case _ => false
      }
      findHeaplet(isGhostPoints, goal.pre.sigma) match {
        case None => Nil
        case Some(PointsTo(x@Var(_), offset, a@Var(_))) =>
          val y = generateFreshVar(goal, a.name)
          val tpy = goal.getType(a)

          val subGoal = goal.spawnChild(pre = pre.subst(a, y),
            post = post.subst(a, y),
            gamma = goal.gamma + (y -> tpy),
            programVars = y :: goal.programVars)
          val kont: StmtProducer = PrependProducer(Load(y, tpy, x, offset)) >> HandleGuard(goal) >> ExtractHelper(goal)
          List(RuleResult(List(subGoal), kont, this, goal))
        case Some(h) =>
          ruleAssert(false, s"Read rule matched unexpected heaplet ${h.pp}")
          Nil
      }

    }
  }


  /*
  Alloc rule: allocate memory for an existential block

           X ∈ GV(post) / GV (pre)        y, Ai fresh
         Γ ; {φ ; y -> (A0 .. An) * P} ; {ψ ; [y/X]Q} ---> S
     -------------------------------------------------------------- [alloc]
     Γ ; {φ ; P} ; {ψ ; block(X, n) * Q} ---> let y = malloc(n); S
  */
  object AllocRule extends SynthesisRule with GeneratesCode {
    override def toString: Ident = "Alloc"

    val MallocInitVal = 666
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def findTargetHeaplets(goal: Goal): Option[(Block, Seq[Heaplet])] = {
      def isExistBlock: Heaplet => Boolean = {
        case Block(x@Var(_), _) => goal.isExistential(x)
        case _ => false
      }

      findBlockAndChunks(isExistBlock, _ => true, goal.post.sigma)
    }

    def apply(goal: Goal): Seq[RuleResult] = {

      val pre = goal.pre
      val post = goal.post

      findTargetHeaplets(goal) match {
        case None => Nil
        case Some((h@Block(x@Var(_), sz), pts)) =>
          val y = generateFreshVar(goal, x.name)
          val tpy = LocType

          val freshChunks = for {
            off <- 0 until sz
          } yield PointsTo(y, off, IntConst(MallocInitVal))
          val freshBlock = Block(x, sz).subst(x, y)
          val newPre = Assertion(pre.phi, mkSFormula(pre.sigma.chunks ++ freshChunks ++ List(freshBlock)))

          val subGoal = goal.spawnChild(newPre,
                                        post.subst(x, y),
                                        gamma = goal.gamma + (y -> tpy),
                                        programVars = y :: goal.programVars)
          val kont: StmtProducer = PrependProducer(Malloc(y, tpy, sz)) >> HandleGuard(goal) >> ExtractHelper(goal)
          List(RuleResult(List(subGoal), kont, this, goal))
        case _ => Nil
      }
    }

  }

  /*
  Free rule: free a non-ghost block from the pre-state

                     Γ ; {φ ; P} ; {ψ ; Q} ---> S     GV(li) = Ø
   ------------------------------------------------------------------------ [free]
   Γ ; {φ ; block(x, n) * x -> (l1 .. ln) * P} ; { ψ ; Q } ---> free(x); S
*/
  object FreeRule extends SynthesisRule with GeneratesCode {

    override def toString: Ident = "Free"

    def findTargetHeaplets(goal: Goal): Option[(Block, Seq[Heaplet])] = {
      // Heaplets have no ghosts
      def noGhosts(h: Heaplet): Boolean = h.vars.forall(v => goal.isProgramVar(v))

      findBlockAndChunks(noGhosts, _ => true, goal.pre.sigma)
    }
    def apply(goal: Goal, e:Option[Examples]) : Seq[RuleResult] ={
      Seq()
    }
    def apply(goal: Goal): Seq[RuleResult] = {
      val pre = goal.pre

      findTargetHeaplets(goal) match {
        case None => Nil
        case Some((h@Block(x@Var(_), _), pts)) =>
          val toRemove = mkSFormula(pts.toList) ** h
          val newPre = Assertion(pre.phi, pre.sigma - toRemove)

          val subGoal = goal.spawnChild(newPre)
          val kont: StmtProducer = PrependProducer(Free(x)) >> HandleGuard(goal) >> ExtractHelper(goal)

          List(RuleResult(List(subGoal), kont, this, goal))
        case Some(_) => Nil
      }
    }

  }

}