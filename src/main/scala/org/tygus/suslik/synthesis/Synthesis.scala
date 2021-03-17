package org.tygus.suslik.synthesis

import org.tygus.suslik.certification.CertTree
import org.tygus.suslik.language.Expressions
import org.tygus.suslik.language.Expressions.{BinaryExpr, HeapConst, IntConst, OpSetEq, SetLiteral, Subst, Var}
import org.tygus.suslik.language.Statements.{Solution, _}
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic._
import org.tygus.suslik.logic.smt.{CyclicProofChecker, SMTSolving}
import org.tygus.suslik.report.{Log, ProofTrace}
import org.tygus.suslik.synthesis.Memoization._
import org.tygus.suslik.synthesis.SearchTree._
import org.tygus.suslik.synthesis.Evaluator._
import org.tygus.suslik.synthesis.Termination._
import org.tygus.suslik.synthesis.rules.DelegatePureSynthesis
import org.tygus.suslik.synthesis.rules.OperationalRules.ReadRule
import org.tygus.suslik.synthesis.tactics.Tactic
import org.tygus.suslik.synthesis.rules.Rules._
import org.tygus.suslik.util.SynStats
import ujson.IndexedValue.False

import scala.Console._
import scala.annotation.tailrec
import scala.collection.mutable

/**
  * @author Nadia Polikarpova, Ilya Sergey
  */

// Current goal: given a set of complete input-output examples of linked lists, each of which has as output the last element of the linked list,
// the synthesizer should be able to synthesize a program that returns the last element of the linked list.
class Synthesis(tactic: Tactic, implicit val log: Log, implicit val trace: ProofTrace ) extends SepLogicUtils {
  import log.out.testPrintln
  def synthesizeProc(funGoal: FunSpec, env: Environment, sketch: Statement,
                     examples:Option[Examples]):
  (List[Procedure], SynStats) = {
    implicit val config: SynConfig = env.config
    implicit val stats: SynStats = env.stats

    var FunSpec(name, tp, formals, pre, post, var_decl) = funGoal

    if (!CyclicProofChecker.isConfigured()) {
      log.print(List((s"Cyclic proof checker is not configured! All termination check will be considered TRUE (this not sound).\n", Console.RED)))
    } else {
      log.print(List((s"The mighty cyclic proof checker is available. Well done!\n", Console.GREEN)))
    }

    if (config.delegatePure && !DelegatePureSynthesis.isConfigured()) {
      log.print(List((s"CVC4 is not available! All pure synthesis steps will be performed by enumeration (this takes more steps).\n", Console.RED)))
    }
    val goal = topLevelGoal(pre, post, formals, name, env, sketch, var_decl)
    log.print(List(("Initial specification:", Console.RESET), (s"${goal.pp}\n", Console.BLUE)))
    SMTSolving.init()
    memo.clear()
    ProofTrace.current = trace
    try {
      synthesize(goal, examples)(stats = stats) match {
        case (Some((body, helpers))) =>
          testPrintln(s"Succeeded leaves (${successLeaves.length}): ${successLeaves.map(n => s"${n.pp()}").mkString(" ")})")
          val main = Procedure(funGoal, body)
          (main :: helpers, stats)
        case (None) =>
          log.out.printlnErr(s"Deductive synthesis failed for the goal\n ${goal.pp}")
          (Nil, stats)

      }
    } catch {
      case SynTimeOutException(msg) =>
        log.out.printlnErr(msg)
        (Nil, stats)
    }
  }

  protected def synthesize(goal: Goal, examples:Option[Examples])
                          (stats: SynStats): (Option[Solution] ) = {
    // initialize goal as an OrNode
    val example_write2: Examples = List((Map(Var("x") -> HeapConst(100), Var("y") -> HeapConst(200)),
      Map(100 -> HeapConst(43), 200 -> HeapConst(239)),
      Map(100 -> HeapConst(43), 200 -> HeapConst(43))))

    val example_fstelement2a: Examples =
      List((Map(Var("ret") -> HeapConst(100), Var("x") -> HeapConst(200), Var("v") -> IntConst(1)),
        Map(100 -> Var("x"), 200 -> Var("v") ),
        Map(100 -> Var("v"), 200 -> Var("v"))))
    testPrintln(s"test ${goal.pre.sigma}")
    val test = exampleResolvedHeapToSFormula(resolveHeapLHS(example_fstelement2a.head._2, example_fstelement2a.head._1))
    testPrintln(s"test2 ${test}")
    init(goal)
    processWorkList(stats, goal.env.config, Some(example_fstelement2a))

  }

  @tailrec final def processWorkList(implicit
                                     stats: SynStats,
                                     config: SynConfig,
                                     examples:Option[Examples]): (Option[Solution] ) = {
    // Check for timeouts
    if (!config.interactive && stats.timedOut) {
      throw SynTimeOutException(s"\n\nThe derivation took too long: more than ${config.timeOut} seconds.\n")
    }
    val sz = worklist.length
    stats.updateMaxWLSize(sz)

    if (worklist.isEmpty) (None) // No more goals to try: synthesis failed
    else {
      val (node, addNewNodes) = selectNode // Select next node to expand based on DFS/BFS/Min-cost
      // default = min-cost. combining strategy: add new node where the curr min-cost node was.
      val goal = node.goal
      implicit val ctx: log.Context = log.Context(goal)
      stats.addExpandedGoal(node)
      trace.add(node)

      // Lookup the node in the memo
      val res = memo.lookup(goal) match {
        case Some(Failed) => { // Same goal has failed before: record as failed
          trace.add(node.id, Failed, Some("cache"))
          worklist = addNewNodes(Nil)
          node.fail
          None
        }
        case Some(Succeeded(sol)) =>
        { // Same goal has succeeded before: return the same solution
          log.print(List((s"Recalled solution ${sol._1.pp}", RED)))
          trace.add(node.id, Succeeded(sol), Some("cache"))
          worklist = addNewNodes(Nil)
          node.succeed(sol)
        }
        case Some(Expanded) => { // Same goal has been expanded before: wait until it's fully explored
          log.print(List(("Suspend", RED)))
          memo.suspend(node)
          worklist = addNewNodes(List(node))
          None
        }
        case None =>
          // First time we see this goal: do expand
          log.print(List(("EXPAND", RED)))
          examples match {
            case None =>
              expandNodeWithExamples(node, addNewNodes, None)
            case _ =>
              expandNodeWithExamples(node, addNewNodes, examples) match {
                case Some(s) =>
                  examples match {
                    case Some(eg) =>
                      log.print(List(("EVALUATING EXAMPLES", RED)))
                      log.print(List((s"Solution = ${s}", RED)))
                      val pass = evaluateSolution(Some(s), eg)
                      if (pass){
                        log.print(List(("Success", RED)))
                        Some(s)
                      } else {
                        node.fail
                        None
                      }
                    case None =>
                      Some(s)
                  }
                case None =>
                  log.print(List(("NO EXPANSION", YELLOW)))
                  None
              }
          }
      }

      res match {
        case None =>
         // testPrintln(s"${worklist.map(x => x.goal.pp )}")
          processWorkList //solveSubGoals
        case Some(sol) => {
          Some(sol)
        }
      }
    }
  }

  // Given a worklist, return the next node to work on
  // and a strategy for combining its children with the rest of the list
  // DLL to SLL and specify that we want the set of nodes via examples instead
  // getting last element of a linked list is hard (use examples? to specify instead
  protected def selectNode(implicit config: SynConfig): (OrNode, Worklist => Worklist) =
    if (config.depthFirst) // DFS? Pick the first one, insert new nodes in the front
      (worklist.head, _ ++ worklist.tail)
    else if (config.breadthFirst) { // BFS? Pick the first one non-suspended, insert new nodes in the back
      val best = worklist.minBy(n => memo.isSuspended(n))
      val idx = worklist.indexOf(best)
      (best, worklist.take(idx) ++ worklist.drop(idx + 1) ++ _)
    } else { // Otherwise pick a minimum-cost node that is not suspended
      val best = worklist.minBy(n => (memo.isSuspended(n), n.cost))
      val idx = worklist.indexOf(best)
      (best, worklist.take(idx) ++ _ ++ worklist.drop(idx + 1))
    }

  // Expand node and return either a new worklist or the final solution
  protected def expandNodeWithExamples(node: OrNode, addNewNodes: List[OrNode] => List[OrNode],
                           examples: Option[Examples])
                          (implicit stats: SynStats, config: SynConfig): Option[(Solution)] = {
    val goal = node.goal

    memo.save(goal, Expanded)
    implicit val ctx = log.Context(goal)
    // Apply all possible rules to the current goal to get a list of alternative expansions,
    // each of which can have multiple open subgoals
    // rules are any phased rules since our sketch is a Hole and we set phased config to true.
    val rules = tactic.nextRules(node)

    //applyRules enumerates all possible sub-derivations obtained by applying the list of rules to our current node.
    log.print(List((s" CURRENT GOAL  ${goal.pp}", RED)))
    val allExpansions = applyRules(rules)(node, stats, config, ctx, examples)
    // no filtering is done for Phased strategy
    val expansions = tactic.filterExpansions(allExpansions)
    // Check if any of the expansions is a terminal
    // returns the first instance where predicate is satisfied for res
    val res = expansions.find(res => res.subgoals.isEmpty) match {
      case Some(e) =>
        // if an expansion is a terminal, then we add that expansion to successLeaves,
        // update our worklist, and set it to node.succeed, which further prunes the worklist
        // and checks if our expansion begins from the root. If so, then it is the solution to the goal, and we return it.
        // otherwise, it is simply a solution to a subgoal, so we continue this process, taking care to memoize.
        if (config.certTarget != null) {
          // [Certify]: Add a terminal node and its ancestors to the certification tree
          CertTree.addSuccessfulPath(node, e)
        }
        trace.add(e, node)
        successLeaves = node :: successLeaves
        worklist = addNewNodes(Nil)
        val sol = node.succeed(e.producer(Nil))
        sol


        /**
          * TBA: Handle back-tracking in a more efficient manner by using continuations to keep track of the control-flow
          */
      case None => { // no terminals: add all expansions to worklist
         None
      }
    }
    (examples, res) match {
      case (None, Some(_)) =>
        res
      case _ =>
        val newNodes = for {
          (e, i) <- expansions.zipWithIndex
          andNode = AndNode(i +: node.id, node, e)
          if isTerminatingExpansion(andNode) // termination check
          nSubs = e.subgoals.size; () = trace.add(andNode, nSubs)
          (g, j) <- if (nSubs == 1) List((e.subgoals.head, -1)) // this is here only for logging
          else e.subgoals.zipWithIndex
        } yield {
          val extraCost = if (j == -1) 0 else e.subgoals.drop(j + 1).map(_.cost).sum
          OrNode(j +: andNode.id, g, Some(andNode), node.extraCost + extraCost)
        }
        // Suspend nodes with older and-siblings
        newNodes.foreach(n => {
          val idx = n.childIndex
          if (idx > 0) {
            val sib = newNodes.find(s => s.parent == n.parent && s.childIndex == idx - 1).get
            log.print(List((s"Suspending ${n.pp()} until ${sib.pp()} succeeds", RED)))
            memo.suspendSibling(n, sib) // always process the left and-goal first; unsuspend next once it succeeds

          }
        })
        worklist = addNewNodes(newNodes.toList)
        if (newNodes.isEmpty) {
          // This is a dead-end: prune worklist and try something else
          log.print(List((s"Cannot expand goal: BACKTRACK", Console.RED)))
          trace.add(node.id, Failed)
          node.fail
        } else {
          stats.addGeneratedGoals(newNodes.size)
        }
        res
    }

  }

  protected def evaluateSolution(solution: Option[Solution], examples: Examples)
                                (implicit stats: SynStats, config: SynConfig): Boolean = {
    solution match {
      case Some(sol) =>
        var pass = true
        for (eg <- examples){
          val (store, init_heap, fin_heap) = eg
          val (output_heap, output_store) = evaluate(sol._1, init_heap, store)
          val resolved_testHeap = resolveHeap(output_heap, output_store)
          val resolved_egHeap = resolveHeap(fin_heap, output_store)
          testPrintln(s"curr heap is  ${resolved_testHeap} while expected heap is ${resolved_egHeap} ")
          testPrintln(s"with solution ${sol._1}")
          testPrintln(s"with store ${output_store}")
          log.print(List((s"curr heap is ${resolved_testHeap.toSet} while expected heap is ${resolved_egHeap.toSet}", RED)))
          // check that example heap is a subset of the actual output heap
          if (!(resolved_egHeap.toSet subsetOf resolved_testHeap.toSet) || !(resolved_egHeap == resolved_testHeap)){
            log.print(List((s"curr heap is ${resolved_testHeap.toSet} while expected heap is ${resolved_egHeap.toSet}", RED)))
            pass = false
          }
        }
        pass
      case None => false
    }
  }

  /**
    * Result of a rule application:
    * sub-goals to be solved and
    * a statement producer that assembles the sub-goal results
    */
  // producer = continuation that builds a solution to the synthesis problem from its constituent subproblems
  // enumerates all sub-derivations from applying all possible rules to a sub-goal
  protected def applyRules(rules: List[SynthesisRule])(implicit node: OrNode,
                                                       stats: SynStats,
                                                       config: SynConfig,
                                                       ctx: log.Context,
                                                       examples_opt : Option[Examples]): Seq[RuleResult] = {
    implicit val goal: Goal = node.goal
    rules match {
      case Nil => Vector() // No more rules to apply: done expanding the goal
      case r :: rs =>
        val children = r match {
          case ReadRule => stats.recordRuleApplication(r.toString, r(goal,examples_opt))
          case _ => stats.recordRuleApplication(r.toString, r(goal))
        }
        // Invoke the rule (application of the rule occurs here)
        // successful result = one or more alternative sub-derivations
        // sub-derivations are just a pair (x,y) where x are zero or more sub-goals to be solved and y is the continuation to form the solution
        if (children.isEmpty) { // this path is only taken by synthesis without examples. in synthesis with examples we handle this above.
          // Rule not applicable: try other rules
          log.print(List((s"$r FAIL", RESET)))
          applyRules(rs)
        } else {
          // Rule applicable: try all possible sub-derivation
          val childFootprints = children.map(log.showChildren(goal))
          log.print(List((s"$r (${children.size}): ${childFootprints.head}", RESET)))
          for {c <- childFootprints.tail}
            log.print(List((s" <|>  $c", CYAN)))
          /**
            *  Let's just try every rule regardless, for now.
            */
          examples_opt match  {
            case None =>
              if (r.isInstanceOf[InvertibleRule]) { // optimization
                // The rule is invertible: do not try other rules on this goal
                children
              } else {
                // Both this and other rules apply
                children ++ applyRules(rs)
              }
            case Some(_) =>
              children ++ applyRules(rs)
          }

        }
     }
  }
}