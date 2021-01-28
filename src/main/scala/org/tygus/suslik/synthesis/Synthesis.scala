package org.tygus.suslik.synthesis

import org.tygus.suslik.certification.CertTree
import org.tygus.suslik.language.Expressions.{IntConst, SetLiteral, Var}
import org.tygus.suslik.language.Statements.{Solution, _}
import org.tygus.suslik.logic.Specifications._
import org.tygus.suslik.logic._
import org.tygus.suslik.logic.smt.{CyclicProofChecker, SMTSolving}
import org.tygus.suslik.report.{Log, ProofTrace}
import org.tygus.suslik.synthesis.Memoization._
import org.tygus.suslik.synthesis.SearchTree._
import org.tygus.suslik.synthesis.Termination._
import org.tygus.suslik.synthesis.rules.DelegatePureSynthesis
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
                     examples:Option[List[(Map[String, Int], (String, String, List[Int]) , Int)]]):
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

  protected def synthesize(goal: Goal, examples:Option[List[(Map[String, Int], (String, String, List[Int]) , Int)]])
                          (stats: SynStats): (Option[Solution] ) = {
    // initialize goal as an OrNode


    val preSpatial = goal.pre.sigma
    val prePure = goal.pre.phi
    val postSpatial = goal.post.sigma
    val postPure = goal.post.phi
    val has_examples = examples match {
      case Some(_) => true
      case None => false
    }
    if (has_examples){
      val ret_binding = postSpatial.ptss match {
        case retPointsTo::Nil=> Some(retPointsTo.value)
        case _ =>
          testPrintln("currently only forcing there to be one ret value")
          None
      }
      val examples_parsed = examples match {
        case Some(ls) => ls.map(x => (x._1.toList.map(y => (Var(y._1), IntConst(y._2))).toMap,
          x._2._3.map(y=> IntConst(y)), IntConst(x._3)))
        case None => ???
      }
      val set_binding = preSpatial.apps.head.args.last.asInstanceOf[Var]

      testPrintln(set_binding.toString)
      val new_examples = examples_parsed.map(x=> (x._1 ,
        Map(set_binding ->SetLiteral(x._2)), Map(ret_binding.get.asInstanceOf[Var] -> x._3)))
      val test = new_examples(0)
      val newPreSpatial = preSpatial.subst(test._1)
      val newPostSpatial = postSpatial.subst(test._3)
      val newPre = Assertion(goal.pre.phi, newPreSpatial)
      val newPost = Assertion(goal.post.phi, newPostSpatial)
      testPrintln(newPreSpatial.toString)
      testPrintln(newPostSpatial.toString)
      testPrintln(examples.toString)
      val newgoal = Goal(newPre, newPost, goal.gamma, goal.programVars,
        goal.universalGhosts, goal.fname, goal.label, goal.parent,
        goal.env, goal.sketch, goal.callGoal
      )
//      (pre: Assertion,
//        post: Assertion,
//        gamma: Gamma, // types of all variables (program, universal, and existential)
//        programVars: List[Var], // program-level variables
//        universalGhosts: Set[Var], // universally quantified ghost variables
//        fname: String, // top-level function name
//        label: GoalLabel, // unique id within the derivation
//        parent: Option[Goal], // parent goal in the derivation
//        env: Environment, // predicates and components
//        sketch: Statement, // sketch
//        callGoal: Option[SuspendedCallGoal]
//      )

      testPrintln(s"new examples is ${new_examples}")
      init(goal)
      testPrintln(goal.toString)
      testPrintln(goal.pre.sigma.chunks.toString)
      val test_map = Map(Var("x") -> IntConst(0), Var("y") -> IntConst(10), Var("z") -> IntConst(10))

      testPrintln(s"pre without subst: ${goal.pre.sigma.toString}")
      testPrintln(s"pre subst: ${goal.pre.sigma.subst(test_map).toString}")
      testPrintln(s"${goal.post.sigma.ptss.toString()}")
      testPrintln(s"post without subst: ${goal.post.sigma.toString}")
      testPrintln(s"post subst: ${goal.post.sigma.subst(test_map)}")

      testPrintln("\n")
      testPrintln(s"pre pure: ${goal.pre.phi.pp}, pre spatial: ${goal.pre.sigma.pp}" )
      testPrintln(s"post pure: ${goal.post.phi.pp}, post spatial: ${goal.post.sigma.pp}" )
      // take original pre and post, create example worlds by subst. the vars with concrete values and then run the synthesizer?
      // alternatively, at every subgoal, we want to extract out goal information, and then subst with concrete values and evaluate
      // that they are satisfied, else backtrack? but isn't this a more labor intensive way of doing what we do in the first method?
      // process work list which is a singleton containing OrNode(goal)
      processWorkList(stats, goal.env.config, Some(new_examples))
    } else {
      init(goal)
      processWorkList(stats, goal.env.config, None)
    }

  }

  @tailrec final def processWorkList(implicit
                                     stats: SynStats,
                                     config: SynConfig,
                                     examples:Option[List[(Map[Var, IntConst], (Map[Var,SetLiteral]) , Map[Var,IntConst])]]): (Option[Solution] ) = {
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
        case None => expandNode(node, addNewNodes, examples) // First time we see this goal: do expand
      }
      res match {
        case None => processWorkList //solveSubGoals
        case sol => {
          (sol)
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
  protected def expandNode(node: OrNode, addNewNodes: List[OrNode] => List[OrNode],
                           examples: Option[List[(Map[Var, IntConst], (Map[Var, SetLiteral]) , Map[Var, IntConst])]])
                          (implicit stats: SynStats, config: SynConfig): Option[Solution] = {
    val goal = node.goal

    memo.save(goal, Expanded)
    implicit val ctx = log.Context(goal)
    examples match {
      case Some(e) => testPrintln(e.toString())
      case None => ()
    }
    testPrintln("pre and post incoming: ")
    testPrintln(goal.pre.sigma.toString)
    testPrintln(goal.post.sigma.toString)
    // Apply all possible rules to the current goal to get a list of alternative expansions,
    // each of which can have multiple open subgoals
    // rules are any phased rules since our sketch is a Hole and we set phased config to true.
    val rules = tactic.nextRules(node)
    //applyRules enumerates all possible sub-derivations obtained by applying the list of rules to our current node.
    val allExpansions = applyRules(rules)(node, stats, config, ctx)
    // no filtering is done for Phased strategy
    val expansions = tactic.filterExpansions(allExpansions)
    // Check if any of the expansions is a terminal
    // returns the first instance where predicate is satisfied for res
    expansions.find(res => res.subgoals.isEmpty) match {
      case Some(e) =>
        if (config.certTarget != null) {
          // [Certify]: Add a terminal node and its ancestors to the certification tree
          CertTree.addSuccessfulPath(node, e)
        }
        // if an expansion is a terminal, then we add that expansion to successLeaves,
        // update our worklist, and set it to node.succeed, which further prunes the worklist
        // and checks if our expansion begins from the root. If so, then it is the solution to the goal, and we return it.
        // otherwise, it is simply a solution to a subgoal, so we continue this process, taking care to memoize.
        trace.add(e, node)
        successLeaves = node :: successLeaves
        worklist = addNewNodes(Nil)
        val sol = node.succeed(e.producer(Nil))
        sol
      case None => { // no terminals: add all expansions to worklist
        // Create new nodes from the expansions
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
        /**
         Either perform the example-driven pruning here, or below
        */
        testPrintln(s"current worklist")
        for (x <- worklist){
          testPrintln(x.goal.pp)
        }
        testPrintln("new nodes")
        for (node <- newNodes){
          testPrintln(node.goal.pp)
        }

        //think about how to propagate the map forward (update the environment since variable names
        // change as we traverse the worklist (i.e., x -> x2)
        // * probably by keeping set of elements, then checking for introduction of new variables, then adding that to map.
        // ** try to extract this renaming info from the rule application (rather than calculating it every iteration)

        // think about how to even use the pre and post subst to determine whether or not to reject???

        testPrintln("\n")

        // Suspend nodes with older and-siblings
        newNodes.foreach(n => {
          val idx = n.childIndex
          if (idx > 0) {
            val sib = newNodes.find(s => s.parent == n.parent && s.childIndex == idx - 1).get
            log.print(List((s"Suspending ${n.pp()} until ${sib.pp()} succeeds", RED)))
            memo.suspendSibling(n, sib) // always process the left and-goal first; unsuspend next once it succeeds
          }
        })

        for (e <- examples.getOrElse(List())){
          for (node <- newNodes) {
            val currgoal = node.goal
            val preSpatial = currgoal.pre.sigma.subst(e._1).subst(e._2)
            val postSpatial = currgoal.post.sigma.subst(e._3)
            val ghosts = currgoal.pre.ghosts(e._1.keySet.union(e._3.keySet))
            testPrintln(s"MapPre: ${e._1} \n MapPost: ${e._3} \n ProgramVariables: ${ghosts} " +
              s"\n PreSpatialSubst: ${preSpatial} \n PostSpatialSubst: ${postSpatial}")
          }
        }
        worklist = addNewNodes(newNodes.toList)
        if (newNodes.isEmpty) {
          // This is a dead-end: prune worklist and try something else
          log.print(List((s"Cannot expand goal: BACKTRACK", Console.RED)))
          trace.add(node.id, Failed)
          node.fail
        } else {
          stats.addGeneratedGoals(newNodes.size)
        }
        None
      }
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
                                                       ctx: log.Context): Seq[RuleResult] = {
    implicit val goal: Goal = node.goal
    rules match {
      case Nil => Vector() // No more rules to apply: done expanding the goal
      case r :: rs =>
        // Invoke the rule (application of the rule occurs here)
        // successful result = one or more alternative sub-derivations
        // sub-derivations are just a pair (x,y) where x are zero or more sub-goals to be solved and y is the continuation to form the solution
        val children = stats.recordRuleApplication(r.toString, r(goal))

        if (children.isEmpty) {
          // Rule not applicable: try other rules
          log.print(List((s"$r FAIL", RESET)), isFail = true)
          applyRules(rs)
        } else {
          // Rule applicable: try all possible sub-derivation
          val childFootprints = children.map(log.showChildren(goal))
          log.print(List((s"$r (${children.size}): ${childFootprints.head}", RESET)))
          for {c <- childFootprints.tail}
            log.print(List((s" <|>  $c", CYAN)))

          /**
            *  Or perform the example-driven pruning here.
            * */
          if (r.isInstanceOf[InvertibleRule]) { // optimization
            // The rule is invertible: do not try other rules on this goal
            children
          } else {
            // Both this and other rules apply
            children ++ applyRules(rs)
          }
        }
    }
  }
}