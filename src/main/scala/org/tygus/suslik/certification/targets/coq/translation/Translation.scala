package org.tygus.suslik.certification.targets.coq.translation

import org.tygus.suslik.certification.CertTree
import org.tygus.suslik.certification.targets.coq.language.Expressions._
import org.tygus.suslik.certification.targets.coq.language.Statements._
import org.tygus.suslik.certification.targets.coq.language._
import org.tygus.suslik.certification.targets.coq.logic.Proof.{CEnvironment, CGoal, CProof}
import org.tygus.suslik.language.Expressions._
import org.tygus.suslik.language.Statements._
import org.tygus.suslik.language._
import org.tygus.suslik.logic.Specifications.{Assertion, Goal}
import org.tygus.suslik.logic._
import org.tygus.suslik.synthesis.{ChainedProducer, PartiallyAppliedProducer, StmtProducer}
import org.tygus.suslik.synthesis.rules.UnfoldingRules.Open

object Translation {
  case class TranslationException(msg: String) extends Exception(msg)

  trait Traversable

  type Producer[A] = List[A] => A

  /**
    * Composes two producer continuations
    * @param f the continuation to apply first
    * @param g the continuation to apply to the result of f
    * @return the composed result
    */
  def composeProducer[A](f: Producer[A], g: Producer[A]): Producer[A] =
    (steps: List[A]) => g(List(f(steps)))

  /**
    * Creates a new continuation that prepends items to the argument of an existing continuation `kont`
    * @param args arguments to prepend to `kont`
    * @param kont the original continuation
    * @return a new continuation
    */
  def prependArgsProducer[A](args: List[A], kont: Producer[A]): Producer[A] =
    (args1: List[A]) => kont(args ++ args1)

  /**
    * Creates a new continuation that combines the result of multiple traversals at a branching point
    * @param items a list of things to traverse
    * @param kont the final (root) continuation to apply after collecting all child results
    * @return a new continuation
    */
  def joinProducer[A, T <: Traversable](op: (T, Producer[A]) => A)(items: List[T], kont: Producer[A]): Producer[A] =
    items.foldLeft(kont) {
      case (kontAcc, arg) =>
        items1 => op(arg, prependArgsProducer[A](items1, kontAcc))
    }

  /**
    * Given a program point, derives the currently focused statement and its children
    * @param stmt the program point
    * @return a tuple of an optional current statement and a sequence of child statements
    */
  def expandStmt(stmt: Statement) : (Option[Statement], Seq[Statement]) = stmt match {
    case SeqComp(s1, s2) => (Some(s1), Seq(s2))
    case If(_, tb, eb) => (None, Seq(tb, eb))
    case Guarded(_, body, els, _) => (None, Seq(body, els))
    case _ => (Some(stmt), Seq())
  }

  /**
    * Unwraps a statement producer to get to the part relevant to certification
    * @param p the producer
    * @return an unwrapped producer
    */
  @scala.annotation.tailrec
  def unwrapStmtProducer(p: StmtProducer) : StmtProducer = p match {
    case PartiallyAppliedProducer(p, _) => unwrapStmtProducer(p)
    case ChainedProducer(p1, _) => unwrapStmtProducer(p1)
    case p => p
  }

  /**
    * Produces a Coq certificate from the tree of successful derivations and a synthesized procedure
    * @param node the root of the derivation tree
    * @param proc the synthesized procedure
    * @param env the synthesis environment
    * @return the inductive predicates, fun spec, proof, and program translated to Coq
    */
  def translate(node: CertTree.Node, proc: Procedure)(implicit env: Environment):
    (Seq[CInductivePredicate], CFunSpec, CProof, CProcedure) = {
    val cpreds = for (label <- (node.goal.pre.sigma.apps ++ node.goal.post.sigma.apps).map(_.pred).distinct) yield {
      val predicate = env.predicates(label)
      translateInductivePredicate(predicate.resolveOverloading(env))
    }
    val initialGoal = translateGoal(node.goal)
    val spec = translateFunSpec(node)
    val initialCEnv = CEnvironment(spec, cpreds)
    val proof = ProofTranslation.translate(node, proc, initialGoal, initialCEnv)
    val stmtBody = ProgramTranslation.translate(node, proc, initialGoal)

    val cproc = CProcedure(proc.name, translateSSLType(proc.tp), proc.formals.map(translateParam), stmtBody, spec.inductive)
    (cpreds, spec, proof, cproc)
  }

  private def translateFunSpec(node: CertTree.Node)(implicit env: Environment): CFunSpec = {
    val FunSpec(_, tp, _, _, _, _) = node.goal.toFunSpec
    val goal = node.goal
    val pureParams = goal.universalGhosts
      .intersect(goal.gamma.keySet)
      .map(v => translateParam((v, goal.gamma(v)))).toList
    val ctp = translateSSLType(tp)
    val cparams = goal.formals.map(translateParam)
    val cpre = translateAsn(goal.pre)
    val cpost = translateAsn(goal.post)
    CFunSpec(
      goal.fname,
      ctp,
      cparams,
      pureParams,
      cpre,
      cpost,
      node.rule == Open
    )
  }

  private def translateInductivePredicate(el: InductivePredicate): CInductivePredicate = {
    val cParams = el.params.map(translateParam) :+ (CHeapType, CVar("h"))
    val cClauses = el.clauses.zipWithIndex.map { case (c, i) => translateClause(s"${el.name}$i", c) }
    CInductivePredicate(el.name, cParams, cClauses)
  }

  private def translateParam(el: (Var, SSLType)): (CoqType, CVar) =
    (translateSSLType(el._2), CVar(el._1.name))

  private def translateClause(name: String, el: InductiveClause): CInductiveClause = {
    val selector = translateExpr(el.selector)
    CInductiveClause(name, selector, translateAsn(el.asn))
  }

  def translateSSLType(el: SSLType): CoqType = el match {
    case BoolType => CBoolType
    case IntType => CNatType
    case LocType => CPtrType
    case IntSetType => CNatSeqType
    case VoidType => CUnitType
  }

  def translateGoal(goal: Goal): CGoal = {
    val pre = translateAsn(goal.pre)
    val post = translateAsn(goal.post)
    val gamma = goal.gamma.map { case (value, lType) => (CVar(value.name), translateSSLType(lType)) }
    val programVars = goal.programVars.map(v => CVar(v.name))
    val universalGhosts = goal.universalGhosts.intersect(goal.gamma.keySet).map(v => CVar(v.name)).toSeq
    CGoal(pre, post, gamma, programVars, universalGhosts, goal.fname)
  }

  def translateExpr(el: Expr): CExpr = el match {
    case Var(name) => CVar(name)
    case BoolConst(value) => CBoolConst(value)
    case IntConst(value) => CNatConst(value)
    case el@UnaryExpr(_, _) => translateUnaryExpr(el)
    case el@BinaryExpr(_, _, _) => translateBinaryExpr(el)
    case el@OverloadedBinaryExpr(_, _, _) => translateOverloadedBinaryExpr(el)
    case SetLiteral(elems) => CSetLiteral(elems.map(e => translateExpr(e)))
    case IfThenElse(c, t, e) => CIfThenElse(translateExpr(c), translateExpr(t), translateExpr(e))
  }

  private def translateHeaplet(el: Heaplet): CExpr = el match {
    case PointsTo(loc, offset, value) => CPointsTo(translateExpr(loc), offset, translateExpr(value))
    case SApp(pred, args, tag, card) => CSApp(pred, args.map(translateExpr))
  }

  private def translateAsn(el: Assertion): CAssertion = {
    val phi: CExpr = translateExpr(el.phi.toExpr).simplify
    val sigma = translateSFormula(el.sigma)
    CAssertion(phi, sigma)
  }

  private def translateSFormula(el: SFormula): CSFormula = {
    val ptss = el.ptss.map(translateHeaplet).asInstanceOf[List[CPointsTo]]
    val apps = el.apps.map(translateHeaplet).asInstanceOf[List[CSApp]]
    CSFormula("h", apps, ptss)
  }

  private def translateUnaryExpr(el: UnaryExpr) : CExpr = el match {
    case UnaryExpr(OpNot, e) => e match {
      case BinaryExpr(OpEq, left, right) => COverloadedBinaryExpr(COpNotEqual, translateExpr(left), translateExpr(right))
      case _ => CUnaryExpr(COpNot, translateExpr(e))
    }
    case UnaryExpr(OpUnaryMinus, e) => ???
  }

  private def translateOverloadedBinaryExpr(el: OverloadedBinaryExpr) : CExpr = el match {
    case OverloadedBinaryExpr(OpOverloadedEq, l, r) =>
      COverloadedBinaryExpr(COpOverloadedEq, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpNotEqual, l, r) =>
      COverloadedBinaryExpr(COpNotEqual, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpGt, l, r) =>
      COverloadedBinaryExpr(COpGt, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpGeq, l, r) =>
      COverloadedBinaryExpr(COpGeq, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpGeq, l, r) =>
      COverloadedBinaryExpr(COpGeq, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpOverloadedPlus, l, r) =>
      COverloadedBinaryExpr(COpOverloadedPlus, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpOverloadedMinus, l, r) =>
      COverloadedBinaryExpr(COpOverloadedMinus, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpOverloadedLeq, l, r) =>
      COverloadedBinaryExpr(COpOverloadedLeq, translateExpr(l), translateExpr(r))
    case OverloadedBinaryExpr(OpOverloadedStar, l, r) =>
      COverloadedBinaryExpr(COpOverloadedStar, translateExpr(l), translateExpr(r))
  }

  private def translateBinaryExpr(el: BinaryExpr) : CExpr = el match {
    case BinaryExpr(OpImplication, l, r) =>
      CBinaryExpr(COpImplication, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpPlus, l, r) =>
      CBinaryExpr(COpPlus, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpMinus, l, r) =>
      CBinaryExpr(COpMinus, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpMultiply, l, r) =>
      CBinaryExpr(COpMultiply, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpEq, l, r) =>
      CBinaryExpr(COpEq, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpBoolEq, l, r) =>
      CBinaryExpr(COpBoolEq, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpLeq, l, r) =>
      CBinaryExpr(COpLeq, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpLt, l, r) =>
      CBinaryExpr(COpLt, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpAnd, l, r) =>
      CBinaryExpr(COpAnd, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpOr, l, r) =>
      CBinaryExpr(COpOr, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpUnion, l, r) =>
      CBinaryExpr(COpUnion, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpDiff, l, r) =>
      CBinaryExpr(COpDiff, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpIn, l, r) =>
      CBinaryExpr(COpIn, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpSetEq, l, r) =>
      CBinaryExpr(COpSetEq, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpSubset, l, r) =>
      CBinaryExpr(COpSubset, translateExpr(l), translateExpr(r))
    case BinaryExpr(OpIntersect, l, r) =>
      CBinaryExpr(COpIntersect, translateExpr(l), translateExpr(r))
  }
}
