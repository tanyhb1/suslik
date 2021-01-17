package org.tygus.suslik.synthesis

import org.scalatest.{FunSpec, Matchers}
import org.tygus.suslik.language.Expressions.Var

/**
  * @author Nadia Polikarpova, Ilya Sergey
  */

class HintsTest extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String, params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, params)
    it(desc) {
      // hints is a pair of lists, with first element being the hints for the precondition, and second element being the hints for the postcondition
      synthesizeFromSpec(testName, in, out, params)
    }
  }
  override def doRunWithHints(testName: String, desc: String, in: String, out: String, params: SynConfig = defaultConfig): Unit = {
    val hints = (List((Var("x"), 10), (Var("y"), 20)), List((Var("x"), 20), (Var("y"), 20)))
    super.doRun(testName, desc, in, out, params)
    it(desc) {
      // hints is a pair of lists, with first element being the hints for the precondition, and second element being the hints for the postcondition
      synthesizeFromSpec(testName, in, out, params, hints)
    }
  }
  override def doRunWithExamples(testName: String, desc: String, in: String, out: String, params: SynConfig = defaultConfig): Unit = {
    val hints = (List((Var("x"), 10), (Var("y"), 20)), List((Var("x"), 20), (Var("y"), 20)))
    // predicate lseg(x,y,S) describes a linked list that starts at location x, ends at location y, and contains a set of elements S.
    // Represented abstractly as SApp(Ident "lseg", List(_startloc_, _endloc_, _set_), _tag_, _name_)
    // To keep track of the evolution of variable values during the syn-
    //thesis process, we tie to each ex an execution environment σ which
    //maps variables to values.
//    The goal of the synthesis proce-
//      dure is to derive a program that satisfies each example world given
//      in X .
    //x is starting memory address, y is ending memory address, and y -> 3
    // example takes the form of (sigma, input, output) where sigma is execution environment that maps variables to values, input and output are examples.
    val example = (Map("x" -> 0, "y" -> 10), ("x", "y", (1,2,3,4)), 3)

    // Min-cost based approach
    // The synthesis problem Let an input-output example be a term
    //a i 7→ b i , where a i and b i are closed programs. The input to
    //our synthesis problem is a set E in of such examples. Our goal
    //is to compute a minimal-cost closed program e that satisfies the
    //examples — i.e., for each i, we have (e a i )
    //b i . In what follows,
    //we refer to e as the target program.

    // Hypothesis (programs with free variables as placeholders) approach ???


    // another way to express this? which to use?
    val example1 = (List(1,2,4,3), 4)
    val example2 = (List(4,6,1,2,10,5), 5)
    val example3 = (List(10,9,8,7,6,5,4,3), 3)
    val examples = List(example1, example2, example3)
    super.doRun(testName, desc, in, out, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, params, hints)
    }
  }
  describe("Last element of Linked Lists with complete input-output examples"){
    runSingleTestFromDir("hints", "lastelement.syn")
  }
  describe("SL-based synthesizer without hints") {
    runSingleTestFromDir("hints", "write2.syn")
  }
  describe("With Hints"){
    runSingleTestFromDirWithHints("hints", "write2.syn")
  }

}
