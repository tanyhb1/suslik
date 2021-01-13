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
    val example1 = (List(1,2,3,4), 4)
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
