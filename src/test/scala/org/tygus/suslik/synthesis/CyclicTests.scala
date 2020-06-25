package org.tygus.suslik.synthesis

import org.scalatest.{FunSpec, Matchers}

class CyclicTests extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String, params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, params)
    }
  }

  describe("Contrived cyclic benchmarks") {
    runAllTestsFromDir("cyclic-benchmarks/contrived")
  }

  describe("Rose tree") {
    runAllTestsFromDir("cyclic-benchmarks/rose-tree")
  }

//  describe("Skiplist") {
//    runAllTestsFromDir("cyclic-benchmarks/skiplist")
//  }

//  describe("Single-Linked Lists") {
//    runAllTestsFromDir("cyclic-benchmarks/sll")
//  }

  describe("Trees") {
    runAllTestsFromDir("cyclic-benchmarks/tree")
  }

}
