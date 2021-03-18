package org.tygus.suslik.synthesis

import org.scalatest.{FunSpec, Matchers}

class CyclicTests extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String,
                     examples:String,params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, examples, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, examples, params)
    }
  }

  describe("Contrived cyclic benchmarks") {
    runAllTestsFromDir("cyclic-benchmarks/contrived")
  }

  describe("Rose tree") {
    runAllTestsFromDir("cyclic-benchmarks/rose-tree")
  }

  describe("Multi-list") {
    runAllTestsFromDir("cyclic-benchmarks/multi-list")
  }

//  describe("Skiplist") {
//    runAllTestsFromDir("cyclic-benchmarks/skiplist")
//  }

  describe("Single-Linked Lists") {
    runAllTestsFromDir("cyclic-benchmarks/sll")
  }

  describe("Trees") {
    runAllTestsFromDir("cyclic-benchmarks/tree")
  }

  describe("Sorted lists") {
    runAllTestsFromDir("cyclic-benchmarks/srtl")
  }

  describe("Binary search tree") {
    runAllTestsFromDir("cyclic-benchmarks/bst")
  }


}
