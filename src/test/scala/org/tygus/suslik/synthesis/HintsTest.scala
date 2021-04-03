package org.tygus.suslik.synthesis

import org.scalatest.{FunSpec, Matchers}

/**
  * @author Nadia Polikarpova, Ilya Sergey
  */

class HintsTest extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String,
                     examples:String,params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, examples, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, examples, params)
    }
  }
  override def doRunWithExamples(testName: String, desc: String, in: String, out: String,
                                 examples: String, params: SynConfig = defaultConfig): Unit = {

    super.doRun(testName, desc, in, out, examples, params)
    it(desc) {
      synthesizeFromSpecWithExamples(testName, in, out, examples, params)
    }
  }

//  describe("Last element of Linked Lists without complete input-output examples"){
//    runSingleTestFromDir("hints", "lastelement.syn")
//  }
//  describe("First element of Linked Lists without complete input-output examples"){
//
//    runSingleTestFromDir("hints", "fstelement.syn")
//  }
//  describe("Last element of Linked Lists parameterized by sets without complete input-output examples"){
//    runSingleTestFromDir("hints", "lastelement2.syn")
//  }
//  describe("First element of Linked Lists parameterized by sets without complete input-output examples"){
//    runSingleTestFromDir("hints", "fstElement.syn")
//  }
//
//  describe("Last element of Linked Lists with complete input-output examples"){
//    //x is starting memory address, y is ending memory address, and y -> 3
//    // example takes the form of (sigma, input, output) where sigma is execution environment that maps variables to values, input and output are examples.
//    val fst_example = (Map("x" -> 4, "y" -> 200), ("x", "y", List(4,7,23,6)), 6)
//    val snd_example = (Map("x" -> 4, "y" -> 200), ("x", "y", List(4,6,1,2,10,5)), 5)
//    val thd_example = (Map("x" -> 10, "y" -> 200), ("x", "y", List(10,9,8,7,6,5,4,3)), 3)
//    val examples2 = List(fst_example, snd_example, thd_example)
//    runSingleTestFromDirWithExamples("hints", "lastelement.syn", examples2)
//  }
//  describe("First element of Linked Lists with complete input-output examples"){
//    val fst_example = (Map("x" -> 7, "y" -> 200), ("x", "y", List(7,4,6,5,10)), 7)
//    val snd_example = (Map("x" -> 4, "y" -> 200), ("x", "y", List(4,6,7,2,10,5,0)), 4)
//    val thd_example = (Map("x" -> 10, "y" -> 200), ("x", "y", List(10,9,8,7,6,5,4,3,0)), 10)
//    val examples2 = List(fst_example, snd_example, thd_example)
//    runSingleTestFromDirWithExamples("hints", "fstelement.syn", examples2)
//  }
//
//  describe("Last element of Linked Lists parameterized by sets with complete input-output examples"){
//    //x is starting memory address, y is ending memory address, and y -> 3
//    // example takes the form of (sigma, input, output) where sigma is execution environment that maps variables to values, input and output are examples.
//    val fst_example = (Map("x" -> 100, "y" -> 200), ("x", "y", List(1,2,3,4)), 4)
//    val snd_example = (Map("x" -> 100, "y" -> 200), ("x", "y", List(4,6,1,2,10,5)), 5)
//    val thd_example = (Map("x" -> 100, "y" -> 100), ("x", "y", List(10,9,8,7,6,5,4,3)), 3)
//    val examples2 = List(fst_example, snd_example, thd_example)
//    runSingleTestFromDirWithExamples("hints", "lastelement2.syn", examples2)
//  }
//  describe("First element of Linked Lists parameterized by sets with complete input-output examples"){
//    val fst_example = (Map("x" -> 100), ("x", "y", List(3,14,33,6)), 3)
//    val snd_example = (Map("x" -> 100), ("x", "y", List(4,6,12,2,10,5)), 4)
//    val thd_example = (Map("x" -> 100), ("x", "y", List(10,9,8,7,6,5,4,3)), 10)
//    val examples2 = List(fst_example, snd_example, thd_example)
//    runSingleTestFromDirWithExamples("hints", "fstElement.syn", examples2)
//  }
//  describe("fst ele"){
//
//    runSingleTestFromDirWithExamples("hints", "fstElement.syn", examples2)
//  }
//  describe("write 2"){
//    // Store; Input Heap; Output Heap
//    val fst_example: (Subst, Heap, Heap) = (Map(Var("x") -> IntConst(100), Var("y") -> IntConst(200)),
//      Map(100 -> IntConst(43), 200 -> IntConst(239)),
//      Map(100 -> IntConst(43), 200 -> IntConst(43)))
//    runSingleTestFromDirWithExamples("hints", "pick.syn", List(fst_example))
//  }

}
