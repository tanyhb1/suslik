package org.tygus.suslik.synthesis

import org.scalatest.{FunSpec, Matchers}

/**
  * @author Ilya Sergey
  */

class SynthesisWithTheories extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String,
                     examples:String,params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, examples, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, examples, params)
    }
  }

  describe("SL-based synthesizer with theory of integers") {
    runAllTestsFromDir("ints")
  }

  describe("SL-based synthesizer with theory of finite sets") {
    runAllTestsFromDir("sets")
  }

}
