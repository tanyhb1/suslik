package org.tygus.suslik.parsing

import org.scalatest.{FunSpec, Matchers}
import org.tygus.suslik.synthesis.{SynConfig, SynthesisRunnerUtil, defaultConfig}


class TestNewSyntax extends FunSpec with Matchers with SynthesisRunnerUtil {

  override def doRun(testName: String, desc: String, in: String, out: String,
                     examples: String, params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out,examples, params)
    it(desc) {
      synthesizeFromSpec(testName, in, out, examples, params)
    }
  }
  
  describe("New syntax test") {
    runAllTestsFromDir("syntax")
  }
}
