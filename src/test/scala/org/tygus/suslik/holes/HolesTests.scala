package org.tygus.suslik.holes

import org.scalatest.{FunSpec, Matchers}
import org.tygus.suslik.synthesis._

/**
  * @author Roman Shchedrin
  */

class HolesTests extends FunSpec with Matchers with SynthesisRunnerUtil  {

  def isNegative(out: String): Boolean = {
    out == "ERROR"
  }

  override def doRun(testName: String, desc: String, in: String, out: String,
                     examples: String, params: SynConfig = defaultConfig): Unit = {
    super.doRun(testName, desc, in, out, examples, params)
    if (isNegative(out)) {
      // If this is a negative test, make sure it throws a synthesis exception
      it(desc) {
        val err = intercept[Exception](synthesizeFromSpec(testName, in, out, examples,  params))
        // Can be either a memory error, or a failed entailment proof, which manifests as synthesis exception:
        assert(err.isInstanceOf[SymbolicExecutionError] || err.isInstanceOf[SynthesisException])
      }
    } else {
      // Otherwise, make sure it succeeds and produces the right results
      it(desc) {
        synthesizeFromSpec(testName, in, out, examples, params)
      }
    }
  }

  describe("Holes tests") {
    runAllTestsFromDir("holes")
  }

}
