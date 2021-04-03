package org.tygus.suslik.synthesis

import java.io.{File, PrintWriter}
import java.nio.file.Paths

import org.tygus.suslik.LanguageUtils
import org.tygus.suslik.language.Expressions.{HeapConst, IntConst, Subst, Var}
import org.tygus.suslik.language.Statements.Procedure
import org.tygus.suslik.logic.Preprocessor._
import org.tygus.suslik.logic.smt.SMTSolving
import org.tygus.suslik.logic.{Environment, PointsTo}
import org.tygus.suslik.parsing.SSLParser
import org.tygus.suslik.report._
import org.tygus.suslik.synthesis.Evaluator.{Examples, Heap}
import org.tygus.suslik.synthesis.SearchTree.AndNode
import org.tygus.suslik.synthesis.tactics._
import org.tygus.suslik.util._

import scala.io.Source

/**
  * @author Nadia Polikarpova, Ilya Sergey
  */

trait SynthesisRunnerUtil {

  {
    // Warm up SMT solver:
    SMTSolving
  }

  val log : Log = new Log(SynLogLevels.Test)

  val testSeparator = "###"
  val testExtension = "syn"
  val sketchExtension = "sus"
  val defExtension = "def"
  val paramPrefix = "#"
  val noOutputCheck = "nope"

  // The path starts from the project root.
  val rootDir: String = "./src/test/resources/synthesis".replace("/", File.separator)

  def doRun(testName: String, desc: String, in: String, out: String,
            examples: String, params: SynConfig = defaultConfig) = {
    LanguageUtils.resetFreshNameGenerator()
  }


  def doRunWithHints(testName: String, desc: String, in: String, out: String,
                     examples: String, params: SynConfig = defaultConfig) = {
    LanguageUtils.resetFreshNameGenerator()
  }

  def doRunWithExamples(testname: String, desc: String, in: String, out: String,
                        examples: String, params: SynConfig = defaultConfig) = {
    LanguageUtils.resetFreshNameGenerator()
  }
  def getDescInputOutput(testFilePath: String, initialParams: SynConfig = defaultConfig):
  (String, String, String, String, String, SynConfig) = {
    val file = new File(testFilePath)
    val format = testFilePath match {
      case s if s.endsWith(testExtension) => dotSyn
      case s if s.endsWith(sketchExtension) => dotSus
    }
    // The path is counted from the root
    val allLines = Source.fromFile(file).getLines.toList
    val (params, lines) =
      if (allLines.nonEmpty && allLines.head.startsWith(paramPrefix)) {
        (SynthesisRunner.parseParams(allLines.head.drop(paramPrefix.length).split(' '), initialParams), allLines.tail)
      } else (initialParams, allLines)

    def splitAtSeparator(lines: List[String], default: List[String] = List()): (List[String], List[String]) = {
      val i = lines.indexWhere(_.trim.contains(testSeparator))
      if (i == -1) (lines, default)
      else {
        val (before, _ :: after) = lines.splitAt(i) // a::b matches head `a` and tail `b`
        (before, after)
      }
    }

    def parseSyn = {
      val (testDescr, afterDescr) = splitAtSeparator(lines)
      val fname = removeSuffix(file.getName, s".$testExtension")
      val dirName = file.getParentFile.getName
      val description = if (testDescr.isEmpty) "Testing synthesis" else testDescr.mkString("\n").trim
      // The first part is the description
      val testName = s"$dirName/$fname"
      val desc = s"[$testName] $description"

      val (spec, afterSpec) = splitAtSeparator(afterDescr)
      val (examples, afterExamples) = splitAtSeparator(afterSpec)
      val (expectedSrc, rawScript) = splitAtSeparator(afterExamples)
      val input = spec.mkString(" ").trim
      val output = expectedSrc.mkString("\n").trim
      val script = rawScript.mkString("\n").trim.split("\n").toList.filter(_.nonEmpty).map(_.toInt)
      (testName, desc, input, output, examples.mkString(" ").trim, params.copy(inputFormat = format, script = script))
    }

    def parseSus = {
      val hasDescr = lines.head.trim.startsWith("/*")
      val desc = if(hasDescr) lines.head.trim else ""

      val (spec, expectedSrc) = splitAtSeparator(lines, List(noOutputCheck))
      val (examples, afterExamples) = splitAtSeparator(expectedSrc)
      val input = spec.mkString("\n").trim
      val testName = testFilePath
      val output = expectedSrc.mkString("\n").trim
      (testName, desc, input, output, examples.mkString(" ").trim, params.copy(inputFormat = format))
    }

    format match {
      case `dotSyn` => parseSyn
      case `dotSus` => parseSus
    }
  }

  // Create synthesizer object, choosing search tactic based on the config
  def createSynthesizer(env: Environment): Synthesis = {
    val tactic =
      if (env.config.interactive)
        new InteractiveSynthesis(env.config, env.stats)
      else if (env.config.script.nonEmpty)
        new ReplaySynthesis(env.config)
      else
        new PhasedSynthesis(env.config)
    val trace : ProofTrace = env.config.traceToJsonFile match {
      case None => ProofTraceNone
      case Some(file) => new ProofTraceJson(file)
    }
    new Synthesis(tactic, log, trace)
  }

  def synthesizeFromFile(dir: String, testName: String): Unit = {
    val (_, _, in, out, examples, params) = getDescInputOutput(testName)
    synthesizeFromSpec(testName, in, out, examples, params)
  }

  def synthesizeFromSpec(testName: String, text: String, out: String = noOutputCheck,
                         examples: String, params: SynConfig = defaultConfig): Unit = {
    synthesizeFromSpecWithExamples(testName, text,out, examples, params)

  }
  def synthesizeFromSpecWithExamples(testName: String, text: String,out: String = noOutputCheck,
                                     examples:String, params: SynConfig = defaultConfig)
  {
    import log.out.testPrintln
    val parser = new SSLParser
    val res = params.inputFormat match {
      case `dotSyn` => parser.parseGoalSYN(text)
      case `dotSus` => parser.parseGoalSUS(text)
    }
    if (!res.successful) {
      throw SynthesisException(s"Failed to parse the input:\n$res")
    }
    val parsed_eg = parser.parseGoalEG(examples)
    var eg: Option[Examples]= None
    if (parsed_eg.successful && !parsed_eg.get.isEmpty) {
      val (pre, post) = parsed_eg.get(0)
      var store :Subst= Map()
      var init_heap : Heap = Map()
      var fin_heap : Heap = Map()
      var curr = 100
      //initialize store
      for (x <- pre.chunks) {
        x match {
          case PointsTo(loc, offset, value) =>
            value match {
              case key: Var =>
                store.get(key) match {
                  case None =>
                    store += (key -> HeapConst(curr))
                    curr += 100
                  case Some(e) =>
                    ()
                }
              case _ => ()
            }
            store.get(loc.asInstanceOf[Var]) match {
              case None =>
                store += (loc.asInstanceOf[Var] -> HeapConst(curr))
                curr += 100
              case Some(HeapConst(e)) =>
                loc match {
                  case Var(nm) =>
                    store += (Var(nm + "+" + offset.toString) -> HeapConst(e+offset))
                }
              case Some(_) => ()
            }
          case _ => ()
        }
      }
      //set up init heap and fin heap
      for (x <- pre.chunks){
        x match {
          case PointsTo(loc, offset, value) =>
            var loc_var = loc match {
              case Var(nm) =>
                if (offset == 0){
                  Var(nm)
                } else {
                  Var(nm + "+" +offset.toString)
                }
            }

            value match {
              case val_var: Var =>
                store(loc_var) match {
                  case HeapConst(v) => init_heap += (v.asInstanceOf[Int] -> val_var)
                  case IntConst(v) => init_heap += (v.asInstanceOf[Int]-> val_var)
                  case _ => ()
                }
              case val_val: IntConst =>
                store(loc_var) match {
                  case HeapConst(v) => init_heap += (v.asInstanceOf[Int] -> val_val)
                  case IntConst(v) => init_heap += (v.asInstanceOf[Int] -> val_val)
                  case _ => ()
                }
              case _ => ()
            }
        }
      }
      for (x <- post.chunks){
        x match {
          case PointsTo(loc, offset, value) =>
            val loc_var = loc.asInstanceOf[Var]
            value match {
              case val_var: Var =>
                store(loc_var) match {
                  case HeapConst(v) => fin_heap += (v.asInstanceOf[Int] -> val_var)
                  case IntConst(v) => fin_heap += (v.asInstanceOf[Int] -> val_var)
                  case _ => ()
                }
              case val_val : IntConst =>
                store(loc_var) match {
                  case HeapConst(v) => fin_heap += (v.asInstanceOf[Int]  -> val_val)
                  case IntConst(v) => fin_heap += (v.asInstanceOf[Int] -> val_val)
                  case _ => ()
                }
              case _ =>
            }
          case _ => ()
        }
      }
      eg = Some(List((store, init_heap, fin_heap)))
    }
    val prog = res.get
    val (specs, predEnv, funcEnv, body) = preprocessProgram(prog, params)

    if (specs.lengthCompare(1) != 0) {
      throw SynthesisException("Expected a single synthesis goal")
    }

    val spec = specs.head
    val env = Environment(predEnv, funcEnv, params, new SynStats(params.timeOut))
    val synthesizer = createSynthesizer(env)

    env.stats.start()
    val sresult = synthesizer.synthesizeProc(spec, env, body, eg)
    val duration = env.stats.duration

    SynStatUtil.log(testName, duration, params, spec, sresult._1, sresult._2)
    def printHotNode(hotNode: AndNode, descs: Int): String =
      s"${hotNode.rule.toString} at depth ${hotNode.parent.depth} with ${descs} descendants expanded"

    def printRuleApplication(name: String, stat: RuleStat): String =
      s"$name: succeeded ${stat.numSuccess} times (${stat.timeSuccess}ms), failed ${stat.numFail} times (${stat.timeFail}ms)"

    def printStats(stats: SynStats) = {
      testPrintln(s"Goals generated: ${stats.numGoalsGenerated}")
      testPrintln(s"Goals expanded: ${stats.numGoalsExpanded}")
      testPrintln(s"And-nodes backtracked: ${stats.numGoalsFailed}")
      testPrintln(s"Maximum worklist size: ${stats.maxWorklistSize}")
      testPrintln(s"Maximum goal depth: ${stats.maxGoalDepth}")
      testPrintln(s"Final memo size: ${stats.memoSize}")
      testPrintln(s"Final size of SMT cache: ${stats.smtCacheSize}")
      testPrintln(s"Time spent cycling: ${stats.timeCycling}ms")
//      val hotNodesString = stats.hotNodes(5).map{case (n, s) => printHotNode(n, s)}.mkString("\n")
//      testPrintln(s"Hot nodes:\n $hotNodesString")
      val expensiveRuleString = stats.expensiveRules(5).map {case (n, s) => printRuleApplication(n, s)}.mkString("\n")
      testPrintln(s"Expensive rules:\n$expensiveRuleString\n")
      testPrintln(s" ${stats.applications}")
      testPrintln(StopWatch.summary.toString)
    }

    sresult._1 match {
      case Nil =>
        printStats(sresult._2)
        throw SynthesisException(s"Failed to synthesise:\n$sresult")
      case procs =>
        val result = if (params.printSpecs) {
          procs.map(p => {
            val (pre, post) = (p.f.pre.pp.trim, p.f.post.pp.trim)
            List(pre, post, p.pp.trim).mkString("\n")
          }).mkString("\n\n")
        } else {
          val funstr = procs.map(proc => proc match {
            case Procedure(f,_) => f.pp
          })
          val bodystr = procs.map(proc => proc match {
            case Procedure(_,body) => body.pp
          })
          "Function spec:  "  + funstr.mkString("\n") + "\n\n" +
            "Body:  " + bodystr.mkString("\n") + "\n\n"+
          procs.map(_.pp.trim).mkString("\n\n") + "\n" +
            "AST for Pre: \n" + procs.head.f.pre.toString() +  "\n\n" +
            "AST for Post: \n" + procs.head.f.post.toString() + "\n\n" +
            "AST for Body: \n" + procs.head.body.toString()

        }
        if (params.printStats) {
          testPrintln(s"\n[$testName]:", Console.MAGENTA)
          testPrintln(params.pp)
          testPrintln(s"${spec.pp}\n", Console.BLUE)
          testPrintln(s"Successfully synthesised in $duration milliseconds:", Console.GREEN)
          printStats(sresult._2)

          testPrintln(result, Console.YELLOW)
          testPrintln("-----------------------------------------------------")
        } else {
          println(result)
        }
        if (out != noOutputCheck) {
          val tt = out.trim.lines.map(_.trim).toList
          val res = result.trim.lines.toList.map(_.trim)
          if (params.assertSuccess && res != tt) {
            throw SynthesisException(s"\nThe expected output\n$tt\ndoesn't match the result:\n$res")
          }
        }
        if (params.interactive) {
          testPrintln(sresult._2.getExpansionChoices.mkString("\n"))
          testPrintln("-----------------------------------------------------")
        }
        if (params.certTarget != null) {
          val certTarget = params.certTarget
          val targetName = certTarget.name
          val certificate = certTarget.certify(procs.head, env)
          if (params.certDest == null) {
            testPrintln(s"\n$targetName certificate:", Console.MAGENTA)
            testPrintln(certificate.body)
          } else {
            val path = Paths.get(params.certDest.getCanonicalPath, certificate.fileName).toFile
            new PrintWriter(path) { write(certificate.body); close() }
            testPrintln(s"\n$targetName certificate exported to $path", Console.MAGENTA)
          }
        }
    }
  }

  def getDefs(defFiles: List[File]): String = {
    if (defFiles.isEmpty) return ""
    assert(defFiles.size == 1, "More than one file with definitions in the folder")
    val file = new File(defFiles.head.getAbsolutePath)
    Source.fromFile(file).getLines.toList.mkString("\n")
  }

  def runAllTestsFromDir(dir: String) {
    val path = List(rootDir, dir).mkString(File.separator)
    val testDir = new File(path)
    if (testDir.exists() && testDir.isDirectory) {
      // Create log file
      SynStatUtil.init(defaultConfig)
      // Get definitions
      val defs = getDefs(testDir.listFiles.filter(f => f.isFile && f.getName.endsWith(s".$defExtension")).toList)
      // Get specs
      val tests = testDir.listFiles.filter(f => f.isFile
        && (f.getName.endsWith(s".$testExtension") ||
            f.getName.endsWith(s".$sketchExtension"))).toList
      for (f <- tests) {
        val (testName, desc, in, out, examples, params) = getDescInputOutput(f.getAbsolutePath)
        val fullInput = List(defs, in).mkString("\n")
        doRun(testName, desc, fullInput, out, examples, params)
      }
    }
  }

  def runSingleTestFromDir(dir: String, fname: String, params: SynConfig = defaultConfig) {
    var testDir = new File(dir)
    if (!testDir.exists()) {
      val path = List(rootDir, dir).mkString(File.separator)
      println(s"Trying the path $path")
      testDir = new File(path)
      if (!testDir.exists()) {
        System.err.println(s"Found no directory $dir.")
        return
      }
    }
    if (testDir.exists() && testDir.isDirectory) {
      // Maybe create log file (depending on params)
      SynStatUtil.init(params)
      // Get definitions
      val defs = getDefs(testDir.listFiles.filter(f => f.isFile && f.getName.endsWith(s".$defExtension")).toList)
      // Get specs
      val tests = testDir.listFiles.filter(f => f.isFile
        && (f.getName.endsWith(s".$testExtension") ||
            f.getName.endsWith(s".$sketchExtension"))).toList
      tests.find(f => f.getName == fname) match {
        case Some(f) =>
          val (testName, desc, in, out, examples, allParams) = getDescInputOutput(f.getAbsolutePath, params)
          val fullInput = List(defs, in).mkString("\n")
          doRun(testName, desc, fullInput, out, examples, allParams)
        case None =>
          System.err.println(s"No file with the name $fname found in the directory $dir.")
      }
    }
  }
  def runSingleTestFromDirWithExamples(dir: String, fname: String, params: SynConfig = defaultConfig, examples: Examples) {
    var testDir = new File(dir)
    if (!testDir.exists()) {
      val path = List(rootDir, dir).mkString(File.separator)
      println(s"Trying the path $path")
      testDir = new File(path)
      if (!testDir.exists()) {
        System.err.println(s"Found no directory $dir.")
        return
      }
    }
    if (testDir.exists() && testDir.isDirectory) {
      // Maybe create log file (depending on params)
      SynStatUtil.init(params)
      // Get definitions
      val defs = getDefs(testDir.listFiles.filter(f => f.isFile && f.getName.endsWith(s".$defExtension")).toList)
      // Get specs
      val tests = testDir.listFiles.filter(f => f.isFile
        && (f.getName.endsWith(s".$testExtension") ||
        f.getName.endsWith(s".$sketchExtension"))).toList
      tests.find(f => f.getName == fname) match {
        case Some(f) =>
          val (testName, desc, in, out, examples, allParams) = getDescInputOutput(f.getAbsolutePath, params)
          val fullInput = List(defs, in).mkString("\n")
          doRunWithExamples(testName, desc, fullInput, out, examples, allParams)
        case None =>
          System.err.println(s"No file with the name $fname found in the directory $dir.")
      }
    }
  }
  def runSingleTestFromDirWithHints(dir: String, fname: String, params: SynConfig = defaultConfig) {
    var testDir = new File(dir)
    if (!testDir.exists()) {
      val path = List(rootDir, dir).mkString(File.separator)
      println(s"Trying the path $path")
      testDir = new File(path)
      if (!testDir.exists()) {
        System.err.println(s"Found no directory $dir.")
        return
      }
    }
    if (testDir.exists() && testDir.isDirectory) {
      // Maybe create log file (depending on params)
      SynStatUtil.init(params)
      // Get definitions
      val defs = getDefs(testDir.listFiles.filter(f => f.isFile && f.getName.endsWith(s".$defExtension")).toList)
      // Get specs
      val tests = testDir.listFiles.filter(f => f.isFile
        && (f.getName.endsWith(s".$testExtension") ||
        f.getName.endsWith(s".$sketchExtension"))).toList
      tests.find(f => f.getName == fname) match {
        case Some(f) =>
          val (testName, desc, in, out, examples, allParams) = getDescInputOutput(f.getAbsolutePath, params)
          val fullInput = List(defs, in).mkString("\n")
          doRunWithHints(testName, desc, fullInput, out, examples, allParams)
        case None =>
          System.err.println(s"No file with the name $fname found in the directory $dir.")
      }
    }
  }




  def removeSuffix(s: String, suffix: String): String = {
    if (s.endsWith(suffix)) s.substring(0, s.length - suffix.length) else s
  }

}

