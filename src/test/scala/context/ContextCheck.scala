package com.glassbeam.context

import java.io.{ File, PrintWriter }

//import com.glassbeam.loader.Init
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ParallelTestExecution, PropSpec, Suites}

import scala.collection.immutable.HashMap
import scala.language.postfixOps

trait ContextTest {
  Init.inittype = Init.Test
  val genStringNonEmpty: Gen[String] = Gen.alphaStr.suchThat(s => s.length > 0)
  val genStringTuple: Gen[(String, String)] = Gen.alphaStr.map(x => (x, x))
  val genMap = Gen.mapOf(genStringTuple).suchThat(m => m.nonEmpty).suchThat(n => {
    var ne = true
    for (k <- n.keys; if ne) {
      if (k.isEmpty || n(k).isEmpty) ne = false else ne = true
    }
    ne
  })
  val genNumber = Gen.choose(1, 1000)
  val genStringList = Gen.listOf(genStringNonEmpty).suchThat(f => f.nonEmpty).suchThat(n => {
    var ne = true
    for (e <- n; if ne) {
      if (e.isEmpty) ne = false else ne = true
    }
    ne
  })

  def genStringListWithStringSizeLessThan(size: Int) = genStringList.suchThat(f => f.size < size)

  val genStringListLessTen = genStringListWithStringSizeLessThan(10)
  val genOperation = Gen.oneOf("==", ">=", "<=", ">", "<")

  val emptyCR = ContextReason(HashMap[String, String](), "")
  val emptyCEFA = ContextExecFnArguments(emptyCR, null, -1)

  def getContextLine(contextline: String) = ContextClassArguments(contextline, 1, "test_ec", "test_mfr", "test_prod", "test_sch")

  def getCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, null, -1)

  val test_filename = "testfile.log"
  val test_path = "/tmp/" + test_filename

  def writeToNewFile(randomstring: String): File = {
    val f = new File(test_path)
    f.delete()

    val p = new PrintWriter(f)
    p.print(randomstring)
    p.close()
    f
  }
}

class LiteralTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context literal assignments") {
    forAll(genStringNonEmpty, genStringNonEmpty) {
      (lhs: String, rhs: String) =>
        val cca = getContextLine(lhs + "='" + rhs + "'")
        val cr = new Literal(cca).execute(emptyCEFA)
        // println("returned context = " + cr)
        assert(cr.contextStrings.contains(lhs) &&
          cr.contextStrings(lhs).equals(rhs))
    }
  }
}

class ConcatTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context concat function") {
    forAll(genMap) {
      (stringMap: Map[String, String]) =>
        var hm = HashMap[String, String]()
        stringMap.foreach(x => hm += (x._1 -> x._2))
        val cr = ContextReason(hm, "")
        val arg = stringMap.keys.mkString(", ")
        val cca = getContextLine("x = concat(" + arg + ")")
        val newcr = new Concat(cca).execute(getCEFA(cr))
        val result = stringMap.values.mkString
        // println("stringMap = " + stringMap + " result = " + result + "newcr = " + newcr)
        assert(newcr.contextStrings.contains("x") &&
          newcr.contextStrings("x").equals(result))
    }
  }
}

class CoalesceTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context coalesce function") {
    forAll(genStringNonEmpty, genStringList) {
      (singleval: String, slist: List[String]) =>
        var hm = HashMap[String, String]()
        slist.foreach(x => hm += (x -> ""))
        hm += (singleval -> singleval)
        val cr = ContextReason(hm, "")
        val arg = hm.keys.mkString(", ")
        val cca = getContextLine("x = coalesce(" + arg + ")")
        // println(s"cr = $cr, arg = $cca")
        val newcr = new Coalesce(cca).execute(getCEFA(cr))
        // println("newcr = " + newcr)
        assert(newcr.contextStrings.contains("x") &&
          newcr.contextStrings("x").equals(singleval))
    }
  }
}

class AssertTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context assert function") {
    forAll(genStringNonEmpty, Gen.alphaStr) {
      (key: String, value: String) =>
        val hm = HashMap[String, String](key -> value)
        val cr = ContextReason(hm, "")
        val cca = getContextLine("f.assert(" + key + ")")
        val newcr = new Assert(cca).execute(getCEFA(cr))
        assert(value.isEmpty ^ newcr.reason.isEmpty)
    }
  }
}

class LgrepTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context lgrep function") {
    forAll(genStringList) {
      (slist: List[String]) =>
        var lregex: String = ""
        for (i <- 0 until slist.size - 1) lregex = lregex + """\\([^\\]+)"""
        val vars = slist
        val lhs = vars.mkString("(", ",", ")")
        lregex = lregex + """\\*([^\\]*)\\*"""
        val rhs = s"l.grepX /$lregex/"
        val cca = getContextLine(s"$lhs=$rhs")
        // println("cca = " + cca)

        val value = slist.mkString("\\", "\\", "\\")
        var hm = HashMap[String, String]()
        hm += ("X" -> value)
        val cr = ContextReason(hm, "")
        // println("cr = " + cr)

        var found = true
        var i = 0
        val newcr = new Lgrep(cca).execute(getCEFA(cr))
        // println("newcr = " + newcr)
        for (l <- vars if found) {
          // println(s"newcr $l found = " + newcr.contextStrings.get(l.toString) + " list elem = " + slist(i))
          newcr.contextStrings.get(l.toString) match {
            case None => found = false
            case Some(v) => found = v == slist(i)
          }
          i = i + 1
        }
        assert(found)
    }
  }
}

class LCMPSTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context l.customer and similar functions") {
    forAll(genStringNonEmpty) {
      (seed: String) =>
        val ccac = ContextClassArguments("customer=l.customer", 1, "", "", "", "")
        val ccap = ContextClassArguments("product=l.product", 1, s"${seed}_ec", s"${seed}_mfr", s"${seed}_prod", s"${seed}_sch")
        val ccam = ContextClassArguments("manufacturer=l.manufacturer", 1, s"${seed}_ec", s"${seed}_mfr", s"${seed}_prod", s"${seed}_sch")
        val ccas = ContextClassArguments("schema=l.schema", 1, s"${seed}_ec", s"${seed}_mfr", s"${seed}_prod", s"${seed}_sch")
        val ccasnow = ContextClassArguments("now=s.now", 1, s"${seed}_ec", s"${seed}_mfr", s"${seed}_prod", s"${seed}_sch")

        val cr = ContextReason(HashMap[String, String]("ec" -> "test_ec"), "")
        val crc = new Lcustomer(ccac).execute(getCEFA(cr))
        val crp = new Lproduct(ccap).execute(getCEFA(crc))
        val crm = new Lmanufacturer(ccam).execute(getCEFA(crp))
        val crs = new Lschema(ccas).execute(getCEFA(crm))
        val crn = new Snow(ccasnow).execute(getCEFA(crs))

        // println("crn = " + crn)

        crn.contextStrings.get("customer").contains(s"${seed}_ec") === true
        crn.contextStrings.get("product").contains(s"${seed}_prod") === true
        crn.contextStrings.get("manufacturer").contains(s"${seed}_mfr") === true
        crn.contextStrings.get("schema").contains(s"${seed}_sch") === true
        (crn.contextStrings.get("now").get.toLong < (System.currentTimeMillis - 1000)) === true
        (crn.contextStrings.get("now").get.toLong > System.currentTimeMillis) === true

    }
  }
}

class FileSimpleTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context simple file functions") {
    forAll(genStringNonEmpty) {
      (randomstring: String) =>
        val f = writeToNewFile(randomstring)

        val fdateline = getContextLine("date=f.date")
        val fnameline = getContextLine("name=f.name")
        val fpathline = getContextLine("path=f.path")
        val flengline = getContextLine("fl=f.length")

        val cr = ContextReason(HashMap[String, String](), "")
        def getFileCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, f, -1)
        val crfd = new Fdate(fdateline).execute(getFileCEFA(cr))
        val crfn = new Fname(fnameline).execute(getFileCEFA(crfd))
        val crfp = new Fpath(fpathline).execute(getFileCEFA(crfn))
        val crfl = new Flength(flengline).execute(getFileCEFA(crfp))

        // println(s"crfl = $crfl, t = ${System.currentTimeMillis()} flen = ${f.length()}")

        var success = true
        if (crfl.contextStrings.get("date").get.toLong < (System.currentTimeMillis - 5000))
          success = false
        else if (!crfl.contextStrings.get("name").contains(test_filename))
          success = false
        else if (!crfl.contextStrings.get("path").contains(test_path))
          success = false
        else if (!crfl.contextStrings.get("fl").contains(randomstring.length.toString))
          success = false

        f.delete()

        assert(success)
    }
  }
}

trait DateHelper {
  val fweekdays: Array[String] = Array("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  val sweekdays: Array[String] = Array("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  val genDayInWeek: Gen[String] = Gen.oneOf(fweekdays ++ sweekdays ++ fweekdays.map(_.toUpperCase()) ++ sweekdays.map(_.toUpperCase()))
  //ToDo: will have to create more such generators
}

class SDF2EPOCHTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest with DateHelper {
  property("test context time conversion") {
    forAll(genStringNonEmpty) {
      (lhs: String) =>
        val refdatestring = "Wed Jul 09 00:27:00 PDT 2014"
        val refindian = "Wed Jul 09 12:57:00 IST 2014"
        val refdateepoch = "1404890820000"

        val cca = getContextLine(lhs + s"='$refdatestring'")
        val cr = ContextReason(HashMap[String, String](), "")
        val cefa1 = getCEFA(cr)
        val newcr = new Literal(cca).execute(cefa1)
        // println(s"newcr = $newcr")

        val sel = getContextLine(s"obs_ts=SDF2EPOCH 'EEE MMM dd HH:mm:ss zzz yyyy', $lhs")
        val cefa2 = getCEFA(newcr)
        val newercr = new SDF2EPOCH(sel).execute(cefa2)
        // println(s"newercr = $newercr")

        var success = newercr.contextStrings.get("obs_ts") match {
          case None => false
          case Some(x) => x == refdateepoch
        }

        if (success) {
          val esl = getContextLine(s"obs_reverse=EPOCH2SDF 'EEE MMM dd HH:mm:ss z yyyy', obs_ts")
          val cefa3 = getCEFA(newercr)
          val newestcr = new EPOCH2SDF(esl).execute(cefa3)
          // println(s"newestcr = $newestcr")

          success = newestcr.contextStrings.get("obs_reverse") match {
            case None => false
            case Some(x) => x == refdatestring || x == refindian
          }
        }

        assert(success)
    }
  }
}

class FnamepathGrepTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context file name/path grep") {
    forAll(genStringNonEmpty) {
      (lhs: String) =>
        val f = writeToNewFile(lhs)
        def getFileCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, f, -1)
        val cr = ContextReason(HashMap[String, String](), "")

        val ccafn = getContextLine(s"filenm1=fname.grep /testfile.(.*)/")
        val cefa1 = getFileCEFA(cr)
        val newcr = new FnameGrep(ccafn).execute(cefa1)

        val ccafp = getContextLine(s"filenm2=fpath.grep /.*testfile.(.*)/")
        val cefa2 = getFileCEFA(newcr)
        val newercr = new FpathGrep(ccafp).execute(cefa2)
        // println(s"newercr = $newercr")

        val success = newercr.contextStrings.get("filenm1") match {
          case None => false
          case Some(x) =>
            if (x != "log") false
            else newercr.contextStrings.get("filenm2") match {
              case None => false
              case Some(y) => y == "log"
            }
        }

        f.delete()
        assert(success)
    }
  }
}

class FgrepTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test fgrep") {
    forAll(genStringNonEmpty) {
      (lhs: String) =>
        val rhs = "Thu Mar  3 13:56:06 PST 2011"
        val f = writeToNewFile(s"System Time:$rhs")
        def getFileCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, f, -1)
        val cr = ContextReason(HashMap[String, String](), "")

        val ccafg = getContextLine("""system_timeA=f.grep /System\s+Time\:(.*)/""")
        val cefa1 = getFileCEFA(cr)
        val newcr = new Fgrep(ccafg).execute(cefa1)

        /*val ccamg = getContextLine("""system_timeB=m.grep /System\s+Time\:(.*)/ /System\s+Time\:(.*)/""")
        val cefa2 = getFileCEFA(newcr)
        val newercr = new FpathGrep(ccamg).execute(cefa2) */
        println(s"newercr = $newcr")

        val success = newcr.contextStrings.get("system_timeA") match {
          case None => false
          case Some(x) => x == rhs
        }

        f.delete()
        assert(success)
    }
  }
}

class BnamepathGrepTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test context bundle name/path grep") {
    forAll(genStringNonEmpty) {
      (lhs: String) =>
        val f = writeToNewFile(lhs)
        def getFileCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, f, -1)
        val cr = ContextReason(HashMap[String, String](), "")

        val ccafn = getContextLine(s"filenm1=fname.grep /testfile.(.*)/")
        val cefa1 = getFileCEFA(cr)
        val newcr = new FnameGrep(ccafn).execute(cefa1)

        val ccafp = getContextLine(s"filenm2=fpath.grep /.*testfile.(.*)/")
        val cefa2 = getFileCEFA(newcr)
        val newercr = new FpathGrep(ccafp).execute(cefa2)
        // println(s"newercr = $newercr")

        val success = newercr.contextStrings.get("filenm1") match {
          case None => false
          case Some(x) =>
            if (x != "log") false
            else newercr.contextStrings.get("filenm2") match {
              case None => false
              case Some(y) => y == "log"
            }
        }

        f.delete()
        assert(success)
    }
  }
}

class AssertNumericTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test assertNumeric") {
    forAll(genOperation) {
      (operation: String) =>
        val ccA = getContextLine("""A='1'""")
        val ccB = getContextLine("""B='1'""")
        val ccC = getContextLine("""C='2'""")

        val ccAssertAB = getContextLine(s"eval_AB=assertNumeric(A, B, $operation)")
        val ccAssertBC = getContextLine(s"eval_BC=assertNumeric(B, C, $operation)")
        val ccAssertAC = getContextLine(s"eval_AC=assertNumeric(A, C, $operation)")

        val cr = ContextReason(HashMap[String, String](), "")
        val crA = new Literal(ccA).execute(getCEFA(cr))
        val crB = new Literal(ccB).execute(getCEFA(crA))
        val crC = new Literal(ccC).execute(getCEFA(crB))

        val crAB = new AssertNumeric(ccAssertAB).execute(getCEFA(crC))
        val crBC = new AssertNumeric(ccAssertBC).execute(getCEFA(crAB))
        val crAC = new AssertNumeric(ccAssertAC).execute(getCEFA(crBC))

        println(s"crAC = $crAC")

        val success = operation match {
          case "==" =>
            crAC.contextStrings.get("eval_AB") match {
              case None => false
              case Some(x) =>
                if (x != "true") false
                else {
                  crAC.contextStrings.get("eval_BC") match {
                    case None => false
                    case Some(y) =>
                      if (y == "true") false
                      else {
                        crAC.contextStrings.get("eval_AC") match {
                          case None => false
                          case Some(z) => z != "true"
                        }
                      }
                  }
                }
            }
          case ">" =>
            crAC.contextStrings.get("eval_AB") match {
              case None => false
              case Some(x) =>
                if (x == "true") false
                else {
                  crAC.contextStrings.get("eval_BC") match {
                    case None => false
                    case Some(y) =>
                      if (y == "true") false
                      else {
                        crAC.contextStrings.get("eval_AC") match {
                          case None => false
                          case Some(z) => z != "true"
                        }
                      }
                  }
                }
            }
          case "<" => true
          case ">=" => true
          case "<=" => true
          case _ => true
        }
        assert(success)
    }
  }
}

/* functions which dont need H2 database =>
    XmlValue

    AssertUncompressionFail
    AssertTruthy
*/

class EncodingTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {

  import com.glassbeam.context.TestHelpers._

  property("test encoding") {
    forAll(genStringNonEmpty) {
      (encoding: String) =>
        val rhs = encoding
        val f = s"$rhs.txt".toFile
        def getFileCEFA(cr: ContextReason): ContextExecFnArguments = ContextExecFnArguments(cr, f, -1)
        val cr = ContextReason(HashMap[String, String](), "")

        val ccafg = getContextLine(s"""t.encoding=('$encoding',/$rhs.txt/)""")
        val cefa1 = getFileCEFA(cr)
        val newcr = new Encoding(ccafg).execute(cefa1)
        println(s"newercr = $newcr")
        f.delete()

        assert(newcr.contextStrings.get("t.encoding") === Some(rhs))
    }
  }
}

class AssertTruthyTest extends PropSpec with GeneratorDrivenPropertyChecks with ContextTest {
  property("test assert truthy") {
    forAll(genStringNonEmpty, genStringNonEmpty) {
      (lhs1: String, lhs2: String) =>
        val ccatrue = getContextLine(s"$lhs1='true'")
        val ccafalse = getContextLine(s"$lhs2='false'")

        val assertruthytrue = getContextLine(s"f.assertTruthy($lhs1)")
        val assertruthyfalse = getContextLine(s"f.assertTruthy($lhs2)")

        val cr = ContextReason(HashMap[String, String](), "")
        val crA = new Literal(ccatrue).execute(getCEFA(cr))
        val crB = new Literal(ccafalse).execute(getCEFA(crA))

        val crC = new AssertTruthy(assertruthytrue).execute(getCEFA(crB))
        val crD = new AssertTruthy(assertruthyfalse).execute(getCEFA(crC))

        assert(!crD.reason.isEmpty)
    }
  }
}

class ContextCheckTestSuite extends Suites(
  new LiteralTest, new ConcatTest, new CoalesceTest, new AssertTest, new LgrepTest, new LCMPSTest,new EncodingTest,
  new FileSimpleTest, new SDF2EPOCHTest, new FnamepathGrepTest, new FgrepTest, new BnamepathGrepTest,
  new AssertNumericTest, new AssertTruthyTest) with ParallelTestExecution
