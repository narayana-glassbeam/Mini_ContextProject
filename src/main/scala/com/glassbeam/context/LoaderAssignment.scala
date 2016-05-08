package com.glassbeam.context

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import java.util.concurrent.Callable

import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.StartupConfig._
import com.glassbeam.model._
import com.google.common.cache.{Cache, CacheBuilder}
import com.glassbeam.context.ContextHelpers._
import com.ximpleware.{VTDGen, VTDNav}
import icons.{Icon, XmlParser}

import scala.collection.immutable.HashMap
import scala.io.Source
import scala.language.postfixOps
import scala.util.control.NonFatal
import scala.util.matching.Regex

object Literal extends LoaderContextAssignment {
  val rhsRegex = """'(.*)'\s*$""".r

  def getObject(carg: ContextClassArguments) = new Literal(carg)
}

class Literal(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Literal) with MutableLoaderFunction {
  private def literal(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val value = texts.head.trim
    ContextReason(cefa.cr.contextStrings + (lhs -> value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(literal, cefa)
}

/*
 * Context function to concatenate multiple values
 */
object Concat extends LoaderContextAssignment {
  val rhsRegex = """^s.concat\((.*)\)\s*$""".r

  def getObject(carg: ContextClassArguments) = new Concat(carg)
}

class Concat(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Concat) with MutableLoaderFunction {
  private def concat(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val text = texts.head.trim
    val pvals = for (pval <- text.split(",")) yield getParm(pval.trim, cefa.cr.contextStrings)
    ContextReason(cefa.cr.contextStrings + (lhs -> pvals.mkString), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(concat, cefa)
}

/**
  * Context function to Coalesce multiple values
  */
object Coalesce extends LoaderContextAssignment {
  val rhsRegex = """^s.coalesce\((.+?)\)\s*$""".r

  def getObject(carg: ContextClassArguments) = new Coalesce(carg)
}

class Coalesce(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Coalesce) with MutableLoaderFunction {

  import Coalesce._

  private def coalesce(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    // the expected format of str is val1,,,, or val1, , , , ,
    // check if the str contains
    val text = texts.head.trim
    val sysId: Array[String] = text.split(",").view.map(s => getParm(s.trim, cefa.cr.contextStrings)).filter(x => x.nonEmpty).force
    (sysId.nonEmpty, sysId.length == 1) match {
      case (true, true) =>
        ContextReason(cefa.cr.contextStrings + (lhs -> sysId(0)), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
      case (false, _) =>
        val err = s"coalesce error: none of the variable specified had a value. $lhs array = $texts"
        logger.error(mps, err)
        ContextReason(cefa.cr.contextStrings + (lhs -> ""), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
      case (true, false) =>
        logger.info(mps, s"coalesce error: more than one variable specified has a value. $lhs array = $texts. picking the first = ${sysId(0)}")
        ContextReason(cefa.cr.contextStrings + (lhs -> sysId(0)), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
    }
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(coalesce, cefa)
}

/**
  * L.Grep
  */
object Lgrep extends LoaderContextAssignment {
  val rhsRegex = """^l.grep(\w+)\s+/(.+?)/\s*$""".r

  def getObject(carg: ContextClassArguments) = new Lgrep(carg)
}

class Lgrep(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Lgrep) with MutableLoaderFunction {

  import Lgrep._

  private val eqSplit = fullRegex.unapplySeq(carg.context).get
  private val (l, r) = (eqSplit.head.trim, eqSplit(1).trim)
  private val lgrepdest: Array[String] = l.substring(1, l.length() - 1).split(",")
  private val rSplit = rhsRegex.unapplySeq(r).get
  private val (r1, r2) = (rSplit.head, rSplit(1))

  def execute(cefa: ContextExecFnArguments): ContextReason = {
    var cr2 = cefa.cr
    cefa.cr.contextStrings.get(r1) match {
      case None =>
        logger.debug(mps, s"Nothing to lgrep on since no previous fgrep done to assign value to $r1")
      case Some(fgrepVal) =>
        // logger.debug(mps, s"found fgrep val $fgrepVal")
        val re = r2.r
        re findFirstMatchIn fgrepVal match {
          case None =>
            for (lCol <- lgrepdest)
              cr2 = ContextReason(cr2.contextStrings + (lCol -> ""), cr2.reason, cr2.failure, cefa.cr.bproperties)

          case Some(m) => if (m.groupCount > 0) {
            for (i <- 1 to m.groupCount) {
              logger.debug(mps, s"found lgrep ${lgrepdest(i - 1).trim} => ${m.group(i)}")
              cr2 = ContextReason(cr2.contextStrings + (lgrepdest(i - 1).trim -> m.group(i)), cr2.reason, cr2.failure, cefa.cr.bproperties)
            }
          }
        }
    }
    cr2
  }
}

/*
  Lcustomer
 */
object Lcustomer extends LoaderContextAssignment {
  val rhsRegex = """^l.customer\s*$""".r

  def getObject(carg: ContextClassArguments) = new Lcustomer(carg)
}

class Lcustomer(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Lcustomer) with MutableLoaderFunction {

  import Lcustomer._

  private def lcustomer(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val customerOption = cefa.cr.contextStrings.get("ec")
    customerOption match {
      case Some(customer) =>
        ContextReason(cefa.cr.contextStrings + (lhs -> customer), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
      case None =>
        logger.error(mps, s"ec not found in context, cefa = $cefa")
        cefa.cr
    }
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(lcustomer, cefa)
}

/*
  Lproduct
 */
object Lproduct extends LoaderContextAssignment {
  val rhsRegex = """^l.product\s*$""".r

  def getObject(carg: ContextClassArguments) = new Lproduct(carg)
}

class Lproduct(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Lproduct) with MutableLoaderFunction {
  private def lproduct(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> carg.product), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(lproduct, cefa)
}

/*
  Lmanufacturer
 */
object Lmanufacturer extends LoaderContextAssignment {
  val rhsRegex = """^l.manufacturer\s*$""".r

  def getObject(carg: ContextClassArguments) = new Lmanufacturer(carg)
}

class Lmanufacturer(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Lmanufacturer) with MutableLoaderFunction {
  private def lmanufacturer(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> carg.manufacturer), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(lmanufacturer, cefa)
}

/*
  Lschema
 */
object Lschema extends LoaderContextAssignment {
  val rhsRegex = """^l.schema\s*$""".r

  def getObject(carg: ContextClassArguments) = new Lschema(carg)
}

class Lschema(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Lschema) with MutableLoaderFunction {
  private def lschema(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> carg.schema), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(lschema, cefa)
}

/*
  Snow
 */
object Snow extends LoaderContextAssignment {
  val rhsRegex = """^s.now\s*$""".r

  def getObject(carg: ContextClassArguments) = new Snow(carg)
}

class Snow(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Snow) with MutableLoaderFunction {
  private def snow(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> System.currentTimeMillis.toString), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(snow, cefa)
}

/*
  Fdate
 */
object Fdate extends LoaderContextAssignment {
  val rhsRegex = """^f.date\s*$""".r

  def getObject(carg: ContextClassArguments) = new Fdate(carg)
}

class Fdate(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Fdate) with MutableLoaderFunction {
  private def fdate(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> Option(cefa.file).fold("None")(_.lastModified.toString)), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fdate, cefa)
}

/*
  Fname
 */
object Fname extends LoaderContextAssignment {
  val rhsRegex = """^f.name\s*$""".r

  def getObject(carg: ContextClassArguments) = new Fname(carg)
}

class Fname(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Fname) with MutableLoaderFunction {
  private def fname(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> cefa.file.getName), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fname, cefa)
}

/*
  Fpath
 */
object Fpath extends LoaderContextAssignment {
  val rhsRegex = """^f.path\s*$""".r

  def getObject(carg: ContextClassArguments) = new Fpath(carg)
}

class Fpath(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Fpath) with MutableLoaderFunction {
  private def fpath(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> cefa.file.getPath), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fpath, cefa)
}

/*
  Flength
 */
object Flength extends LoaderContextAssignment {
  val rhsRegex = """^f.length\s*$""".r

  def getObject(carg: ContextClassArguments) = new Flength(carg)
}

class Flength(carg: ContextClassArguments) extends AbstractLoaderContext(carg, Flength) with MutableLoaderFunction {
  private def flength(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> cefa.file.length().toString), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(flength, cefa)
}

/*
  Bname
 */
object Bname extends LoaderContextAssignment {
  val rhsRegex = """^b.name\s*$""".r
  val bundleNameObtainer: File => String = file => getBundleNameSize(file.getAbsolutePath)._1

  def getObject(carg: ContextClassArguments) = new Bname(carg,bundleNameObtainer)
}

class Bname(carg: ContextClassArguments,bundleNameObtainer: File => String) extends AbstractLoaderContext(carg, Bname) with MutableLoaderFunction {
  private def bname(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    if (cefa.file != null)
      ContextReason(cefa.cr.contextStrings + (lhs -> bundleNameObtainer(cefa.file)), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
    else
      cefa.cr
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(bname, cefa)
}

/*
  Bsize
 */
object Bsize extends LoaderContextAssignment {
  val rhsRegex = """^b.size\s*$""".r
   val bundleSizeObtainer: File => Long = file => getBundleNameSize(file.getAbsolutePath)._2

  def getObject(carg: ContextClassArguments) = new Bsize(carg, bundleSizeObtainer)
}

class Bsize(carg: ContextClassArguments,bundleSizeObtainer:File =>Long) extends AbstractLoaderContext(carg, Bsize) with MutableLoaderFunction {
  private def bsize(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason =
    ContextReason(cefa.cr.contextStrings + (lhs -> bundleSizeObtainer(cefa.file).toString), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(bsize, cefa)
}

/*
  AssertFileDuplicate
 */
object AssertFileDuplicate extends LoaderContextAssignment {
  val fnName = "f.duplicate"
  val rhsRegex = raw"""^$fnName\s*\((.+?)\)\s*$$""".r

  def getObject(carg: ContextClassArguments) = new AssertFileDuplicate(carg)
}

class AssertFileDuplicate(carg: ContextClassArguments) extends AbstractLoaderContext(carg, AssertFileDuplicate) with MutableLoaderFunction {

  import AssertFileDuplicate._

  protected def count(fname:String, loadId:Long, fullkey:String) = {
    val opsCnt = OpsDao.getCountByLoadIdNameState(fname, loadId, ProcessingState.Seen.id.toByte)
    val cnt = if (opsCnt == 0) {
      try {
        LogSignatureDao.getCountForKey(fullkey)
      } catch {
        case t: Exception => 0
      }
    } else 0 // if file was in Seen then it cannot be a duplicate as it has not been processed yet
    cnt
  }
  private def assertFileDuplicate(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    // return "key" if not- duplicate, "None" otherwise
    val text = texts.head.trim
    logger.debug(mps, s"$fnName: hit assert file duplicate. keys = $text")
    val fullkey = text.split(",")
      .map(_.trim)
      .foldLeft("")((accum, key) => accum + cefa.cr.contextStrings.getOrElse(key, {
        val err = s"$fnName: lookup failed for $key"
        logger.error(mps, err)
        return cefa.cr.copy(reason = cefa.cr.reason + err)
      }))
    val cnt = count(cefa.file.toString, cefa.loadId,fullkey)

    logger.debug(mps, s"$fnName: fullkey for duplicate check = [$fullkey] cnt of recs = $cnt")
    val key = if (cnt > 0) {
      // duplicate
      logger.debug(mps, "$fnName: records with same key = " + cnt)
      None.toString
    } else fullkey // not-duplicate
    ContextReason(cefa.cr.contextStrings + (lhs -> key), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(assertFileDuplicate, cefa)
}

/*
  SDF to epoch
 */
object SDF2EPOCH extends LoaderContextAssignment {
  val rhsRegex = """^sdf2epoch\s+'(.+?)'\s*,\s*([\w_]+)\s*$""".r

  def getObject(carg: ContextClassArguments) = new SDF2EPOCH(carg)
}

class SDF2EPOCH(carg: ContextClassArguments) extends AbstractLoaderContext(carg, SDF2EPOCH) with MutableLoaderFunction {

  import SDF2EPOCH._

  private def sdf2epoch(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    // todo: use Qsplitter instead to handle funky sdf patterns
    // def stripQuotePS(y: String) = y stripPrefix "'" stripSuffix "'"
    val (p, v) = (texts.head.trim, texts(1).trim)
    val sdfToEpo = if (p.isEmpty) {
      val err = s"CONTEXT/SDF2EPOCH: pattern is empty: $texts"
      logger.error(mps, err) // $l
      return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(Sdf2EpochError))
    } else if (cefa.cr.contextStrings.getOrElse(v, "").isEmpty) {
      val err = s"CONTEXT/SDF2EPOCH: $v is empty. texts = $texts"
      logger.error(mps, err) // $l
      "" // Don't hold file because of empty input
    } else {
      try {
        val sdf = new SimpleDateFormat(p)
        sdf.parse(cefa.cr.contextStrings.getOrElse(v, "")).getTime.toString
      } catch {
        case t: Exception =>
          val err = s"Could not parse date $v (${cefa.cr.contextStrings.getOrElse(v, "")}) with format ($p)"
          logger.error(t, mps, err)
          //return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(Sdf2EpochError))
          "" // Don't hold file because of wrong input. Specify an assert if required.
      }
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> sdfToEpo), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(sdf2epoch, cefa)
}

/*
  epoch to SDF
 */
object EPOCH2SDF extends LoaderContextAssignment {
  val rhsRegex = """^epoch2sdf\s+'(.+?)'\s*,\s*([\w_]+)\s*$""".r

  def getObject(carg: ContextClassArguments) = new EPOCH2SDF(carg)
}

class EPOCH2SDF(carg: ContextClassArguments) extends AbstractLoaderContext(carg, EPOCH2SDF) with MutableLoaderFunction {

  import EPOCH2SDF._

  private def epoch2sdf(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (p, v) = (texts.head.trim, texts(1).trim)
    val x = if (p.isEmpty) {
      val err = s"CONTEXT/EPOCH2SDF: pattern is empty: $texts"
      logger.error(mps, err) // $l
      return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(Epoch2SdfError))
    } else if (cefa.cr.contextStrings.getOrElse(v, "").isEmpty) {
      val err = s"CONTEXT/EPOCH2SDF: value is empty: $texts"
      logger.error(mps, err) // $l
      return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(Epoch2SdfError))
    } else {
      try {
        val sdf = new SimpleDateFormat(p)
        val value = cefa.cr.contextStrings.getOrElse(v, "0").toLong
        // We need epoch value in milliseconds, but may also get in seconds. For a faster way to differentiate, compare against a 1970's epoch value (consider the maximum digits which definitely belong to the 1970's, i.e. 99999999
        val MAX_1970_EPOCH = 99999999L
        val epoch = if (value / (MAX_1970_EPOCH * 1000) <= 0) value * 1000 else value
        val date = new Date(epoch)
        sdf.format(date)
      } catch {
        case t: Exception =>
          val err = s"CONTEXT/EPOCH2SDF: Could not parse epoch $v (${cefa.cr.contextStrings.getOrElse(v, "")}) with format ($p)"
          logger.error(t, mps, err)
          return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(Epoch2SdfError))
      }
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> x), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(epoch2sdf, cefa)
}

/*
  FnameGrep
 */
object FnameGrep extends LoaderContextAssignment {
  val rhsRegex = """^fname.grep\s+/(.+?)/\s*$""".r

  def getObject(carg: ContextClassArguments) = new FnameGrep(carg)
}

class FnameGrep(carg: ContextClassArguments) extends AbstractLoaderContext(carg, FnameGrep) with MutableLoaderFunction {
  private def fnameGrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val text = texts.head.trim
    val re = new Regex(text)
    val s = re findFirstMatchIn cefa.file.getName match {
      case None => ""
      case Some(x) => x.group(1)
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> s), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fnameGrep, cefa)
}

/*
  Fgrep
 */
// Todo remove this class as it only wraps a pure function as method 
abstract class FgrepHelper(carg: ContextClassArguments, ACO: AbstractContextObject) extends AbstractLoaderContext(carg, ACO) {
  protected def fgrep(f: File, cx: HashMap[String, String], r: Regex*): String = {
    val source = Source.fromFile(f)(getFileCodec(cx))
    val lines = source.getLines
    var re = r.head
    val end_r = if (r.size == 1) r.head else r.tail.head
    for (ln <- lines) {
      re findFirstMatchIn ln match {
        case None =>
        case Some(x) =>
          if (re.equals(end_r)) {
            source.close()
            return if (x.groupCount > 0) x.group(1) else ln
          } else {
            re = end_r
          }
      }
    }
    source.close()
    ""
  }
}

object Fgrep extends LoaderContextAssignment {
  val rhsRegex = """^f.grep\s+/(.+?)/\s*$""".r

  //Todo use this instead of FGrepHelper
  def fgrep(f: File, cx: HashMap[String, String], r: Regex*): String = {
    val source = Source.fromFile(f)(getFileCodec(cx))
    val lines = source.getLines
    var re = r.head
    val end_r = if (r.size == 1) r.head else r.tail.head
    for (ln <- lines) {
      re findFirstMatchIn ln match {
        case None =>
        case Some(x) =>
          if (re.equals(end_r)) {
            source.close()
            return if (x.groupCount > 0) x.group(1) else ln
          } else {
            re = end_r
          }
      }
    }
    source.close()
    ""
  }

  def getObject(carg: ContextClassArguments) = new Fgrep(carg)
}

class Fgrep(carg: ContextClassArguments) extends FgrepHelper(carg, Fgrep) with MutableLoaderFunction {
  private def fGrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val text = texts.head.trim
    val re = new Regex(text)
    val v = fgrep(cefa.file, cefa.cr.contextStrings, re)
    ContextReason(cefa.cr.contextStrings + (lhs -> v), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fGrep, cefa)
}

/*
  Mgrep
 */
object Mgrep extends LoaderContextAssignment {
  val rhsRegex = """^m.grep\s+/(.+?)/\s+/(.+?)/\s*$""".r

  def getObject(carg: ContextClassArguments) = new Mgrep(carg)
}

class Mgrep(carg: ContextClassArguments) extends FgrepHelper(carg, Mgrep) with MutableLoaderFunction {
  private def mgrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (sr:: er::_) = texts
    val v = fgrep(cefa.file, cefa.cr.contextStrings, sr.trim.r, er.trim.r)
    ContextReason(cefa.cr.contextStrings + (lhs -> v), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(mgrep, cefa)
}

/*
  FpathGrep
 */
object FpathGrep extends LoaderContextAssignment {
  val rhsRegex = """^fpath.grep\s+/(.+?)/\s*$""".r

  def getObject(carg: ContextClassArguments) = new FpathGrep(carg)
}

class FpathGrep(carg: ContextClassArguments) extends AbstractLoaderContext(carg, FpathGrep) with MutableLoaderFunction {
  private def fpathGrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val text = texts.head.trim
    val re = new Regex(text)
    val s = re findFirstMatchIn cefa.file.getPath match {
      case None => ""
      case Some(x) => x.group(1)
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> s), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fpathGrep, cefa)
}

/*
  Bgrep
 */
object Bgrep extends LoaderContextAssignment {

  // it is important to have linenum in the key as there can be multiple bundle grep functions in the context of single MPS
  case class BGrepKey(loadId: Long, linenum: Int)

  case class BGrepValue( timestamp: Long, bgrep: String)

  type BGrepCache = Cache[BGrepKey, BGrepValue]

  private[this] val cache: BGrepCache = CacheBuilder.newBuilder()
                                        .maximumSize(200)
                                        .build()

  val rhsRegex = """(cache)?\s*b.grep\s+/(.+?)/\s+/(.+?)/\s*$""".r

  def matcher(fp: String)(fn: String): Boolean = {
    new Regex(fp) findFirstMatchIn fn match {
      case Some(p) => true
      case None => false
    }
  }

  def bGrepCacheLoader(cache: BGrepCache)(fileNamesObtainer: Long => Seq[String])(bundleCacheKey: BGrepKey, cefa: ContextExecFnArguments, fp: String, rp: String, mps: String) = {
    val fileNameOption = fileNamesObtainer(bundleCacheKey.loadId).find(matcher(fp))

    val result = fileNameOption map { fn =>
      logger.debug(mps, s"file $fn matched b.grep regex pattern $fp")
      Fgrep.fgrep(new File(fn), cefa.cr.contextStrings, new Regex(rp))
    } getOrElse ""

    logger.debug(mps, s"Bundle/REGEX: $result")

    BGrepValue(System.currentTimeMillis(), result)
  }

  def getObject(carg: ContextClassArguments) = new Bgrep(carg, cache)
}

class Bgrep(carg: ContextClassArguments, cache: Bgrep.BGrepCache) extends FgrepHelper(carg, Bgrep) with MutableLoaderFunction {
  import com.glassbeam.context.Bgrep.{BGrepKey, BGrepValue, logger}

  val bGrepCacheLoader = Bgrep.bGrepCacheLoader(cache)(loadId => OpsDao.getFileForBundleGrepByLoadId(loadId)) _

  private def bgrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (_, fp, rp) = (texts.head, texts(1).trim, texts(2).trim)
    val bk = BGrepKey(cefa.loadId, carg.linenum)

    val value = try {
      cache.get(bk, new Callable[BGrepValue] {
        override def call(): BGrepValue = bGrepCacheLoader(bk, cefa, fp, rp, mps)
      })
    } catch {
      case NonFatal(e) =>
        logger.warning(s"Exception while obtaining value from b.grep cache for key $bk Retrying without cache", e)
        bGrepCacheLoader(bk, cefa, fp, rp, mps)
    }

    ContextReason(cefa.cr.contextStrings + (lhs -> value.bgrep), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }


  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(bgrep, cefa)
}

/*
  BfnameGrep
 */
object BfnameGrep extends LoaderContextAssignment {

  // it is important to have linenum in the key as there can be multiple bundle grep functions in the context of single MPS
  case class BFnameGrepKey( loadId: Long,  linenum: Int)

  case class BFnameGrepValue( timestamp: Long,  bfnamegrep: String)

  type BFnameGrepCache = Cache[BFnameGrepKey, BFnameGrepValue]

  private[this] val cache: BFnameGrepCache = CacheBuilder.newBuilder()
                                              .maximumSize(200)
                                              .build()

  val rhsRegex = """(cache)?\s*b.fname.grep\s+/(.+?)/\s+/(.+?)/\s*$""".r

  def bFnameGrepCacheLoader(fileNamesObtainer: Long => Seq[String])(bundleCacheKey: BFnameGrepKey, cefa: ContextExecFnArguments, fp: String, rp: String, mps: String) = {
    val fileNameOption = fileNamesObtainer(bundleCacheKey.loadId).view.map(fnamegrep(new Regex(fp), new Regex(rp))).find(!_.isEmpty)

    val result = fileNameOption getOrElse ""
    logger.debug(mps, s"Bundle/REGEX: $result")

    BFnameGrepValue(System.currentTimeMillis(), result)
  }

  private def fnamegrep(fp_r: Regex, rp_r: Regex)(fn: String): String = {
    var result = ""
    val fname = fn.split(filesep).last
    fp_r findFirstMatchIn fname match {
      case Some(p) =>
        rp_r findFirstMatchIn fname match {
          case Some(m) =>
            result = m.group(1)
          case None =>
        }
      case None =>
    }
    result
  }

  def getObject(carg: ContextClassArguments) = new BfnameGrep(carg, cache)
}

class BfnameGrep(carg: ContextClassArguments, cache: BfnameGrep.BFnameGrepCache) extends AbstractLoaderContext(carg, BfnameGrep) with MutableLoaderFunction {
  import com.glassbeam.context.BfnameGrep.{BFnameGrepKey, BFnameGrepValue, logger}

  val bFnameGrepCacheLoader = BfnameGrep.bFnameGrepCacheLoader(loadId => {
    lazy val cmpFiles = CompressedFilesDao.getNamesByLoadId(loadId)
    OpsDao.getNamesByLoadId(loadId).toStream append cmpFiles
  }
  ) _

  private def bfnameGrep(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (_, fp, rp) = (texts.head, texts(1).trim, texts(2).trim)
    val bk = BFnameGrepKey(cefa.loadId, carg.linenum)

    val value = try {
      cache.get(bk, new Callable[BFnameGrepValue] {
        override def call(): BFnameGrepValue = bFnameGrepCacheLoader(bk, cefa, fp, rp, mps)
      })
    } catch {
      case NonFatal(e) =>
        logger.warning(s"Exception while obtaining value from b.fname.grep cache for key $bk. Retrying without cache", e)
        bFnameGrepCacheLoader(bk, cefa, fp, rp, mps)
    }

    ContextReason(cefa.cr.contextStrings + (lhs -> value.bfnamegrep), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }


  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(bfnameGrep, cefa)
}

/*
  Lookup
 */
object Lookup extends LoaderContextAssignment {
  val rhsRegex = """^lookup\s*\((.+?)\s*\)\s*$""".r

  val lookupValueObtainer:(String,String)=>Option[String] = LookupTableDao.getFirstValueForKey _
  def getObject(carg: ContextClassArguments) = new Lookup(carg,lookupValueObtainer)
}

class Lookup(carg: ContextClassArguments, lookupValueObtainer:(String,String)=>Option[String]) extends AbstractLoaderContext(carg, Lookup) with MutableLoaderFunction {

  import Lookup._

  private val sep_mps = filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema

  private def lookup(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val customerOption = cefa.cr.contextStrings.get("ec")
    customerOption match {
      case Some(customer) =>
        val emps = customer + sep_mps
        val text = texts.head.trim
        val key = getParm(text, cefa.cr.contextStrings)
        val value = lookupValueObtainer(key, emps)
        logger.debug(mps, s"lookup value is: $key = $value, emps = $emps")
        val lookupval = value match {
          case Some(v) => v
          case None =>
            val err = s"CONTEXT lookup($key) not yet inserted into LOOKUP table, emps = $emps"
            logger.error(mps, err)
            "" // Don't hold up parsing if lookup key is not inserted
        }
        ContextReason(cefa.cr.contextStrings + (lhs -> lookupval), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
      case None =>
        val err = s"customer not found in context, mps = $mps, cefa = $cefa"
        logger.error(mps, err)
        cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(LookupFailure))
    }
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(lookup, cefa)
}

/*
  XmlValue
 */
object XmlValue extends LoaderContextAssignment {
  val rhsRegex = """^xmlValue\s*\((.+?),(.+?)\)\s*$""".r

  def getObject(carg: ContextClassArguments) = new XmlValue(carg)
}

class XmlValue(carg: ContextClassArguments) extends AbstractLoaderContext(carg, XmlValue) with MutableLoaderFunction {

  import XmlValue._

  private def xmlValue(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (fp::parm::_) = texts
    var vtdNav: VTDNav = null
    val fpRegex = new Regex(fp.trim.init.tail)
    val xpath = parm.trim.init.tail
    // If Filename matches filepattern, then evaluate xpath for this file, else return empty
    val xv = fpRegex findFirstMatchIn cefa.file.getName match {
      case Some(v) =>
        try {
          if (vtdNav == null) {
            val vg = new VTDGen
            if (vg.parseFile(cefa.file.toString, true))
              vtdNav = vg.getNav
          }
          //modified and need to put back this one -> val xParser = new Icon(null,null) with XmlParser
          val xParser = new Icon(null) with XmlParser
          if (xParser.isValidXpath(xpath) && vtdNav != null) {
            val vn = vtdNav.duplicateNav
            xParser.getXMLColValue(vn, xpath)
          } else {
            // logger.error(mps, s"Context error: Xml function has wrong xpath or a non-xml file: xpath=$xpath, file=$f.getString. Defaulting to blanks")
            ""
          }
        } catch {
          case NonFatal(t) =>
            val err = s"Could not parse xml file ${cefa.file.getName}. exception = ${t.getMessage}"
            logger.error(t, mps, err)
            return cefa.cr.copy(reason = cefa.cr.reason + err, failure = Some(XmlError))
        }
      case None => ""
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> xv), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(xmlValue, cefa)
}

/*
  ProcessFileToContext
 */
object ProcessFileToContext extends LoaderContextAssignment with ParsableObtainer {
  val rhsRegex = """^e.processFileToContext\s+/(.+?)/\s+(.+?)\s*$""".r

  def getObject(carg: ContextClassArguments) = new ProcessFileToContext(carg, parsableObtainer)
}

class ProcessFileToContext(carg: ContextClassArguments,getParsable: String => Parsable) extends AbstractLoaderContext(carg, ProcessFileToContext) with MutableLoaderFunction {

  import ProcessFileToContext._

  private def processFileToContext(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (regex, classname) = (texts.head.trim, texts(1).trim)
    val cxt_value = if (regex.r.pattern.matcher(cefa.file.getAbsolutePath).matches()) {
      val parseObj = getParsable(classname)
      // the two log.info message are required to ensure that timestamps are recorded and a slow extensbility implementation
      // can be reported back as it affects LCP Performance
      logger.info(mps, s"calling processFileToContext on class: $classname for file ${cefa.file.getAbsolutePath}")
      val v_v = parseObj.processFileToContext(cefa.file.toPath)
      logger.info(mps, s"finished processFileToContext in class: $classname for file ${cefa.file.getAbsolutePath}")
      v_v
    } else ""
    logger.debug(mps, s"processFileToContext: regex = $regex, class = $classname, value = $cxt_value")
    ContextReason(cefa.cr.contextStrings + (lhs -> cxt_value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(processFileToContext, cefa)
}

trait ParsableObtainer {
  val parsableObtainer = (classname:String) => getParsable(classname)
}
/*
  ProcessBundleToContext
 */
object ProcessBundleToContext extends LoaderContextAssignment with ParsableObtainer {
  val rhsRegex = """^e.processBundleToContext\s+/(.+?)/\s+(.+?)\s*$""".r
  def getObject(carg: ContextClassArguments) = new ProcessBundleToContext(carg,parsableObtainer)
}

class ProcessBundleToContext(carg: ContextClassArguments, getParsable: String => Parsable) extends AbstractLoaderContext(carg, ProcessBundleToContext) with MutableLoaderFunction {

  import ProcessBundleToContext._

  private def processBundleToContext(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val (regex, classname) = (texts.head.trim, texts(1).trim)
    val cxt_value = if (regex.r.pattern.matcher(cefa.file.getParent).matches()) {
      val parseObj = getParsable(classname)
      // the two log.info message are required to ensure that timestamps are recorded and a slow extensbility implementation
      // can be reported back as it affects LCP Performance
      logger.info(mps, s"calling processBundleToContext on class: $classname for file ${cefa.file.getAbsolutePath}")
      val v_v = parseObj.processBundleToContext(cefa.file.toPath)
      logger.info(mps, s"finished processBundleToContext in class: $classname for file ${cefa.file.getAbsolutePath}")
      v_v
    } else ""
    logger.debug(mps, s"processBundleToContext: regex = $regex, class = $classname, value = $cxt_value")
    ContextReason(cefa.cr.contextStrings + (lhs -> cxt_value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(processBundleToContext, cefa)
}

/*
  Fcount
 */
object Fcount extends LoaderContextAssignment {
  val rhsRegex = """^f.count\s+/(.+?)/\s*$""".r
  val fileNameObtainerForLoadId:Long=>Seq[String] = OpsDao.getNamesByLoadId _
  def getObject(carg: ContextClassArguments) = new Fcount(carg,fileNameObtainerForLoadId)
}

class Fcount(carg: ContextClassArguments,fileNameObtainerForLoadId:Long=>Seq[String] ) extends AbstractLoaderContext(carg, Fcount) with MutableLoaderFunction {
  private def fcount(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val bk = BundleCacheKey(cefa.loadId, carg.linenum)
    val fcountExists: Boolean = {
      if (!BundleCache.contains(bk)) false
      else if (BundleCache.get(bk).get.fcount == null) false
      else true
    }
    val fc = if (fcountExists) {
      BundleCache.get(bk).get.fcount.toString
    } else {
      val regex = texts.head.trim.r
      val names =fileNameObtainerForLoadId(cefa.loadId)
//      val fnames = names.filterNot(regex.pattern.matcher(_).matches())
      val isMatch:String=>Boolean = regex.pattern.matcher(_).matches()
      val fcount  = names.foldLeft(0)((acc,str) => if(isMatch(str)) acc +1 else acc) toString
      //val fcount = fnames.length.toString
      val bv =
        if (BundleCache.contains(bk)) BundleCache.get(bk).get.copy(fcount = fcount)
        else BundleCacheValue(System.currentTimeMillis(), null, null, fcount)
      BundleCache += (bk -> bv)
      fcount
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> fc), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(fcount, cefa)
}

/*
  AssertNumeric
 */
object AssertNumeric extends LoaderContextAssignment {
  val rhsRegex = """^assertNumeric\((.*)\)\s*$""".r

  def getObject(carg: ContextClassArguments) = new AssertNumeric(carg)
}

class AssertNumeric(carg: ContextClassArguments) extends AbstractLoaderContext(carg, AssertNumeric) with MutableLoaderFunction {
  private def assertNumeric(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val text = texts.head.split(",").map(_.trim)
    def getP(index: Int): Int = getParm(text(index), cefa.cr.contextStrings).toInt
    val (nOne, nTwo, operator) = (getP(0), getP(1), text(2).trim)
    val result: Boolean = operator match {
      case "==" => nOne == nTwo
      case ">" => nOne > nTwo
      case "<" => nOne < nTwo
      case ">=" => nOne >= nTwo
      case "<=" => nOne <= nTwo
      case _ => false
    }
    ContextReason(cefa.cr.contextStrings + (lhs -> result.toString), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(assertNumeric, cefa)
}
