package com.glassbeam.context


import com.glassbeam.context.Context.{ContextReason, LoaderClassArguments, LoaderEvalArguments}
import com.glassbeam.context.ContextHelpers._
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.StartupConfig._
import com.glassbeam.model.{ContextFailure, _}
import org.json4s.NoTypeHints

import scala.util.matching.Regex

object AssertUncompressionFail extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^b.assertOnUncompressionFailure\s*(\(\s*(\d+)\s*\)|)(\(\s*(\d+)\s*,\s*(.*)\s*\)|)(\(\s*(.*)\s*\)|)?\s*$""".r

  def getObject(carg: LoaderClassArguments) = new AssertUncompressionFail(carg)
}

class AssertUncompressionFail(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, AssertUncompressionFail)  {

  import AssertUncompressionFail._

  private val emps = carg.customer + filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema
  private val cmps = (carg.customer, carg.manufacturer, carg.product, carg.schema)
  val sendMail = Init.inittype == Init.Run && assertOptionalTemplateId.isDefined

  protected def obtainCompressedFiles(loadId: Long) = CompressedFilesDao.getAllCompressedArchives(loadId)

  protected def onFailure(loadId: Long, customMsg: String) = {
    val names: Seq[String] = OpsDao.getNamesByLoadId(loadId)
    for (name <- names) OpsDao.updateErrorStateByNameLoadId(name, loadId)(customMsg, ProcessingState.Failed.id.toByte, "", ContextFailure.AssertUncompressionFailure)
   // Context.purgeLoadId(loadId)

    // Insert a record in Bundle CF with a dummy bundleid - with custom message
    //LoaderUtils.insertFailedBundle(carg.customer, carg.manufacturer, carg.product, carg.schema, loadId, customMsg)
  }

  private def assertUncompressionFail(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    // get all compressed archives. See if any of them has a non-zero or non-existant exit-code
    // If so, then mark all files as failed and set the error_count=1 and error_list to have a comment
    //  for all files in the bundle
    val archive_states: Seq[(String, Option[Int])] = obtainCompressedFiles(cefa.loadId)
    var failure_exists = false
    var error_stmt = ""

    for (as <- archive_states; if !failure_exists) {
      if (as._2.isEmpty) {
        error_stmt = s"\narchive = ${as._1} not uncompressed successfully in bundle with loadid = ${cefa.loadId}"
        logger.debug(mps, s"on assertOnUncompressionFailure. $error_stmt")
        failure_exists = true
      } else if (as._2.get != 0) {
        // No error statement here.. Check
        logger.debug(mps, s"inside assertOnUncompressionFailure. $error_stmt")
        failure_exists = true
      }
    }
    val isFailure = if (failure_exists) Some(AssertUncompressionFailure) else None

    if (failure_exists) {
      println("failure exists just uncomment the code")
      val customMsg = if (assertOptionalMsg.isDefined) substituteContextInMsg(assertOptionalMsg.get, cefa.cr.contextStrings) else error_stmt
      error_stmt = customMsg
      onFailure(cefa.loadId, customMsg)

      // Send email notification if template id is specified - with custom message
      //if (sendMail)
       // rtfActorRef ! ContextAssertionFailure(getTemplate(customMsg, cefa.loadId), cefa.cr.contextStrings, assertOptionalTemplateId)
    }

    logger.debug(s"in AssertUnCompression function ${failure_exists} hence the result ${isFailure}")
    ContextReason(cefa.cr.contextStrings, cefa.cr.reason + error_stmt, isFailure, cefa.cr.bproperties)
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(assertUncompressionFail, cefa)
}


object Assert extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^f.assert\s*\((\s*[A-Za-z_]\w+\s*)(,\s*(\d+)\s*|)(,\s*(\d+)\s*,\s*(.*)\s*|)(,\s*(.*)\s*|)?\)\s*$""".r
  def getObject(carg: LoaderClassArguments) = new Assert(carg)
}

class Assert(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, Assert)  {

  import Assert._

  val sendMail = Init.inittype == Init.Run && assertOptionalTemplateId.isDefined

  private def assert(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    val text = if (texts.isDefined) texts.get.head.trim else ""
    val cr2 = if (cefa.cr.contextStrings.getOrElse(text, "").toString.trim.isEmpty) {
      val error_stmt = s"File failed to process as mandatory field '$text' is missing/unavailable. Please contact dl-support@glassbeam.com"
      val customMsg = if (assertOptionalMsg.isDefined) substituteContextInMsg(assertOptionalMsg.get, cefa.cr.contextStrings) else error_stmt
      logger.warning(customMsg)

      // Send email notification if template id is specified - with custom message
      if (sendMail) {
        //rtfActorRef ! ContextAssertionFailure(getTemplate(customMsg, cefa.loadId), cefa.cr.contextStrings, assertOptionalTemplateId)
      }

      ContextReason(cefa.cr.contextStrings, cefa.cr.reason + "\n" + customMsg, Some(AssertFailure), cefa.cr.bproperties)

    } else {
      cefa.cr
    }
    cr2
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(assert, cefa)
}


object AssertPxFileCount extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^b.assertPxFileCount\s*\((\s*\d+\s*)(,\s*(\d+)\s*|)(,\s*(\d+)\s*,\s*(.*)\s*|)(,\s*(.*)\s*|)?\)\s*$""".r
  val pxCountObtainer = (loadId: Long) => LoadIdDao.getPxCountByLoadId(loadId)
  def getObject(carg: LoaderClassArguments) = new AssertPxFileCount(carg, pxCountObtainer)
}

class AssertPxFileCount(carg: LoaderClassArguments, getPxCount: Long => Option[Long]) extends AbstractLoaderContext(carg, AssertPxFileCount) with MLoaderState  {

  import AssertPxFileCount._

  val sendMail = Init.inittype == Init.Run && assertOptionalTemplateId.isDefined

  protected def onFailure(loadId: Long, customMsg: String) = {
    val names: Seq[String] = OpsDao.getNamesByLoadId(loadId)
    for (name <- names) OpsDao.updateErrorStateByNameLoadId(name, loadId)(customMsg, ProcessingState.Failed.id.toByte, "", ContextFailure.AssertPxCountFailure)
//    Context.purgeLoadId(loadId)
//
//    // Insert a record in Bundle CF with a dummy bundleid - with custom message
//    LoaderUtils.insertFailedBundle(carg.customer, carg.manufacturer, carg.product, carg.schema, loadId, customMsg)
  }

  private def assertPxFileCount(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    val number = texts.get(0).trim.toInt
    val pxCount = getPxCount(cefa.loadId)

    def assertError(error_stmt: String) = {
      val customMsg = if (assertOptionalMsg.isDefined) substituteContextInMsg(assertOptionalMsg.get, cefa.cr.contextStrings) else error_stmt
      logger.warning(customMsg)
      onFailure(cefa.loadId, customMsg)

      // Send email notification if template id is specified - with custom message
      if (sendMail) {
       // rtfActorRef ! ContextAssertionFailure(getTemplate(customMsg, cefa.loadId), cefa.cr.contextStrings, assertOptionalTemplateId)
      }

      ContextReason(cefa.cr.contextStrings, cefa.cr.reason + customMsg, Some(AssertPxCountFailure), cefa.cr.bproperties) // CONTEXT($x)
    }
    val cr2 = if (pxCount.isEmpty) {
      assertError(s"AssertPxFileCount: BUNDLE/CONTEXT(${cefa.file}}) on hold: px count empty in load_id table " +
        s"for load_id = ${cefa.loadId}")
    } else if (pxCount.get > number) {
      assertError(s"AssertPxFileCount: BUNDLE/CONTEXT(${cefa.file}}) on hold: px count in load_id table " +
        s"${pxCount.get} HIGHER than threshold of $number for bundle with load_id= ${cefa.loadId}")
    } else {
      cefa.cr
    }
    cr2
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(assertPxFileCount, cefa)
}


object Validate extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^validate\s*\((.+?)\s*\)\s*$""".r
  private[this] val valueObtainerForKey: (String, String) => Seq[String] = ValidateTableDao.getValuesForKey _


  def getObject(carg: LoaderClassArguments) = new Validate(carg, valueObtainerForKey)
}

class Validate(carg: LoaderClassArguments, valueObtainerForKey: (String, String) => Seq[String]) extends AbstractLoaderContext(carg, Validate)  {

  import Validate._

  private val emps = carg.customer + filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema

  private def validate(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    val text = texts.get.head.trim
    val key = getParm(text, cefa.cr.contextStrings)
    val record = valueObtainerForKey(key, emps)
    val cr2 = if (record.isEmpty) {
      logger.error(mps, s"CONTEXT validate($key) not yet inserted into Validate table")
      ContextReason(cefa.cr.contextStrings, cefa.cr.reason + "\n" + s"CONTEXT validate($key) not yet inserted into Validate table\n",
        Some(ValidateFailure), cefa.cr.bproperties)
    } else {
      cefa.cr
    }
    cr2
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(validate, cefa)
}


object CombineLines extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^combinelines\s*\((.+?)\s*,\s*(.+?)\s*,\s*(.+?)\s*,\s*(.*?)\s*\)\s*$""".r
  val combinelines = "combinelines"
  def getObject(carg: LoaderClassArguments) = new CombineLines(carg)
}

class CombineLines(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, CombineLines)  {

  import CombineLines._

  def combineLines(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    val Some(src :: dest :: concatDelim :: replaceDelim :: _) = texts
    logger.debug(mps, s"src: $src, dest: $dest, concatDelim: $concatDelim, replaceDelim: $replaceDelim")
    val cl = s"$src,$dest,$concatDelim,$replaceDelim"
    ContextReason(cefa.cr.contextStrings + (combinelines -> cl), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa:LoaderEvalArguments): ContextReason = evalStatement(combineLines, cefa)
}


object Encoding extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^t.encoding\s*=\s*(.*$)\s*$""".r
  val fr_encode_tuple_r = """'(.+?)'\s*,\s*/(.+?)/""".r
  val textencoding = "t.encoding"
  def getObject(carg: LoaderClassArguments) = new Encoding(carg)
}

class Encoding(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, Encoding)  {

  import Encoding._

  private def encoding(texts: Option[List[String]], cefa:LoaderEvalArguments): ContextReason = {
    // "t.encoding=('aaa',/xxx/)('bbb',/yyy/)('ccc',/zzz/)"
    val text = texts.get.head
    val n_enc: Array[String] = text.split("(?=\\()").view.map(_.trim).filter(_.nonEmpty).force
    val fre: Array[(String, String)] = try {
      for (
        e <- n_enc;
        (encoding, regx) = e.tail.init match {
          case fr_encode_tuple_r(enc, rx) => (enc, rx)
          case _ => ("", "")
        } if cefa.file.getName.matches(regx)
      ) yield (regx, encoding)
    } catch {
      case e: Exception =>
        val err = s"t.encoding exception: carg: ${carg}, texts = $texts, cefa = ${cefa}"
        logger.error(e, mps, err)
        return ContextReason(cefa.cr.contextStrings, cefa.cr.reason + "\n" + err, Some(ContextExecFailure), cefa.cr.bproperties)
    }
    if (fre.nonEmpty) {
      logger.debug(mps, s"file name ${cefa.file.getName} matched following regex/encoding = $fre. Going to use regex = ${fre(0)._1} with encoding = ${fre(0)._2}")
      ContextReason(cefa.cr.contextStrings + (textencoding -> fre(0)._2), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
    } else {
      cefa.cr
    }
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(encoding, cefa)
}


/*
  AssertTruthy
 */
object AssertTruthy extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^f.assertTruthy\s*\((\s*[A-Za-z_]\w+\s*)(,\s*(\d+)\s*|)(,\s*(\d+)\s*,\s*(.*)\s*|)(,\s*(.*)\s*|)?\)\s*$""".r
  def getObject(carg: LoaderClassArguments) = new AssertTruthy(carg)
}

class AssertTruthy(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, AssertTruthy)  {

  import AssertTruthy._

  val sendMail = Init.inittype == Init.Run && assertOptionalTemplateId.isDefined

  private def assertTruthy(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    def testVar(text: String): Boolean = {
      // Invalid or missing inputs will evaulate to true i.e. hold file
      val tmpVal = cefa.cr.contextStrings.get(text).get.toString
      //tmpVal.isDefined && !tmpVal.trim.isEmpty
      !tmpVal.trim.isEmpty match {
        case true =>
          tmpVal.toUpperCase match {
            case "TRUE"|"FALSE" => tmpVal.toBoolean
            case _ => true
          }
        case _ => true
      }
    }
    val text = texts.get.head.trim
    val cr2 = if (testVar(text)) {
      logger.warning(mps, s"assertTruthy: FILE(${cefa.file}}) on hold: $text")
      val error_stmt = s"Bundle not processed due to Assertion failure, Please contact dl-support@glassbeam.com"
      val customMsg = if (assertOptionalMsg.isDefined) substituteContextInMsg(assertOptionalMsg.get, cefa.cr.contextStrings) else error_stmt

      if (sendMail) {
        //rtfActorRef ! ContextAssertionFailure(getTemplate(customMsg, cefa.loadId), cefa.cr.contextStrings, assertOptionalTemplateId)
      }
      ContextReason(cefa.cr.contextStrings, cefa.cr.reason + "\n" + customMsg, Some(AssertTruthyFailure), cefa.cr.bproperties) // CONTEXT($x)
    } else {
      cefa.cr
    }
    cr2
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(assertTruthy, cefa)
}


object BProperties extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^b.properties\s*\((.+?)\)\s*$""".r

  def getObject(carg: LoaderClassArguments) = new BProperties(carg)
}

object BPropertiesIdHelper extends Logger {

  import org.json4s.jackson.Serialization
  import org.json4s.jackson.Serialization.write

  implicit val formats = Serialization.formats(NoTypeHints)
  private val logger = Logging(this)

  def bundlePropertiesIdHelper(texts: Option[List[String]], cefa: LoaderEvalArguments, emps: String, contextLHS: String): ContextReason = {
    var cr = cefa.cr
    texts match {
      case Some(txts) =>
        val text = txts.head.split(",").map(_.trim)
        var m: Map[Int, Map[String, String]] = Map() // Note: the order of properties should be kept the same
      var bp: Map[String, String] = Map()
        for ((t, i) <- text.zipWithIndex) {
          cefa.cr.contextStrings.get(t) match {
            case None =>
              val err = s"value is NULL for key $t defined in b.properties for emps = $emps"
              cr = cr.copy(reason = cr.reason + err)
              logger.error(emps, err)
            case Some(value) =>
              if (value.toString.isEmpty) {
                val err = s"value is EMPTY for key $t defined in b.properties for emps = $emps"
                cr = cr.copy(reason = cr.reason)
                logger.warning(emps, err)
              }
              m = m + (i -> Map(BKEY -> t.toString, BVALUE -> value.toString))
              bp = bp + (t.toString -> value.toString)
          }
        }
        m.isEmpty match {
          case true =>
            cr
          case false =>
            val b_properties_id = write(m)
            logger.debug(emps, s"file: ${cefa.file.getAbsolutePath}, bundle properties = $b_properties_id")
            ContextReason(cr.contextStrings + (contextLHS -> b_properties_id), cr.reason, cr.failure, Some(bp))
        }
      case None => cr
    }
  }

}

class BProperties(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, BProperties)  {

  private val emps = carg.customer + filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema

  private def bundleProperties(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason =
    BPropertiesIdHelper.bundlePropertiesIdHelper(texts, cefa, emps, BPROPERTIES)

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(bundleProperties, cefa)
}

object BId extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^b.id\s*\((.+?)\)\s*$""".r

  def getObject(carg: LoaderClassArguments) = new BId(carg)
}

class BId(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, BId)  {

  private val emps = carg.customer + filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema

  private def bId(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason =
    BPropertiesIdHelper.bundlePropertiesIdHelper(texts, cefa, emps, BID)

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(bId, cefa)
}

object AssertBundleDuplicate extends LoaderContextStatement with MLoaderState {
  val fullRegex = """^b.assertBundleDuplicate\s*(\(\s*(\d+)\s*\)|)(\(\s*(\d+)\s*,\s*(.*)\s*\)|)(\(\s*(.*)\s*\)|)?\s*$""".r
  def getObject(carg: LoaderClassArguments) = new AssertBundleDuplicate(carg)
  val BundleDuplicate = "assertBundleDuplicate"
  val exists = "Exists"
  val template = "assertBundleDuplicateOptionalTemplate"
  val Msg = "assertBundleDuplicateOptionalMsg"
}


class AssertBundleDuplicate(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, AssertBundleDuplicate)  {
  import AssertBundleDuplicate._
  def assertBundleDuplicate(texts: Option[List[String]], cefa: LoaderEvalArguments): ContextReason = {
    val optTemplate = if (assertOptionalTemplateId.isDefined) assertOptionalTemplateId.get else ""
    val optMsg = if (assertOptionalMsg.isDefined) assertOptionalMsg.get else ""
    val isBundleDuplicateExists = (BundleDuplicate,exists)
    val bundleDupOptTemp = (template,optTemplate.toString)
    val bundleDupOptMsg = (Msg,optMsg)
    //Have Doubt on this i have to check this function with out missing
    val assertbundle = cefa.cr.contextStrings.+(isBundleDuplicateExists,bundleDupOptTemp,bundleDupOptMsg)
    ContextReason(assertbundle, cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa: LoaderEvalArguments): ContextReason = evalStatement(assertBundleDuplicate, cefa)
}

object AssertBundleFail extends LoaderContextStatement with MLoaderState {
  override val fullRegex: Regex = """^b.assertBundleFail\s*\((.+?),(.+?),(\d+)\)\s*$""".r

  override def getObject(carg: LoaderClassArguments) = new AssertBundleFail(carg)
}

class AssertBundleFail(carg: LoaderClassArguments) extends AbstractLoaderContext(carg, AssertBundleFail)  {
  private val logger = Logging(this)

  private def evalArgs(texts: Option[List[String]], cefa: LoaderEvalArguments) = {
    val isFailedBundle = cefa.cr.contextStrings.getOrElse(texts.get.head.trim, "").toString.trim.isEmpty

    val (assertBundle, errorStmt) = if (isFailedBundle) {
      val errs = ""
        if(texts.isDefined) substituteContextInMsg(texts.get.apply(1), cefa.cr.contextStrings) else ""
      if (texts.isDefined && texts.get.size == 3) {
        val emailTemplId = texts.get(2).toInt
        //rtfActorRef ! ContextAssertBundleFail(getTemplate(errs, cefa.loadId), cefa.cr.contextStrings, emailTemplId)
      }
      (Some(AssertBundleFailure), errs)
    } else (None, "")
    ContextReason(cefa.cr.contextStrings, cefa.cr.reason + errorStmt, assertBundle, cefa.cr.bproperties)
  }

  override def execute(cr: LoaderEvalArguments): ContextReason = {
    evalStatement(evalArgs, cr)
  }
}




