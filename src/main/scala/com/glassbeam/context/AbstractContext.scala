package com.glassbeam.context

/**
  * Created by narayana on 18/4/16.
  */
import java.util.Calendar

import com.glassbeam.context.Context._
import com.glassbeam.context.ContextSection.ContextSection
import com.glassbeam.context.ContextStage.ContextStage
import com.glassbeam.failuredefs.MsgTemplate
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.Logger

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

trait AbstractContextPattern extends Logger {
  val isAssignment: Boolean
  val fullRegex: Regex
  val rhsRegex: Option[Regex]
  val assign:Regex = """(.+?)=(.+?)""".r
  val contextSection: ContextSection
  val contextStage: ContextStage
  val logger = Logging(this)
}

trait LCPContextAssignment extends AbstractContextPattern {
  val isAssignment = true
  val rhsRegex = Some("""(.+?)""".r)
}

trait WatcherContextStatement extends AbstractContextPattern {
  val name:String
  val isAssignment = false
  val rhsRegex = None
  //def getObject(carg: LoaderClassArguments): AbstractWatcherContext
}

trait LoaderContextStatement extends AbstractContextPattern {
  val isAssignment = false
  val rhsRegex = None
  def getObject(carg: ContextClassArguments): ALoaderContextExtract
}

trait LoaderContextAssignment extends AbstractContextPattern {
  val isAssignment = true
  val fullRegex = assign
  def getObject(carg: ContextClassArguments): ALoaderContextExtract
}

abstract class AbstractContextExtract(carg:ClassArguments, acpat: AbstractContextPattern)  {

  val mps = carg.manufacturer + "/" + carg.product + "/" + carg.schema

  def getLhsRhsRegex = {
    val (lhs, rhsSplit) = if (acpat.isAssignment) {
      acpat.assign.unapplySeq(carg.context) match {
        case None =>
          acpat.logger.error(s"assignment context function did NOT match assignment regex pattern: ${carg.context}")
          (null, None)
        case Some(eqSplit) =>
          val (l, r) = (eqSplit.head.trim, eqSplit(1).trim)
          if (r.equals("''")) {
            (l, Some(List("")))
          } else {
            val rSplit = acpat.rhsRegex.get.unapplySeq(r)
            (l, rSplit)
          }
      }
    } else {
      (null, acpat.fullRegex.unapplySeq(carg.context))
    }
    (lhs,rhsSplit)
  }

}

abstract class AWatcherContextExtract(warg:ContextClassArguments, wcpat:AbstractContextPattern) extends AbstractContextExtract(warg:ContextClassArguments,wcpat:AbstractContextPattern) {

  val (lhs, rhsSplit) =  getLhsRhsRegex

  def evalFileMatchesPattern(wefa: WatcherEvalArguments):Boolean = {
    // ToDo: Create the regex and match it with the filename
    rhsSplit match {
      case Some(filepat) =>
        filepat.exists(regstr => regstr.r.pattern.matcher(wefa.file_name).matches())
      case None =>
        wcpat.logger.error(s"Regex is not proper for mps ${wefa.mps} for filename ${wefa.file_name}")
        false
    }
  }

  def evalUncompressLevel(wefa: WatcherEvalArguments):Int = {
    val rhsdepthmatch = rhsSplit.getOrElse(List())
    var bundle_depth = 5
    if(rhsdepthmatch.length == 2){
      val bundle_name = rhsdepthmatch.head
      val bundle_pattern = bundle_name.r.pattern
      if (bundle_pattern.matcher(wefa.file_name).matches()) {
        bundle_depth = rhsdepthmatch.lift(1) match {
          case Some(depth) => depth.toInt
          case None =>
            wcpat.logger.error(s"Bundle Depth not found for mps ${wefa.mps} for filename ${wefa.file_name}")
            bundle_depth
        }
      }
    }else{
      wcpat.logger.error(s"Uncompressel Level pattern has too many matches for mps ${wefa.mps} for filename ${wefa.file_name}")
    }
    bundle_depth
  }

}

abstract class ALCPContextExtract(farg:ContextClassArguments, fcpat:AbstractContextPattern) extends AbstractContextExtract(farg: ContextClassArguments, fcpat: AbstractContextPattern) {

  val (lhs, rhsSplit) =  getLhsRhsRegex


  def execute(cr: LCPEvalArguments): ContextReason

  def evalAssignment(callback: (String, List[String], LCPEvalArguments) => ContextReason, cefa: LCPEvalArguments) = {
    lhs == null || rhsSplit.isEmpty match {
      case true =>
        val err = s"Assignment statement not okay. context line = ${farg.context}, lhs = $lhs, rhsSplit = $rhsSplit"
        fcpat.logger.error(mps, err)
        ContextReason(new HashMap[String, String](), err, Some(AssignmentStmtError))
      case _ =>
        callback(lhs, rhsSplit.get, cefa)
    }
  }
}

abstract class ALoaderContextExtract(carg: ContextClassArguments, lcpat: AbstractContextPattern) extends AbstractContextExtract(carg: ContextClassArguments, lcpat: AbstractContextPattern) {

  val (lhs, rhsSplit) =  getLhsRhsRegex

  lazy val CMPS: String =
    if (carg.customer == null) "loader"
    else carg.customer + "/" + carg.manufacturer + "/" + carg.product + "/" + carg.schema

  lazy val calendar = Calendar.getInstance()

  lazy val (assertOptionalTemplateId, assertOptionalMsg) = getOptionalParams

  def execute(cr: LoaderEvalArguments): ContextReason

  def arg: ContextClassArguments = carg

  def evalAssignment(callback: (String, List[String],LoaderEvalArguments) => ContextReason, cefa: LoaderEvalArguments) = {
    lhs == null || rhsSplit.isEmpty match {
      case true =>
        val err = s"Assignment statement not okay. context line = ${carg.context}, linenum = ${carg.linenum}, CMPS = ${CMPS}, lhs = $lhs, rhsSplit = $rhsSplit"
        lcpat.logger.error(mps, err)
        ContextReason(new HashMap[String, String](), err, Some(AssignmentStmtError))
      case _ =>
        callback(lhs, rhsSplit.get, cefa)
    }
  }
  def evalStatement(callback: (Option[List[String]], LoaderEvalArguments) => ContextReason, cefa: LoaderEvalArguments) =
    callback(rhsSplit, cefa)


  def getTemplate(errStr: String, loadId: Long) = MsgTemplate(Option(CMPS), Option(loadId.toInt), errStr)

  def getOptionalParams = {
    // Handling optional regex groups as per the regex
    rhsSplit match {
      case Some(x) => x match {
        case List(a, templateId1, b, templateId2, msg2, c, msg3) =>
          getParams(templateId1, templateId2, msg2, msg3)
        case List(ctxvar, a, templateId1, b, templateId2, msg2, c, msg3) =>
          getParams(templateId1, templateId2, msg2, msg3)
        case _ => (None, None)
      }
      case None => (None, None)
    }
  }

  def getParams(templateId1: String, templateId2: String, msg2: String, msg3: String) = {
    // Parms can be (tid | tid, msg | msg): Any or all input params can be null
    val t = List(Option(templateId1), Option(templateId2)).flatten
    val m = List(Option(msg2), Option(msg3)).flatten
    val templateId = if (t.nonEmpty) Some(t.head.trim.toInt) else None
    val msg = if (m.nonEmpty) Some(m.head.trim) else None
    (templateId, msg)
  }

}
