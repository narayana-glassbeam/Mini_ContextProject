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

trait AbstractContextObject extends Logger {
  val isAssignment: Boolean
  val fullRegex: Regex
  val rhsRegex: Regex
  val contextSection: ContextSection
  val contextStage: ContextStage
  val logger = Logging(this)
}

trait LCPContextAssignment extends AbstractContextObject {
  val isAssignment = true
  val fullRegex = """(.+?)=(.+?)""".r
}

trait WatcherContextStatement extends AbstractContextObject {
  val name:String
  val isAssignment = false
  val rhsRegex = null
  //def getObject(carg: LoaderClassArguments): AbstractWatcherContext
}

trait LoaderContextStatement extends AbstractContextObject {
  val isAssignment = false
  val rhsRegex = null
  def getObject(carg: ContextClassArguments): AbstractLoaderContext
}

trait LoaderContextAssignment extends AbstractContextObject {
  val isAssignment = true
  val fullRegex = """(.+?)=(.+?)""".r
  def getObject(carg: ContextClassArguments): AbstractLoaderContext
}

abstract class AbstractContextClass(carg:ClassArguments, ACO: AbstractContextObject)  {

  val mps = carg.manufacturer + "/" + carg.product + "/" + carg.schema

  def getLhsRhsRegex = {
    val (lhs, rhsSplit) = if (ACO.isAssignment) {
      ACO.fullRegex.unapplySeq(carg.context) match {
        case None =>
          ACO.logger.error(s"assignment context function did NOT match assignment regex pattern: ${carg.context}")
          (null, None)
        case Some(eqSplit) =>
          val (l, r) = (eqSplit.head.trim, eqSplit(1).trim)
          if (r.equals("''")) {
            (l, Some(List("")))
          } else {
            val rSplit = ACO.rhsRegex.unapplySeq(r)
            (l, rSplit)
          }
      }
    } else {
      (null, ACO.fullRegex.unapplySeq(carg.context))
    }
    (lhs,rhsSplit)
  }

}

abstract class AbstractWatcherContext(warg:ContextClassArguments,AWCO:AbstractContextObject) extends AbstractContextClass(warg:ContextClassArguments,AWCO:AbstractContextObject) {

  val (lhs, rhsSplit) =  getLhsRhsRegex

  def evalFileMatchesPattern(wefa: WatcherEvalArguments):Boolean = {
    // ToDo: Create the regex and match it with the filename
    var filematched = false
    rhsSplit match {
      case Some(filepat) =>
        filepat.foreach(regpat =>{ val pat =  regpat.r.pattern
          if (pat.matcher(wefa.file_name).matches()) {
            filematched = true
          }
        } )
      case None =>
        AWCO.logger.error(s"Regex is not proper for mps ${wefa.mps} for filename ${wefa.file_name}")
    }
    filematched
  }

  def evalUncompressLevel(wefa: WatcherEvalArguments):Int = {
    //println(s"received get max Uncompress level method for "+wefa.file_name+" for mps "+wefa.mps)
    val rhsdepthmatch = rhsSplit.get
    var bfound = false
    var bundle_depth = 5
    if(rhsdepthmatch.length == 2){
      val bundle_name = rhsdepthmatch.head
      val bundle_pattern = bundle_name.r.pattern
      if (bundle_pattern.matcher(wefa.file_name).matches()) {
        bfound = true
        bundle_depth = rhsdepthmatch.lift(1) match {
          case Some(depth) => depth.toInt
          case None =>
            AWCO.logger.error(s"Bundle Depth not found for mps ${wefa.mps} for filename ${wefa.file_name}")
            return  bundle_depth
        }
      }
    }else{
      AWCO.logger.error(s"Uncompressel Level pattern has too many matches for mps ${wefa.mps} for filename ${wefa.file_name}")
    }
    //println(s"for file name ${wefa.file_name} bundle depth is "+bundle_depth)
    bundle_depth
  }

}

abstract class AbstractLCPContext(farg:ContextClassArguments,FCO:AbstractContextObject) extends AbstractContextClass(farg: ContextClassArguments, FCO: AbstractContextObject) {

  val (lhs, rhsSplit) =  getLhsRhsRegex

  def execute(cr: LCPEvalArguments): ContextReason

  def evalAssignment(callback: (String, List[String], LCPEvalArguments) => ContextReason, cefa: LCPEvalArguments) = {
    lhs == null || rhsSplit.isEmpty match {
      case true =>
        val err = s"Assignment statement not okay. context line = ${farg.context}, lhs = $lhs, rhsSplit = $rhsSplit"
        FCO.logger.error(mps, err)
        ContextReason(new HashMap[String, String](), err, Some(AssignmentStmtError))
      case _ =>
        callback(lhs, rhsSplit.get, cefa)
    }
  }
}

abstract class AbstractLoaderContext(carg: ContextClassArguments, ACO: AbstractContextObject) extends AbstractContextClass(carg: ContextClassArguments, ACO: AbstractContextObject) {

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
        ACO.logger.error(mps, err)
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
