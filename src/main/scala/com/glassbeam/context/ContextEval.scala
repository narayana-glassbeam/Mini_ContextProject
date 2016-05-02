package com.glassbeam.context

import akka.actor.{Actor, Props}
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.Logger
import com.glassbeam.model.StartupConfig._

import scala.collection.immutable.HashMap

/**
  * Created by bharadwaj on 29/03/16.
  */

object ContextEval extends Logger {
  import com.glassbeam.context.ContextHelpers._

  val logger = Logging(this)

  def ceval_name(mps:String) = "context_"+alphanumeric(mps)

  def props(mps:String,mutableLines:String,immutableLines:String) = {
    (Props(classOf[ContextEval],mps,mutableLines,immutableLines),ceval_name(mps))
  }

  private val watcher_statements: Array[WatcherContextStatement] = Array(DelPat,SkipPat,IncVaultPat,IncParsePat,BinaryPat,TextPat,ReversePat,MostRecPat,ProcessFilePat,UncompressLevel)

  private val loader_assignments: Array[LoaderContextAssignment] = Array(
    Lgrep, Mgrep, // Line level function
    Snow, SDF2EPOCH, EPOCH2SDF, // Time related functions
    Lcustomer, Lproduct, Lmanufacturer, Lschema, // Fetch known variables into context
    Fdate, Fname, Fpath, Flength, FnameGrep, Fgrep, FpathGrep, Fcount, // File level functions; Fgrep is at line level within file
    Bname, Bsize, Bgrep, BfnameGrep, // Bundle level
    AssertFileDuplicate, AssertNumeric, // Assertions in assignment form
    Concat, Coalesce, Lookup, XmlValue, // Variable level functions
    ProcessFileToContext, ProcessBundleToContext // Extensibility functions
  )

  private val loader_statements: Array[LoaderContextStatement] = Array(AssertUncompressionFail, Assert, AssertTruthy, Validate,
    CombineLines, BProperties, BId, AssertBundleDuplicate, AssertBundleFail,Encoding,AssertPxFileCount)
}

// ToDo: this Actor should be backed by a Router
class ContextEval(emps: String,mContext:String,immContext:String) extends Actor with ContextLines {
  import ContextEval._

  val splitKeys = emps.split(filesep)
  val (customer,manufacturer,product,schema) = ("",splitKeys(0), splitKeys(1), splitKeys(2))

  // mps to immutable variable values
  var immutableVariableCache: Map[String, ContextReason] = Map()

  // mps to mutable variable abstract object (which will be evaluated)
  var mutableVariableCache: Map[String, Array[AbstractContextClass]] = Map()

  // mps to mutable function abstract object (which will be evaluated)
  val mutableLoaderFunction: scala.collection.mutable.HashMap[String, Vector[AbstractLoaderContext]] = scala.collection.mutable.HashMap.empty[String,Vector[AbstractLoaderContext]]

  val mutableWatcherFunction:scala.collection.mutable.HashMap[String, Vector[AbstractWatcherContext]] =  scala.collection.mutable.HashMap.empty[String,Vector[AbstractWatcherContext]]

  def assignMatch(defn: AbstractContextObject, context: String): Boolean = {
    val rhsOption: Option[List[String]] = defn.fullRegex.unapplySeq(context)
    rhsOption match {
      case None => false
      case Some(rhs) =>
        rhs.length match {
          case 2 => defn.rhsRegex.pattern.matcher(rhs(1).trim).matches()
          case _ => false
        }
    }
  }

  def parseContext(mConLines:Array[String],immConLines:Array[String]) = {
    // ToDo: Query context from H2 and populate immutableVariableCache, mutableVariableCache and mutableFunctionCache
    // ToDo: Traverse the list of H2 context lines and match the fullRegex of DeleteFile object and store the DeleteFile object in mutableFunctionCache
    // ToDo: Store 'n' instances of DeleteFile object in mutableFunctionCache
    var linenum = 1
    for (context_line <- mConLines; context = context_line.trim) {
      if (context.nonEmpty) {
        var matchedSomething = false
        try {
          //println("in parse context "+context_line)
          for (watcher_regex <- watcher_statements; if !matchedSomething && watcher_regex.fullRegex.pattern.matcher(context).matches()) {
            matchedSomething = true
            //println("in parse context "+context_line)
            if (mutableWatcherFunction.contains(watcher_regex.name)) {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              var contextVec = mutableWatcherFunction.get(watcher_regex.name).get
              contextVec = contextVec :+ coninst
              mutableWatcherFunction(watcher_regex.name) = contextVec
              //println("in watcher match "+watcher_regex.name)
            } else {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableWatcherFunction += watcher_regex.name -> Vector(coninst)
              //println("in watcher match "+watcher_regex.name)
            }
          }
          for(loader_assign <- loader_assignments; if !matchedSomething && assignMatch(loader_assign, context)){
            logger.info(emps, s"matched as assignment, key = $emps, line = $linenum, context = $context, context-class = ${loader_assign.getClass.getName}")
            matchedSomething = true
            //println("in parse context "+context_line)
            //println("in loader assign boolean "+loader_assign.isAssignment)
            if (mutableLoaderFunction.contains(emps)) {
              val coninst = loader_assign.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              var contextVec = mutableLoaderFunction.get(emps).get
              contextVec = contextVec :+ coninst
              mutableLoaderFunction(emps) = contextVec
              //println("in loader assign if added to map  "+loader_assign.isAssignment)
            } else {
              val coninst = loader_assign.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableLoaderFunction += emps -> Vector(coninst)
             // println("in loader assign else added to map  "+loader_assign.isAssignment)
            }
          }
          for(loader_statm <- loader_statements; if !matchedSomething && loader_statm.fullRegex.pattern.matcher(context).matches()){
            logger.info(emps, s"matched as simple statement, key = $emps, line = $linenum, context = $context, context-class = ${loader_statm.getClass.getName}")
            matchedSomething = true
            //println("in parse context "+context_line)
            println("in loader stat  "+loader_statm.isAssignment)
            if (mutableLoaderFunction.contains(emps)) {
              val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              var contextVec = mutableLoaderFunction.get(emps).get
              contextVec = contextVec :+ coninst
              mutableLoaderFunction(emps) = contextVec
              println("in loader stat if added to map  "+loader_statm.isAssignment)
            } else {
              val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableLoaderFunction += emps -> Vector(coninst)
              println("in loader stat else added to map  "+loader_statm.isAssignment)
            }
          }
        }catch {
          case e: Exception =>
            val err = s"context match exception while working on key = $emps,  context line = $context "
            logger.error(err)
        }
      }
      linenum += 1
    }
    logger.info(s"For MPS "+emps+" mutable lines "+mConLines.mkString+" immutable lines "+immContext)
  }


  def receive = {

    case InitializeContext =>
      val (mLines,immLines) = getContextLines(mContext,immContext)
      parseContext(mLines,immLines)

    case LoadidToContext(loadid,mps) =>
      import com.glassbeam.context.LoadIdToContext._
      val immwfmap = HashMap(mutableWatcherFunction.toSeq:_*)
      val immlfmap = HashMap(mutableLoaderFunction.toSeq:_*)
      val child_props = props(loadid,mps,immwfmap,immlfmap)
      val loadid_context = context.actorOf(child_props._1,child_props._2)

    case msg:WatcherContext =>
      import com.glassbeam.context.LoadIdToContext._
      val childActorname = ltc_name(msg.loadid)
      context.child(childActorname) match {
        case Some(loadidContextActor) =>
          loadidContextActor.forward(msg)
        case None =>
          logger.error(s"child actor of mps ${msg.mps} not found")
      }

    case lc:LoaderContext =>
      import com.glassbeam.context.LoadIdToContext._
      val childActorname = ltc_name(lc.loadid)
      context.child(childActorname) match {
        case Some(loadidContextActor) =>
          loadidContextActor.forward(lc)
        case None =>
          logger.error(s"child actor of mps ${lc.mps} for loadid ${lc.loadid} not found")
      }

    case x =>
      logger.error(s"Unknown ContextEval message $x")
  }
}
