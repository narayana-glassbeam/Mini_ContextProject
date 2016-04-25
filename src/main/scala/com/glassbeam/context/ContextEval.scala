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

  def name(mps:String) = "context_"+alphanumeric(mps)

  def props(mps:String,mutableLines:String,immutableLines:String) = {
    (Props(classOf[ContextEval],mps,mutableLines,immutableLines),name(mps))
  }

  val watcher_statements: Array[WatcherContextStatement] = Array(DelPat,SkipPat,IncVaultPat,IncParsePat,BinaryPat,TextPat,ReversePat,MostRecPat,ProcessFilePat,UncompressLevel)

}

// ToDo: this Actor should be backed by a Router
class ContextEval(emps: String,mContext:String,immContext:String) extends Actor with ContextLines {
  import ContextEval._

  override def preStart() = {

  }

  val splitKeys = emps.split(filesep)
  val (customer,manufacturer,product,schema) = ("",splitKeys(0), splitKeys(1), splitKeys(2))

  // mps to immutable variable values
  var immutableVariableCache: Map[String, ContextReason] = Map()

  // mps to mutable variable abstract object (which will be evaluated)
  var mutableVariableCache: Map[String, Array[AbstractContextClass]] = Map()

  // mps to mutable function abstract object (which will be evaluated)
  val mutableFunctionCache: scala.collection.mutable.HashMap[String, Vector[AbstractContextClass]] = scala.collection.mutable.HashMap.empty[String,Vector[AbstractContextClass]]

  val mutableWatcherFunction:scala.collection.mutable.HashMap[String, Vector[AbstractWatcherContext]] =  scala.collection.mutable.HashMap.empty[String,Vector[AbstractWatcherContext]]

  def parseContext(mConLines:Array[String],immConLines:Array[String]) = {
    // ToDo: Query context from H2 and populate immutableVariableCache, mutableVariableCache and mutableFunctionCache
    // ToDo: Traverse the list of H2 context lines and match the fullRegex of DeleteFile object and store the DeleteFile object in mutableFunctionCache
    // ToDo: Store 'n' instances of DeleteFile object in mutableFunctionCache
    var linenum = 1
    for (context_line <- mConLines; context = context_line.trim) {

      if (context.nonEmpty) {
        var matchedSomething = false
        try {
          for (watcher_regex <- watcher_statements; if !matchedSomething && watcher_regex.fullRegex.pattern.matcher(context).matches()) {
            matchedSomething = true
            if (mutableWatcherFunction.contains(watcher_regex.name)) {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              var contextVec = mutableWatcherFunction.get(watcher_regex.name).get
              contextVec = contextVec :+ coninst
              mutableWatcherFunction(watcher_regex.name) = contextVec
            } else {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableWatcherFunction += watcher_regex.name -> Vector(coninst)
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

  def isFileMatched(fileName:String,ContextPatternName:String):Boolean = {
    val wca = WatContextArguments(fileName,emps)
    var filematched = false
    mutableWatcherFunction.get(ContextPatternName) match {
      case Some(instances) =>
        instances.foreach(inst =>  if(!filematched) {
        filematched = inst.evalFileMatchesPattern(wca)
        })
        filematched
      case None =>
        logger.error(s"No ContextIsntances Found for $ContextPatternName")
        filematched
    }

  }


  def receive = {

    case InitializeContext =>
      val (mLines,immLines) = getContextLines(mContext,immContext)
      parseContext(mLines,immLines)

    case ContextLoaderEval =>
      sender ! ContextReason(HashMap[String, String](), "")

    case ContextLoaderWatcherEval =>
      sender ! ContextReason(HashMap[String, String](), "")

    case ContextWatcherFileEval =>

    case DeleteFile(fileName:String,mps:String)=>  sender ! isFileMatched(fileName,DelPat.name)

    case SkipFile(fileName:String,mps:String) =>  sender ! isFileMatched(fileName,SkipPat.name)

    case BinaryFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,BinaryPat.name)

    case ReverseFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,ReversePat.name)

    case TextFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,TextPat.name)

    case IncludeVaultFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,IncVaultPat.name)

    case IncludeParseFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,IncParsePat.name)

    case MostRecentFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,MostRecPat.name)

    case ExtensibilityFile(fileName:String,mps:String) => sender ! isFileMatched(fileName,ProcessFilePat.name)

    case UncompressBundleDepth(fileName:String,mps:String) =>
      val wca = WatContextArguments(fileName,emps)
      val depth =  mutableWatcherFunction.get(UncompressLevel.name) match {
        case Some(insts) =>
          insts.foreach(uncompressInst => uncompressInst.evalUncompressLevel(wca))
        case None =>
          logger.error(s" MaxUncompresLevel not found for file name ${fileName} for mps ${emps}")
          0
      }
      sender ! depth.asInstanceOf[Int]

    case ContextWatcherBundleEval =>
      // ToDo: Evaluate all the 'watcher' functions from mutableFunctionCache. Build the Context Map and return it
      mutableFunctionCache.get(emps) match {
        case Some(arrayFunctions) =>


        case None => logger.error(s"Something wrong. None of functions in $mutableFunctionCache belonged to watcher")
      }
      sender ! ContextReason(HashMap[String, String](), "")

    case ContextIsChanged =>
      // 1) this message is sent on bundle receipt at Watcher to check if context has been modified
      // 2) send sender true if context has been changed; false if context is unmodified
      // 3) when true, write the modified context to a context audit trail table in H2
      sender ! false

    case ContextReload =>
      // Objective: Recache the Context after a modification for loader and watcher level mutable variables and functions
      // 1) refresh the mutableVariableCache and mutableFunctionCache
      // 2) if no errors in refreshed cache return true, else return false
      sender ! false

    case x =>
      logger.error(s"Unknown ContextEval message $x")
  }
}
