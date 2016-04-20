package com.glassbeam.context

import akka.actor.{Actor, Props}
import com.glassbeam.model.Logger

import scala.collection.immutable.HashMap

/**
  * Created by bharadwaj on 29/03/16.
  */
sealed trait ContextCases
case object ContextLoaderEval extends ContextCases
case object ContextWatcherBundleEval extends ContextCases
case object ContextWatcherFileEval extends ContextCases
case object ContextLoaderWatcherEval extends ContextCases
case object ContextIsChanged extends ContextCases
case object ContextReload extends ContextCases

object ContextEval extends Logger {
  import com.glassbeam.context.ContextHelpers._

  val logger = Logging(this)

  def name(mps:String) = "context_"+alphanumeric(mps)

  def props(mps:String,mutableLines:String,immutableLines:String) = {
    (Props(classOf[ContextEval],mps,mutableLines,immutableLines),name(mps))
  }
}

// ToDo: this Actor should be backed by a Router
class ContextEval(emps: String,mContext:String,immContext:String) extends Actor with ContextLines {
  import ContextEval._

  override def preStart() = {
    val (mLines,immLines) = getContextLines(mContext,immContext)
    parseContext(mLines,immLines)
  }


  // mps to immutable variable values
  var immutableVariableCache: Map[String, ContextReason] = Map()

  // mps to mutable variable abstract object (which will be evaluated)
  var mutableVariableCache: Map[String, Array[AbstractContextClass]] = Map()

  // mps to mutable function abstract object (which will be evaluated)
  var mutableFunctionCache: Map[String, Array[AbstractContextClass]] = Map()

  def parseContext(mConLines:Array[String],immConLines:Array[String]) = {
    // ToDo: Query context from H2 and populate immutableVariableCache, mutableVariableCache and mutableFunctionCache
    // ToDo: Traverse the list of H2 context lines and match the fullRegex of DeleteFile object and store the DeleteFile object in mutableFunctionCache
    // ToDo: Store 'n' instances of DeleteFile object in mutableFunctionCache

    logger.info(s"For MPS "+emps+" mutable lines "+mConLines+" immutable lines "+immContext)
    println(s"For MPS "+emps+" mutable lines "+mConLines.mkString+" immutable lines "+immContext)
  }

  def receive = {

    case ContextLoaderEval =>
      sender ! ContextReason(HashMap[String, String](), "")

    case ContextLoaderWatcherEval =>
      sender ! ContextReason(HashMap[String, String](), "")

    case ContextWatcherFileEval =>

    case ContextWatcherBundleEval =>
      // ToDo: Evaluate all the 'watcher' functions from mutableFunctionCache. Build the Context Map and return it
      mutableFunctionCache.get(emps) match {
        case Some(arrayFunctions) =>

//          arrayFunctions.filter(_.contextStage == Watcher).foreach {
//            val xyz = _.execute(xyz)
//          }

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
