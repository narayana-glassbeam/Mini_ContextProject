package com.glassbeam.context

import java.io.File

import akka.actor.{Actor, Props}
import com.glassbeam.context.Context._
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.Logger
import com.glassbeam.context.WatcherStatements._

import scala.collection.immutable.HashMap

object BundleContext {

  def bundleContext_name(loadid:Long) = "context_"+loadid

  def props(loadid:Long, mps:String, MWCache:Map[String, Vector[AbstractWatcherContext]], MLCache:Map[String, Vector[AbstractLoaderContext]]) = {
    (Props(classOf[BundleContext],MWCache,MLCache,loadid,mps),bundleContext_name(loadid))
  }

}

class BundleContext(WatcherFunction:Map[String, Vector[AbstractWatcherContext]],LoaderFunction:Map[String, Vector[AbstractLoaderContext]],loadid:Long,emps:String) extends Actor with Logger {

  private val logger = Logging(this)


  def isFileMatched(fileName:String,ContextPatternName:String):Boolean = {

    val wca = WatcherEvalArguments(fileName,emps)
    var filematched = false
    WatcherFunction.get(ContextPatternName) match {
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

  def evalContext(mps:String,loadid:Long,filename:String):ContextReason = {
    val file_eval:File = new File(filename)
    var cr = ContextReason(HashMap[String, String](), "")
    val contextInstances = LoaderFunction.get(mps).get
    //contextInstances.foreach(f => println("lc name "+f.arg.context))
    for(context_instance <- contextInstances;if cr.reason.isEmpty){
      try {
        val cefa = LoaderEvalArguments(cr, file_eval, loadid,mps)
        cr = context_instance.execute(cefa)
      }catch {
        case e:Exception =>
          val err = s"exception while processing context [Class Args: ${context_instance.arg}] [Execute Args: $cr]"
          cr.copy(reason = cr.reason + err, failure = Some(ContextExecFailure))
          logger.error(e,mps,err)
      }
    }
    cr
  }

  def receive = {

    case DeleteFile(fileName,mps,loadid)=> sender ! isFileMatched(fileName,DelPat.toString)

    case SkipFile(fileName,mps,loadid) =>  sender ! isFileMatched(fileName,SkipPat.toString)

    case BinaryFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,BinaryPat.toString)

    case ReverseFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,ReversePat.toString)

    case TextFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,TextPat.toString)

    case IncludeVaultFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,IncVaultPat.toString)

    case IncludeParseFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,IncParsePat.toString)

    case MostRecentFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,MostRecPat.toString)

    case ExtensibilityFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,ProcessFilePat.toString)

    case UncompressBundleDepth(fileName,mps,loadid) =>
      val wca = WatcherEvalArguments(fileName,emps)
      var depth:Int = 0
      WatcherFunction.get(UncompressLevelPat.toString) match {
        case Some(insts) =>
          insts.foreach(uncompressInst =>{ depth = uncompressInst.evalUncompressLevel(wca)})
        case None =>
          logger.error(s" MaxUncompresLevel not found for file name ${fileName} for mps ${emps}")
      }
      sender ! depth

    case EvalFileContext(filename,mps,loadid) => sender ! evalContext(mps,loadid,filename)

  }
}
