package com.glassbeam.context

import java.io.File

import akka.actor.{Actor, Props}
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.Logger

import scala.collection.immutable.HashMap

object BundleContext {

  def bundleContext_name(loadid:Long) = "context_"+loadid

  def props(loadid:Long, mps:String, MWCache:HashMap[String, Vector[AbstractWatcherContext]], MLCache:HashMap[String, Vector[AbstractLoaderContext]]) = {
    (Props(classOf[BundleContext],MWCache,MLCache,loadid,mps),bundleContext_name(loadid))
  }

}

class BundleContext(mutableWatcherFunction:HashMap[String, Vector[AbstractWatcherContext]],mutableLoaderFunction:HashMap[String, Vector[AbstractLoaderContext]],loadid:Long,emps:String) extends Actor with Logger {

  private val logger = Logging(this)


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

  def eval_Context(mps:String,loadid:Long,filename:String):ContextReason = {
    val file_eval:File = new File(filename)
    var cr = ContextReason(HashMap[String, String](), "")
    val contextInstances = mutableLoaderFunction.get(mps).get
    contextInstances.foreach(f => println("lc name "+f.arg.context))
    for(context_instance <- contextInstances;if cr.reason.isEmpty){
      try {
        val cefa = ContextExecFnArguments(cr, file_eval, loadid)
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

    case DeleteFile(fileName,mps,loadid)=> sender ! isFileMatched(fileName,DelPat.name)

    case SkipFile(fileName,mps,loadid) =>  sender ! isFileMatched(fileName,SkipPat.name)

    case BinaryFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,BinaryPat.name)

    case ReverseFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,ReversePat.name)

    case TextFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,TextPat.name)

    case IncludeVaultFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,IncVaultPat.name)

    case IncludeParseFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,IncParsePat.name)

    case MostRecentFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,MostRecPat.name)

    case ExtensibilityFile(fileName,mps,loadid) => sender ! isFileMatched(fileName,ProcessFilePat.name)

    case UncompressBundleDepth(fileName,mps,loadid) =>
      val wca = WatContextArguments(fileName,emps)
      var depth:Int = 0
      mutableWatcherFunction.get(UncompressLevel.name) match {
        case Some(insts) =>
          insts.foreach(uncompressInst =>{ depth = uncompressInst.evalUncompressLevel(wca)})
        case None =>
          logger.error(s" MaxUncompresLevel not found for file name ${fileName} for mps ${emps}")
      }
      sender ! depth

    case file_eval(filename,mps,loadid) => sender ! eval_Context(mps,loadid,filename)

  }
}
