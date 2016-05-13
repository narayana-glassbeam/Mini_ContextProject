package com.glassbeam.context

import java.io.File

import akka.actor.{Actor, Props}
import com.glassbeam.context.Context._
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.Logger

import scala.collection.immutable.HashMap

object BundleContext {

  def bundleContext_name(loadid:Long) = "context_"+loadid

  def props(loadid:Long, mps:String, MWCache:Map[String, Vector[AWatcherContextExtract]], MLCache:Map[String, Vector[ALoaderContextExtract]]) = {
    (Props(classOf[BundleContext],MWCache,MLCache,loadid,mps),bundleContext_name(loadid))
  }

}

class BundleContext(WatcherFunction:Map[String, Vector[AWatcherContextExtract]], LoaderFunction:Map[String, Vector[ALoaderContextExtract]], loadid:Long, emps:String) extends Actor with Logger {

  private val logger = Logging(this)


  def isFileMatched(fileName:String,ContextPatternName:String):Boolean = {

    val wca = WatcherEvalArguments(fileName,emps)

     WatcherFunction.get(ContextPatternName) match {
      case Some(instances) =>
          instances.exists(inst => inst.evalFileMatchesPattern(wca))
      case None =>
        logger.error(s"No ContextIsntances Found for $ContextPatternName")
        false
    }

  }

  def evalContext(mps:String,loadid:Long,filename:String):ContextReason = {
    val file_eval:File = new File(filename)
    var cr = ContextReason(HashMap[String, String](), "")
    val assignmentInstances = LoaderFunction.getOrElse(LoaderAssignment.getName,Vector())
    val statementInstances =  LoaderFunction.getOrElse(LoaderStatements.getName,Vector())
    val contextInstances = Vector(assignmentInstances,statementInstances).flatten
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
      val wca = WatcherEvalArguments(fileName,emps)
      val result:Int = WatcherFunction.get(UncompressLevelPat.name) match {
        case Some(insts) => insts.view.map(_.evalUncompressLevel(wca)).head
        case None =>
          logger.error(s" MaxUncompresLevel not found for file name ${fileName} for mps ${emps}")
          5
      }

      sender ! result

    case EvalFileContext(filename,mps,loadid) => sender ! evalContext(mps,loadid,filename)

  }
}
