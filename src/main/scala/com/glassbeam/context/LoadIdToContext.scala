package com.glassbeam.context

import akka.actor.{Actor, Props}
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.Logger

import scala.collection.immutable.HashMap

object LoadIdToContext {

  def ltc_name(loadid:Long) = "context_"+loadid

  def props(loadid:Long, mps:String, MWCache:HashMap[String, Vector[AbstractWatcherContext]], MLCache:HashMap[String, Vector[AbstractLoaderContext]]) = {
    (Props(classOf[LoadIdToContext],MWCache,MLCache,loadid,mps),ltc_name(loadid))
  }

}

class LoadIdToContext(mutableWatcherFunction:HashMap[String, Vector[AbstractWatcherContext]],mutableLoaderFunction:HashMap[String, Vector[AbstractWatcherContext]],loadid:Long,emps:String) extends Actor with Logger {

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

    //case file_eval(filename:File,mps,loadid)
  }
}
