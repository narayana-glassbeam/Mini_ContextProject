package com.glassbeam.context

import java.io.File

import com.glassbeam.context.ContextSection.ContextSection
import com.glassbeam.context.ContextStage.ContextStage
import com.glassbeam.model.ContextFailure._

import scala.collection.immutable.HashMap

/**
  * Created by narayana on 22/4/16.
  */


object Context {

  sealed trait Context

//  trait ActorCreationSupport {
//
//    def createChild(props:Props,name:String):ActorRef
//
//    def forward[ReqTyp <: Context](msg:ReqTyp,actor_name:String)
//  }

  trait MPSRequest extends Context { def mps:String }
  case class buildContext(mps:String,mMpsContextLines:Array[String]) extends MPSRequest
  case class CreateBundleContext(loadid:Long,mps:String) extends MPSRequest

  trait BundleEval extends MPSRequest {
    def fileName:String
    def loadid:Long
  }
  trait WatcherContext extends BundleEval
  trait LoaderContext extends BundleEval

  case class DeleteFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class SkipFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class BinaryFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class ReverseFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class TextFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class IncludeVaultFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class IncludeParseFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class MostRecentFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class ExtensibilityFile(fileName:String, mps:String,loadid:Long) extends WatcherContext
  case class UncompressBundleDepth(fileName:String,mps:String,loadid:Long) extends WatcherContext

  case class EvalFileContext(fileName:String,mps:String,loadid:Long) extends LoaderContext

  sealed trait Arguments
  trait ClassArguments extends Arguments {
    def context:String
    def customer:String
    def manufacturer:String
    def product:String
    def schema:String
  }

  trait EvalArguments extends Arguments {
    def mps:String
  }

  case class ContextReason(val contextStrings: HashMap[String, String], val reason: String, val failure: Option[ContextFailure] = None,
                           val bproperties: Option[Map[String, String]] = None)
  case class ContextClassArguments(context: String, linenum: Int, customer: String, manufacturer: String, product: String, schema: String) extends ClassArguments
  case class LoaderEvalArguments(cr: ContextReason, file: File, loadId: Long,mps:String) extends EvalArguments

  case class LCPEvalArguments(cr: ContextReason,mps:String) extends EvalArguments

  case class WatcherEvalArguments(file_name:String,mps:String) extends EvalArguments

  case class MatchArguments(conline:String,cSection:ContextSection) extends Context

}

object ContextSection extends Enumeration {
  type ContextSection = Value
  val MutableState, ImmutableState = Value
}

object ContextStage extends Enumeration {
  type ContextStage = Value
  val Watcher, Loader, Both = Value
}

trait MLoaderState {
  val contextSection: ContextSection = ContextSection.MutableState
  val contextStage: ContextStage = ContextStage.Loader
}

trait MWatcherState {
  val contextSection: ContextSection = ContextSection.MutableState
  val contextStage: ContextStage = ContextStage.Watcher
}

trait ILcpState {
  val contextSection:ContextSection = ContextSection.ImmutableState
  val contextStage:ContextStage = ContextStage.Loader
}

trait MLcpState {
  val contextSection:ContextSection = ContextSection.MutableState
  val contextStage:ContextStage = ContextStage.Loader
}
