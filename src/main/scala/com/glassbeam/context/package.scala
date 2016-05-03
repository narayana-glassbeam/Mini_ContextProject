package com.glassbeam.context

/**
  * Created by narayana on 22/4/16.
  */


object ContextCases {

  sealed trait Context

//  trait ActorCreationSupport {
//
//    def createChild(props:Props,name:String):ActorRef
//
//    def forward[ReqTyp <: Context](msg:ReqTyp,actor_name:String)
//  }

  trait MPSRequest extends Context { def mps:String }
  case class InitializeContext(mps:String) extends MPSRequest
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

  case class file_eval(fileName:String,mps:String,loadid:Long) extends LoaderContext

}
