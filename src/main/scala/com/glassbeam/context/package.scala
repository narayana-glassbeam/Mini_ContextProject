package com.glassbeam.context

/**
  * Created by narayana on 22/4/16.
  */


object ContextCases {

  sealed trait Context

  trait WatcherContext extends Context {
    def fileName:String
    def mps:String
    def loadid:Long
  }


  case class LoadidToContext(loadid:Long,mps:String)

  case object ContextLoaderEval extends Context
  case object ContextWatcherBundleEval extends Context
  case object ContextWatcherFileEval extends Context
  case object ContextLoaderWatcherEval extends Context
  case object ContextIsChanged extends Context
  case object ContextReload extends Context
  case object InitializeContext extends Context

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

}
