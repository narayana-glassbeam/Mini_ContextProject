package com.glassbeam.context

import com.glassbeam.context.Context.LoaderClassArguments
import com.glassbeam.context.ContextStage.{apply => _}

import scala.util.matching.Regex


//context classes  at watcher level file match pattern
case class DeleteFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg, DelPat)
case class SkipFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg, SkipPat)
case class IncludeLogVaultClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg, IncVaultPat)
case class IncludeParseClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg, IncParsePat)
case class BinaryFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,BinaryPat)
case class TextFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,TextPat)
case class ReverseFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,ReversePat)
case class MostRecentFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,MostRecPat)
case class ProcessFileClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,ProcessFilePat)
case class UncompressLevelClass(carg: LoaderClassArguments) extends AbstractWatcherContext(carg,UncompressLevel)
//
object DelPat extends WatcherContextStatement with MWatcherState {
  override val name = "DeletePattern"
  override val fullRegex: Regex = """\s*(?i)f.deletefiles\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = DeleteFileClass(carg: LoaderClassArguments)
}


object SkipPat extends WatcherContextStatement with MWatcherState {
  override val name = "SkipPattern"
  override val fullRegex:Regex = """\s*(?i)f.skipfiles\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext =  SkipFileClass(carg)
}


object IncVaultPat extends WatcherContextStatement with MWatcherState {
  override val name = "IncludeVaultPattern"
  override val fullRegex:Regex = """\s*(?i)f.includevault\s*=(.+?)""".r
  def getObject(carg: LoaderClassArguments): AbstractWatcherContext = IncludeLogVaultClass(carg)
}



object IncParsePat extends WatcherContextStatement with MWatcherState {
  override val name = "IncludeParsePattern"
  override val fullRegex:Regex = """\s*(?i)f.includeparse\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = IncludeParseClass(carg)
}


object BinaryPat extends WatcherContextStatement with MWatcherState {
  override val name = "BinaryFilePattern"
  override val fullRegex:Regex = """\s*(?i)f.binary\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments):AbstractWatcherContext =  BinaryFileClass(carg)
}


object TextPat extends WatcherContextStatement with MWatcherState {
  override val name = "TextFilePattern"
  override val fullRegex:Regex = """\s*(?i)f.text\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = TextFileClass(carg)
}


object ReversePat extends WatcherContextStatement with MWatcherState {
  override val name = "ReversePattern"
  override val fullRegex:Regex = """\s*(?i)f.reversefiles\s*=(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = new ReverseFileClass(carg)
}


object MostRecPat extends WatcherContextStatement with MWatcherState {
  override val name = "MostRecentPattern"
  override val fullRegex:Regex = """f.selectFile\s*\((.+?)\)\s*$""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext= new MostRecentFileClass(carg)
}


object ProcessFilePat extends WatcherContextStatement with MWatcherState {
  override val name = "ProcessFilePattern"
  override val fullRegex:Regex = """e.processToFile\s+/(.+?)/\s+(.+?)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = new ProcessFileClass(carg)
}


object UncompressLevel extends WatcherContextStatement with MWatcherState {
  override val name = "UncompressPattern"
  override val fullRegex:Regex = """b.uncompress.level\s+/(.+?)/\s+(\d+)""".r
  override def getObject(carg: LoaderClassArguments): AbstractWatcherContext = new UncompressLevelClass(carg)
}



