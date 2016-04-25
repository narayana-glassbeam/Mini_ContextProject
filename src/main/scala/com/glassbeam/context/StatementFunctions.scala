package com.glassbeam.context

import com.glassbeam.context.ContextStage.{apply => _}

import scala.util.matching.Regex


//context classes  at watcher level file match pattern
case class DeleteFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg, DelPat) with MutableWatcherFunction
case class SkipFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg, SkipPat) with MutableWatcherFunction
case class IncludeLogVaultClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg, IncVaultPat) with MutableWatcherFunction
case class IncludeParseClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg, IncParsePat) with MutableWatcherFunction
case class BinaryFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,BinaryPat) with MutableWatcherFunction
case class TextFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,TextPat) with MutableWatcherFunction
case class ReverseFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,ReversePat) with MutableWatcherFunction
case class MostRecentFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,MostRecPat) with MutableWatcherFunction
case class ProcessFileClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,ProcessFilePat) with MutableWatcherFunction
class UncompressLevelClass(carg: ContextClassArguments) extends AbstractWatcherContext(carg,UncompressLevel) with MutableWatcherFunction
//
object DelPat extends WatcherContextStatement {
  override val name = "DeletePattern"
  override val fullRegex: Regex = """\s*(?i)f.deletefiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = DeleteFileClass(carg: ContextClassArguments)
}


object SkipPat extends WatcherContextStatement {
  override val name = "SkipPattern"
  override val fullRegex:Regex = """\s*(?i)f.skipfiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext =  SkipFileClass(carg)
}


object IncVaultPat extends WatcherContextStatement  {
  override val name = "IncludeVaultPattern"
  override val fullRegex:Regex = """\s*(?i)f.includevault\s*=(.+?)""".r
  def getObject(carg: ContextClassArguments): AbstractWatcherContext = IncludeLogVaultClass(carg)
}



object IncParsePat extends WatcherContextStatement {
  override val name = "IncludeParsePattern"
  override val fullRegex:Regex = """\s*(?i)f.includeparse\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = IncludeParseClass(carg)
}


object BinaryPat extends WatcherContextStatement {
  override val name = "BinaryFilePattern"
  override val fullRegex:Regex = """\s*(?i)f.binary\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments):AbstractWatcherContext =  BinaryFileClass(carg)
}


object TextPat extends WatcherContextStatement {
  override val name = "TextFilePattern"
  override val fullRegex:Regex = """\s*(?i)f.text\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = TextFileClass(carg)
}


object ReversePat extends WatcherContextStatement {
  override val name = "ReversePattern"
  override val fullRegex:Regex = """\s*(?i)f.reversefiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = new ReverseFileClass(carg)
}


object MostRecPat extends WatcherContextStatement {
  override val name = "MostRecentPattern"
  override val fullRegex:Regex = """f.selectFile\s*\((.+?)\)\s*$""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext= new MostRecentFileClass(carg)
}


object ProcessFilePat extends WatcherContextStatement {
  override val name = "ProcessFilePattern"
  override val fullRegex:Regex = """e.processToFile\s+/(.+?)/\s+(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = new ProcessFileClass(carg)
}


object UncompressLevel extends WatcherContextStatement {
  override val name = "UncompressPattern"
  override val fullRegex:Regex = """b.uncompress.level\s+/(.+?)/\s+(\d+)""".r
  override def getObject(carg: ContextClassArguments): AbstractWatcherContext = new UncompressLevelClass(carg)
}



