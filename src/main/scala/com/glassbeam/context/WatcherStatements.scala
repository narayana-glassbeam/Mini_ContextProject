package com.glassbeam.context

import com.glassbeam.context.Context.{ContextClassArguments, MatchArguments}
import com.glassbeam.context.ContextStage.{apply => _}

import scala.util.matching.Regex

case class WatcherInstanceExtract(warg:ContextClassArguments, wop:WatcherContextStatement) extends AWatcherContextExtract(warg,wop)

object WatcherStatements {

  val watcherPatterns:Array[WatcherContextStatement] = Array(DelPat,SkipPat,IncVaultPat,IncParsePat,BinaryPat,TextPat,ReversePat,MostRecPat,ProcessFilePat,UncompressLevelPat)

  def getObject(carg: ContextClassArguments,wo:WatcherContextStatement): AWatcherContextExtract = WatcherInstanceExtract(carg:ContextClassArguments,wo:WatcherContextStatement)

  def unapply(ma:MatchArguments): Option[WatcherContextStatement] = watcherPatterns.find(wopat => wopat.fullRegex.pattern.matcher(ma.conline).matches() && wopat.contextSection == ma.cSection)
}

object DelPat extends WatcherContextStatement with MWatcherState {
     override val name = "DeletePattern"
     override val fullRegex: Regex = """\s*(?i)f.deletefiles\s*=(.+?)""".r
}

object SkipPat extends WatcherContextStatement with MWatcherState {
     override val name = "SkipPattern"
     override val fullRegex:Regex = """\s*(?i)f.skipfiles\s*=(.+?)""".r
}
 
 
object IncVaultPat extends WatcherContextStatement with MWatcherState {
     override val name = "IncludeVaultPattern"
     override val fullRegex:Regex = """\s*(?i)f.includevault\s*=(.+?)""".r
}

object IncParsePat extends WatcherContextStatement with MWatcherState {
     override val name = "IncludeParsePattern"
     override val fullRegex:Regex = """\s*(?i)f.includeparse\s*=(.+?)""".r
}

object BinaryPat extends WatcherContextStatement with MWatcherState {
  override val name = "BinaryFilePattern"
  override val fullRegex: Regex = """\s*(?i)f.binary\s*=(.+?)""".r
}

object TextPat extends WatcherContextStatement with MWatcherState {
     override val name = "TextFilePattern"
     override val fullRegex:Regex = """\s*(?i)f.text\s*=(.+?)""".r
}

object ReversePat extends WatcherContextStatement with MWatcherState {
     override val name = "ReversePattern"
     override val fullRegex:Regex = """\s*(?i)f.reversefiles\s*=(.+?)""".r
}
 
object MostRecPat extends WatcherContextStatement with MWatcherState {
     override val name = "MostRecentPattern"
     override val fullRegex:Regex = """f.selectFile\s*\((.+?)\)\s*$""".r
}

object ProcessFilePat extends WatcherContextStatement with MWatcherState {
     override val name = "ProcessFilePattern"
     override val fullRegex:Regex = """e.processToFile\s+/(.+?)/\s+(.+?)""".r
}

object UncompressLevelPat extends WatcherContextStatement with MWatcherState {
  override val name = "UncompressPattern"
  override val fullRegex: Regex = """b.uncompress.level\s+/(.+?)/\s+(\d+)""".r
}



