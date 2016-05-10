package com.glassbeam.context

import com.glassbeam.context.Context.{ContextClassArguments, MatchArguments}
import com.glassbeam.context.ContextStage.{apply => _}

import scala.util.matching.Regex


abstract class WatcherContextPattern(val fullRegex:Regex,val name:String) extends WatcherContextStatement

case class WatcherInstance(warg:ContextClassArguments,WOP:WatcherContextPattern) extends AbstractWatcherContext(warg,WOP)

object WatcherObject {
  def getObject(carg: ContextClassArguments,WOP:WatcherContextPattern): AbstractWatcherContext = WatcherInstance(carg:ContextClassArguments,WOP:WatcherContextPattern)
}

object WatcherStatements extends Enumeration {

  val DelPat,SkipPat,IncVaultPat,IncParsePat,BinaryPat,TextPat,ReversePat,MostRecPat,ProcessFilePat,UncompressLevelPat = Value

  def getDefinition(typ:Value):WatcherContextPattern = typ match {
    case DelPat               => new WatcherContextPattern("""\s*(?i)f.deletefiles\s*=(.+?)""".r,DelPat.toString) with MWatcherState
    case SkipPat              => new WatcherContextPattern("""\s*(?i)f.skipfiles\s*=(.+?)""".r,SkipPat.toString) with MWatcherState
    case IncVaultPat          => new WatcherContextPattern("""\s*(?i)f.includevault\s*=(.+?)""".r,IncVaultPat.toString) with MWatcherState
    case IncParsePat          => new WatcherContextPattern("""\s*(?i)f.includeparse\s*=(.+?)""".r,IncParsePat.toString) with MWatcherState
    case BinaryPat            => new WatcherContextPattern("""\s*(?i)f.binary\s*=(.+?)""".r,BinaryPat.toString) with MWatcherState
    case TextPat              => new WatcherContextPattern("""\s*(?i)f.text\s*=(.+?)""".r,TextPat.toString) with MWatcherState
    case ReversePat           => new WatcherContextPattern("""\s*(?i)f.reversefiles\s*=(.+?)""".r,ReversePat.toString) with MWatcherState
    case MostRecPat           => new WatcherContextPattern("""f.selectFile\s*\((.+?)\)\s*$""".r,MostRecPat.toString) with MWatcherState
    case ProcessFilePat       => new WatcherContextPattern("""e.processToFile\s+/(.+?)/\s+(.+?)""".r,ProcessFilePat.toString) with MWatcherState
    case UncompressLevelPat   => new WatcherContextPattern("""b.uncompress.level\s+/(.+?)/\s+(\d+)""".r,UncompressLevelPat.toString) with MWatcherState
  }

  def unapply(ma:MatchArguments): Option[WatcherContextPattern] = this.values.map(getDefinition).find(wopat => wopat.fullRegex.pattern.matcher(ma.conline).matches() && wopat.contextSection == ma.cSection)
}



