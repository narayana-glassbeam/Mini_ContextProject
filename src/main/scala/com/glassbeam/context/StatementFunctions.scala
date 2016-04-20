import com.glassbeam.context.ContextStage.{apply => _}
import com.glassbeam.context.{ContextReason, _}

import scala.collection.immutable.HashMap
import scala.util.matching.Regex

object DeletePattern extends ContextStatementObject {
  override val fullRegex: Regex = """\s*(?i)deletefiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new DeletePattern(carg)
}

class DeletePattern(carg: ContextClassArguments) extends AbstractContextClass(carg, DeletePattern) with MutableWatcherFunction {

  private def getDeleteFile(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {
    // ToDo: Create the regex and match it with the filename
    val hm: Map[String, String] = Map("deleted" -> "true")
    ContextReason(hm, "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getDeleteFile, cefa)
}

object SkipPattern extends ContextStatementObject with MutableWatcherFunction {
  override val fullRegex:Regex = """\s*(?i)skipfiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new SkipPattern(carg)
}

class SkipPattern(carg: ContextClassArguments) extends AbstractContextClass(carg,SkipPattern) with MutableWatcherFunction {

  private def getSkipFile(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getSkipFile, cefa)
}

object UncompressLevel extends ContextStatementObject {
  override val fullRegex:Regex = """b.uncompress.level\s+/(.+?)/\s+(\d+)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new UncompressLevel(carg)
}

class UncompressLevel(carg: ContextClassArguments) extends AbstractContextClass(carg,UncompressLevel) with MutableWatcherFunction {

  private def getMaxUnCompressLevel(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {
    val hm: Map[String, String] = Map("maxdepth" -> "5")
    ContextReason(hm, "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getMaxUnCompressLevel, cefa)

}


object IncludeLogVault extends ContextStatementObject with MutableWatcherFunction {
  override val fullRegex:Regex = """\s*(?i)includevault\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new IncludeLogVault(carg)
}
class IncludeLogVault(carg: ContextClassArguments) extends AbstractContextClass(carg,IncludeLogVault) with MutableWatcherFunction {

  private def getIncludeLogVault(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {
    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getIncludeLogVault, cefa)

}


object IncludeParse extends ContextStatementObject {
  override val fullRegex:Regex = """\s*(?i)includeparse\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new IncludeParse(carg)
}

class IncludeParse(carg: ContextClassArguments) extends AbstractContextClass(carg,IncludeParse) with MutableWatcherFunction {

  private def getIncludeParse(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {
    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getIncludeParse, cefa)

}

object BinaryFilePattern extends ContextStatementObject {
  override val fullRegex:Regex = """\s*(?i)f.binary\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new BinaryFilePattern(carg)
}
class BinaryFilePattern(carg: ContextClassArguments) extends AbstractContextClass(carg,BinaryFilePattern) with MutableWatcherFunction {

  private def getBinaryFilePattern(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getBinaryFilePattern, cefa)

}

object TextFile extends ContextStatementObject {
  override val fullRegex:Regex = """\s*(?i)f.text\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new TextFile(carg)
}
class TextFile(carg: ContextClassArguments) extends AbstractContextClass(carg,TextFile) with MutableWatcherFunction {

  private def getParseFilePattern(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getParseFilePattern, cefa)
}



object ReverseFile extends ContextStatementObject {
  override val fullRegex:Regex = """\s*(?i)reversefiles\s*=(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new ReverseFile(carg)
}
class ReverseFile(carg: ContextClassArguments) extends AbstractContextClass(carg,ReverseFile) with MutableWatcherFunction {

  private def getReverseFilePattern(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getReverseFilePattern, cefa)
}




object MostRecentFile extends ContextStatementObject {
  override val fullRegex:Regex = """selectFile\s*\((.+?)\)\s*$""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new MostRecentFile(carg)
}
class MostRecentFile(carg: ContextClassArguments) extends AbstractContextClass(carg,MostRecentFile) with MutableWatcherFunction {

  private def getMostRecentFilePattern(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getMostRecentFilePattern, cefa)
}


object ProcessFile extends ContextStatementObject {
  override val fullRegex:Regex = """e.processToFile\s+/(.+?)/\s+(.+?)""".r
  override def getObject(carg: ContextClassArguments): AbstractContextClass = new ProcessFile(carg)
}
class ProcessFile(carg: ContextClassArguments) extends AbstractContextClass(carg,ProcessFile) with MutableWatcherFunction {

  private def getProcessFilePattern(texts: Option[List[String]], cefa: ContextExecFnArguments): ContextReason = {

    ContextReason(new HashMap[String, String](), "")
  }

  def execute(cefa: ContextExecFnArguments): ContextReason = evalStatement(getProcessFilePattern, cefa)
}
