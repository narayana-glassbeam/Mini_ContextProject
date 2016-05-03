package com.glassbeam.context

import java.nio.charset.CodingErrorAction
import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap

import com.glassbeam.model.{LoadIdDao, OpsDao}

import scala.collection.JavaConverters._
import scala.collection.immutable
import scala.collection.immutable.HashMap
import scala.io.Codec


object Init extends Enumeration {
  type Init = Value
  val Run, Test, TestWithH2, TestWithCassandra, TestWithSolr = Value
  var inittype = Run
}

object ContextHelpers {

  val BPROPERTIES = "b.properties"
  val BKEY = "key"
  val BVALUE = "value"
  val BID = "b.id"
  val hh_r = """\{(.*?)\}""".r
  val ctx_r = """context.(.*)""".r

  val extensibility_cache = new ConcurrentHashMap[String, Parsable]() asScala

  def getParsable(classname: String): Parsable =
    extensibility_cache.getOrElseUpdate(classname,Class.forName(classname).newInstance().asInstanceOf[Parsable])

  case class BundleCacheKey(val loadId: Long, val linenum: Int)
  case class BundleCacheValue(val timestamp: Long, val bgrep: String, val bfnamegrep: String, val fcount: String)

  val BundleCache = new ConcurrentHashMap[BundleCacheKey, BundleCacheValue]() asScala

  def alphanumeric(name: String): String = {
    // name.replaceAll("[^A-Za-z0-9]", "")
    name.replaceAll("/", "_").replaceAll(":", "_")
  }

  def getParm(p1: String, contextStrings: HashMap[String, String]): String = {
    if (p1 == null) ""
    else if (p1.trim.head == '\'' && p1.trim.last == '\'')
      p1.trim.init.tail
    else if (p1.trim.head == '\"' && p1.trim.last == '\"')
      p1.trim.init.tail
    else
      contextStrings.getOrElse(p1.trim, "") // Dont trim value
  }

  def getBundleNameSize(fileName: String): (String, Long) = {
    val loadId = OpsDao.getLoadIdByName(fileName)
    if (loadId.isEmpty) throw new Exception(s"getBundleNameSize failed for file = ${fileName}")
    val name = LoadIdDao.getNameSizeByLoadId(loadId.get)
    if (name.isEmpty) throw new Exception(s"getBundleNameSize failed for loadid = ${loadId.get}")
    name.get
  }


  def getFileCodec(cx: immutable.HashMap[String, String]): scala.io.Codec = {
    val codec = Codec(cx.getOrElse("t.encoding", "US-ASCII")) // by default we use US-ASCII
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    codec
  }

  def processContext(contextStrings: HashMap[String, String], ctxStr: String, strBuilder: StringBuilder): Unit = {
    val ctxKey = ctx_r.findAllIn(ctxStr).matchData.toList.map(x => x.group(1)).head
    val ctxVal = contextStrings.getOrElse(ctxKey, "")
    val reconstruct = "{context." + ctxKey + "}"
    val st = strBuilder.indexOf(reconstruct)
    val en = st + reconstruct.length
    val newStr = strBuilder.replace(st, en, ctxVal)
  }

  def substituteContextInMsg(msgstr: String, contextStrings: HashMap[String, String]): String = {
    val strb = new StringBuilder(msgstr)
    try {
      hh_r.findAllIn(msgstr).matchData foreach {m => processContext(contextStrings, m.group(1), strb)}
    } catch {
      case ex: Exception => //logger.error(msgstr,s"Error while substituting context in custom message.")
    }
    strb.toString
  }

  def stripQuotes(x: String): String =
    if (x != null && x.nonEmpty &&
      ((x.head == '\'' && x.last == '\'') || (x.head == '/' && x.last == '/') || (x.head == '\"' && x.last == '\"'))) x.init.tail
    else x

}

//class ParseException(msg: String)            extends Exception(msg)
//class NamespaceException(ex: Throwable)      extends ParseException(ex.getMessage)
//class TableException(ex: Throwable)          extends ParseException(ex.getMessage)
//class ChildRefParentException(ex: Throwable) extends ParseException(ex.getMessage)
//class ColumnException(ex: Throwable)         extends ParseException(ex.getMessage)
//class IconException(ex: Throwable)           extends ParseException(ex.getMessage)
//
//object ParseException          { def apply(msg: String)   = new ParseException(msg)}
//object NamespaceException      { def apply(ex: Throwable) = new NamespaceException(ex)}
//object TableException          { def apply(ex: Throwable) = new TableException(ex)}
//object ChildRefParentException { def apply(ex: Throwable) = new ChildRefParentException(ex)}
//object ColumnException         { def apply(ex: Throwable) = new ColumnException(ex)}
//object IconException           { def apply(ex: Throwable) = new IconException(ex)}
//
//class ParseExceptionMessage(val pe: Exception, val file: String, val loadid: Long, val ec: String, val mfr: String,
//                            val prod: String, val sch: String, val obsts: Timestamp, val sysid: String)
//
//case class IconExceptionMessage(ie: IconException, colname: String, override val file: String, override val loadid: Long,
//                                override val ec: String, override val mfr: String, override val prod: String,
//                                override val sch: String, override val obsts: Timestamp, override val sysid: String)
//  extends ParseExceptionMessage(ie, file, loadid, ec, mfr, prod, sch, obsts, sysid)


trait ContextLines {
  private val newLineChar = "\r\n"
  private val addBundleLine = List[String]("""_BUNDLENAME=b.name""")
  val getBundleLine = addBundleLine mkString newLineChar

  def getContextLines(mContext:String,immContext:String) = {
              val mutable_lines = ( getBundleLine +newLineChar+ mContext ).split("\r\n").map(_.trim)
    //println("in get context lines mutable lines "+ (getBundleLine +newLineChar+ mContext).split("\r\n").map(_.trim).mkString )
              val immutable_lines = immContext.split("\r\n").map(_.trim)
    //println(" filtered immutable lines "+immutable_lines.mkString+" immutable context "+immContext)

    (mutable_lines,immutable_lines)
  }

}


trait Parsable {

  def processToFile(p: Path): Path
  def processFileToContext(p: Path): String
  def processBundleToContext(p: Path): String

}


