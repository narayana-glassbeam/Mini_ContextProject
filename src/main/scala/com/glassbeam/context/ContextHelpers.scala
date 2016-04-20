package com.glassbeam.context

object ContextHelpers {
  def alphanumeric(name: String): String = {
    // name.replaceAll("[^A-Za-z0-9]", "")
    name.replaceAll("/", "_").replaceAll(":", "_")
  }
}
trait ContextLines {
  private val newLineChar = "\r\n"
  private val addBundleLine = List[String]("\r\n", """_BUNDLENAME=b.name""")
  val getBundleLine = addBundleLine mkString newLineChar

  def getContextLines(mContext:String,immContext:String) = {
              val mutable_lines = ( getBundleLine + mContext ).split("\r\n").map(_.trim)
    println(s" mutable lines in trait "+mutable_lines.mkString)
              val immutable_lines = immContext.split("\r\n").map(_.trim)
    (mutable_lines,immutable_lines)
  }

}


