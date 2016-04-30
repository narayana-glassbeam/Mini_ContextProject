package icons

import com.glassbeam.context.ContextHelpers._
import com.glassbeam.model.Logger
import com.ximpleware._

import scala.collection.immutable

object XmlParser extends Logger {
  final val logger = Logging(system, this)
  val name = "XmlIcon"
}

trait XmlParser extends IconTrait {

  val name = XmlParser.name
  def parseLine(l: String, counter: Long): (Boolean, Boolean, immutable.ListMap[String, String], String) = null
  def reset = {}

  def isValidXpath(xmlpath: String): Boolean = {
    var isValid = true
    try {
      val testAP = new AutoPilot()
      testAP.selectXPath(xmlpath)
    } catch {
      case e: Exception =>
        isValid = false
        //supervisor ! IconExceptionMessage(IconException(e), name, SM.logfn, SM.loadId, SM.ec, SM.mfr, SM.prod, SM.sch, SM.obsts, SM.sysId)
    }
    isValid
  }

  def getXMLColValue(vn: VTDNav, xmlpath: String): String = {
    val attr = getLastAttribute(xmlpath)
    if (xmlpath.head == '\"') stripQuotes(xmlpath) else getXMLValue(vn, xmlpath, attr)
  }

  def getLastAttribute(xmlpath: String): String = {
    // Check if xpath ends in a node, or has an attribute
    val lastElem = xmlpath.split("/").last.trim
    if (lastElem.head == '@') lastElem.tail else ""
  }

  def getXMLValue(vn: VTDNav, xmlpath: String, attrName: String): String = {
    val ap = new AutoPilot(vn)
    ap.selectXPath(xmlpath)
    var retStr = ""

    while (ap.evalXPath != -1) {
      val id = if(attrName.nonEmpty) vn.getAttrVal(attrName) else vn.getText
      if (id != -1) {
        val uid = vn.toNormalizedString(id)
        retStr = uid
      }
    }
    ap.resetXPath
    retStr
  }

}

