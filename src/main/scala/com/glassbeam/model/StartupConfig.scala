package com.glassbeam.model

import java.io.File

import scala.sys.SystemProperties

trait RunEnvironment{
  val sp = new SystemProperties()
  // Get the JAVA system properties
  final val filesep: String = sp.get("file.separator").getOrElse(File.separator)
  final val user_dir = sp.get("user.dir").getOrElse("") + filesep
  final val home_dir = sp.get("user.home").getOrElse("") + filesep
}

object StartupConfig extends Logger with RunEnvironment{

  var node_id = "127.0.0.1"
  var opsdb = "jdbc:h2:tcp://localhost/lcp-opdb_cr"
  //var node_ip = ""
  var cluster_id = ""
  var mode = "DEV"
  var remoteLogvault = ""

 // private var rtfActorRef: ActorRef = null

  val driver = "org.h2.Driver"

  private final val logger = Logging(this)
  logger.debug(s"StartupConfig initialized.")

  //def setRTFActorRef(actorRef: ActorRef) = rtfActorRef = actorRef

  def getLoaderAll(key: String): Vector[String] = {
    val logger = Logging(this)
    val keycolumn = LoaderDao.getValueForKey(key)
    var values: Vector[String] = Vector()
    if (keycolumn.nonEmpty) {
      val column = keycolumn.head.split("\\r?\\n").toSeq
      // logger.debug(s"allrows for key = $key = $column")
      for (row <- column) {
        val l = row.split("=", 2).toSeq
        logger.debug(s"split line for key = $key, is = $l")
        values = values :+ l.head.trim
      }
    }
    values
  }

  def getLoaderVal(key: String, lhs: String, mandatory: Boolean = false): String = {
    val logger = Logging(this)
    var found: Boolean = false
    var value: String = null
    try {
      val keycolumn = LoaderDao.getValueForKey(key)
      // logger.debug(s"keycolumn = $keycolumn")
      if (keycolumn.nonEmpty) {
        val column = keycolumn.head.split("\\r?\\n").toSeq
        // logger.debug(s"allrows for key = $key = $column")
        for (row <- column; if !found) {
          val l = row.split("=", 2).toSeq
          // logger.debug(s"split line for key = $key, lhs = $lhs, is = $l")
          if (l.head.trim.equals(lhs)) {
            logger.info(s"KEY = $key, LHS = $lhs, Value = ${l(1)}")
            value = l(1).trim
            found = true
          } else {
            // logger.debug(s"l0 = ${l(0)} and lhs = $lhs, did NOT match")
          }
        }
      }
    } catch {
      case t: Exception =>
        val msg = s"getLoaderVal exception = ${t.getMessage}"
        logger.error(t, msg)
    }
    if (mandatory && !found) {
      val msg = s"Value for mandatory key = $key and lhs = $lhs NOT found in Loader Table in H2. Exiting LCP."
//      import com.glassbeam.failuredefs.{MandatoryColsFailure, MsgTemplate}
//      val errMsg = MsgTemplate(Option(key), None, msg)
//      rtfActorRef ! MandatoryColsFailure(errMsg)
//      printAndLog(msg)
//      shutdown
      logger.info(s"$msg")
      System.exit(255)
    }
    value
  }

  trait WatchedAndPermanentPath{
    def watchedPatht:String
    def permPatht:String
  }

}
