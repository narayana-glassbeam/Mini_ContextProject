package com.glassbeam.context

import java.sql.Timestamp
import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.{ContextTableDao, LoaderDao, Logger}

import scala.collection.concurrent
import scala.collection.convert.decorateAsScala.mapAsScalaConcurrentMapConverter

object ContextSupervisor {

  val name = "ContextSupervisor"

  val Cassandra = "Cassandra"

  val Solr = "Solr"

  val S3 = "S3Vault"

  def props = Props[ContextSupervisor]

}

trait H2ContextProvider {

  def getAllKeys() = ContextTableDao.getAllKeys()

  def getContext(key:String) = ContextTableDao.getContextForKey(key)

  def getConfig(key:String) = LoaderDao.getValueForKey(key).toArray[String]
}

trait CSCreationSupport extends  Logger {
  this: ContextSupervisor =>

  val logger = Logging(this)

  def context: ActorContext

  def createChild(props: Props, name: String): ActorRef = context.actorOf(props, name)

  def forward[MPSType <: MPSRequest](msg:MPSType, actor_name: String) = {

    context.child(actor_name) match {
      case Some(mpsContextActor) =>
        mpsContextActor.forward(msg)
      case None =>
        logger.error(s"child actor for this messsage ${msg} not found")
    }
  }



}

class ContextSupervisor extends Actor with CSCreationSupport with H2ContextProvider {
  import com.glassbeam.context.MpsContext._
  import com.glassbeam.context.ContextSupervisor._

  override def preStart() = {
    initializeAllMPS()
  }

  val keytots: concurrent.Map[String, Timestamp] = new ConcurrentHashMap[String, Timestamp].asScala

  lazy val cassConfig = getConfig(Cassandra)

  lazy val solrConfig = getConfig(Solr)

  lazy val s3Config = getConfig(S3)

  lazy val (mCommonCont,immCommonCont) = getContext("Common") match {
    case Some(commonContext) =>
      val mCommonContext   = commonContext._1.split("\r\n").map(_.trim)
      val immCommonContext = commonContext._2.split("\r\n").map(_.trim)
      ( mCommonContext,immCommonContext)
    case None =>
      logger.error(" ")
      (Array(""),Array(""))
  }


  def initializeAllMPS() = getAllKeys().foreach(mpsKey => createMpsContext(mpsKey))


  def createMpsContext(key:String) = {

      getContext(key) match {
        case Some(mps_context) =>
          keytots.putIfAbsent(key,mps_context._3)
          val immMpsLines = mps_context._2.split("\r\n").map(_.trim)
          val mMpsLines   = mps_context._1.split("\r\n").map(_.trim)
          val mContextLines = mCommonCont ++ mMpsLines
          val immContextLines = solrConfig ++ cassConfig ++ s3Config ++ immCommonCont ++ immMpsLines
          createChild(props(key,immContextLines),mpsContext_name(key)) ! buildContext(key,mContextLines)
        case None =>
          logger.info(s"No key found in h2 context table")
      }

  }

  def receive = {

    case csForwardMsg:MPSRequest => forward[MPSRequest](csForwardMsg,mpsContext_name(csForwardMsg.mps))

    case Done => logger.info("Done")

  }

}