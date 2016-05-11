package com.glassbeam.context

import java.sql.Timestamp
import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.Context._
import com.glassbeam.model.{ContextTableDao, LoaderDao, Logger}

import scala.collection.concurrent
import scala.collection.convert.decorateAsScala.mapAsScalaConcurrentMapConverter

object ContextSupervisor {

  val name = "ContextSupervisor"

  def props = Props[ContextSupervisor]

}

trait H2ContextProvider {

  def asLines(config:String) = config.split("\r\n").map(_.trim)

  def getAllKeys() = ContextTableDao.getAllKeys()

  def getContext(key:String) = ContextTableDao.getContextForKey(key)

  def getConfig(key:String) = asLines(LoaderDao.getValueForKey(key).mkString)

  def getModidfiedContext(mps:String,ts:Timestamp) =  ContextTableDao.getModifiedContext(mps,ts)

}

trait ContextManagerCreationSupport extends  Logger {
  this: ContextSupervisor =>

  val logger = Logging(this)

  def context: ActorContext

  def createChild(props: Props, mps: String): ActorRef = context.actorOf(props, MpsContext.name(mps))

  def getChild(actorName:String) = context.child(MpsContext.name(actorName))

  def forward[MPSType <: MPSRequest](msg:MPSType, actor_name: String) = {

    context.child(actor_name) match {
      case Some(mpsContextActor) =>
        mpsContextActor.forward(msg)
      case None =>
        logger.error(s"child actor for this messsage ${msg} not found")
    }
  }

}

class ContextSupervisor extends Actor with ContextManagerCreationSupport with H2ContextProvider {
  import com.glassbeam.context.Constants._

  override def preStart() = {
    initializeAllMPS()
  }

  val mpstots: concurrent.Map[String, Timestamp] = new ConcurrentHashMap[String, Timestamp].asScala

  lazy val cassConfig = getConfig(Cassandra.H2Key)

  lazy val solrConfig = getConfig(Solr.H2Key)

  //lazy val s3Config = getConfig(S3.H2Key)

  lazy val (mCommonCont,immCommonCont) = getContext("Common") match {
    case Some(commonContext) =>
      val mCommonContext   = asLines(commonContext._1)
      val immCommonContext = asLines(commonContext._2)
      ( mCommonContext,immCommonContext)
    case None =>
      logger.error(" ")
      (Array(""),Array(""))
  }

  def initializeAllMPS() = getAllKeys().foreach(mpsKey => createContext(mpsKey))

  def createContext(mps:String) = {

      getContext(mps) match {
        case Some(mps_context) =>
          val mMpsLines   = asLines(mps_context._1)
          val immMpsLines = asLines(mps_context._2)
          mpstots.putIfAbsent(mps,mps_context._3)
          val mContextLines = mCommonCont ++ mMpsLines
          val immContextLines = solrConfig ++ cassConfig ++ immCommonCont ++ immMpsLines
          createChild(MpsContext.props(mps,immContextLines),mps) //! BuildContext(mps,mContextLines)
        case None =>
          logger.info(s"No key found in h2 context table")
      }

  }

  def updateContext(mps:String) = {

    getModidfiedContext(mps,mpstots(mps)) match {
      case Some(modified_context) =>
        val mMpsLines   = asLines(modified_context._1)
        mpstots.replace(mps,modified_context._2)
        val mContextLines = mCommonCont ++ mMpsLines
        getChild(mps) match {
          case Some(mpsActor) =>
            mpsActor ! BuildContext(mps,mContextLines)
          case None =>
            logger.info("Child actor not found check either actor name u are looking for or check keytots hash map")
        }
      case None =>
        logger.info("Context is not modified ")
    }
  }

  def receive = {

    case csForwardMsg:MPSRequest => forward[MPSRequest](csForwardMsg,MpsContext.name(csForwardMsg.mps))

    case BundleEvent(mps:String) =>
      if(mpstots.contains(mps) ) {
        updateContext(mps)
      }else{
        createContext(mps)
      }

    case Done => logger.info("Done")

  }

}