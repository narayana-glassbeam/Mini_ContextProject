package com.glassbeam.context

import java.sql.Timestamp
import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.{ContextTableDao, Logger}

import scala.collection.concurrent
import scala.collection.convert.decorateAsScala.mapAsScalaConcurrentMapConverter

object ContextSupervisor {

  val name = "ContextSupervisor"

  def props = Props[ContextSupervisor]

}

trait CSCreationSupport extends  Logger {
  this: ContextSupervisor =>

  val logger = Logging(this)

  def context: ActorContext

  def createChild(props: Props, name: String): ActorRef = context.actorOf(props, name)

  def forward[MPSType <: MPSRequest](msg:MPSType, actor_name: String) = {

    context.child(actor_name) match {
      case Some(loadidContextActor) =>
        loadidContextActor.forward(msg)
      case None =>
        logger.error(s"child actor for this messsage ${msg} not found")
    }
  }

}


class ContextSupervisor extends Actor with CSCreationSupport {
  import com.glassbeam.context.MpsContext._

  val keytots: concurrent.Map[String, Timestamp] = new ConcurrentHashMap[String, Timestamp].asScala

  override def preStart() = {
    initializeAllMPS()
  }

  def initializeAllMPS() = {
    val allMPSKeys = ContextTableDao.getAllKeys()

    allMPSKeys.foreach(mpskey => createMpsContext(mpskey))
  }

  def createMpsContext(key:String) = {

    if(!keytots.contains(key) ) {
      ContextTableDao.getContextForKey(key) match {
        case Some(mps_context) =>
          keytots.putIfAbsent(key,mps_context._3)
          val child_props:(Props,String) = props(key,mps_context._1,mps_context._2)
          val contextMpsEval_A = createChild(child_props._1,child_props._2)
          contextMpsEval_A ! InitializeContext(key)
          logger.info(s"Context Supervisor created child mps "+key+" with name "+contextMpsEval_A.path.name)
        case None =>
          logger.info(s"No key found in h2 context table")
      }
    }
  }

  def receive = {

    case csForwardMsg:MPSRequest => forward[MPSRequest](csForwardMsg,mpsContext_name(csForwardMsg.mps))

    case Done => logger.info("Done")

  }

}