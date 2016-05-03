package com.glassbeam.context

import java.sql.Timestamp
import java.util.concurrent.ConcurrentHashMap

import akka.Done
import akka.actor.{Actor, Props}
import com.glassbeam.context.ContextCases.{InitializeContext, LoaderContext, LoadidToContext, WatcherContext}
import com.glassbeam.model.{ContextTableDao, Logger}

import scala.collection.concurrent
import scala.collection.convert.decorateAsScala.mapAsScalaConcurrentMapConverter


object ContextSupervisor {

  val name = "ContextSupervisor"

  def props = Props[ContextSupervisor]

}

class ContextSupervisor extends Actor with Logger {
  import com.glassbeam.context.ContextEval._
  import com.glassbeam.context.ContextHelpers._

  private val logger = Logging(this)
  val keytots: concurrent.Map[String, Timestamp] = new ConcurrentHashMap[String, Timestamp].asScala

  override def preStart() = {
    initializeAllMPS()
   // println("in context supervisor prestart")
  }

  private def getActorname(mps:String)={"context_"+alphanumeric(mps)}

  def initializeAllMPS() = {
    val allMPSKeys = ContextTableDao.getAllKeys()

    allMPSKeys.foreach(mpskey => createMpsContext(mpskey))
  }

  def createMpsContext(key:String) = {

    if(!keytots.contains(key) ) {
      ContextTableDao.getContextForKey(key) match {
        case Some(mps_context) =>
          keytots.putIfAbsent(key,mps_context._3)
          //println("initializing context for mps "+mps_context)
          val child_props:(Props,String) = props(key,mps_context._1,mps_context._2)
          val contextMpsEval_A = context.actorOf(child_props._1,child_props._2)
         // println("mps context created for "+contextMpsEval_A.path)
          contextMpsEval_A ! InitializeContext
          logger.info(s"Context Supervisor created child mps "+key+" with name "+contextMpsEval_A.path.name)
        case None =>
          logger.info(s"No key found in h2 context table")
      }
    }
  }


  def receive = {

    case msg:WatcherContext =>
        val childActorname = ceval_name(msg.mps)
          //getActorname(msg.mps)
        context.child(childActorname) match {
            case Some(loadidContextActor) =>
                loadidContextActor.forward(msg)
            case None =>
                logger.error(s"child actor of mps ${msg.mps} not found")
        }
    case create_context@LoadidToContext(loadid,mps) =>
      val childActorname = getActorname(mps)
      context.child(childActorname) match {
        case Some(mpsContextActor) =>
          //println("create loadid to context message receieved in supervisor")
          mpsContextActor.forward(create_context)
        case None =>
          logger.error(s"child actor of mps ${mps} not found")
      }

    case lc:LoaderContext =>
      val childActorname = ceval_name(lc.mps)
      context.child(childActorname) match {
        case Some(loadidContextActor) =>
          loadidContextActor.forward(lc)
        case None =>
          logger.error(s"child actor of mps ${lc.mps} not found")
      }

    case Done => logger.info("Done")

  }

}