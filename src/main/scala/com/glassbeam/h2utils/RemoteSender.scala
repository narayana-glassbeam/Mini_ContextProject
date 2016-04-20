package com.glassbeam.h2utils

import akka.actor.{Actor, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

case class Message(node:String, content:String)

object H2RemoteSender  {

  implicit val system = ActorSystem("LocalSystem", ConfigFactory.load("h2RemoteTrigger"))
  val triggerActor = system.actorOf(Props[H2RemoteSender], name = "H2TriggerActor")

  def send(liveNodes:List[String],msg:String) = {
    for(node <- liveNodes) {
      println(s"Sending MSG:($msg) to Scalar Node:$node")
      triggerActor ! Message(node,msg)
    }
  }
}

class H2RemoteSender extends Actor {

  def receive = {
    case msg:Message =>
      val remote = context.actorSelection(s"akka.tcp://Scalar@${msg.node}:7011/user/RulesListener")
      remote ! msg.content
  }
}
