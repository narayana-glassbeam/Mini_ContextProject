package icons

import akka.actor.ActorRef

import scala.collection._

//case class Icon(val SM: SharedMutables, val supervisor: ActorRef)
case class Icon(val supervisor: ActorRef)

trait IconTrait {

	//val SM: SharedMutables
	val supervisor: ActorRef
	val name: String

	def parseLine(l: String, counter: Long): (Boolean, Boolean, immutable.ListMap[String, String], String)

	def reset
}
