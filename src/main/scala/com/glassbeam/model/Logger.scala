package com.glassbeam.model

import java.io.File

import akka.actor._

import akka.event.{ BusLogging, LoggingAdapter, LogSource }
import com.typesafe.config.ConfigFactory

import scala.language.existentials
import scala.sys.SystemProperties

private object ActorSystemObj {
  val sp = new SystemProperties() // Get the JAVA system properties
  val loggerConf = sp.get("user.dir").getOrElse("") + File.separator + "app.conf"
  println(loggerConf.toString)
  val customConf = ConfigFactory.parseFile(new File(loggerConf))
  val system = ActorSystem("Scalar", ConfigFactory.load(customConf)) // Create only one Actor system
  val props = Props[UnhandledLogger]
  system.eventStream.subscribe(system.actorOf(props), classOf[UnhandledMessage])
}

trait Logger {
  implicit val system = ActorSystemObj.system


  final val Logging = (myClass: Any) => {
    val (str, clazz) = LogSource(myClass.getClass, system)
    LcpLogger(new BusLogging(system.eventStream, str, clazz, system.asInstanceOf[ExtendedActorSystem].logFilter))
  }


  // this class wraps Logger for performance and memory optimisations.
  // Both are achieved using scala's by name parameters
  // any other methods on logger can be written here wrapping actual logger method in similar manner
  class LcpLogger private (log: LoggingAdapter) {

    def isErrorEnabled: Boolean = log.isErrorEnabled

    def error(cause: Throwable, message: => String): Unit = { if (isErrorEnabled) log.error(cause, message) }
    def error(message: => String): Unit = { if (isErrorEnabled) log.error(message) }
    def error(template: => String, arg1: => Any): Unit = { if (isErrorEnabled) log.error(template, arg1) }
    def error(template: => String, arg1: => Any, arg2: => Any): Unit = { if (isErrorEnabled) log.error(template, arg1, arg2) }
    def error(template: => String, arg1: => Any, arg2: => Any, arg3: => Any): Unit = { if (isErrorEnabled) log.error(template, arg1, arg2, arg3) }
    def error(template: => String, arg1: => Any, arg2: => Any, arg3: => Any, arg4: => Any): Unit = { if (isErrorEnabled) log.error(template, arg1, arg2, arg3, arg4) }
    def error(cause: Throwable, mps: => String, message: => String): Unit = error(cause, s"[$mps] $message")


    def isWarningEnabled: Boolean = log.isWarningEnabled

    def warning(message: => String): Unit = { if (isWarningEnabled) log.warning(message) }

    def warning(mps: => String, message: => Any): Unit = message match {
      case m:String => warning(s"[$mps] $m")
      case _ => log.warning(mps, message)
    }

    def isInfoEnabled = log.isInfoEnabled

    def info(message: => String) { if (isInfoEnabled) log.info(message) }
    def info(mps: => String, message: => String): Unit = info(s"[$mps] $message")


    def isDebugEnabled: Boolean = log.isDebugEnabled

    def debug(message: => String) { if (isDebugEnabled) log.debug(message) }
    def debug(mps: => String, message: => String): Unit = debug(s"[$mps] $message")
  }

  private object LcpLogger{
    def apply(logAdapter: LoggingAdapter) = new LcpLogger(logAdapter)
  }
}

class UnhandledLogger extends Actor with ActorLogging {
  /** logs on warn level the message and the original recipient (sender is deadLetters) */
  override def receive = {
    case ua :UnhandledMessage =>
      log.error(s"Unhandled: ${ua.message} to ${ua.recipient}")
  }
}
