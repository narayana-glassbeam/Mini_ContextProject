package com.glassbeam.context
import akka.Done
import akka.pattern.ask
import akka.util.Timeout
import com.glassbeam.context.Constants.Loader
import com.glassbeam.context.Context._
import com.glassbeam.model.{Logger, Opsdb}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object ContextMain extends Logger {

  import com.glassbeam.context.ContextSupervisor._

  private final val logger = Logging(this)

  def main(args: Array[String]): Unit = {
    val odb = Opsdb
    odb.init()
    implicit val timeout = Timeout(1 minutes)
    val contextSupervisor = system.actorOf(props,name)
    val file_name = "/home/narayana/work/perfmarker/Mini_ContextProject/var/log/oslog/tech-support.log"
    val mps = "aruba/aruba/podv1"
    val loadid = 1234
    contextSupervisor ! CreateBundleContext(loadid,mps)
    val deletefile:Future[Boolean] = (contextSupervisor ? DeleteFile(file_name,mps,loadid)).mapTo[Boolean]
    val result2 = Await.result(deletefile, 1 minutes)
    println("delete file result is "+deletefile)
    val bundle_depth:Future[Int] = (contextSupervisor ? UncompressBundleDepth(file_name,mps,loadid)).mapTo[Int]
    val result3 = Await.result(bundle_depth, 5 minutes)
    println(" bundle depth result is "+bundle_depth)

    val context_result:Future[ContextReason] = (contextSupervisor ? EvalFileContext(file_name,mps,loadid)).mapTo[ContextReason]

    context_result.onComplete{
      case Success(cr) =>
        val crstrings = cr.contextStrings
        val crreason = cr.reason
        val crbp = cr.bproperties.getOrElse("")
        val crfail = cr.failure.getOrElse("")
        //println(" context Strings are "+crstrings.mkString("\r\n"))
      case Failure(ex) =>
        println("exception " +ex)

    }

    val loaderValues:Future[ContextReason] = (contextSupervisor ? LCPValues(Loader.prefix,mps)).mapTo[ContextReason]
    loaderValues.onComplete{
      case Success(cr) =>
        val crstrings = cr.contextStrings
        val crreason = cr.reason
        val crbp = cr.bproperties.getOrElse("")
        val crfail = cr.failure.getOrElse("")
        println(" context Strings are "+crstrings.mkString("\r\n"))
      case Failure(ex) =>
        println("exception " +ex)
    }
//
//    val solrValues = (contextSupervisor ? LCPValues(Solr.prefix,mps)).mapTo[ContextReason]
//      solrValues.onComplete{
//        case Success(cr) =>
//          val crstrings = cr.contextStrings
//          val crreason = cr.reason
//          val crbp = cr.bproperties.getOrElse("")
//          val crfail = cr.failure.getOrElse("")
//          println(" context Strings are "+crstrings.mkString("\r\n"))
//        case Failure(ex) =>
//          println("exception " +ex)
//      }
//    val cassValues = (contextSupervisor ? LCPValues(Cassandra.prefix,mps)).mapTo[ContextReason]
//      cassValues.onComplete{
//        case Success(cr) =>
//          val crstrings = cr.contextStrings
//          val crreason = cr.reason
//          val crbp = cr.bproperties.getOrElse("")
//          val crfail = cr.failure.getOrElse("")
//          println(" context Strings are "+crstrings.mkString("\r\n"))
//        case Failure(ex) =>
//          println("exception " +ex)
//      }

    contextSupervisor ! Done
  }


}

