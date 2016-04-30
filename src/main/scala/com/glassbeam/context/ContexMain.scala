package com.glassbeam.context
import akka.Done
import akka.pattern.ask
import akka.util.Timeout
import com.glassbeam.context.ContextCases._
import com.glassbeam.model.{Logger, Opsdb}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object ContextMain extends Logger {

  import com.glassbeam.context.ContextSupervisor._

  private final val logger = Logging(this)

  def main(args: Array[String]): Unit = {
    val odb = Opsdb
    odb.init()
    implicit val timeout = Timeout(1 minutes)
    val contextSupervisor = system.actorOf(props,name)
    val file_name = "/home/narayana/var/log/oslog/RFM.log"
    val mps = "aruba/aruba/podv1"
    Thread.sleep(20000)
    val deletefile:Future[Boolean] = (contextSupervisor ? DeleteFile(file_name:String,mps:String)).mapTo[Boolean]
    val result2 = Await.result(deletefile, 1 minutes)
    println(s"sent delete file pattern $file_name resut  "+result2)
    val bundle_depth:Future[Int] = (contextSupervisor ? UncompressBundleDepth(file_name:String,mps:String)).mapTo[Int]
    val result3 = Await.result(bundle_depth, 2 minutes)
    println("received bundle depth "+result3)
    contextSupervisor ! Done
  }


}

object Init extends Enumeration {
  type Init = Value
  val Run, Test, TestWithH2, TestWithCassandra, TestWithSolr = Value
  var inittype = Run
}