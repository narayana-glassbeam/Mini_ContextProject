package com.glassbeam.context

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.Constants.{Cassandra, Loader, Solr}
import com.glassbeam.context.Context._
import com.glassbeam.context.ContextSection.ContextSection
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.Logger
import com.glassbeam.model.StartupConfig._

import scala.collection.immutable.HashMap

/**
  * Created by bharadwaj on 29/03/16.
  */

trait MPSCreationSupport extends  Logger {
  this: MpsContext =>

  val logger = Logging(this)

  def context: ActorContext

  def createChild(props: Props, name: String): ActorRef = context.actorOf(props, name)

   def forward[BundleTyp <: BundleEval](msg:BundleTyp, actor_name: String) = {
    context.child(actor_name) match {
      case Some(loadidContextActor) =>
        loadidContextActor.forward(msg)
      case None =>
        logger.error(s"child actor for this messsage ${msg} not found")
    }

  }
}


object MpsContext  {
  import com.glassbeam.context.ContextHelpers._

  def name(mps:String) = "context_"+alphanumeric(mps)

  def props(mps:String,immContextLines:Array[String]):Props = Props(classOf[MpsContext],mps,immContextLines)

}

// ToDo: this Actor should be backed by a Router
class MpsContext(emps: String,immContextLines:Array[String]) extends Actor with ContextLines with MPSCreationSupport {
  import com.glassbeam.context.BundleContext._

  import scala.collection.mutable.{HashMap => MutableH}

  override def preStart() = {
    immContextLines.foreach(line => println(" immutable line "+line))
    parseContext(immContextLines,ContextSection.ImmutableState)
  }

  val splitKeys = emps.split(filesep)
  val (customer,manufacturer,product,schema) = ("",splitKeys(0), splitKeys(1), splitKeys(2))

  val lcpValues:MutableH[String,ContextReason] = MutableH.empty[String,ContextReason]
  // mps to mutable function abstract object (which will be evaluated)
  val mutableLoaderInstances: MutableH[String, Vector[AbstractLoaderContext]] = MutableH.empty[String,Vector[AbstractLoaderContext]]
  val immutableLoaderInstances: MutableH[String, Vector[AbstractLoaderContext]] = MutableH.empty[String,Vector[AbstractLoaderContext]]

  val mutableWatcherInstances:MutableH[String, Vector[AbstractWatcherContext]] = MutableH.empty[String,Vector[AbstractWatcherContext]]
  val immutableWatcherInstances:MutableH[String, Vector[AbstractWatcherContext]] = MutableH.empty[String,Vector[AbstractWatcherContext]]

  val mutableLcpInstances:MutableH[String, Vector[AbstractLCPContext]] = MutableH.empty[String,Vector[AbstractLCPContext]]
  val immutableLcpInstances:MutableH[String, Vector[AbstractLCPContext]] = MutableH.empty[String,Vector[AbstractLCPContext]]

  def addLCInstances(key:String,cSection:ContextSection,inst:AbstractLoaderContext) = {
    cSection match {
      case ContextSection.MutableState =>
        if (mutableLoaderInstances.contains (key) ) {
          mutableLoaderInstances (key) = mutableLoaderInstances.get(key).get :+ inst
          println ("added mutabe Loader context instance " + inst.lhs)
        }else {
          mutableLoaderInstances += key -> Vector (inst)
          println ("added first mutable Loader context instance " + inst.lhs)
        }
      case ContextSection.ImmutableState =>
        if (immutableLoaderInstances.contains (key) ) {
          immutableLoaderInstances (key) = immutableLoaderInstances.get(key).get :+ inst
          println ("added immutable Loader context instance " + inst.lhs)
        }else {
          immutableLoaderInstances += key -> Vector (inst)
          println ("added first immutable Loader context instance " + inst.lhs)
        }
    }
  }

  def addWCInstances(key:String,cSection:ContextSection,inst:AbstractWatcherContext) = {
    cSection match {
      case ContextSection.MutableState =>
        if (mutableWatcherInstances.contains (key) ) {
          mutableWatcherInstances (key) = mutableWatcherInstances.get (key).get :+ inst
          println ("added mutabe watcher context instance " + inst.lhs)
        }else {
          mutableWatcherInstances += key -> Vector (inst)
          println ("added first mutabe watcher context instance " + inst.lhs)
        }
      case ContextSection.ImmutableState =>
        if (immutableWatcherInstances.contains (key) ) {
          immutableWatcherInstances (key) = immutableWatcherInstances.get (key).get :+ inst
          println ("added immutable  watcher context instance " + inst.lhs)
        }else {
          immutableWatcherInstances += key -> Vector (inst)
          println ("added first immutable watcher context instance " + inst.lhs)
        }
    }
  }

  def addLCPInstances(key:String,cSection:ContextSection,inst:AbstractLCPContext) = {

    cSection match {
      case ContextSection.MutableState =>
        if (mutableLcpInstances.contains(key)) {
          mutableLcpInstances(key) = mutableLcpInstances.get(key).get :+ inst
          println("added mutabe lcp context instance " + inst.lhs)
        }else{
            mutableLcpInstances += key -> Vector(inst)
            println("added first mutabe lcp context instance " + inst.lhs)
        }
      case ContextSection.ImmutableState =>
        if (immutableLcpInstances.contains(key)) {
          immutableLcpInstances(key) = immutableLcpInstances.get(key).get :+ inst
          println("added immutable lcp context instance " + inst.lhs)
        }else{
          immutableLcpInstances += key -> Vector(inst)
          println("added first immutable lcp context instance " + inst.lhs)
        }
    }
  }


  // ToDo: The Implementation of this is improved as things get clear or at the end of completion we should improve it as much as we can
  def parseContext(ContextLines:Array[String],cSection:ContextSection ) = {
    // ToDo: Query context from H2 and populate immutableVariableCache, mutableVariableCache and mutableFunctionCache
    // ToDo: Traverse the list of H2 context lines and match the fullRegex of DeleteFile object and store the DeleteFile object in mutableFunctionCache
    // ToDo: Store 'n' instances of DeleteFile object in mutableFunctionCache
    var linenum = 1
    for (context_line <- ContextLines; context = context_line.trim) {
      if (context.nonEmpty) {
        try {
          val lca = ContextClassArguments(context,linenum,customer,manufacturer,product,schema)
          MatchArguments(context,cSection) match {
            case WatcherStatements(wp)  =>
              val watInst = WatcherObject.getObject(lca,wp)
              addWCInstances(wp.name,cSection,watInst)
            case LoaderStatements(ls)   =>
              val lstInst = ls.getObject(lca)
              addLCInstances(LoaderStatements.getName,cSection,lstInst)
            case LoaderAssignment(la)   =>
              val lstInst = la.getObject(lca)
              addLCInstances(LoaderAssignment.getName,cSection,lstInst)
            case LcpStatements(ls,key)  =>
              val lcpInst = LcpObject.getObject(lca,ls)
              addLCPInstances(key,cSection,lcpInst)
            case x => println("Not matched with "+x.conline)

          }

        }catch {
          case e: Exception =>
            val err = s"context match exception while working on key = $emps,  context line = $context "
            logger.error(err)
        }
      }
      linenum += 1
    }
    println(" size of lcp instances = "+immutableLcpInstances.size+"\n size of solr insts "+immutableLcpInstances.get(Solr.prefix).get.length+"\n size of cass insts "+immutableLcpInstances.get(Cassandra.prefix).get.length+"\n size of loader instances "+immutableLcpInstances.get(Loader.prefix).get.length)
    logger.info(s"For MPS "+emps+" mutable lines "+ContextLines.mkString)
  }

  def evalLcpInstances(key:String,mps:String):ContextReason = {
    var cr = ContextReason(HashMap[String, String](), "")
    val mutableInstances   = mutableLcpInstances.getOrElse(key,Vector())
    val immutableInstances = immutableLcpInstances.getOrElse(key,Vector())
    println(s"\n\nIn Eval size of immutable ${key} insts "+immutableLcpInstances.get(key).get.length)
    val lcpInstances = Vector(mutableInstances,immutableInstances).flatten
    println(s" in Eval size of combine lcp instances = "+lcpInstances.length)
    lcpInstances.foreach(li =>
      try{
      val cefa = LCPEvalArguments(cr,mps)
      println(" LCP EVAL "+cr.contextStrings.mkString+" for key "+key)
      cr = li.execute(cefa)
    }catch {
        case e:Exception =>
          val err = s"exception while processing context [Class Args: ${li.lhs}] [Execute Args: $cr]"
          cr.copy(reason = cr.reason + err, failure = Some(ContextExecFailure))
          logger.error(e,mps,err)
      })
    cr
  }


  def receive = {

    case BuildContext(mps,mMpsContextLines) =>
      mutableLoaderInstances.clear()
      mutableWatcherInstances.clear()
      mutableLcpInstances.clear()
      parseContext(mMpsContextLines,ContextSection.MutableState)

    case CreateBundleContext(loadid,mps) =>
      val bundleWatcherInstances = mutableWatcherInstances.toMap[String,Vector[AbstractWatcherContext]]
      val bundleLoaderInstances = mutableLoaderInstances.toMap[String,Vector[AbstractLoaderContext]]
      val child_props = BundleContext.props(loadid,mps,bundleWatcherInstances,bundleLoaderInstances)
      createChild(child_props._1,child_props._2)

    case mpsEvalMsg:BundleEval => forward[BundleEval](mpsEvalMsg,bundleContext_name(mpsEvalMsg.loadid))

    case LCPValues(key:String,mps:String) => sender ! lcpValues.getOrElseUpdate(key,evalLcpInstances(key,mps))

    case _ =>
      logger.error(s"Unknown ContextEval message ")
  }
}
