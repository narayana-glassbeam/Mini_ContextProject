package com.glassbeam.context

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.Context._
import com.glassbeam.model.Logger
import com.glassbeam.model.StartupConfig._

import scala.collection.immutable.HashMap

/**
  * Created by bharadwaj on 29/03/16.
  */

trait MPSCCreationSupport extends  Logger {
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

  private val watcher_statements: Array[WatcherContextStatement] = Array(DelPat,SkipPat,IncVaultPat,IncParsePat,BinaryPat,TextPat,ReversePat,MostRecPat,ProcessFilePat,UncompressLevel)

  private val loader_assignments: Array[LoaderContextAssignment] = Array(
    Lgrep, Mgrep, // Line level function
    Snow, SDF2EPOCH, EPOCH2SDF, // Time related functions
    Lcustomer, Lproduct, Lmanufacturer, Lschema, // Fetch known variables into context
    Fdate, Fname, Fpath, Flength, FnameGrep, Fgrep, FpathGrep, Fcount, // File level functions; Fgrep is at line level within file
    Bname, Bsize, Bgrep, BfnameGrep, // Bundle level
    AssertFileDuplicate, AssertNumeric, // Assertions in assignment form
    Concat, Coalesce, Lookup, XmlValue, // Variable level functions
    ProcessFileToContext, ProcessBundleToContext // Extensibility functions
  )

  private val loader_statements: Array[LoaderContextStatement] = Array(AssertUncompressionFail, Assert, AssertTruthy, Validate,
    CombineLines, BProperties, BId, AssertBundleDuplicate, AssertBundleFail,Encoding,AssertPxFileCount)
}

// ToDo: this Actor should be backed by a Router
class MpsContext(emps: String,immContextLines:Array[String]) extends Actor with ContextLines with MPSCCreationSupport {
  import MpsContext._
  import com.glassbeam.context.BundleContext._

  import scala.collection.mutable.{HashMap => MutableH}

  val splitKeys = emps.split(filesep)
  val (customer,manufacturer,product,schema) = ("",splitKeys(0), splitKeys(1), splitKeys(2))

  // mps to immutable variable values
  var immutableVariableCache: Map[String, ContextReason] = Map()

  // mps to mutable variable abstract object (which will be evaluated)
  var mutableVariableCache: Map[String, Array[AbstractContextClass]] = Map()

  // mps to mutable function abstract object (which will be evaluated)
  val loaderInstances: MutableH[String, Vector[AbstractLoaderContext]] = MutableH.empty[String,Vector[AbstractLoaderContext]]

  val watcherInstances:MutableH[String, Vector[AbstractWatcherContext]] = MutableH.empty[String,Vector[AbstractWatcherContext]]

  val frozenInstances:MutableH[String, Vector[AbstractLCPContext]] = MutableH.empty[String,Vector[AbstractLCPContext]]

  def addLCInstances(key:String,inst:AbstractLoaderContext) = {
    if (loaderInstances.contains(emps)) {
      loaderInstances(emps) =loaderInstances.get(emps).get :+ inst
      println("added Loader context instance "+inst.lhs)
    } else {
      loaderInstances += emps -> Vector(inst)
      println("added Loader context instance "+inst.lhs)
    }
  }

  def addWCInstances(key:String,inst:AbstractWatcherContext) = {
    if (watcherInstances.contains(emps)) {
      watcherInstances(emps) = watcherInstances.get(emps).get :+ inst
      println("added watcher context instance "+inst.lhs)
    } else {
      watcherInstances += emps -> Vector(inst)
      println("added watcher context instance "+inst.lhs)
    }
  }

  def addFCInstances(key:String,inst:AbstractLCPContext) = {
    if (frozenInstances.contains(emps)) {
      frozenInstances(emps) = frozenInstances.get(emps).get :+ inst
      println("added watcher context instance "+inst.lhs)
    } else {
      frozenInstances += emps -> Vector(inst)
      println("added watcher context instance "+inst.lhs)
    }
  }

//  val cassLines = CassConfig.split("\r\n").map(_.trim)
//  cassLines.foreach(cl => parseCassContext(cl))
//  val solrLines = SolrConfig.split("\r\n").map(_.trim)
//  solrLines.foreach(cl => parseSolrContext(cl))
//  val S3Lines = S3Config.split("\r\n").map(_.trim)
//  S3Lines.foreach(cl => parseS3Context(cl))
//  val immCommonLines = immCommCon.split("\r\n").map(_.trim)
//  //  immCommonLines.foreach(line => println("combined line "+line))
//
//  val immMpsLines = immMpsCon.split("\r\n").map(_.trim)
//  immMpsLines.foreach(cl => parseLcpContext(cl))
//
//  val combineLines = solrLines ++ cassLines ++ S3Lines ++ immCommonLines ++ immMpsLines
//    combineLines.foreach(line => println("combined line "+line))

  immContextLines.foreach(line => {
    println(" parsing immutable line "+line)
    parseCassContext(line)})

  def parseCassContext(conline:String):Boolean = {
    import com.glassbeam.context.LCPInstance._

    val casstyp = LCPType.getSubtype(LCPType.Cassandra)
    casstyp.values.foreach(cassval =>{
      val cassdef = casstyp.getDefinition(cassval)
      if(cassdef.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = LCPClassArguments(conline,customer,manufacturer,product,schema)
        val froinstance = getLcpContextObject(carg,cassdef)
        addFCInstances(LCPType.Cassandra.toString,froinstance)
        println("Cass typ "+LCPType.Cassandra+" cass rhs regex " + cassdef.rhsRegex + " cass context line " + conline + " matched ")
      }
    })

    true
  }

  def parseS3Context(conline:String):Boolean = {
    import com.glassbeam.context.LCPInstance._
    val S3typ = LCPType.getSubtype(LCPType.S3)
    S3typ.values.foreach(S3val =>{
      val S3def = S3typ.getDefinition(S3val)
      if(S3def.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = LCPClassArguments(conline,customer,manufacturer,product,schema)
        val froinstance = getLcpContextObject(carg,S3def)
        addFCInstances(LCPType.S3.toString(),froinstance)
        println("Cass typ"+S3typ.toString()+" s3 rhs regex " + S3def.rhsRegex + " s3 context line " + conline + " matched ")
      }
    })

    true
  }

  def parseLcpContext(conline:String):Boolean = {
    import com.glassbeam.context.LCPInstance._
    val Lcptyp = LCPType.getSubtype(LCPType.Loader)
    Lcptyp.values.foreach(Lcpval =>{
      val Lcpdef = Lcptyp.getDefinition(Lcpval)
      if(Lcpdef.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = LCPClassArguments(conline,customer,manufacturer,product,schema)
        val froinstance = getLcpContextObject(carg,Lcpdef)
        addFCInstances(LCPType.Loader.toString(),froinstance)
        println("Cass typ"+Lcptyp.toString()+" Lcp rhs regex " + Lcpdef.rhsRegex + " Lcp context line " + conline + " matched ")
      }
    })

    true
  }

  def parseSolrContext(conline:String):Boolean ={
    import com.glassbeam.context.LCPInstance._
    val solrtyp = LCPType.getSubtype(LCPType.Solr)
    solrtyp.values.foreach(solrval =>{
       val solrdef = solrtyp.getDefinition(solrval)
       if(solrdef.rhsRegex.pattern.matcher(conline).matches()) {
         val carg = LCPClassArguments(conline,customer,manufacturer,product,schema)
         val froinstance = getLcpContextObject(carg,solrdef)
         addFCInstances(LCPType.Solr.toString(),froinstance)
        println("Cass typ"+solrtyp.toString()+" solr rhs regex " + solrdef.rhsRegex + " sorl context line " + conline + " matched ")
      }
    })
    true
  }

  def assignMatch(defn: AbstractContextObject, context: String): Boolean = {
    val rhsOption: Option[List[String]] = defn.fullRegex.unapplySeq(context)
    rhsOption match {
      case None => false
      case Some(rhs) =>
        rhs.length match {
          case 2 =>
            defn.rhsRegex.pattern.matcher(rhs(1).trim).matches()
          case _ => false
        }
    }
  }

  // ToDo: The Implementation of this is improved as things get clear or at the end of completion we should improve it as much as we can
  def parseContext(mMpsContextLines:Array[String]) = {
    // ToDo: Query context from H2 and populate immutableVariableCache, mutableVariableCache and mutableFunctionCache
    // ToDo: Traverse the list of H2 context lines and match the fullRegex of DeleteFile object and store the DeleteFile object in mutableFunctionCache
    // ToDo: Store 'n' instances of DeleteFile object in mutableFunctionCache
    var linenum = 1
    for (context_line <- mMpsContextLines; context = context_line.trim) {
      if (context.nonEmpty) {
        var matchedSomething = false
        try {
          for (watcher_regex <- watcher_statements; if !matchedSomething && watcher_regex.fullRegex.pattern.matcher(context).matches()) {
            matchedSomething = true
            val coninst = watcher_regex.getObject(LoaderClassArguments(context, linenum, customer, manufacturer, product, schema))
            addWCInstances(watcher_regex.name,coninst)
          }
          for(loader_assign <- loader_assignments; if !matchedSomething && assignMatch(loader_assign, context)){
            logger.info(emps, s"matched as assignment, key = $emps, line = $linenum, context = $context, context-class = ${loader_assign.getClass.getName}")
            matchedSomething = true
            val coninst = loader_assign.getObject(LoaderClassArguments(context, linenum, customer, manufacturer, product, schema))
            addLCInstances(emps,coninst)
          }
          for(loader_statm <- loader_statements; if !matchedSomething && loader_statm.fullRegex.pattern.matcher(context).matches()){
            logger.info(emps, s"matched as simple statement, key = $emps, line = $linenum, context = $context, context-class = ${loader_statm.getClass.getName}")
            matchedSomething = true
            val coninst = loader_statm.getObject(LoaderClassArguments(context, linenum, customer, manufacturer, product, schema))
            addLCInstances(emps,coninst)
          }
        }catch {
          case e: Exception =>
            val err = s"context match exception while working on key = $emps,  context line = $context "
            logger.error(err)
        }
      }
      linenum += 1
    }
    logger.info(s"For MPS "+emps+" mutable lines "+mMpsContextLines.mkString)
  }


  def receive = {

    case buildContext(mps,mMpsContextLines) =>  parseContext(mMpsContextLines)

    case CreateBundleContext(loadid,mps) =>
      val immwfmap = HashMap(watcherInstances.toSeq:_*)
      val immlfmap = HashMap(loaderInstances.toSeq:_*)
      val child_props = BundleContext.props(loadid,mps,immwfmap,immlfmap)
      createChild(child_props._1,child_props._2)

    case mpsEvalMsg:BundleEval => forward[BundleEval](mpsEvalMsg,bundleContext_name(mpsEvalMsg.loadid))

    case x =>
      logger.error(s"Unknown ContextEval message $x")
  }
}
