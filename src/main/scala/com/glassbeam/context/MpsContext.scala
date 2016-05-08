package com.glassbeam.context

import akka.actor.{Actor, ActorContext, ActorRef, Props}
import com.glassbeam.context.ContextCases._
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

  def mpsContext_name(mps:String) = "context_"+alphanumeric(mps)

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
  val mutableLoaderFunction: MutableH[String, Vector[AbstractLoaderContext]] = MutableH.empty[String,Vector[AbstractLoaderContext]]

  val mutableWatcherFunction:MutableH[String, Vector[AbstractWatcherContext]] = MutableH.empty[String,Vector[AbstractWatcherContext]]

  def addLCInstances(key:String,inst:AbstractLoaderContext) = {
    if (mutableLoaderFunction.contains(emps)) {
      //val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
      mutableLoaderFunction(emps) = mutableLoaderFunction.get(emps).get :+ inst
      println("added context instance "+inst.lhs)

    } else {
      //val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
      mutableLoaderFunction += emps -> Vector(inst)
      println("added context instance "+inst.lhs)
    }
  }

  def addWCInstances(key:String,inst:AbstractWatcherContext) = {
    if (mutableWatcherFunction .contains(emps)) {
      //val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
      mutableWatcherFunction (emps) = mutableWatcherFunction.get(emps).get :+ inst
      println("added context instance "+inst.lhs)

    } else {
      //val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
      mutableWatcherFunction  += emps -> Vector(inst)
      println("added context instance "+inst.lhs)
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

  def parseCassContext(conline:String):Boolean = {
    import com.glassbeam.context.FrozenInstance._

    val casstyp = FrozenType.getSubtype(FrozenType.Cassandra)
    //println("solr string "+sorlstring)
    casstyp.values.foreach(cassval =>{
      val cassdef = casstyp.getDefinition(cassval)
      if(cassdef.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = ContextClassArguments(conline,0,customer,manufacturer,product,schema)
        val froinstance = getFrozenInstance(carg,cassdef)
        addLCInstances(casstyp.toString(),froinstance)
        println(" cass rhs regex " + cassdef.rhsRegex + " cass context line " + conline + " matched ")
      }
    })

    true
  }

  def parseS3Context(conline:String):Boolean = {
    import com.glassbeam.context.FrozenInstance._
    val S3typ = FrozenType.getSubtype(FrozenType.S3)
    //println("solr string "+sorlstring)
    S3typ.values.foreach(S3val =>{
      val S3def = S3typ.getDefinition(S3val)
      if(S3def.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = ContextClassArguments(conline,0,customer,manufacturer,product,schema)
        val froinstance = getFrozenInstance(carg,S3def)
        addLCInstances(S3typ.toString(),froinstance)
        println(" s3 rhs regex " + S3def.rhsRegex + " s3 context line " + conline + " matched ")
      }
    })

    true
  }


  def parseLcpContext(conline:String):Boolean = {
    import com.glassbeam.context.FrozenInstance._
    val Lcptyp = FrozenType.getSubtype(FrozenType.LCP)
    //println("solr string "+sorlstring)
    Lcptyp.values.foreach(Lcpval =>{
      val Lcpdef = Lcptyp.getDefinition(Lcpval)
      if(Lcpdef.rhsRegex.pattern.matcher(conline).matches()) {
        val carg = ContextClassArguments(conline,0,customer,manufacturer,product,schema)
        val froinstance = getFrozenInstance(carg,Lcpdef)
        addLCInstances(Lcptyp.toString(),froinstance)
        println(" Lcp rhs regex " + Lcpdef.rhsRegex + " Lcp context line " + conline + " matched ")
      }
    })

    true
  }



  def parseSolrContext(conline:String):Boolean ={
    import com.glassbeam.context.FrozenInstance._
    val solrtyp = FrozenType.getSubtype(FrozenType.Solr)
    //println("solr string "+sorlstring)
    solrtyp.values.foreach(solrval =>{
       val solrdef = solrtyp.getDefinition(solrval)
       if(solrdef.rhsRegex.pattern.matcher(conline).matches()) {
         val carg = ContextClassArguments(conline,0,customer,manufacturer,product,schema)
         val froinstance = getFrozenInstance(carg,solrdef)
         addLCInstances(solrtyp.toString(),froinstance)
        println(" solr rhs regex " + solrdef.rhsRegex + " sorl context line " + conline + " matched ")
      }
    })
    true
  }


  def assignMatch(defn: AbstractContextObject, context: String): Boolean = {
    val rhsOption: Option[List[String]] = defn.fullRegex.unapplySeq(context)
   // println(" rhsoption  "+rhsOption+" for context line "+context+" for def full regex "+defn.fullRegex)
    rhsOption match {
      case None => false
      case Some(rhs) =>
        rhs.length match {
          case 2 =>
            //println(" in rhs match rhsoption  "+rhsOption+" for rhs(1) "+rhs(1).trim+" for def rhs regex "+defn.rhsRegex)
            defn.rhsRegex.pattern.matcher(rhs(1).trim).matches()
            //defn.rhsRegex.pattern.matcher(context).matches()
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
          //println("in parse context "+context_line)
          for (watcher_regex <- watcher_statements; if !matchedSomething && watcher_regex.fullRegex.pattern.matcher(context).matches()) {
            matchedSomething = true
            //println("in parse context "+context_line)
            if (mutableWatcherFunction.contains(watcher_regex.name)) {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableWatcherFunction(watcher_regex.name) = mutableWatcherFunction.get(watcher_regex.name).get :+ coninst
              //println("in watcher match "+watcher_regex.name)
            } else {
              val coninst = watcher_regex.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
              mutableWatcherFunction += watcher_regex.name -> Vector(coninst)
              //println("in watcher match "+watcher_regex.name)
            }
          }
          for(loader_assign <- loader_assignments; if !matchedSomething && assignMatch(loader_assign, context)){
            logger.info(emps, s"matched as assignment, key = $emps, line = $linenum, context = $context, context-class = ${loader_assign.getClass.getName}")
            matchedSomething = true
            //println("in parse context "+context_line)
            //println("in loader assign boolean "+loader_assign.isAssignment)
            val coninst = loader_assign.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
            addLCInstances(emps,coninst)
          }
          for(loader_statm <- loader_statements; if !matchedSomething && loader_statm.fullRegex.pattern.matcher(context).matches()){
            logger.info(emps, s"matched as simple statement, key = $emps, line = $linenum, context = $context, context-class = ${loader_statm.getClass.getName}")
            matchedSomething = true
            //println("in parse context "+context_line)
            val coninst = loader_statm.getObject(ContextClassArguments(context, linenum, customer, manufacturer, product, schema))
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
      val immwfmap = HashMap(mutableWatcherFunction.toSeq:_*)
      val immlfmap = HashMap(mutableLoaderFunction.toSeq:_*)
      val child_props = BundleContext.props(loadid,mps,immwfmap,immlfmap)
      createChild(child_props._1,child_props._2)

    case mpsEvalMsg:BundleEval => forward[BundleEval](mpsEvalMsg,bundleContext_name(mpsEvalMsg.loadid))

    case x =>
      logger.error(s"Unknown ContextEval message $x")
  }
}
