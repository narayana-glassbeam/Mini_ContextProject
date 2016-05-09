package com.glassbeam.context

import com.glassbeam.context.Context.{ContextReason, LCPClassArguments, LCPEvalArguments}
import com.glassbeam.context.{ContextSection => _, ContextStage => _}
import com.glassbeam.model.Logger

import scala.util.matching.Regex

abstract class LcpContext(val rhsRegex: Regex) extends LCPContextAssignment

object LCPType extends Enumeration {
  type LCPType = Value
  val Solr, Cassandra, S3, Loader = Value
  def getSubtype(typ: LCPType): LCPSubtype = typ match {
    case Solr      => SolrSubtype
    case Cassandra => CassandraSubtype
    case S3        => S3Subtype
    case Loader    => LoaderSubtype
  }
}

abstract class LCPSubtype extends Enumeration with Logger {
  val logger = Logging(this)
  def getDefinition(clid:Value): LcpContext
}

object SolrSubtype extends LCPSubtype {
  type SolrSubtype = Value

  val SolrHome, SolrZkHost, SolrHost, SolrNumShards, SolrReplicationFactor, SolrZkHostLogVault, SolrOnlyLogVault,
  SolrBucketHost, SolrBucketColPlacementInfo, SolrBucketColSize, SolrBucketShardReplicaInfo, SolrDynamicSchema, SolrMaxContentLines,
  SolrStoreIndex, SolrRejectNumPastDays, SolrRejectNumFutureDays, SolrNumShardsLV, SolrReplicationFactorLV = Value

  //println("solr home in string "+SolrHome.toString)

  def getDefinition(solrline: Value): LcpContext = solrline match {
    case SolrHome                   => new LcpContext("""Solr.DataDir='(.+?)'""".r) with ILcpState
    case SolrZkHost                 => new LcpContext("""Solr.ZkHost='(.+?)'""".r) with ILcpState
    case SolrHost                   => new LcpContext("""Solr.Host='(.+?)'""".r) with ILcpState
    case SolrNumShards              => new LcpContext("""Solr.NumShards='(.+?)'""".r) with ILcpState
    case SolrReplicationFactor      => new LcpContext("""Solr.ReplicationFactor='(.+?)'""".r) with ILcpState
    case SolrZkHostLogVault         => new LcpContext("""Solr.ZkHostLogVault='(.+?)'""".r) with ILcpState
    case SolrOnlyLogVault           => new LcpContext("""Solr.OnlyLogVault='(.+?)'""".r) with ILcpState
    case SolrBucketHost             => new LcpContext("""Solr.BucketHost='(.+?)'""".r) with ILcpState
    case SolrBucketColPlacementInfo => new LcpContext("""Solr.BucketColPlacementInfo='(.+?)'""".r) with ILcpState
    case SolrBucketColSize          => new LcpContext("""Solr.BucketColSize='(.+?)'""".r) with ILcpState
    case SolrBucketShardReplicaInfo => new LcpContext("""Solr.BucketShardReplicaInfo='(.+?)'""".r) with ILcpState
    case SolrDynamicSchema          => new LcpContext("""Solr.DynamicSchema='(.+?)'""".r) with ILcpState
    case SolrMaxContentLines        => new LcpContext("""Solr.MaxContentLines='(.+?)'""".r) with ILcpState
    case SolrStoreIndex             => new LcpContext("""Solr.StoreIndex='(.+?)'""".r) with ILcpState
    case SolrRejectNumPastDays      => new LcpContext("""Solr.RejectNumPastDays='(.+?)'""".r) with MLcpState
    case SolrRejectNumFutureDays    => new LcpContext("""Solr.RejectNumFutureDays='(.+?)'""".r) with MLcpState
    case SolrNumShardsLV            => new LcpContext("""Solr.NumShardsLV='(.+?)'""".r) with ILcpState
    case SolrReplicationFactorLV    => new LcpContext("""Solr.ReplicationFactorLV='(.+?)'""".r) with ILcpState
    case x => logger.error(s"unknown Solr subtype = $solrline value of $x"); null
  }
}

object CassandraSubtype extends LCPSubtype {
  type CassandraSubtype = Value

  val CassKeyspace, CassTimeslice, CassPreparedInserts, CassAsync, CassEventBatchCount, CassFetchSize, CassConsistencyLevel, CassHost = Value

  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case CassKeyspace         =>  new LcpContext("""Cass.Keyspace=(.+?)""".r) with ILcpState
    case CassTimeslice        =>  new LcpContext("""Cass.Timeslice=(.+?)""".r) with ILcpState
    case CassPreparedInserts  =>  new LcpContext("""Cass.PreparedInserts=(.+?)""".r) with ILcpState
    case CassAsync            =>  new LcpContext("""Cass.Async=(.+?)""".r) with ILcpState
    case CassEventBatchCount  =>  new LcpContext("""Cass.EventBatchCount=(.+?)""".r) with ILcpState
    case CassFetchSize        =>  new LcpContext("""Cass.FetchSize=(.+?)""".r) with ILcpState
    case CassConsistencyLevel =>  new LcpContext("""Cass.ConsistencyLevel=(.+?)""".r) with ILcpState
    case CassHost             =>  new LcpContext("""Cass.Host='(.+?)'""".r) with ILcpState
    case x => logger.error(s"unknown Cassandra subtype = $subtyp value of $x"); null
  }
}
//
object S3Subtype extends LCPSubtype {
  type S3Subtype = Value

  val S3AccessKey, S3SecurityKey, S3Bucket = Value

  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case S3AccessKey   =>  new LcpContext( """S3.AccessKey=(.+?)""".r) with ILcpState
    case S3SecurityKey =>  new LcpContext("""S3.SecurityKey=(.+?)""".r) with ILcpState
    case S3Bucket      =>  new LcpContext("""S3.Bucket=(.+?)""".r) with ILcpState
    case x => logger.error(s"unknown S3 subtype = $subtyp value of $x"); null
  }
}
//
object LoaderSubtype extends LCPSubtype {
  type LcpSubtype = Value
  val UploadedBy, ObsTs, Sysid, RxDate, RxSource, BundleType, NumberOfParsers, LoadIdPriority = Value
  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case UploadedBy      =>  new LcpContext("""Loader.Uploaded_by=(.+?)""".r) with ILcpState
    case ObsTs           =>  new LcpContext("""Loader.Obs_ts=(.+?)""".r) with ILcpState
    case Sysid           =>  new LcpContext("""Loader.Sysid=(.+?)""".r) with ILcpState
    case RxDate          =>  new LcpContext("""Loader.Rx_date=(.+?)""".r) with ILcpState
    case RxSource        =>  new LcpContext("""Loader.Rx_source=(.+?)""".r) with ILcpState
    case BundleType      =>  new LcpContext("""Loader.Bundle_type=(.+?)""".r) with ILcpState
    case NumberOfParsers =>  new LcpContext("""Loader.NoOfParsers=(.+?)""".r) with ILcpState
    case LoadIdPriority  =>  new LcpContext("""Loader.LoadIdPriority=(.+?)""".r) with ILcpState
    case x => logger.error(s"unknown LCP subtype = $subtyp value of $x"); null
  }
}

object LCPInstance  {
  def getLcpContextObject(carg:LCPClassArguments,frozenObject:LCPContextAssignment) = new LCPContext(carg,frozenObject)
}

class LCPContext(carg:LCPClassArguments,contextObject:LCPContextAssignment) extends AbstractLCPContext(carg,contextObject) {

  def literal(lhs: String, texts: List[String], cefa: LCPEvalArguments): ContextReason = {
    val value = texts.head.trim
    ContextReason(cefa.cr.contextStrings + (lhs -> value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa:LCPEvalArguments) = evalAssignment(literal, cefa)
}
