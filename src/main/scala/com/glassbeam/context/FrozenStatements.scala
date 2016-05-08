package com.glassbeam.context

import com.glassbeam.context.{ContextSection => _, ContextStage => _}
import com.glassbeam.model.Logger

import scala.util.matching.Regex

case class FrozenContextLine(val rhsRegex: Regex) extends FrozenContextAssignment

object FrozenType extends Enumeration {
  type FrozenType = Value
  val Solr, Cassandra, S3, LCP = Value
  def getSubtype(typ: FrozenType): FrozenSubtype = typ match {
    case Solr      => SolrSubtype
    case Cassandra => CassandraSubtype
    case S3        => S3Subtype
    case LCP       => LcpSubtype
  }
}

abstract class FrozenSubtype extends Enumeration with Logger {
  val logger = Logging(this)
  def getDefinition(clid:Value): FrozenContextLine
}

object SolrSubtype extends FrozenSubtype {
  type SolrSubtype = Value

  val SolrHome, SolrZkHost, SolrHost, SolrNumShards, SolrReplicationFactor, SolrZkHostLogVault, SolrOnlyLogVault,
  SolrBucketHost, SolrBucketColPlacementInfo, SolrBucketColSize, SolrBucketShardReplicaInfo, SolrDynamicSchema, SolrMaxContentLines,
  SolrStoreIndex, SolrRejectNumPastDays, SolrRejectNumFutureDays, SolrNumShardsLV, SolrReplicationFactorLV = Value

  //println("solr home in string "+SolrHome.toString)

  def getDefinition(solrline: Value): FrozenContextLine = solrline match {
    case SolrHome                   => FrozenContextLine("""Solr.DataDir='(.+?)'""".r)
    case SolrZkHost                 => FrozenContextLine("""Solr.ZkHost='(.+?)'""".r)
    case SolrHost                   => FrozenContextLine("""Solr.Host='(.+?)'""".r)
    case SolrNumShards              => FrozenContextLine("""Solr.NumShards='(.+?)'""".r)
    case SolrReplicationFactor      => FrozenContextLine("""Solr.ReplicationFactor='(.+?)'""".r)
    case SolrZkHostLogVault         => FrozenContextLine("""Solr.ZkHostLogVault='(.+?)'""".r)
    case SolrOnlyLogVault           => FrozenContextLine("""Solr.OnlyLogVault='(.+?)'""".r)
    case SolrBucketHost             => FrozenContextLine("""Solr.BucketHost='(.+?)'""".r)
    case SolrBucketColPlacementInfo => FrozenContextLine("""Solr.BucketColPlacementInfo='(.+?)'""".r)
    case SolrBucketColSize          => FrozenContextLine("""Solr.BucketColSize='(.+?)'""".r)
    case SolrBucketShardReplicaInfo => FrozenContextLine("""Solr.BucketShardReplicaInfo='(.+?)'""".r)
    case SolrDynamicSchema          => FrozenContextLine("""Solr.DynamicSchema='(.+?)'""".r)
    case SolrMaxContentLines        => FrozenContextLine("""Solr.MaxContentLines='(.+?)'""".r)
    case SolrStoreIndex             => FrozenContextLine("""Solr.StoreIndex='(.+?)'""".r)
    case SolrRejectNumPastDays      => FrozenContextLine("""Solr.RejectNumPastDays='(.+?)'""".r)
    case SolrRejectNumFutureDays    => FrozenContextLine("""Solr.RejectNumFutureDays='(.+?)'""".r)
    case SolrNumShardsLV            => FrozenContextLine("""Solr.NumShardsLV='(.+?)'""".r)
    case SolrReplicationFactorLV    => FrozenContextLine("""Solr.ReplicationFactorLV='(.+?)'""".r)
    case x => logger.error(s"unknown Solr subtype = $solrline value of $x"); null
  }
}

object CassandraSubtype extends FrozenSubtype {
  type CassandraSubtype = Value

  val CassKeyspace, CassTimeslice, CassPreparedInserts, CassAsync, CassEventBatchCount, CassFetchSize, CassConsistencyLevel, CassHost = Value

  def getDefinition(subtyp: Value): FrozenContextLine = subtyp match {
    case CassKeyspace         => FrozenContextLine("""Cass.Keyspace=(.+?)""".r)
    case CassTimeslice        => FrozenContextLine("""Cass.Timeslice=(.+?)""".r)
    case CassPreparedInserts  => FrozenContextLine("""Cass.PreparedInserts=(.+?)""".r)
    case CassAsync            => FrozenContextLine("""Cass.Async=(.+?)""".r)
    case CassEventBatchCount  => FrozenContextLine("""Cass.EventBatchCount=(.+?)""".r)
    case CassFetchSize        => FrozenContextLine("""Cass.FetchSize=(.+?)""".r)
    case CassConsistencyLevel => FrozenContextLine("""Cass.ConsistencyLevel=(.+?)""".r)
    case CassHost             => FrozenContextLine("""Cass.Host='(.+?)'""".r)
    case x => logger.error(s"unknown Cassandra subtype = $subtyp value of $x"); null
  }
}
//
object S3Subtype extends FrozenSubtype {
  type S3Subtype = Value

  val S3AccessKey, S3SecurityKey, S3Bucket = Value

  def getDefinition(subtyp: Value): FrozenContextLine = subtyp match {
    case S3AccessKey   => FrozenContextLine( """S3.AccessKey=(.+?)""".r)
    case S3SecurityKey => FrozenContextLine("""S3.SecurityKey=(.+?)""".r)
    case S3Bucket      => FrozenContextLine("""S3.Bucket=(.+?)""".r)
    case x => logger.error(s"unknown S3 subtype = $subtyp value of $x"); null
  }
}
//
object LcpSubtype extends FrozenSubtype {
  type LcpSubtype = Value
  val UploadedBy, ObsTs, Sysid, RxDate, RxSource, BundleType, NumberOfParsers, LoadIdPriority = Value
  def getDefinition(subtyp: Value): FrozenContextLine = subtyp match {
    case UploadedBy      => FrozenContextLine("""Lcp.Uploaded_by=(.+?)""".r)
    case ObsTs           => FrozenContextLine("""Lcp.Obs_ts=(.+?)""".r)
    case Sysid           => FrozenContextLine("""Lcp.Sysid=(.+?)""".r)
    case RxDate          => FrozenContextLine("""Lcp.Rx_date=(.+?)""".r)
    case RxSource        => FrozenContextLine("""Lcp.Rx_source=(.+?)""".r)
    case BundleType      => FrozenContextLine("""Lcp.Bundle_type=(.+?)""".r)
    case NumberOfParsers => FrozenContextLine("""Lcp.NoOfParsers=(.+?)""".r)
    case LoadIdPriority  => FrozenContextLine("""Lcp.LoadIdPriority=(.+?)""".r)
    case x => logger.error(s"unknown LCP subtype = $subtyp value of $x"); null
  }
}

object FrozenInstance  {
  def getFrozenInstance(carg:ContextClassArguments,frozenObject:FrozenContextAssignment) = new FrozenContext(carg,frozenObject)
}


class FrozenContext(carg: ContextClassArguments,contextObject:FrozenContextAssignment) extends AbstractLoaderContext(carg,contextObject) with FrozenState {

  def literal(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
    val value = texts.head.trim
    ContextReason(cefa.cr.contextStrings + (lhs -> value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa:ContextExecFnArguments) = evalAssignment(literal, cefa)
}
