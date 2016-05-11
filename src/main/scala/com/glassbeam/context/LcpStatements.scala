package com.glassbeam.context

import com.glassbeam.context.Constants._
import com.glassbeam.context.Context._
import com.glassbeam.model.Logger

import scala.util.matching.Regex

abstract class LcpContext(val rhsRegex: Regex) extends LCPContextAssignment

object LcpStatements {

  def unapply(ma:MatchArguments): Option[(LcpContext,String)] = {
    println("lcp line " + ma.conline)
    ma.conline.split('.') match {
      case Array(Solr.prefix, _) => SolrContextLines.unapply(ma) map ((_, Solr.prefix))
      case Array(Cassandra.prefix, _) => CassandraContextLines.unapply(ma) map ((_, Cassandra.prefix))
      case Array(S3.prefix, _) => S3ContextLines.unapply(ma) map ((_, S3.prefix))
      case Array(Loader.prefix, _) => LoaderContextLines.unapply(ma) map ((_, Loader.prefix))
      case _ => None
    }
  }

}

abstract class LcpSubtype extends Enumeration with Logger {
  val logger = Logging(this)
  def getDefinition(clid:Value): LcpContext
}

object SolrContextLines extends LcpSubtype {

  type SolrSubtype = Value
  val S = Solr.prefix

  val SolrHome, SolrZkHost, SolrHost, SolrNumShards, SolrReplicationFactor, SolrZkHostLogVault, SolrOnlyLogVault,
  SolrBucketHost, SolrBucketColPlacementInfo, SolrBucketColSize, SolrBucketShardReplicaInfo, SolrDynamicSchema, SolrMaxContentLines,
  SolrStoreIndex, SolrRejectNumPastDays, SolrRejectNumFutureDays, SolrNumShardsLV, SolrReplicationFactorLV = Value

  def getDefinition(solrline: Value): LcpContext = solrline match {
    case SolrHome                   => new LcpContext(s"""$S.DataDir='(.+?)'""".r) with ILcpState
    case SolrZkHost                 => new LcpContext(s"""$S.ZkHost='(.+?)'""".r) with ILcpState
    case SolrHost                   => new LcpContext(s"""$S.Host='(.+?)'""".r) with ILcpState
    case SolrNumShards              => new LcpContext(s"""$S.NumShards='(.+?)'""".r) with ILcpState
    case SolrReplicationFactor      => new LcpContext(s"""$S.ReplicationFactor='(.+?)'""".r) with ILcpState
    case SolrZkHostLogVault         => new LcpContext(s"""$S.ZkHostLogVault='(.+?)'""".r) with ILcpState
    case SolrOnlyLogVault           => new LcpContext(s"""$S.OnlyLogVault='(.+?)'""".r) with ILcpState
    case SolrBucketHost             => new LcpContext(s"""$S.BucketHost='(.+?)'""".r) with ILcpState
    case SolrBucketColPlacementInfo => new LcpContext(s"""$S.BucketColPlacementInfo='(.+?)'""".r) with ILcpState
    case SolrBucketColSize          => new LcpContext(s"""$S.BucketColSize='(.+?)'""".r) with ILcpState
    case SolrBucketShardReplicaInfo => new LcpContext(s"""$S.BucketShardReplicaInfo='(.+?)'""".r) with ILcpState
    case SolrDynamicSchema          => new LcpContext(s"""$S.DynamicSchema='(.+?)'""".r) with ILcpState
    case SolrMaxContentLines        => new LcpContext(s"""$S.MaxContentLines='(.+?)'""".r) with ILcpState
    case SolrStoreIndex             => new LcpContext(s"""$S.StoreIndex='(.+?)'""".r) with ILcpState
    case SolrRejectNumPastDays      => new LcpContext(s"""$S.RejectNumPastDays='(.+?)'""".r) with MLcpState
    case SolrRejectNumFutureDays    => new LcpContext(s"""$S.RejectNumFutureDays='(.+?)'""".r) with MLcpState
    case SolrNumShardsLV            => new LcpContext(s"""$S.NumShardsLV='(.+?)'""".r) with ILcpState
    case SolrReplicationFactorLV    => new LcpContext(s"""$S.ReplicationFactorLV='(.+?)'""".r) with ILcpState
  }

  def unapply(ma:MatchArguments): Option[LcpContext] = this.values.map(getDefinition).find{ solrline => solrline.rhsRegex.pattern.matcher(ma.conline).matches() && solrline.contextSection == ma.cSection }
}

object CassandraContextLines extends LcpSubtype {

  type CassandraSubtype = Value
  val C = Cassandra.prefix

  val CassMetaKeyspace, CassMpsKeyspace,CassTimeslice, CassPreparedInserts, CassAsync, CassEventBatchCount, CassFetchSize, CassConsistencyLevel, CassHost = Value

  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case CassMetaKeyspace         =>  new LcpContext(s"""$C.MetaKeyspace=(.+?)""".r) with ILcpState
    case CassMpsKeyspace         =>  new LcpContext(s"""$C.MpsKeyspace=(.+?)""".r) with ILcpState
    case CassTimeslice        =>  new LcpContext(s"""$C.Timeslice=(.+?)""".r) with ILcpState
    case CassPreparedInserts  =>  new LcpContext(s"""$C.PreparedInserts=(.+?)""".r) with ILcpState
    case CassAsync            =>  new LcpContext(s"""$C.Async=(.+?)""".r) with ILcpState
    case CassEventBatchCount  =>  new LcpContext(s"""$C.EventBatchCount=(.+?)""".r) with ILcpState
    case CassFetchSize        =>  new LcpContext(s"""$C.FetchSize=(.+?)""".r) with ILcpState
    case CassConsistencyLevel =>  new LcpContext(s"""$C.ConsistencyLevel=(.+?)""".r) with ILcpState
    case CassHost             =>  new LcpContext(s"""$C.Host='(.+?)'""".r) with ILcpState
  }

  def unapply(ma:MatchArguments): Option[LcpContext] = this.values.map(getDefinition).find{ cassline => cassline.rhsRegex.pattern.matcher(ma.conline).matches() && cassline.contextSection == ma.cSection }
}
//
object S3ContextLines extends LcpSubtype {

  type S3Subtype = Value
  val s3 = S3.prefix

  val S3AccessKey, S3SecurityKey, S3Bucket = Value

  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case S3AccessKey   =>  new LcpContext(s"""$s3.AccessKey=(.+?)""".r) with ILcpState
    case S3SecurityKey =>  new LcpContext(s"""$s3.SecretKey=(.+?)""".r) with ILcpState
    case S3Bucket      =>  new LcpContext(s"""$s3.Bucket=(.+?)""".r) with ILcpState
  }

  def unapply(ma:MatchArguments): Option[LcpContext] = this.values.map(getDefinition).find{ s3line => s3line.rhsRegex.pattern.matcher(ma.conline).matches() && s3line.contextSection == ma.cSection }
}


//
object LoaderContextLines extends LcpSubtype {
  type LcpSubtype = Value
  val L = Loader.prefix
  val UploadedBy, ObsTs, Sysid, RxDate, RxSource, BundleType, NumberOfParsers, LoadIdPriority = Value
  def getDefinition(subtyp: Value): LcpContext = subtyp match {
    case UploadedBy      =>  new LcpContext(s"""$L.Uploaded_by=(.+?)""".r) with ILcpState
    case ObsTs           =>  new LcpContext(s"""$L.Obs_ts=(.+?)""".r) with ILcpState
    case Sysid           =>  new LcpContext(s"""$L.Sysid=(.+?)""".r) with ILcpState
    case RxDate          =>  new LcpContext(s"""$L.Rx_date=(.+?)""".r) with ILcpState
    case RxSource        =>  new LcpContext(s"""$L.Rx_source=(.+?)""".r) with ILcpState
    case BundleType      =>  new LcpContext(s"""$L.Bundle_type=(.+?)""".r) with ILcpState
    case NumberOfParsers =>  new LcpContext(s"""$L.NoOfParsers=(.+?)""".r) with ILcpState
    case LoadIdPriority  =>  new LcpContext(s"""$L.LoadIdPriority=(.+?)""".r) with ILcpState
  }

  def unapply(ma:MatchArguments): Option[LcpContext] = this.values.map(getDefinition).find{ loaderline => loaderline.rhsRegex.pattern.matcher(ma.conline).matches() && loaderline.contextSection == ma.cSection }
}

object LcpObject  {
  def getObject(carg:ContextClassArguments,frozenObject:LCPContextAssignment) = new LCPContextInstance(carg,frozenObject)
}

class LCPContextInstance(carg:ContextClassArguments,contextObject:LCPContextAssignment) extends AbstractLCPContext(carg,contextObject) {

  def literal(lhs: String, texts: List[String], cefa: LCPEvalArguments): ContextReason = {
    val value = texts.head.trim
    ContextReason(cefa.cr.contextStrings + (lhs -> value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
  }

  def execute(cefa:LCPEvalArguments) = {
    println("IN LCP Instance for "+cefa.cr)
    evalAssignment(literal, cefa)
  }
}
