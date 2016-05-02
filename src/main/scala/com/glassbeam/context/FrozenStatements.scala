//import FrozenType.FrozenType
//import com.glassbeam.context.ContextSection._
//import com.glassbeam.context.ContextStage._
//import com.glassbeam.context.{ContextReason, ContextSection => _, ContextStage => _, _}
//import com.glassbeam.model.Logger
//
//import scala.util.matching.Regex
//
//case class FrozenDefinition(section: ContextSection, stage: ContextStage, regex: Regex)
//
//object FrozenType extends Enumeration {
//  type FrozenType = Value
//  val Solr, Cassandra, S3, LCP = Value
//  def getSubtype(typ: FrozenType): FrozenSubtype = typ match {
//    case Solr      => SolrSubtype
//    case Cassandra => CassandraSubtype
//    case S3        => S3Subtype
//    case LCP       => LcpSubtype
//  }
//}
//
//abstract class FrozenSubtype extends Enumeration with Logger {
//  val logger = Logging(this)
//  def getDefinition(subtyp: FrozenSubtype): FrozenDefinition
//}
//
//object SolrSubtype extends FrozenSubtype {
//  type SolrSubtype = Value
//
//  val SolrHome, SolrZkHost, SolrHost, SolrNumShards, SolrReplicationFactor, SolrZkHostLogVault, SolrOnlyLogVault,
//  SolrBucketHost, SolrBucketColPlacementInfo, SolrBucketColSize, SolrBucketShardReplicaInfo, SolrDynamicSchema, SolrMaxContentLines,
//  SolrStoreIndex, SolrRejectNumPastDays, SolrRejectNumFutureDays, SolrNumShardsLV, SolrReplicationFactorLV = Value
//
//  def getDefinition(subtyp: FrozenSubtype): FrozenDefinition = subtyp match {
//    case SolrHome                   => FrozenDefinition(ImmutableVariable, Loader, """solr.home='(.+?)'""".r)
//    case SolrZkHost                 => FrozenDefinition(ImmutableVariable, Loader, """solr.zkHost='(.+?)'""".r)
//    case SolrHost                   => FrozenDefinition(ImmutableVariable, Loader, """solr.host='(.+?)'""".r)
//    case SolrNumShards              => FrozenDefinition(ImmutableVariable, Loader, """solr.numShards='(.+?)'""".r)
//    case SolrReplicationFactor      => FrozenDefinition(ImmutableVariable, Loader, """solr.replicationFactor='(.+?)'""".r)
//    case SolrZkHostLogVault         => FrozenDefinition(ImmutableVariable, Loader, """solr.zkHostLogVault='(.+?)'""".r)
//    case SolrOnlyLogVault           => FrozenDefinition(ImmutableVariable, Loader, """solr.onlyLogVault='(.+?)'""".r)
//    case SolrBucketHost             => FrozenDefinition(ImmutableVariable, Loader, """solr.bucketHost='(.+?)'""".r)
//    case SolrBucketColPlacementInfo => FrozenDefinition(ImmutableVariable, Loader, """solr.BucketColPlacementInfo='(.+?)'""".r)
//    case SolrBucketColSize          => FrozenDefinition(ImmutableVariable, Loader, """solr.BucketColSize='(.+?)'""".r)
//    case SolrBucketShardReplicaInfo => FrozenDefinition(ImmutableVariable, Loader, """solr.BucketShardReplicaInfo='(.+?)'""".r)
//    case SolrDynamicSchema          => FrozenDefinition(ImmutableVariable, Loader, """solr.DynamicSchema='(.+?)'""".r)
//    case SolrMaxContentLines        => FrozenDefinition(ImmutableVariable, Loader, """solr.MaxContentLines='(.+?)'""".r)
//    case SolrStoreIndex             => FrozenDefinition(ImmutableVariable, Loader, """solr.StoreIndex='(.+?)'""".r)
//    case SolrRejectNumPastDays      => FrozenDefinition(MutableVariable, Loader, """solr.RejectNumPastDays='(.+?)'""".r)
//    case SolrRejectNumFutureDays    => FrozenDefinition(MutableVariable, Loader, """solr.RejectNumFutureDays='(.+?)'""".r)
//    case SolrNumShardsLV            => FrozenDefinition(ImmutableVariable, Loader, """solr.NumShardsLV='(.+?)'""".r)
//    case SolrReplicationFactorLV    => FrozenDefinition(ImmutableVariable, Loader, """solr.ReplicationFactorLV='(.+?)'""".r)
//    case x => logger.error(s"unknown Solr subtype = $subtyp value of $x"); null
//  }
//}
//
//object CassandraSubtype extends FrozenSubtype {
//  type CassandraSubtype = Value
//
//  val CassKeyspace, CassTimeslice, CassPreparedInserts, CassAsync, CassEventBatchCount, CassFetchSize, CassConsistencyLevel = Value
//
//  def getDefinition(subtyp: FrozenSubtype): FrozenDefinition = subtyp match {
//    case CassKeyspace         => FrozenDefinition(ImmutableVariable, Loader, """cass.Keyspace=(.+?)""".r)
//    case CassTimeslice        => FrozenDefinition(ImmutableVariable, Loader, """cass.Timeslice=(.+?)""".r)
//    case CassPreparedInserts  => FrozenDefinition(ImmutableVariable, Loader, """cass.PreparedInserts=(.+?)""".r)
//    case CassAsync            => FrozenDefinition(ImmutableVariable, Loader, """cass.Async=(.+?)""".r)
//    case CassEventBatchCount  => FrozenDefinition(ImmutableVariable, Loader, """cass.EventBatchCount=(.+?)""".r)
//    case CassFetchSize        => FrozenDefinition(ImmutableVariable, Loader, """cass.FetchSize=(.+?)""".r)
//    case CassConsistencyLevel => FrozenDefinition(ImmutableVariable, Loader, """cass.ConsistencyLevel=(.+?)""".r)
//    case x => logger.error(s"unknown Cassandra subtype = $subtyp value of $x"); null
//  }
//}
//
//object S3Subtype extends FrozenSubtype {
//  type S3Subtype = Value
//
//  val S3AccessKey, S3SecurityKey, S3Bucket = Value
//
//  def getDefinition(subtyp: FrozenSubtype): FrozenDefinition = subtyp match {
//    case S3AccessKey   => FrozenDefinition(ImmutableVariable, Loader, """s3.AccessKey=(.+?)""".r)
//    case S3SecurityKey => FrozenDefinition(ImmutableVariable, Loader, """s3.SecurityKey=(.+?)""".r)
//    case S3Bucket      => FrozenDefinition(ImmutableVariable, Loader, """s3.Bucket=(.+?)""".r)
//    case x => logger.error(s"unknown S3 subtype = $subtyp value of $x"); null
//  }
//}
//
//object LcpSubtype extends FrozenSubtype {
//  type LcpSubtype = Value
//  val UploadedBy, ObsTs, Sysid, RxDate, RxSource, BundleType, NumberOfParsers, LoadIdPriority = Value
//  def getDefinition(subtyp: FrozenSubtype): FrozenDefinition = subtyp match {
//    case UploadedBy      => FrozenDefinition(MutableVariable, Loader, """uploaded_by=(.+?)""".r)
//    case ObsTs           => FrozenDefinition(MutableVariable, Loader, """obs_ts=(.+?)""".r)
//    case Sysid           => FrozenDefinition(MutableVariable, Loader, """sysid=(.+?)""".r)
//    case RxDate          => FrozenDefinition(MutableVariable, Loader, """rx_date=(.+?)""".r)
//    case RxSource        => FrozenDefinition(MutableVariable, Loader, """rx_source=(.+?)""".r)
//    case BundleType      => FrozenDefinition(MutableVariable, Loader, """bundle_type=(.+?)""".r)
//    case NumberOfParsers => FrozenDefinition(ImmutableVariable, Loader, """noOfParsers=(.+?)""".r)
//    case LoadIdPriority  => FrozenDefinition(ImmutableVariable, Loader, """loadIdPriority=(.+?)""".r)
//    case x => logger.error(s"unknown LCP subtype = $subtyp value of $x"); null
//  }
//}
//
//abstract class Frozen extends AbstractContextObject {
//  def getDefinition(typ: FrozenType, subtyp: FrozenSubtype): FrozenDefinition = {
//    val subtype = FrozenType.getSubtype(typ)
//    subtyp.getDefinition(subtyp)
//  }
//}
//
//class FrozenLiteral(typ: FrozenType, subtyp: FrozenSubtype) extends Frozen {
//  val frozenDefinition = getDefinition(typ, subtyp)
//  val fullRegex = frozenDefinition.regex
//  val contextSection = frozenDefinition.section
//  val contextStage = frozenDefinition.stage
//  val rhsRegex = null
//  val isAssignment = true
//  def getObject(carg: ContextClassArguments) = new FrozenLiteralACC(carg)
//}
//
//class FrozenLiteralACC(carg: ContextClassArguments) extends AbstractContextClass(carg, Literal) {
//  private def frozen(lhs: String, texts: List[String], cefa: ContextExecFnArguments): ContextReason = {
//    val value = texts.head.trim
//    ContextReason(cefa.cr.contextStrings + (lhs -> value), cefa.cr.reason, cefa.cr.failure, cefa.cr.bproperties)
//  }
//
//  //def execute(cefa: ContextExecFnArguments): ContextReason = evalAssignment(frozen, cefa)
//}
//
//object FrozenLiterals extends Logger {
//  val logger = Logging(this)
//  var frozenLiterals: Map[String, FrozenLiteral] = Map()
//
//  for (ft <- FrozenType.values) {
//    val subtyp = FrozenType.getSubtype(ft)
//    frozenLiterals ++= subtyp.values.map { st =>
//      (st.toString, new FrozenLiteral(ft, subtyp))
//    }.toMap
//  }
//
//  def getFrozenLiteral(name: String): FrozenLiteral = {
//    frozenLiterals.get(name) match {
//      case Some(fl) =>fl
//      //fl.getObject()
//      case None => logger.error(s"Unknown frozen literal name = $name"); null
//    }
//  }
//}
