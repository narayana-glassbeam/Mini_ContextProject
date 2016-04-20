package com.glassbeam.h2utils

import java.sql.Timestamp
import java.sql.Timestamp

import scala.slick.driver.H2Driver.simple._
import scala.slick.util.TupleMethods._
import scala.slick.collection.heterogenous._
import scala.slick.lifted.Tag

import com.github.tototoshi.slick.H2JodaSupport._
import org.joda.time.DateTime

class GenKVTable(tag: Tag, tableName: String) extends Table[(String, String, String)](tag, tableName) {
  def key = column[String]("KEY", O.DBType("VARCHAR(1024)"))
  def value = column[String]("VALUE", O.DBType("VARCHAR(4096)"))
  def emps = column[String]("EMPS", O.DBType("VARCHAR(200)"))
  def * = key ~ value ~ emps
  def idx = index(tableName + "_emps", emps)
}

class SingleColumnTable(tag: Tag, tableName: String, colName: String) extends Table[(String)](tag, tableName) {
  def version = column[String](colName, O.DBType("VARCHAR(128)"))
  def * = version
}

class OmittedFiles(tag: Tag) extends Table[(String, Long, String, String, String, DateTime)](tag, "OMITTEDFILES") {
  def name = column[String]("NAME", O.DBType("VARCHAR(4096)"))
  def load_id = column[Long]("LOAD_ID")
  def spl = column[String]("SPL", O.DBType("VARCHAR(200)"))
  def ec = column[String]("CUSTOMER", O.DBType("VARCHAR(200)"))
  def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
  def ts = column[DateTime]("TS")

  def * = name ~ load_id ~ spl ~ ec ~ node ~ ts

  def idx1 = index("omittedfiles_date", ts)
  def idx2 = index("omittedfiles_loadid", (node, load_id))
}

class CompressedFiles(tag: Tag) extends Table[(String, Long, String, String, Long, String, Option[Int], Option[Timestamp], Option[Timestamp], DateTime)](tag, "COMPRESSEDFILES") {
  def name = column[String]("NAME", O.DBType("VARCHAR(4096)"))
  def load_id = column[Long]("LOAD_ID")
  def spl = column[String]("SPL", O.DBType("VARCHAR(200)"))
  def ec = column[String]("CUSTOMER", O.DBType("VARCHAR(200)"))
  def size = column[Long]("SIZE")
  def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
  def ustate = column[Option[Int]]("UNCOMPRESS_STATE")
  def ustart = column[Option[Timestamp]]("UNCOMPRESS_START")
  def uend = column[Option[Timestamp]]("UNCOMPRESS_END")
  def ts = column[DateTime]("TS")

  def * = name ~ load_id ~ spl ~ ec ~ size ~ node ~ ustate ~ ustart ~ uend ~ ts

  def idx1 = index("compressedfiles_date", ts)
  def idx2 = index("compressedfiles_loadid", (node, load_id))
}

class Ops(val ts: Timestamp, val obsts: Timestamp, val load_id: Long,
  val seen: Timestamp, val started: Option[Timestamp], val completed: Option[Timestamp],
  val file_type: Byte, val processing_state: Option[Byte], val contextFailure: Option[Byte],
  val parsingFailure: Option[Byte], val cassFailure: Option[Byte], val solrFailure: Option[Byte],
  val rulesFailure: Option[Byte], val alertsFailure: Option[Byte],
  val size: Long, val logvaultstatus: Option[Byte], val node: String,
  val name: String, val spl: String, val parser: Option[String],
  val context: Option[String], val ec: String, val system: Option[String],
  val notes: Option[String], val linecount: Option[Long], val mime: Option[String])

object Ops {
  def apply(ts: Timestamp, obsts: Timestamp, load_id: Long, seen: Timestamp, started: Option[Timestamp],
    completed: Option[Timestamp], file_type: Byte, processing_state: Option[Byte], contextFailure: Option[Byte],
    parsingFailure: Option[Byte], cassFailure: Option[Byte], solrFailure: Option[Byte], rulesFailure: Option[Byte],
    alertsFailure: Option[Byte], size: Long, logvaultstatus: Option[Byte], node: String, name: String, spl: String,
    parser: Option[String], context: Option[String], ec: String, system: Option[String], notes: Option[String],
    linecount: Option[Long], mime: Option[String]) = new Ops(ts, obsts, load_id, seen, started, completed, file_type, processing_state, contextFailure, parsingFailure, cassFailure,
    solrFailure, rulesFailure, alertsFailure, size, logvaultstatus, node, name, spl, parser, context, ec, system, notes, linecount, mime)

  def unapply(x: Ops) = {
    val data = x.ts :: x.obsts :: x.load_id :: x.seen :: x.started :: x.completed :: x.file_type :: x.processing_state ::
      x.contextFailure :: x.parsingFailure :: x.cassFailure :: x.solrFailure :: x.rulesFailure :: x.alertsFailure ::
      x.size :: x.logvaultstatus :: x.node :: x.name :: x.spl :: x.parser :: x.context :: x.ec :: x.system ::
      x.notes :: x.linecount :: x.mime :: HNil
    Option[data.type](data)
  }
}

class OpsTable(tag: Tag) extends Table[Ops](tag, "OPS") {
  def ts = column[Timestamp]("TS")
  def obsts = column[Timestamp]("OBSTS")
  def load_id = column[Long]("LOAD_ID")
  def seen = column[Timestamp]("SEEN")
  def started = column[Option[Timestamp]]("STARTED")
  def completed = column[Option[Timestamp]]("COMPLETED")
  def file_type = column[Byte]("FILE_TYPE")
  def pstate = column[Option[Byte]]("PROCESSING_STATE")
  def cofailure = column[Option[Byte]]("CONTEXT_FAILURE")
  def pfailure = column[Option[Byte]]("PARSING_FAILURE")
  def cafailure = column[Option[Byte]]("CASSANDRA_FAILURE")
  def sfailure = column[Option[Byte]]("SOLR_FAILURE")
  def rfailure = column[Option[Byte]]("RULES_FAILURE")
  def afailure = column[Option[Byte]]("ALERTS_FAILURE")
  def size = column[Long]("SIZE")
  def lvstatus = column[Option[Byte]]("LOGVAULT_STATUS")
  def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
  def name = column[String]("NAME", O.DBType("VARCHAR(4096)"))
  def spl = column[String]("MPS", O.DBType("VARCHAR(200)"))
  def parser = column[Option[String]]("PARSER", O.DBType("VARCHAR(200)"))
  def context = column[Option[String]]("CONTEXT")
  def ec = column[String]("CUSTOMER", O.DBType("VARCHAR(200)"))
  def system = column[Option[String]]("SYSID", O.DBType("VARCHAR(200)"))
  def notes = column[Option[String]]("NOTES", O.DBType("VARCHAR(4096)"))
  def linecount = column[Option[Long]]("LINECOUNT")
  def mime = column[Option[String]]("MIME", O.DBType("VARCHAR(1024)"))

  def * = (ts :: obsts :: load_id :: seen :: started :: completed :: file_type :: pstate ::
    cofailure :: pfailure :: cafailure :: sfailure :: rfailure :: afailure :: size :: lvstatus :: node ::
    name :: spl :: parser :: context :: ec :: system :: notes :: linecount :: mime :: HNil).shaped <> (
      {
        case x => Ops(x(0), x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10), x(11), x(12), x(13), x(14), x(15),
          x(16), x(17), x(18), x(19), x(20), x(21), x(22), x(23), x(24), x(25))
      },
      {
        Ops.unapply _
      }
    )

  def idx0 = index("ops_Node", (node))
  def idx1 = index("ops_NodeLoad", (load_id, node)) // for optimization of query in bfng_r
  def idx2 = index("ops_NodeLoadType", (load_id, node, file_type)) // for optimization of query in bg_r
  def idx3 = index("ops_NodeState", (node, pstate))
  def idx4 = index("ops_NodeLoadState", (load_id, node, pstate)) // for optimization of multiple queries in cleanup
  def idx5 = index("ops_NodeSplState", (node, spl, pstate)) // for optimization of query in CompilerFree
}

class LoadId(val id: Long, val node: String, val mps: String, val bundleType: Byte,
  val bundleName: String, val rxSize: Long, val pxSize: Option[Long], val skipSize: Option[Long],
  val rxCount: Option[Long], val pxCount: Option[Long], val skipCount: Option[Long], val properties: Option[String],
  val completeInOps: Boolean, val parseComplete: Boolean, val seenTime: Timestamp, val completeInOpsTime: Option[Timestamp],
  val parseCompleteTime: Option[Timestamp], val bundleState: Byte, val vaultStatus: Byte,
  val vaultPath: Option[String])

object LoadId {
  def apply(id: Long, node: String, mps: String, bundleType: Byte, bundleName: String, rxSize: Long, pxSize: Option[Long],
    skipSize: Option[Long], rxCount: Option[Long], pxCount: Option[Long], skipCount: Option[Long], properties: Option[String],
    completeInOps: Boolean, parseComplete: Boolean, seenTime: Timestamp, completeInOpsTime: Option[Timestamp],
    parseCompleteTime: Option[Timestamp], bundleState: Byte, vaultStatus: Byte, vaultPath: Option[String]) = new LoadId(id, node, mps, bundleType, bundleName, rxSize, pxSize, skipSize, rxCount, pxCount, skipCount, properties,
    completeInOps, parseComplete, seenTime, completeInOpsTime, parseCompleteTime, bundleState, vaultStatus, vaultPath)

  def unapply(x: LoadId) = {
    val data = x.id :: x.node :: x.mps :: x.bundleType :: x.bundleName :: x.rxSize :: x.pxSize :: x.skipSize :: x.rxCount ::
      x.pxCount :: x.skipCount :: x.properties :: x.completeInOps :: x.parseComplete :: x.seenTime :: x.completeInOpsTime ::
      x.parseCompleteTime :: x.bundleState :: x.vaultStatus :: x.vaultPath :: HNil
    Option[data.type](data)
  }
}

class LoadIds(tag: Tag) extends Table[LoadId](tag, "LOAD_ID") {
  def load_id = column[Long]("LOAD_ID", O.PrimaryKey, O.AutoInc)
  def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
  def mps = column[String]("MPS", O.DBType("VARCHAR(100)"))
  def bundleType = column[Byte]("TYPE")
  def bundleName = column[String]("BUNDLE_NAME", O.DBType("VARCHAR(4096)"))
  def rxSize = column[Long]("RX_SIZE") // files to {parse + skip + delete} size
  def pxSize = column[Option[Long]]("PX_SIZE") // files to parse size
  def skipSize = column[Option[Long]]("SKIP_SIZE") // files to skip size
  def rxCount = column[Option[Long]]("RX_COUNT") // files to {parse + skip + delete} count
  def skipCount = column[Option[Long]]("SKIP_COUNT") // files to skip count
  def pxCount = column[Option[Long]]("PX_COUNT") // files to parse count
  def properties = column[Option[String]]("PROPERTIES", O.DBType("VARCHAR(1024)"))
  def compInOps = column[Boolean]("COMPLETE_IN_OPS")
  def pComplete = column[Boolean]("PARSE_COMPLETE")
  def seenTime = column[Timestamp]("SEEN_TIME")
  def cInOpsTime = column[Option[Timestamp]]("COMPLETE_IN_OPS_TIME")
  def pCompTime = column[Option[Timestamp]]("PARSE_COMPLETE_TIME")
  def bState = column[Byte]("BUNDLE_STATE")
  def vaultStatus = column[Byte]("VAULT_STATUS")
  def vaultPath  = column[Option[String]]("VAULT_PATH", O.DBType("VARCHAR(500)"))

  def * = (load_id :: node :: mps :: bundleType :: bundleName :: rxSize :: pxSize :: skipSize :: rxCount :: pxCount ::
    skipCount :: properties :: compInOps :: pComplete :: seenTime :: cInOpsTime :: pCompTime :: bState :: vaultStatus ::
    vaultPath :: HNil).shaped <> (
      {
        case x => LoadId(x(0), x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10), x(11), x(12), x(13), x(14), x(15),
          x(16), x(17), x(18), x(19))
      },
      {
        LoadId.unapply _
      }
    )

  def idx1 = index("loadid_Node", node) // all queries after all are node specific
  def idx2 = index("loadid_ParseComplete", (node, pComplete))
}

class BundleSignature(tag: Tag) extends Table[(Long, DateTime, Timestamp, DateTime, String, String, Byte, String)](
  tag,
  "BUNDLE_SIGNATURE"
) {
  def load_id = column[Long]("LOAD_ID", O.PrimaryKey)
  def ts = column[DateTime]("TS")
  def obs_ts = column[Timestamp]("OBS_TS")
  val parse_start = column[DateTime]("PARSE_START")
  def mps = column[String]("MPS", O.DBType("VARCHAR(200)"))
  def signature = column[String]("SIGNATURE", O.DBType("VARCHAR(200)"))
  def bSigState = column[Byte]("BUNDLE_SIGNATURE_STATE")
  def ec = column[String]("CUSTOMER", O.DBType("VARCHAR(200)"))
  def * = load_id ~ ts ~ obs_ts ~ parse_start ~ mps ~ signature ~ bSigState ~ ec

  def idx1 = index("bundle_signature_mps", mps)
  def idx2 = index("bundle_signature_date", ts)
}

case class LcpSS(runid: Long, node: String, start_ts: Timestamp, heartbeat_ts: Timestamp)
class LcpStartStop(tag: Tag) extends Table[LcpSS](tag, "LCP_START_STOP") {
  def runid = column[Long]("RUN_ID", O.PrimaryKey, O.AutoInc)
  def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
  def start_ts = column[Timestamp]("START_TS")
  def heartbeat_ts = column[Timestamp]("HEARTBEAT_TS")

  def * = (runid, node, start_ts, heartbeat_ts) <> (LcpSS.tupled, LcpSS.unapply)
}

class SplTable(tag: Tag) extends Table[(String, Timestamp, String, String, String)](tag, "SPL") {
  def name = column[String]("NAME", O.DBType("VARCHAR(200)"))
  def ts = column[Timestamp]("TS")
  def chksum = column[String]("CHKSUM", O.DBType("VARCHAR(1024)"))
  def status = column[String]("STATUS", O.DBType("VARCHAR(1024)"))
  def hold = column[String]("HOLD", O.DBType("VARCHAR(128)"))

  def * = name ~ ts ~ chksum ~ status ~ hold

  def idx1 = index("spl_name", name)
}

class ContextTable(tag: Tag) extends Table[(String, String,String,Timestamp)](tag, "CONTEXT") {
  def key = column[String]("KEY", O.PrimaryKey)
  def mutable_value = column[String]("MUTABLE")
  def immutable_value = column[String]("IMMUTABLE")
  def LastModified = column[Timestamp]("LAST_MODIFIED",O.DBType("TIMESTAMP AS CURRENT_TIMESTAMP()"))
  def * = key ~ mutable_value ~ immutable_value ~ LastModified
  def idx1 = index("context_key", key)
}

class ValidateTable(tag: Tag) extends GenKVTable(tag, "VALIDATE")

class LookupTable(tag: Tag) extends GenKVTable(tag, "LOOKUP")

class ColLookup(tag: Tag) extends Table[(Int, String, Int, String, String)](tag, "COLLOOKUP") {
  def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
  def emps = column[String]("EMPS", O.NotNull)
  def position = column[Int]("POSITION", O.NotNull)
  def key = column[String]("KEY")
  def value = column[String]("VALUE")
  def * = id ~ emps ~ position ~ key ~ value
}

class LogSignature(tag: Tag) extends Table[(String, DateTime, Long)](tag, "LOGSIGNATURE") {
  def key = column[String]("KEY", O.PrimaryKey, O.DBType("VARCHAR(4096)"))
  def ts = column[DateTime]("TS")
  def load_id = column[Long]("LOAD_ID")
  def * = key ~ ts ~ load_id

  def idx1 = index("log_signature_date", ts)
  def idx2 = index("log_signature_key", key)
  def idx3 = index("log_signature_loadid", load_id)
}

class LoaderTable(tag: Tag) extends Table[(String, String)](tag, "LOADER") {
  def key = column[String]("KEY", O.PrimaryKey, O.DBType("VARCHAR(1024)"))
  def value = column[String]("VALUE", O.DBType("VARCHAR(8192)"))
  def * = key ~ value
  def idx = index("loader_key", key)
}

class OpsSchema(tag: Tag) extends SingleColumnTable(tag, "SCHEMA_VERSION", "VERSION")

case class ProvisionId(id: Long, acct_state: Int, created: Timestamp, activated: Timestamp, inactivated: Timestamp, ssh_id: String,
  public_key: String, email: String, phone: String, address: String, notes: String, end_customer: String, manufacturer: String, product: String, schema: String)

class Provision(tag: Tag) extends Table[ProvisionId](tag, "PROVISION") {
  def acct_id = column[Long]("ACCT_ID", O.PrimaryKey, O.AutoInc)
  def acct_state = column[Int]("ACCT_STATE")
  def created = column[Timestamp]("CREATED")
  def activated = column[Timestamp]("ACTIVATED")
  def inactivated = column[Timestamp]("INACTIVATED")
  def ssh_id = column[String]("SSH_ID", O.DBType("VARCHAR(64)"))
  def public_key = column[String]("PUBLIC_KEY", O.DBType("VARCHAR(512)"))
  def email = column[String]("EMAIL", O.DBType("VARCHAR(256)"))
  def phone = column[String]("EMAIL", O.DBType("VARCHAR(64)"))
  def address = column[String]("ADDRESS", O.DBType("VARCHAR(128)"))
  def notes = column[String]("NOTES", O.DBType("VARCHAR(128)"))
  def end_customer = column[String]("END_CUSTOMER", O.DBType("VARCHAR(128)"))
  def manufacturer = column[String]("MANUFACTURER", O.DBType("VARCHAR(128)"))
  def product = column[String]("PRODUCT", O.DBType("VARCHAR(128)"))
  def schema = column[String]("SCHEMA", O.DBType("VARCHAR(128)"))

  def * = (acct_id, acct_state, created, activated, inactivated, ssh_id, public_key, email, phone, address, notes,
    end_customer, manufacturer, product, schema) <> (ProvisionId.tupled, ProvisionId.unapply)

}

class RuleId(val rule_id: Long, val key: String, val rule_name: String, val label: String, val description: String, val author: String,
  val created_ts: Timestamp, val modified_ts: Timestamp, val status: String, val kb_link: String,
  val is_deleted: Boolean, val notes: Option[String], val supported: Boolean,
  val template_id: Int, val notifier_id: Int, val category_id: Int, val severity: Int, val priority: Int,
  val colS: String, val colT: String, val col0: String, val col1: Option[String], val col2: String, val col3: String,
  val logic_actual: Option[String], val logic_display: Option[String], val text_display: Option[String], val max_limit: Int)

object RuleId {
  def apply(rule_id: Long, key: String, rule_name: String, label: String, description: String, author: String,
    created_ts: Timestamp, modified_ts: Timestamp, status: String, kb_link: String,
    is_deleted: Boolean, notes: Option[String], supported: Boolean,
    template_id: Int, notifier_id: Int, category_id: Int, severity: Int, priority: Int,
    colS: String, colT: String, col0: String, col1: Option[String], col2: String, col3: String,
    logic_actual: Option[String], logic_display: Option[String], text_display: Option[String], max_limit: Int) =
    new RuleId(rule_id, key, rule_name, label, description, author, created_ts, modified_ts, status, kb_link, is_deleted,
      notes, supported, template_id, notifier_id, category_id, severity, priority, colS, colT, col0, col1, col2, col3,
      logic_actual, logic_display, text_display, max_limit)

  def unapply(x: RuleId) = {
    val data = x.rule_id :: x.key :: x.rule_name :: x.label :: x.description :: x.author :: x.created_ts :: x.modified_ts ::
      x.status :: x.kb_link :: x.is_deleted :: x.notes :: x.supported :: x.template_id :: x.notifier_id ::
      x.category_id :: x.severity :: x.priority :: x.colS :: x.colT :: x.col0 :: x.col1 :: x.col2 :: x.col3 ::
      x.logic_actual :: x.logic_display :: x.text_display :: x.max_limit :: HNil
    Option[data.type](data)
  }
}

class RulesTable(tag: Tag) extends Table[RuleId](tag, "RULES") {
  def rule_id = column[Long]("RULE_ID", O.AutoInc, O.PrimaryKey)
  def key = column[String]("KEY", O.DBType("VARCHAR(128)"))
  def rule_name = column[String]("RULE_NAME", O.DBType("VARCHAR(512)"))
  def label = column[String]("LABEL", O.DBType("VARCHAR(1024)"))
  def desc = column[String]("DESCRIPTION", O.DBType("VARCHAR(1024)"))
  def author = column[String]("AUTHOR", O.DBType("VARCHAR(64)"))
  def created_ts = column[Timestamp]("CREATED_TS")
  def modified_ts = column[Timestamp]("MODIFIED_TS")
  def status = column[String]("STATUS", O.DBType("VARCHAR(128)"))
  def kb_link = column[String]("KB_LINK", O.DBType("VARCHAR(2048)"))
  def is_deleted = column[Boolean]("IS_DELETED") //is Deleted or not
  def notes = column[Option[String]]("NOTES", O.DBType("VARCHAR(4096)"), O.Nullable)
  def supported = column[Boolean]("SUPPORTED")
  def template_id = column[Int]("EMAIL_TEMPLATE_ID", O.Nullable)
  def notifier_id = column[Int]("ALERT_ID")
  def category_id = column[Int]("CATEGORY_ID")
  def severity = column[Int]("SEVERITY_ID")
  def priority = column[Int]("PRIORITY_ID")
  def colS = column[String]("COLS", O.DBType("VARCHAR(4096)")) //Scope
  def colT = column[String]("COLT", O.DBType("VARCHAR(4096)")) // Rules Table list
  def col0 = column[String]("COL0", O.DBType("VARCHAR(4096)")) //Declaration
  def col1 = column[Option[String]]("COL1", O.DBType("VARCHAR(4096)")) // TBD
  def col2 = column[String]("COL2") //Assignments
  def col3 = column[String]("COL3") //Execution
  def logic_actual = column[Option[String]]("LOGIC_ACTUAL", O.DBType("VARCHAR(4096)"), O.Nullable)
  def logic_display = column[Option[String]]("LOGIC_DISPLAY", O.DBType("VARCHAR(4096)"), O.Nullable)
  def text_display = column[Option[String]]("TEXT_DISPLAY", O.DBType("VARCHAR(4096)"), O.Nullable)
  def max_limit = column[Int]("MAX_LIMIT")

  def * = (rule_id :: key :: rule_name :: label :: desc :: author :: created_ts :: modified_ts :: status :: kb_link ::
    is_deleted :: notes :: supported :: template_id :: notifier_id :: category_id :: severity :: priority :: colS ::
    colT :: col0 :: col1 :: col2 :: col3 :: logic_actual :: logic_display :: text_display :: max_limit :: HNil).shaped <> (
      {
        case x => RuleId(x(0), x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10), x(11), x(12), x(13), x(14), x(15),
          x(16), x(17), x(18), x(19), x(20), x(21), x(22), x(23), x(24), x(25), x(26), x(27))
      },
      {
        RuleId.unapply _
      }
    )
  def idx = index("rules_key", key)
}

case class EmailTemplateId(notifierId: Int, toMailIds: String, ccMailIds: String, bccMailIds: String,
  subject: String, body: String, emailFlag: Boolean, template_name: String, created_ts: Timestamp, key: String)
class EmailTemplate(tag: Tag) extends Table[EmailTemplateId](tag, "EMAIL_TEMPLATE") {
  def notifierId = column[Int]("TEMPLATE_ID", O.AutoInc, O.PrimaryKey)
  def toMailIds = column[String]("TO", O.DBType("VARCHAR(2048)"))
  def ccMailIds = column[String]("CC", O.DBType("VARCHAR(2048)"))
  def bccMailIds = column[String]("BCC", O.DBType("VARCHAR(2048)"))
  def subject = column[String]("SUBJECT", O.DBType("VARCHAR(2048)"))
  def body = column[String]("BODY", O.DBType("VARCHAR(2048)"))
  def emailFlag = column[Boolean]("ALERTPERROW") //Email Flag
  def template_name = column[String]("TEMPLATE_NAME", O.DBType("VARCHAR(2048)"))
  def created_ts = column[Timestamp]("CREATION_DATE")
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))

  def * = (notifierId, toMailIds, ccMailIds, bccMailIds, subject, body, emailFlag, template_name, created_ts, key) <> (EmailTemplateId.tupled, EmailTemplateId.unapply)
  def idx = index("email_notifier", notifierId)
}

class SystemRules(tag: Tag) extends Table[(String, String, Boolean, Int)](tag, "SYSTEM_RULES") {
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))
  def ruleName = column[String]("RULE_NAME", O.DBType("VARCHAR(2048)"))
  def enableAlert = column[Boolean]("ENABLE_ALERT")
  def notifierId = column[Int]("TEMPLATE_ID")
  def * = key ~ ruleName ~ enableAlert ~ notifierId
  def idx = index("system_key", key)
}

case class CategoryId(category_id: Int, category_name: String, ts: Timestamp, key: String, category_description: String)
class CategoryTable(tag: Tag) extends Table[CategoryId](tag, "CATEGORY") {
  def id = column[Int]("CATEGORY_ID", O.AutoInc, O.PrimaryKey)
  def name = column[String]("CATEGORY_NAME", O.DBType("VARCHAR(1024)"))
  def created_ts = column[Timestamp]("CREATION_DATE")
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))
  def desc = column[String]("CATEGORY_DESCRIPTION", O.DBType("VARCHAR(2048)"))
  def * = (id, name, created_ts, key, desc) <> (CategoryId.tupled, CategoryId.unapply)
  def idx = index("category_idx", id)
}

case class SeverityId(severity_id: Int, severity_name: String, ts: Timestamp, key: String, isEnabled: Boolean)
class SeverityTable(tag: Tag) extends Table[SeverityId](tag, "SEVERITY") {
  def id = column[Int]("SEVERITY_ID", O.PrimaryKey)
  def name = column[String]("SEVERITY_NAME", O.DBType("VARCHAR(2048)"))
  def created_ts = column[Timestamp]("CREATION_DATE")
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))
  def isEnabled = column[Boolean]("IS_ENABLED", O.DBType("VARCHAR(2048)"))
  def * = (id, name, created_ts, key, isEnabled) <> (SeverityId.tupled, SeverityId.unapply)
  def idx = index("severity_idx", id)
}

case class PriorityId(priority_id: Int, priority_name: String, ts: Timestamp, key: String, isEnabled: Boolean)
class PriorityTable(tag: Tag) extends Table[PriorityId](tag, "PRIORITY") {
  def id = column[Int]("PRIORITY_ID", O.PrimaryKey)
  def name = column[String]("PRIORITY_NAME", O.DBType("VARCHAR(2048)"))
  def created_ts = column[Timestamp]("CREATION_DATE")
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))
  def isEnabled = column[Boolean]("IS_ENABLED", O.DBType("VARCHAR(2048)"))
  def * = (id, name, created_ts, key, isEnabled) <> (PriorityId.tupled, PriorityId.unapply)
  def idx = index("priority_idx", id)
}

case class AlertId(alert_id: Int, alert_msg: String, recommendation: Option[String], key: String)
class AlertsTable(tag: Tag) extends Table[AlertId](tag, "ALERTS") {
  def alertId = column[Int]("ALERT_ID", O.AutoInc) // alertID ~ notifier_id(RulesTable)
  def alertMsg = column[String]("ALERT_MSG", O.DBType("VARCHAR(2048)"))
  def recommendation = column[Option[String]]("RECOMMENDATION", O.DBType("VARCHAR(2048)"), O.Nullable)
  def key = column[String]("KEY", O.DBType("VARCHAR(2048)"))
  def * = (alertId, alertMsg, recommendation, key) <> (AlertId.tupled, AlertId.unapply)
  def idx = index("alerts_id", alertId)
}

class LCPFailureTable(tag: Tag) extends Table[(Option[String], Option[Int], Option[String], Timestamp, String)](tag, "LCPFAILURES") {
  def key = column[Option[String]]("KEY", O.DBType("VARCHAR(2048)"))
  def loadId = column[Option[Int]]("LOAD_ID") // alertID ~ notifier_id(RulesTable)
  def nodeId = column[Option[String]]("NODE_ID", O.DBType("VARCHAR(128)")) // alertID ~ notifier_id(RulesTable)
  def ts = column[Timestamp]("TS")
  def failureMsg = column[String]("FAILURE_MSG", O.DBType("VARCHAR(2048)"))
  def * = key ~ loadId ~ nodeId ~ ts ~ failureMsg
}

class VaultConfigTable(tag: Tag) extends Table[(String, String, String, Boolean)](tag, "VAULTCONFIG") {
  def mps = column[String]("MPS", O.DBType("VARCHAR(100)"), O.PrimaryKey)
  def bucketOrPath = column[String]("BUCKET_OR_PATH", O.DBType("VARCHAR(200)"))
  def vaultType = column[String]("VAULT_TYPE", O.DBType("VARCHAR(20)"))
  def toupload = column[Boolean]("TO_UPLOAD")

  def * = mps ~ bucketOrPath ~ vaultType ~ toupload

  def idx1 = index("vaultconfig_mps", mps)
}
