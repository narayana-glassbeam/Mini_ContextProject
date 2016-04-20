package com.glassbeam.model

import java.io.File
import java.sql.Timestamp

import com.glassbeam.h2utils.LoaderTable
import com.glassbeam.model.LoaderDao._

import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.JdbcBackend.Database.dynamicSession
import scala.slick.util.TupleMethods._
import com.github.tototoshi.slick.H2JodaSupport._
import com.glassbeam.h2utils._
import com.glassbeam.model.AlertsFailure.AlertsFailure
import com.glassbeam.model.BundleSignatureState.BundleSignatureState
import com.glassbeam.model.CassandraFailure.CassandraFailure
import com.glassbeam.model.ContextFailure._
//import com.glassbeam.model.Counters._
import com.glassbeam.model.ParseFailure.ParseFailure
import com.glassbeam.model.ProcessingState.ProcessingState
import com.glassbeam.model.RulesFailure.RulesFailure
import com.glassbeam.model.SolrFailure.SolrFailure
import com.glassbeam.model.StartupConfig._
import org.joda.time.DateTime

object OmittedFilesDao extends DatabaseAccess {
  private val OmittedFiles = TableQuery[OmittedFiles]

  def DDL = OmittedFiles.ddl

  def getAllByLoadId(loadid: Long) = withDatabasePool withDynSession  {
    OmittedFiles.filter(o => o.node === node_id && o.load_id === loadid).run
  }

  def insertALL(omitBatch: Vector[(String, Long, String, String, String, DateTime)]) = withDatabasePool withDynSession  {
    OmittedFiles.insertAll(omitBatch: _*)
  }

  def updateLoadidByLoadId(from: Long, to: Long) = withDatabasePool withDynSession  {
    OmittedFiles.filter(o => o.node === node_id && o.load_id === from).map(x => x.load_id).update(to)
  }

  def getCountForLoadId(loadid: Long) = withDatabasePool withDynSession {
    OmittedFiles.filter(o => o.node === node_id && o.load_id === loadid).length.run
  }

  def getFilesForLoadId(loadid: Long) = withDatabasePool withDynSession  {
    OmittedFiles.filter(o => o.node === node_id && o.load_id === loadid).map(x => x.name).run
  }

  def deleteByLoadId(loadid: Long) = withDatabasePool withDynSession  {
    OmittedFiles.filter(o => o.node === node_id && o.load_id === loadid).delete
  }

  def deleteMonthOld() = withDatabasePool withDynSession {
    OmittedFiles.filter(_.ts < new DateTime().minusMonths(1)).delete
  }

  def deleteAll() = withDatabasePool withDynSession  {
    OmittedFiles.delete
  }
}


object CompressedFilesDao extends DatabaseAccess {
  private val CompressedFiles = TableQuery[CompressedFiles]

  def DDL = CompressedFiles.ddl

  def insertALL(compressedBatch: Vector[(String, Long, String, String, Long, String, Option[Int], Option[Timestamp], Option[Timestamp], DateTime)]) = withDatabasePool withDynSession  {
    CompressedFiles.insertAll(compressedBatch: _*)
  }

  def updateLoadidByLoadId(from: Long, to: Long) = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === from).map(x => x.load_id).update(to)
  }

  def getAllByLoadId(loadid: Long) = withDatabasePool withDynSession {
    CompressedFiles.filter(o => o.load_id === loadid && o.node === node_id).run
  }

  def getAllCompressedArchives(loadid: Long) = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.load_id === loadid && o.node === node_id).map(x => (x.name, x.ustate)).run
  }

  def getNameLoadIdSplEcByCompressionFails = withDatabasePool withDynSession  {
    CompressedFiles
      .filter(o => o.node === node_id && o.ustate.isEmpty && o.uend.isEmpty && o.ustart.isDefined)
      .map(o => (o.name, o.load_id, o.spl, o.ec)).run
  }

  def getNamesByLoadId(loadid: Long) = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === loadid).map(o => o.name).run
  }

  def getCountForLoadId(loadid: Long) = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === loadid).length.run
  }

  def getMinUncompressStartTime(loadid: Long): Long = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === loadid).map(x => x.ustart).min.run match {
      case None => 0
      case Some(m) => m.getTime
    }
  }

  def getMaxUncompressEndTime(loadid: Long): Long = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === loadid).map(x => x.uend).max.run match {
      case None => 0
      case Some(m) => m.getTime
    }
  }

  def deleteByLoadId(loadid: Long) = withDatabasePool withDynSession  {
    CompressedFiles.filter(o => o.node === node_id && o.load_id === loadid).delete
  }

  def deleteMonthOld() = withDatabasePool withDynSession  {
    CompressedFiles.filter(_.ts < new DateTime().minusMonths(1)).delete
  }

  def deleteAll() = withDatabasePool withDynSession  {
    CompressedFiles.delete
  }
}

object OpsDao extends DatabaseAccess with Logger {
  private final val logger = Logging(this)
  private val OpsTable = TableQuery[OpsTable]

  // DDL
  def DDL = OpsTable.ddl

  // Bulk Insert

  def insertALL(opsBatch: Vector[Ops]) = withDatabasePool withDynSession  {
    OpsTable.insertAll(opsBatch: _*) // bulk insert into OpsTable
  }

  // Updates

  def updateLoadidByLoadId(from: Long, to: Long) = withDatabasePool withDynSession  {
    OpsTable.filter(o => o.node === node_id && o.load_id === from).map(x => x.load_id).update(to)
  }

  def updateStateTypeByNameLoadId(name: String, loadid: Long)(state: Byte, ftype: Byte) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.name === name && o.load_id === loadid && o.node === node_id)
      .map(x => x.pstate ~ x.file_type)
      .update(Some(state), ftype)
  }

  def updateStateByNameLoadId(name: String, loadid: Long)(state: Byte) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.name === name && o.load_id === loadid && o.node === node_id)
      .map(x => x.pstate)
      .update(Some(state))
  }

  def updateStateVaultByNameLoadId(name: String, loadid: Long)(state: Byte, vault: Byte) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.name === name && o.load_id === loadid && o.node === node_id)
      .map(x => x.pstate ~ x.lvstatus)
      .update(Some(state), Some(vault))
  }

  def updateStateCompletedByNameLoadId(name: String, loadid: Long)(tostate: Byte, completed: Option[Timestamp]) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.completed ~ x.pstate)
      .update(completed, Some(tostate))
  }

  def updateStateByState(from: Byte)(to: Byte) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.pstate === from && o.node === node_id)
      .map(x => x.pstate)
      .update(Some(to))
  }

  def updateStateByStateLoadId(from: Byte, loadid: Long)(to: Byte) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.load_id === loadid && o.pstate === from && o.node === node_id)
      .map(x => x.pstate)
      .update(Some(to))
  }

  def updateParseError(name: String, loadid: Long, state: ParseFailure) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.load_id === loadid && o.name === name)
      .map(x => x.pfailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
  }

  def updateCassandraError(name: String, loadid: Long, state: CassandraFailure) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.cafailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
  }

  def updateCassandraError(loadid: Long, state: CassandraFailure, error: String) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.load_id === loadid)
      .map(x => x.cafailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
    getNamesByLoadId(loadid).map(f => appendError(f, loadid)(error))
  }

  def updateSolrError(name: String, loadid: Long, state: SolrFailure) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.sfailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
  }

  def updateRulesError(name: String, loadid: Long, state: RulesFailure) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.rfailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
  }

  def updateRulesError(loadid: Long, state: RulesFailure, error: String) = withDatabasePool withDynSession  {
    OpsTable
      .filter(o => o.node === node_id && o.load_id === loadid)
      .map(x => x.rfailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
    getNamesByLoadId(loadid).map(f => appendError(f, loadid)(error))
  }

  def updateAlertsError(name: String, loadid: Long, state: AlertsFailure) = withDatabasePool  withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.afailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
  }

  def updateAlertsError(loadid: Long, state: AlertsFailure, error: String) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.load_id === loadid)
      .map(x => x.afailure ~ x.pstate)
      .update(Some(state.id.toByte), Some(ProcessingState.Partial.id.toByte))
    getNamesByLoadId(loadid).map(f => appendError(f, loadid)(error))
  }

  private def truncate_notes(notes: String) = notes.substring(0, Math.min(notes.length(), 4096))

  def updateErrorByLoadId(loadId: Long, err: String) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.load_id === loadId)
      .map(x => x.notes ~ x.pstate)
      .update(Some(truncate_notes(err)), Some(ProcessingState.Failed.id.toByte))
  }

  def updateErrorStateByNameLoadId(name: String, loadid: Long)(error: String, state: Byte, cxtStr: String, contextFailure: ContextFailure) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.pstate ~ x.context ~ x.cofailure)
      .update(Some(state), Some(cxtStr), Some(contextFailure.id.toByte))
    appendError(name, loadid)(error)
  }

  def appendError(name: String, loadid: Long)(error: String): Int = withDatabasePool withDynSession {
    val fstmt = OpsTable.filter(o => o.node === node_id && o.name === name && o.load_id === loadid).map(x => x.notes)
    val existing: Option[String] = fstmt.firstOption.flatten
    existing match {
      case None =>
        fstmt.update(Some(truncate_notes(error)))
      case Some(e) =>
        if(e.length < 4000) {
          val updated_err = e + "\n" + error
          fstmt.update(Some(truncate_notes(updated_err)))
        }
    }
    fstmt.run.length
  }

  def updateEcSysidObstsByNameLoadId(name: String, loadid: Long)(customer: String, sysid: String, obsts: Timestamp) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.ec ~ x.system ~ x.obsts)
      .update(customer, Some(sysid), obsts)
  }

  def updateStartedContextByNameLoadId(name: String, loadid: Long)(started: Option[Timestamp], cxtStr: String, splinstance: String) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.started ~ x.context ~ x.parser)
      .update(started, Some(cxtStr), Some(splinstance))
  }

  def updateLogVaultStateByNameLoadId(name: String, loadid: Long)(state: Byte) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === node_id && o.name === name && o.load_id === loadid)
      .map(x => x.lvstatus)
      .update(Some(state))
  }

  def updateLineCount(name: String, loadid: Long)(linecount: Long) = withDatabasePool withDynSession {
    OpsTable
      .filter(o => o.node === StartupConfig.node_id && o.name === name && o.load_id === loadid)
      .map(x => x.linecount)
      .update(Some(linecount))
  }

  // Fetch

  def getAllByLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadid).run
  }

  def getCountForLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadid).length.run
  }

  def getNameByLoadFileType(loadid: Long, fileType: Byte) = withDatabasePool withDynSession {
    OpsTable.filter(l => l.load_id === loadid && l.node === node_id && l.file_type === fileType).map(o => o.name).run
  }

  def getNameByLoadState(loadid: Long, state: Byte) = withDatabasePool withDynSession  {
    OpsTable.filter(l => l.load_id === loadid && l.node === node_id && l.pstate === state).map(o => o.name).run
  }

  def getByLoadState(loadid: Long, state: Byte) = withDatabasePool withDynSession  {
    OpsTable.filter(l => l.load_id === loadid && l.node === node_id && l.pstate === state).length.run
  }

  def getNameLoadIdSystemEcObstsState = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id).map(o => (o.name, o.load_id, o.system, o.ec, o.obsts, o.file_type)).run
  }

  def getNamesByLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadid).map(o => o.name).run
  }

  def getFileCountByLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadid &&
      (o.file_type === FileType.File.id.toByte || o.file_type === FileType.Vault_File.id.toByte || o.file_type === FileType.Reverse.id.toByte ||
        o.file_type === FileType.Binary.id.toByte || o.file_type === FileType.Extensibility.id.toByte)).length.run
  }

  def getCustomerByLoadIdName(loadId: Long, name: String) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadId && o.name === name).map(_.ec).first
  }

  def getNameLoadIdSplBySplStateSeen(spl: String, noOfFiles: Int, loadPriorityFlag: Boolean, grtLoadid: Long) = withDatabasePool withDynSession {
    val query1 = OpsTable.filter(o => (o.pstate === ProcessingState.Seen.id.toByte && o.node === node_id && o.load_id >= grtLoadid))
      .map(o => (o.name, o.load_id, o.spl))
    val query2 = if (spl.nonEmpty) query1.filter(o => o._3 === spl) else query1
    val sort1 = if (loadPriorityFlag) query2.sortBy(_._2.asc.nullsFirst) else query2
    val query3 = sort1.take(noOfFiles)
    query3.run
  }

  def getCountByLoadIdNameState(name: String, loadid: Long, state: Byte) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.name === name && o.load_id === loadid && o.pstate === state).length.run
  }

  def getStateByLoadIdName(name: String, loadid: Long): ProcessingState = withDatabasePool withDynSession {
    val x = OpsTable.filter(o => o.node === node_id && o.name === name && o.load_id === loadid).map(_.pstate).firstOption.flatten
    x match {
      case None    => ProcessingState.Failed
      case Some(y) => ProcessingState.apply(y)
    }
  }

  def getFileForBundleGrepByLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o =>
      o.load_id === loadid &&
        (o.file_type === FileType.File.id.toByte || o.file_type === FileType.Vault_File.id.toByte || o.file_type === FileType.Extensibility.id.toByte || o.file_type === FileType.Reverse.id.toByte) &&
        o.node === node_id).map(_.name).run
  }

  def getTypeSeenSplTsByLoadIdName(name: String, loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name === name && o.load_id === loadid && o.node === node_id).map(o => (o.file_type, o.seen, o.spl, o.ts, o.mime)).first
  }

  def getLoadIdByName(name: String) = withDatabasePool withDynSession {
    OpsTable.filter(l => l.name === name && l.node === node_id).map(o => o.load_id).firstOption
  }

  def getSysidByLoadIdName(loadid: Long, name: String) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name === name && o.load_id === loadid && o.node === node_id).map(_.system).firstOption
  }

  def getEcObstsSysidByLoadIdName(loadid: Long, name: String) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name === name && o.load_id === loadid && o.node === node_id).map(o => (o.ec, o.obsts, o.system)).firstOption
  }

  def getTypeByNameLoadId(loadid: Long, name: String) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name === name && o.load_id === loadid && o.node === node_id).map(x => x.file_type).first
  }

  def getFilesOfTypeInBundle(loadid: Long, ftype: Byte) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.load_id === loadid && o.node === node_id && o.file_type === ftype).map(x => x.name).run
  }

  def getParseSize(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.load_id === loadid && o.node === node_id &&
      (o.file_type === FileType.File.id.toByte || o.file_type === FileType.Extensibility.id.toByte || o.file_type === FileType.Reverse.id.toByte)).map(x => x.size).sum.run
  }

  def getSkipSize(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.load_id === loadid && o.node === node_id &&
      (o.file_type === FileType.Vault_File.id.toByte || o.file_type === FileType.Binary.id.toByte)).map(x => x.size).sum.run
  }

  // Deletes

  def deleteSizeZero(ftype: Byte) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.size === 0L && o.file_type === ftype).delete
  }

  def deleteByNameLoadId(name: String, loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name === name && o.node === node_id && o.load_id === loadid).delete
  }

  def deleteByNameButLoadId(name: String, loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.name =!= name && o.node === node_id && o.load_id === loadid).delete
  }

  def deleteByLoadId(loadid: Long) = withDatabasePool withDynSession {
    OpsTable.filter(o => o.node === node_id && o.load_id === loadid).delete
  }

  def deleteAll() = withDatabasePool withDynSession {
    OpsTable.delete
  }
}

object LoadIdDao extends DatabaseAccess with Logger {
  private final val logger = Logging(this)
  private val LoadIds = TableQuery[LoadIds]

  // DDL

  def DDL = LoadIds.ddl

  // INSERT

  def insert(mps: String, bundleType: Byte, bundle: String, bCompSize: Long, properties: Option[String],
    completeInOps: Boolean, seenTime: Timestamp, completeInOpsTime: Option[Timestamp], state: Byte, vaultStatus: Byte): Long = withDatabasePool withDynSession  {

    val l = LoadId(0, node_id, mps, bundleType, bundle.toString, bCompSize, None, None, None, None, None, properties, completeInOps, false, seenTime,
      completeInOpsTime, None, state, vaultStatus, None)
    (LoadIds returning LoadIds.map(_.load_id) += l)
  }

  // Update
  def updateLoadidByLoadId(from: Long, to: Long) = withDatabasePool withDynSession {
    LoadIds.filter(l => l.load_id === from && l.node === node_id).map(x => x.load_id).update(to)
  }

  def updateBundleToParsing(loadId: Long) = withDatabasePool withDynSession {
    LoadIds.filter(l => l.load_id === loadId && l.node === node_id).map(x => x.bState).update(BundleState.Parsing.id.toByte)
  }

  def updateBundleState(loadid: Long, timestamp: Timestamp, state: Byte) = withDatabasePool withDynSession {
    LoadIds
      .filter(l => l.load_id === loadid && l.node === node_id).map(m => m.pComplete ~ m.pCompTime ~ m.bState)
      .update((true, Some(timestamp), state))
  }

  def updateBundleCompleteInOpsTrue(loadid: Long, timestamp: Timestamp) = withDatabasePool withDynSession {
    val uncompressedParseSize = OpsDao.getParseSize(loadid)
    val uncompressedSkipSize = OpsDao.getSkipSize(loadid)
    LoadIds
      .filter(l => l.load_id === loadid && l.node === node_id)
      .map(m => m.compInOps ~ m.cInOpsTime ~ m.pxSize ~ m.skipSize)
      .update((true, Some(timestamp), uncompressedParseSize, uncompressedSkipSize))
  }

  def updatePropCountName(loadid: Long)(properties: Option[String], counts: (Long, Long, Long), bundleName: String) = withDatabasePool withDynSession {
    val parseCount = counts._1; val delCount = counts._2; val skipCount = counts._3
    val rxCount = parseCount + delCount + skipCount
    LoadIds
      .filter(o => o.load_id === loadid && o.node === node_id).map(x => x.properties ~ x.rxCount ~ x.pxCount ~ x.skipCount ~ x.bundleName)
      .update((properties, Some(rxCount), Some(parseCount), Some(skipCount), bundleName))
  }

  def updatePropCountSize(loadid: Long)(properties: Option[String], counts: (Long, Long, Long), size: Long) = withDatabasePool withDynSession {
    val parseCount = counts._1; val delCount = counts._2; val skipCount = counts._3
    val rxCount = parseCount + delCount + skipCount
    LoadIds
      .filter(o => o.load_id === loadid && o.node === node_id).map(x => x.properties ~ x.rxCount ~ x.pxCount ~ x.skipCount ~ x.rxSize)
      .update((properties, Some(rxCount), Some(parseCount), Some(skipCount), size))
  }

  def updateName(loadid: Long)(bundleName: String) = withDatabasePool withDynSession {
    LoadIds
      .filter(o => o.load_id === loadid && o.node === node_id).map(x => x.bundleName)
      .update((bundleName))
  }

  def updateCountSize(loadid: Long)(rxCount: Option[Long], pxCount: Option[Long], rxSize: Long, pxSize: Option[Long]) = withDatabasePool withDynSession {
    LoadIds
      .filter(o => o.load_id === loadid && o.node === node_id).map(x => x.rxCount ~ x.pxCount ~ x.rxSize ~ x.pxSize)
      .update((rxCount, pxCount, rxSize, pxSize))
  }

  def updateBundleProperties(loadid: Long)(bp: String) = withDatabasePool withDynSession {
    LoadIds
      .filter(o => o.load_id === loadid && o.node === node_id).map(x => x.properties)
      .update(Some(bp))
  }

  def updateVaultStatus(loadId: Long, vaultStatus: Byte) = withDatabasePool withDynSession {
    LoadIds.filter(l => l.load_id === loadId && l.node === node_id).map(x => x.vaultStatus).update(vaultStatus)
  }

  // Fetch

  def getAllByLoadId(loadId: Long) = withDatabasePool withDynSession {
    LoadIds.filter(o => o.load_id === loadId).run
  }

  def getBundleState(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(o => o.load_id === loadid && o.node === node_id).map(x => x.bState).firstOption match {
      case Some(x) => BundleState.apply(x)
      case None => BundleState.Failed
    }
  }

  def getFileCountByLoadId(loadid: Long) = withDatabasePool withDynSession {
    val pxCount = LoadIds.filter(o => o.load_id === loadid && o.node === node_id).map(o => o.pxCount).first
    val skipCount = LoadIds.filter(o => o.load_id === loadid && o.node === node_id).map(o => o.skipCount).first
    (pxCount, skipCount) match {
      case (Some(px), Some(skip)) => Some(px + skip)
      case (Some(px), None) => Some(px)
      case (None, Some(skip)) => Some(skip)
      case _ => None
    }
  }

  def getFileCountSizeByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(o => o.load_id === loadid && o.node === node_id).map(o => (o.rxCount ~ o.pxCount ~ o.rxSize ~ o.pxSize)).first
  }

  def getPxCountByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(o => o.load_id === loadid && o.node === node_id).map(o => o.pxCount).first
  }

  def getPropTypeCompInOps(loadid: Long): Option[BundleDetails] = withDatabasePool withDynSession {
    val obd =
      LoadIds
        .filter(x => x.load_id === loadid && x.node === node_id)
        .map(o => (o.properties, o.bundleType, o.compInOps, o.bundleName, o.bState, o.rxSize)).firstOption
    obd match {
      case None =>
        None
      case Some(bd) =>
        Some(BundleDetails(loadid, bd._1, null, BundleType.apply(bd._2), bd._3, bd._4, new File(bd._4).length()))
    }
  }

  def getNameByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id).map(o => o.bundleName).firstOption
  }

  def getNameSizeByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id).map(o => (o.bundleName, o.rxSize)).firstOption
  }

  def getDistinctMpsByParseState() = withDatabasePool withDynSession {
    LoadIds.filter(x => x.node === node_id && x.pComplete === false).map(_.mps).run.distinct
  }

  def checkLoadIdExists(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id).map(o => o.load_id).firstOption match {
      case None => false
      case Some(l) => true
    }
  }

  def getSeenByLoadId(loadid: Long): Long = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id).map(o => o.seenTime).firstOption match {
      case None => System.currentTimeMillis()
      case Some(z) => z.getTime
    }
  }

  def getVaultStatusByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id).map(o => o.vaultStatus).firstOption
  }

  def checkRemoteVaultUploadComplete(loadid: Long, vaultStatus: Byte) = withDatabasePool withDynSession {
    LoadIds.filter(x => x.load_id === loadid && x.node === node_id && x.vaultStatus === vaultStatus).map(o => o.vaultPath).firstOption
  }

  def getLoadIdNameStateVaultPathByArchived(vaultStatus: Byte) = withDatabasePool withDynSession {
    // Add node
    LoadIds
      .filter(o => o.node === node_id && o.vaultStatus === vaultStatus && o.vaultPath.isDefined)
      .map(x => (x.load_id, x.bundleName, x.bState, x.vaultPath)).run
  }

  // Delete

  def deleteByLoadId(loadid: Long) = withDatabasePool withDynSession {
    LoadIds.filter(o => o.load_id === loadid).delete
  }

  def deleteStreamBundles() = withDatabasePool withDynSession {
    LoadIds.filter(o => o.bundleType === BundleType.StreamFile.id.toByte).delete
  }

  def deleteAll() = withDatabasePool withDynSession {
    LoadIds.delete
  }
}

object BundleSignatureDao extends DatabaseAccess {
  private val BundleSignature = TableQuery[BundleSignature]

  def DDL = BundleSignature.ddl

  def insert(id: Long, emps: String, signature: String, obs_ts: Timestamp): Timestamp = withDatabasePool  withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    // the delete old bundle signatures query runs every hour and can be quite heavy - so, rounding of timestamps at day boundary
    // and having an index on timestamps - this is to make the delete query fast on super large number of rows (>1 Million, can be easily expected)
    val parse_start = new DateTime()
    val ts = parse_start.withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)
    BundleSignature.insert(id, ts, obs_ts, parse_start, mps, signature, BundleSignatureState.New.id.toByte, ec)
    new Timestamp(parse_start.getMillis())
  }

  def deleteThreeMonthOld() = withDatabasePool withDynSession {
    BundleSignature.filter(_.ts < new DateTime().minusMonths(3)).delete
  }

  def getSignForLoadId(loadId: Long, emps: String): Option[String] = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    BundleSignature.filter(o => o.load_id === loadId && o.mps === mps && o.ec === ec).map(x => x.signature).firstOption
  }

  def getSignForLoadIdMPS(loadId: Long, mps: String): Option[String] = withDatabasePool withDynSession {
    // This query ignores the customer field. Needed when only mps is available
    BundleSignature.filter(o => o.load_id === loadId && o.mps === mps).map(x => x.signature).firstOption
  }

  def getLoadIdForSign(signature: String, emps: String) = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    BundleSignature.filter(o => o.signature === signature && o.mps === mps && o.ec === ec).map(x => x.load_id).firstOption
  }

  private def getMPS_Ec(emps: String) : (String, String)= (emps.split("""/""").tail.mkString("/"), emps.split("""/""").head)

  def getStateForBundle(loadid: Long, bundleid: String, emps: String): BundleSignatureState = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    val x = BundleSignature.filter(o => o.load_id === loadid && o.mps === mps && o.ec === ec && o.signature === bundleid).map(x => x.bSigState).first
    BundleSignatureState.apply(x)
  }

  def getParseStart(loadid: Long, emps: String): Option[Timestamp] = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    val x = BundleSignature.filter(o => o.load_id === loadid && o.mps === mps && o.ec === ec).map(x => x.parse_start).firstOption
    x match {
      case None => None
      case Some(y) => Some(new Timestamp(y.getMillis))
    }
  }

  def updateParseStart(loadid: Long, emps: String, parse_start: DateTime) = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    BundleSignature.filter(o => o.load_id === loadid && o.mps === mps && o.ec === ec).map(x => x.parse_start).update(parse_start)
  }

  def setBundleState(loadId: Long, emps: String, state: BundleSignatureState) = withDatabasePool withDynSession {
    val (mps, ec) = getMPS_Ec(emps)
    BundleSignature.filter(o => o.load_id === loadId && o.mps === mps && o.ec === ec).map(x => x.bSigState).update(state.id.toByte)
  }

  def deleteAll() = withDatabasePool withDynSession {
    BundleSignature.delete
  }
}

object LcpStartStopDao extends DatabaseAccess {
  private val LcpStartStop = TableQuery[LcpStartStop]

  def DDL = LcpStartStop.ddl

  def insert(): Long = withDatabasePool withDynSession {
    val ts = new Timestamp(System.currentTimeMillis())
    val l = LcpSS(0, node_id, ts, ts)
    (LcpStartStop returning LcpStartStop.map(_.runid) += l)
  }

  def updateHeartbeat(runid: Long) = withDatabasePool withDynSession {
    LcpStartStop.filter(l => l.runid === runid).map(m => m.heartbeat_ts).update(new Timestamp(System.currentTimeMillis()))
  }
}

object SplTableDao extends DatabaseAccess {
  private val SplTable = TableQuery[SplTable]

  def DDL = SplTable.ddl

  def insert(name: String, ts: Timestamp, cksum: String, status: String, hold: String) = withDatabasePool withDynSession {
    SplTable.insert(name, ts, cksum, status, hold)
  }

  def getSplForName(name: String) = withDatabasePool withDynSession {
    SplTable.filter(s => s.name === name).map(s => s.hold ~ s.chksum).list
  }

  def updateStateForNameCksum(name: String, cksum: String) = withDatabasePool withDynSession {
    SplTable.filter(s => s.name === name && s.chksum === cksum).map(_.status).update("Compiled")
  }
}

object ContextTableDao extends DatabaseAccess {
  private val ContextTable = TableQuery[ContextTable]

  def DDL = ContextTable.ddl

  def getContextForKey(key: String):Option[(String,String,Timestamp)] = withDatabasePool withDynSession {
    ContextTable.filter(c => c.key === key).map(c => (c.mutable_value,c.immutable_value,c.LastModified)).run.headOption
  }

  def getContextForKeyTs(key: String) = withDatabasePool withDynSession {
    val mpscontext = ContextTable.filter(c => c.key === key).map(c => (c.value,c.LastModified)).run.headOption
    mpscontext
  }

  def getAllKeys(): Seq[String] = withDatabasePool withDynSession {
    ContextTable.map(c => c.key).run.filterNot(key => key.trim.equals("loader"))
  }

  def getModifiedContext(key:String,ts:Timestamp) = withDatabasePool withDynSession {
    ContextTable.filter(c => c.key === key && c.LastModified > ts).map(c => (c.value,c.LastModified)).run.headOption
  }

  def deleteAll() = withDatabasePool withDynSession {
    ContextTable.delete
  }
}

object ValidateTableDao extends DatabaseAccess {
  private val ValidateTable = TableQuery[ValidateTable]

  def DDL = ValidateTable.ddl

  def getValuesForKey(key: String, emps: String) = withDatabasePool withDynSession {
    ValidateTable.filter(v => v.key === key && v.emps === emps).map(_.value).run
  }
}

object LookupTableDao extends DatabaseAccess  {
  private val LookupTable = TableQuery[LookupTable]

  def DDL = LookupTable.ddl

  def getFirstValueForKey(key: String, emps: String) = withDatabasePool withDynSession {
    val lookupval=LookupTable.filter(v => v.key === key && v.emps === emps).map(_.value).firstOption
    lookupval
  }
}

case class LookupKey(key: String, pos: Int)

object ColLookupTableDao extends DatabaseAccess {
  private val ColLookup = TableQuery[ColLookup]
  
  def DDL = ColLookup.ddl

  def loadLookupValue: Map[String, Map[LookupKey, String]] =
    withDatabasePool withDynSession {
      ColLookup.list.groupBy(_._2).map(x => x._1 -> x._2.map(e => LookupKey(e._4, e._3) -> e._5).toMap)
    }
}

object LogSignatureDao extends DatabaseAccess {
  private val LogSignature = TableQuery[LogSignature]

  def DDL = LogSignature.ddl

  def insert(key: String, loadid: Long) = withDatabasePool withDynSession {
    // the delete old log signatures query runs every hour and can be quite heavy (in case of large ## of rows)- so, rounding
    // of timestamps at day boundary and having an index on timestamps - this is to make the delete query fast on super large number of rows
    val ts = new DateTime().withHourOfDay(0).withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0)
    LogSignature.insert(key, ts, loadid)
  }

  def getCountForLoadId(loadid: Long) = withDatabasePool withDynSession {
    LogSignature.filter(o => o.load_id === loadid).length.run
  }

  def getCountForKey(key: String) = withDatabasePool withDynSession {
    LogSignature.filter(o => o.key === key).length.run
  }

  def deleteMonthOld() = withDatabasePool withDynSession {
    LogSignature.filter(_.ts < new DateTime().minusMonths(1)).delete
  }

  def deleteAllForLoadId(loadid: Long) = withDatabasePool withDynSession {
    LogSignature.filter(x => x.load_id === loadid).delete
  }

  def updateLoadidByLoadId(from: Long, to: Long) = withDatabasePool withDynSession {
    LogSignature.filter(x => x.load_id === from).map(x => x.load_id).update(to)
  }
}

object LoaderDao extends DatabaseAccess {
  private val LoaderTable = TableQuery[LoaderTable]

  def DDL = LoaderTable.ddl

  def getValueForKey(key: String) = withDatabasePool withDynSession {
    LoaderTable.filter(c => c.key === key).map(c => c.value).run
  }

  def insert(key: String, value: String) = withDatabasePool withDynSession {
    LoaderTable.insert(key, value)
  }
}

object OpsSchemaDao extends DatabaseAccess {
  private val OpsSchema = TableQuery[OpsSchema]

  def DDL = OpsSchema.ddl

  def insert(version: String) = withDatabasePool withDynSession  {
    OpsSchema.insert(version)
  }

  def getVersion = withDatabasePool withDynSession  {
    OpsSchema.map(o => o.version).first
  }
}

object ProvisionDao extends DatabaseAccess {
  private val Provision = TableQuery[Provision]

  def DDL = Provision.ddl
}

object RulesDao extends DatabaseAccess {
  private val RulesTable = TableQuery[RulesTable]

  private val enabledRules = "ENABLED"
  def DDL = RulesTable.ddl

  def getRulesForKey(cmps: String) = withDatabasePool withDynSession  {
    RulesTable.filter(r => r.key === cmps).run
  }
  def getEnabledRulesForKey(cmps: String): Seq[RuleId] = withDatabasePool withDynSession  {
    //RulesTable.filter(r => r.key === cmps && r.status === true).map(s => s.rule_id ~ s.key ~ s.notifier_id ~ s.label ~ s.desc ~ s.author ~ s.created_ts ~ s.modified_ts ~ s.category_id ~
    //  s.severity ~ s.priority ~ s.kb_link ~ s.status ~ s.logic ~ s.notes ~ s.rule_name ~ s.colS ~ s.col0 ~ s.col1 ~ s.col2 ~ s.col3 ~ s.colT ).run
    val x = RulesTable.filter(r => r.key === cmps && r.status === enabledRules).run
    x.map(x => RuleId(x.rule_id, x.key, x.rule_name, x.label, x.description, x.author, x.created_ts, x.modified_ts, x.status, x.kb_link,
      x.is_deleted, x.notes, x.supported, x.template_id, x.notifier_id, x.category_id, x.severity, x.priority,
      x.colS, x.colT, x.col0, x.col1, x.col2, x.col3, x.logic_actual, x.logic_display, x.text_display, x.max_limit))
  }

  def getRowCount() = withDatabasePool withDynSession  {
    RulesTable.length.run
  }

  def getLatestRuleId() : Option[Int] = withDatabasePool withDynSession  {
    val x: Seq[Long] = RulesTable.map(s => s.rule_id).run
    x.isEmpty match{
      case true => Some(0)
      case _ => Some(x.max.toInt)
    }
  }

  def getRuleIdForKey(cmps: String, id: Long) : Option[RuleId] = withDatabasePool withDynSession  {
    //val x = RulesTable.filter(r => r.key === cmps && r.rule_id === id).map(s => s.rule_id ~ s.key ~ s.notifier_id ~ s.label ~ s.desc ~ s.author ~ s.created_ts ~ s.modified_ts ~ s.category_id ~
    //  s.severity ~ s.priority ~ s.kb_link ~ s.status ~ s.logic ~ s.notes ~ s.rule_name ~ s.colS ~ s.col0 ~ s.col1 ~ s.col2 ~ s.col3 ~ s.colT).run.head
    //Some(RuleId(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13, x._14, x._15, x._16, x._17, x._18, x._19, x._20, x._21, x._22))
    val x = RulesTable.filter(r => r.key === cmps && r.rule_id === id).run.headOption
    x match{
      case Some(x) =>
        Some(RuleId(x.rule_id, x.key, x.rule_name, x.label, x.description, x.author, x.created_ts, x.modified_ts, x.status, x.kb_link,
          x.is_deleted, x.notes, x.supported, x.template_id, x.notifier_id, x.category_id, x.severity, x.priority,
          x.colS, x.colT, x.col0, x.col1, x.col2, x.col3, x.logic_actual, x.logic_display, x.text_display,  x.max_limit))
      case None => None
    }
  }
}

/*  case class RuleId(rule_id: Long, key: String, rule_name: String, label: String, desc: String, author: String,
                    created_ts: Timestamp, modified_ts: Timestamp,  status: Boolean, kb_link: String,
                    is_deleted: Boolean, notes: String, supported: Boolean,
                    template_id: Int, notifier_id: Int, category_id: Int, severity: Int, priority: Int,
                    colS: String,colT: String, col0: String, col1: Option[String], col2: String, col3: String, logic: String)
*/

object EmailTemplateDao extends DatabaseAccess {
  private val EmailTemplate = TableQuery[EmailTemplate]

  def DDL = EmailTemplate.ddl

  def getTemplateRowForKey(key: Int) = withDatabasePool withDynSession  {
    EmailTemplate
      .filter(c => c.notifierId === key)
      .map(c => (key, c.toMailIds, c.ccMailIds, c.bccMailIds, c.subject, c.body, c.emailFlag, c.template_name, c.created_ts, c.key)).firstOption
  }

  def insert(notifierId: Int, toMailIds: String, ccMailIds: String, bccMailIds: String, subject: String, body: String,
    emailFlag: Boolean, template_name: String, created_ts: Timestamp, key: String) =
    withDatabasePool withDynSession  {CategoryId
      val e = EmailTemplateId(notifierId, toMailIds, ccMailIds, bccMailIds, subject, body, emailFlag, template_name, created_ts, key)
      EmailTemplate.insert(e)
    }

  def findTemplate(notifierId:Int) = {
    withDatabasePool withDynSession  {
      EmailTemplate.filter(x => x.notifierId === notifierId).first
    }
  }
}

object SystemRulesDao extends DatabaseAccess {
  private val SystemRules = TableQuery[SystemRules]

  def DDL = SystemRules.ddl

  def getTemplateIDForKey(key: String) = withDatabasePool withDynSession {
    SystemRules.filter(c => c.key === key).map(c => (c.notifierId)).run
  }

  def getTemplateRowsForKey(key: String) = withDatabasePool withDynSession  {
    SystemRules.filter(c => c.key === key && c.enableAlert).map(c => (c.ruleName, c.notifierId)).run
  }

  def insert(key: String, ruleName: String, enableAlert: Boolean, templateId: Int) = withDatabasePool withDynSession  {
    SystemRules.insert(key, ruleName, enableAlert, templateId)
  }
}

object CategoryTableDao extends DatabaseAccess {
  private val CategoryTable = TableQuery[CategoryTable]

  def DDL = CategoryTable.ddl

  def getCategoryRowForId(key: Int) = withDatabasePool withDynSession  {
    CategoryTable.filter(c => c.id === key).map(c => (c.id, c.name, c.created_ts, c.key, c.desc)).firstOption
  }

  def getCategoryRowForKey(key: String) = withDatabasePool withDynSession  {
    CategoryTable.filter(c => c.key === key).map(c => (c.id, c.name, c.created_ts, c.key, c.desc)).run
  }
}

object SeverityTableDao extends DatabaseAccess {
  private val SeverityTable = TableQuery[SeverityTable]

  def DDL = SeverityTable.ddl

  def getSeverityRowForId(key: Int) = withDatabasePool withDynSession  {
    SeverityTable.filter(c => c.id === key).map(c => (c.id, c.name, c.created_ts, c.key, c.isEnabled)).firstOption
  }

  def getSeverityRowForKey(key: String) = withDatabasePool withDynSession  {
    SeverityTable.filter(c => c.key === key).map(c => (c.id, c.name, c.created_ts, c.key, c.isEnabled)).run
  }
}

object PriorityTableDao extends DatabaseAccess {
  private val PriorityTable = TableQuery[PriorityTable]

  def DDL = PriorityTable.ddl

  def getPriorityRowForId(key: Int) = withDatabasePool withDynSession  {
    PriorityTable.filter(c => c.id === key).map(c => (c.id, c.name, c.created_ts, c.key, c.isEnabled)).firstOption
  }

  def getPriorityRowForKey(key: String) = withDatabasePool withDynSession  {
    PriorityTable.filter(c => c.key === key).map(c => (c.id, c.name, c.created_ts, c.key, c.isEnabled)).run
  }
}

object AlertsTableDao extends DatabaseAccess {
  private val AlertsTable = TableQuery[AlertsTable]

  def DDL = AlertsTable.ddl

  def getAlertRowForKey(key: Int) = withDatabasePool withDynSession {
    AlertsTable.filter(c => c.alertId === key).map(c => (c.alertId, c.alertMsg, c.recommendation, c.key)).first
  }
}

//All failure messages should be written here!
object LCPFailureTableDao extends DatabaseAccess {
  private val LCPFailureTable = TableQuery[LCPFailureTable]

  def DDL = LCPFailureTable.ddl

  def insert(key: Option[String], loadId: Option[Int], nodeId: Option[String], ts: Timestamp, failureMsg: String) =
    withDatabasePool withDynSession  {
      LCPFailureTable.insert(key, loadId, nodeId, ts, failureMsg)
    }
}

object VaultConfigDao extends DatabaseAccess {
  private val VaultConfig = TableQuery[VaultConfigTable]

  def DDL = VaultConfig.ddl

  def getStatusForMps(mps: String) = withDatabasePool withDynSession {
    VaultConfig.filter(c => c.mps === mps).map(c => c.toupload).firstOption
  }
}

