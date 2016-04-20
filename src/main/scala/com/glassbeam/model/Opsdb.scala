package com.glassbeam.model

import java.io.File

import com.glassbeam.model.H2Templates.{EmailTemplateDefaults, SystemRuleDefaults}
import com.glassbeam.model.StartupConfig._

import scala.slick.driver.H2Driver.simple._
import scala.slick.jdbc.JdbcBackend.Database.dynamicSession

object Opsdb extends DatabaseAccess with Logger {
  private final val logger = Logging(getClass())
  private val SchemaVersion: String = "143"

  def init() = {
    logger.info(s"opsdb: mode =${opsdb.split(':')(2)}, file = ${opsdb.split(':').takeRight(1)(0)}.h2.db")

    // If ServerMode (tcp), the DB might not be local -- don't test for a file
    val mode = opsdb.split(':')(2)
    if (mode == "tcp" || mode == "mem") {
      startupInserts()
    } else if (!new File(opsdb.split(':').takeRight(1)(0) + ".h2.db").exists) {
      logger.info(s"Creating local OpsDB $opsdb schema")
      startupInserts()
    } else {
      checkVersion()
    }

    def createSchema() = withDatabasePool withDynSession {
      // Build schema of all tables combined
      val schema =
        OpsDao.DDL             ++ ContextTableDao.DDL  ++ SplTableDao.DDL        ++ LoadIdDao.DDL          ++
        OpsSchemaDao.DDL       ++ ValidateTableDao.DDL ++ LookupTableDao.DDL     ++ OmittedFilesDao.DDL    ++
        LogSignatureDao.DDL    ++ ProvisionDao.DDL     ++ RulesDao.DDL           ++ CompressedFilesDao.DDL ++
        EmailTemplateDao.DDL   ++ SystemRulesDao.DDL   ++ AlertsTableDao.DDL     ++ LoaderDao.DDL          ++
        LCPFailureTableDao.DDL ++ LcpStartStopDao.DDL  ++ BundleSignatureDao.DDL ++ CategoryTableDao.DDL   ++
        SeverityTableDao.DDL   ++ PriorityTableDao.DDL ++ ColLookupTableDao.DDL ++ VaultConfigDao.DDL
      // Just try to cram in the DDL (non-destructive)
      try {
        schema.create
        OpsSchemaDao.insert(SchemaVersion) // if it didn't exist (throw) then create the schema
      } catch {
        case e: Exception =>
          if (e.getMessage.contains("""org.h2.jdbc.JdbcSQLException: Table "OPS" already exists""") ||
            e.getMessage.contains("""create table "OPS"""")) {
            // ignorable
          } else {
            val errMsg = s"startupInserts failed with exception: ${e.getMessage}"
            logger.error(e, errMsg)
//            alertActorRef match {
//              case None =>
//              case Some(aaf) =>
//                val mt = MsgTemplate(None, None, errMsg)
//                //aaf ! H2InitFailure(mt)
//            }
          }
      }
    }

    def startupInserts() = {
      try {
        createSchema()
        H2Templates.LoaderConfigs.loaderParams.foreach(
          ele => LoaderDao.insert(ele._1, ele._2))
        SystemRulesDao.insert(
          SystemRuleDefaults.defaultKey,
          SystemRuleDefaults.defaultRule,
          SystemRuleDefaults.defaultAlert,
          SystemRuleDefaults.defaultTemplate)
        EmailTemplateDao.insert(
          EmailTemplateDefaults.defaultKey,
          EmailTemplateDefaults.defaultTO,
          EmailTemplateDefaults.defaultCC,
          EmailTemplateDefaults.defaultBCC,
          EmailTemplateDefaults.defaultSubject,
          EmailTemplateDefaults.defaultBody,
          EmailTemplateDefaults.defaultEmailFlag,
          EmailTemplateDefaults.template_name,
          EmailTemplateDefaults.ts,
          EmailTemplateDefaults.key)
      } catch {
        case e: org.h2.jdbc.JdbcSQLException =>
          // println(s"Bounded exception = [[[[" + e.getStackTrace + "]]]]\n[[[" + e.toString + "]]]\n[[" + e.getMessage + "]]\n[" + e.getLocalizedMessage + "]\n" + e.getOriginalMessage)
          if (e.getMessage.contains("""Unique index or primary key violation""")) {
            // ignorable
          } else {
            val errMsg = s"startupInserts failed with exception: ${e.getMessage}"
            logger.error(e, errMsg)
//            alertActorRef match {
//              case None =>
//              case Some(aaf) =>
//                val mt = MsgTemplate(None, None, errMsg)
//                //aaf ! H2InitFailure(mt)
//            }
          }
      }
    }
  }

  def checkVersion() = withDatabasePool withDynSession {
    val fr = OpsSchemaDao.getVersion
    logger.info(s"Schema2=$fr")
    if (fr == SchemaVersion) {
      logger.info(s"Schema match ($SchemaVersion)")
    } else {
      val err = s"Schema mismatch ($fr vs $SchemaVersion) -- migrate or delete lcp_opdb.h2.db"
      //printAndLog(err)
      //shutdown
      System.exit(255)
    }
  }

}
