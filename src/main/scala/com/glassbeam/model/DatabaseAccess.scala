package com.glassbeam.model

import com.glassbeam.model.StartupConfig._
import com.mchange.v2.c3p0.ComboPooledDataSource

import scala.slick.driver.H2Driver.simple._

object DatabaseAccess {
  lazy val database = Database.forURL(opsdb, driver)
  lazy val pool = {
    val ds = new ComboPooledDataSource
    ds.setDriverClass(driver)
    ds.setJdbcUrl(opsdb)
    ds.setMinPoolSize(3)
    ds.setAcquireIncrement(5)
    ds.setMaxPoolSize(100)
    //ds.setMaxStatements(1000)
    //ds.setMaxStatementsPerConnection(100)
    // ds.setUser("")
    Database.forDataSource(ds)
  }
}

trait DatabaseAccess {
  protected val database = DatabaseAccess.database
  protected val withDatabasePool = DatabaseAccess.pool
}