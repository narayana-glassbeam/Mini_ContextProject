//package com.glassbeam.h2utils
//
//import java.sql.Timestamp
//import java.text.SimpleDateFormat
//
//import com.mchange.v2.c3p0.ComboPooledDataSource
//import org.h2.engine.Database
//
//import scala.slick.direct.AnnotationMapper.column
//import scala.slick.lifted.Tag
//import scala.slick.model.Table
//
//trait H2UtilsDatabaseAccess {
//  protected val database = Database.forURL("jdbc:h2:tcp://localhost/lcp-opdb", "org.h2.Driver")
//  protected val databasePool = {
//    val ds = new ComboPooledDataSource
//    ds.setDriverClass("org.h2.Driver")
//    ds.setJdbcUrl("jdbc:h2:tcp://localhost/lcp-opdb")
//    ds.setMinPoolSize(10)
//    ds.setAcquireIncrement(5)
//    ds.setMaxPoolSize(100)
//    Database.forDataSource(ds)
//  }
//}
//
//class H2Ops extends H2UtilsDatabaseAccess {
//  private case class LcpSS(runid: Option[Long], node: String, start_ts: Timestamp, heartbeat_ts: Timestamp)
//
//  private class LcpStartStop(tag: Tag) extends Table[LcpSS](tag, "LCP_START_STOP") {
//    def runid = column[Option[Long]]("RUN_ID", O.PrimaryKey, O.AutoInc)
//    def node = column[String]("NODE", O.DBType("VARCHAR(15)"))
//    def start_ts = column[Timestamp]("START_TS")
//    def heartbeat_ts = column[Timestamp]("HEARTBEAT_TS")
//
//    def * = (runid, node, start_ts, heartbeat_ts) <> (LcpSS.tupled, LcpSS.unapply)
//  }
//
//  private val LcpStartStop = TableQuery[LcpStartStop]
//
//  def DDL = LcpStartStop.ddl
//
//
//  def getLiveScalarNodes() = {
//
//    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")
//    //df.setTimeZone(TimeZone.getTimeZone("GMT"))
//
//    val last3Mins = DateTime.now.minusMinutes(4).toDate
//    println("Last 3 mins Ts " + last3Mins)
//
//    //select node, max(heartbeat_ts) from lcp_start_stop group by node;
//    val nodes = databasePool withDynSession {
//      val x =  for {
//        (node, ts) <- LcpStartStop groupBy (_.node)
//      } yield node -> ts.map(_.heartbeat_ts).max
//
//      x.list
//    }
//
//    var liveNodes = List[String]()
//    for(node <- nodes) {
//      val ip = node._1
//      val hb = node._2.getOrElse("2000-01-01 01:01:01.000").toString
//
//      if(sdf.parse(hb).after(last3Mins)) {
//        liveNodes ++= List(ip)
//      }
//    }
//
//    liveNodes
//
//  }
//}
