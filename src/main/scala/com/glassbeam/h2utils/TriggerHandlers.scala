package com.glassbeam.h2utils

import java.sql.Connection
//
//abstract class RuleTriggerHandler(tType:String) extends Trigger {
//
//  override def remove():Unit = {}
//
//  override def close():Unit = {}
//
//  override def init(conn: Connection, schemaName: String, triggerName: String, tableName: String, before: Boolean, triggerType: Int): Unit = {
//    println(s"Initializing trigger $triggerName on table $tableName. Type = $triggerType")
//  }
//
//  override def fire(conn: Connection, oldRow: Array[AnyRef], newRow: Array[AnyRef]): Unit = {
//    val ruleId = if(tType == "DELETE") oldRow(0).toString else newRow(0).toString
//
//    println(s"Sending Rule trigger info to Scalar nodes. Trigger-Type=$tType RuleId=$ruleId")
//    val h2Ops = new H2Ops
//    val liveNodes = List("127.0.0.1")  //h2Ops.getLiveScalarNodes()
//    val msgContent = s"$tType,$ruleId"
//    H2RemoteSender.send(liveNodes,msgContent)
//  }
//}
//
//class RuleInsertTrigger extends RuleTriggerHandler("INSERT")
//
//class RuleUpdateTrigger extends RuleTriggerHandler("UPDATE")
//
//class RuleDeleteTrigger extends RuleTriggerHandler("DELETE")

