package com.glassbeam.failuredefs

import java.sql.Timestamp
import java.util.Calendar


/**
  * Created by narayana on 18/4/16.
  */
case class MsgTemplate(key: Option[String], loadId: Option[Long], msg: String, filename: Option[String] = None,
                        ts: java.sql.Timestamp = new Timestamp(Calendar.getInstance().getTimeInMillis()),
                        nodeId: Option[String] = Some(""))