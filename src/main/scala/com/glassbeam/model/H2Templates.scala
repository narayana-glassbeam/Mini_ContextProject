package com.glassbeam.model

import com.glassbeam.model.StartupConfig._

object H2Templates {
  object SystemRuleDefaults {
    val (defaultKey, defaultRule, defaultAlert, defaultTemplate) = ("INTERNAL", "INTERNAL_FAILURES", false, 9999)
  }
  //notifierId ~ toMailIds ~ ccMailIds ~ bccMailIds ~ subject ~ body
  object EmailTemplateDefaults {
    val (defaultKey, defaultTO, defaultCC, defaultBCC, defaultSubject, defaultBody, defaultEmailFlag, template_name, ts, key) = (9999, "", "", "", "Critical Failures", "", true, "INTERNAL", new java.sql.Timestamp(java.util.Calendar.getInstance().getTime().getTime()), "INTERNALKEY")
  }
  object LoaderConfigs {

    private val watchedPath = "watched=" + home_dir + "watched" + filesep
    private val permPath = "permanent=" + home_dir + "permanent" + filesep
    private val vaultPath = "logvault=" + home_dir + "logvault" + filesep

    val loaderParams = scala.collection.immutable.HashMap[String, String](
      "paths" -> s"""$watchedPath
        |$permPath
        |$vaultPath""".stripMargin,
      "containers" -> """zip=unzip -o filename -d dir
        |tar=tar -xvf filename -C dir
        |rar=unrar e -y filename
        |7z=7za e -y filename
        |gz=gunzip -vfk filename
        |tgz=tar -zxvf filename -C dir
        |taz=gunzip -vfk filename
        |Z=uncompress -vf filename
        |bz2=bunzip2 -vfk filename
        |tz2=bunzip2 -vfk filename
        |tbz2=bunzip2 -vfk filename
        |tbz=bunzip2 -vfk filename
        |lz=lzip -dvfk filename
        |lzma=unlzma -vfk filename
        |tlz=unlzma -vfk filename
        |lzo=lzop -dvfk filename
        |xz=unxz -vfk filename
        |tar.gz=tar -zxvf filename -C dir
        |tar.tgz=tar -zxvf filename -C dir
        |tar.taz=tar -zxvf filename -C dir
        |tar.Z=tar -Zxvf filename -C dir
        |tar.bz2=tar -jxvf filename -C dir
        |tar.tz2=tar -jxvf filename -C dir
        |tar.tbz2=tar -jxvf filename -C dir
        |tar.tbz=tar -jxvf filename -C dir
        |tar.lz=tar --lzip -xvf filename -C dir
        |tar.lzma=tar --lzma -xvf filename -C dir
        |tar.tlz=tar --lzma -xvf filename -C dir
        |tar.lzo=tar --lzop -xvf filename -C dir
        |tar.xz=tar -Jxvf filename -C dir""".stripMargin,
      "spl" -> """namespacetypes=SECTION,EVENT,SESSION,UNPARSED,TRASH,STAT
        |namespacecolumns=(EVENT=ec,ts,severity,evt_text;SESSION=sessionid1,sessionname,sessionattr;SECTION=ec,ts,sysid)
        |primaryattr=ts,sysid,sessionid1,sessionid2,sessionid3,sessionname,severity
        |sessionlevelattr=(1=sessionid1;2=sessionid2;3=sessionid3)
        |attributes=label, units, aggr, loglabel, sort, suppress, tesuppress, facet, ts, ec, sysid,sessionid1,sessionid2,sessionid3,sessionname,sessionattr,severity,content,fragment_id,begin_offset,namespace,type,obs_size,filename,obs_url,linked,uploaded_by,subsysid
        |solrattributes=name,datatype,indexed,stored,multivalued,omitnorms,storetermvectors,storetermpositions,storetermoffsets,uniquefielddef
        |solrdefaults=(indexed=true, stored=true, multivalued=true, omitnorms=true, uniquefielddef=false, required=false, storetermvectors=false, storetermpositions=false, storetermoffsets=false)
        |mandcols=ec,sysid,obs_ts
        |solrcoresep=-
        |label=s
        |units=s
        |aggr=[avg,sum,max,min,count,distinct,median]
        |loglabel=s
        |sort=b
        |suppress=b
        |tesuppress=b
        |facet=s
        |ts=b
        |ec=b
        |sysid=b
        |name=s
        |datatype=s
        |indexed=b
        |stored=b
        |multivalued=b
        |omitnorms=b
        |storetermvectors=b
        |storetermpositions=b
        |storetermoffsets=b""".stripMargin,
      "email" -> """host=
        |user=
        |pwd=
        |port=
        |from=""".stripMargin,
      "solr" -> """ZK_TIMEOUT=60000
        |SOLR_MAX_CONTENT_LINES=5000
        |solrhome='/tmp'
        |zkHost=127.0.0.1
        |solr=127.0.0.1
        |numShards=2
        |replicationFactor=2
        |zkHostLogVault=127.0.0.1
        |onlyLogvault=false
        |solrBucketHost=
        |solrBucketColPlacementInfo=
        |solrBucketColSize=
        |solrBucketShardReplicaInfo=
        |SolrDynamicSchema=true
        |StoreIndex=0
        |solrRejectNumPastDays=366
        |solrRejectNumFutureDays=30
        |numShardsLV=2
        |replicationFactorLV=2""".stripMargin,
      "cassandra" -> """cassandra=127.0.0.1
                       |CassPreparedInserts=true
                       |casync=true
                       |eventbatchcount=10
                       |keyspace=glassbeam
                       |fetchSize=500
                       |consistencyLevel=QUORUM""".stripMargin,
      "optional" -> """timedcache=false
                      |truncateH2=true
                      |deleteBundleOnComplete=true
                      |jregex=false
                      |re2j=false
                      |maxdepth=20
                      |measuredeep=true
                      |maxconcurrentfutures=1000
                      |parsejmx=false""".stripMargin,
      "remoteS3Vault" -> """accessKey=write_S3_accesskey_here
                      |secretKey=write_s3_secretkey_here
                      |useBundleId=false""".stripMargin,
      "mime" -> """text=application/xml,application/plain,application/mbox,data/data""".stripMargin
    )
  }
}
