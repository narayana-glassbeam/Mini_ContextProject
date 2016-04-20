package com.glassbeam.model

object ContextFailure extends Enumeration {
  type ContextFailure = Value
  val AssertFailure,
  AssertBundleDuplicateFailure,
  AssertUncompressionFailure,
  AssertPxCountFailure,
  AssertTruthyFailure,
  AssertBundleFailure,
  AssignmentStmtError,
  MandatoryColumnNoValueFailure,
  CoalesceError,
  Sdf2EpochError,
  Epoch2SdfError,
  XmlError,
  ContextExecFailure,
  ValidateFailure,
  LookupFailure,
  StreamProtocolPortFailure
  = Value
}
object AlertsFailure extends Enumeration {
  type AlertsFailure = Value
  val OtherAlertsFailure = Value
}

object BundleSignatureState extends Enumeration {
  type BundleSignatureState = Value
  val New, Parsed, Duplicate, Failed, Partial = Value
}

object CassandraFailure extends Enumeration {
  type CassandraFailure = Value
  val WriteTimeoutException, OtherCassandraFailure = Value
}

object ParseFailure extends Enumeration {
  type ParseFailure = Value
  val NameSpaceFailure, TableFailure, ChildRefFailure, ColOpFailure, IconFailure, OtherParseFailure = Value
}

object ProcessingState extends Enumeration {
  type ProcessingState = Value
  val Seen, // initial file state set by Watcher
  Parsing, // file is being parsed and added to logvault
  Parsed, // file has been parsed
  SkipParsing, // file is of type Vault_File, so it is NOT being parsed
  Failed, // something failed in context, parse, rules or db-inserts
  Duplicate, // there is assertDuplicate in this file's context and this file has been found to be duplicate of an instance thats been already parsed
  Partial // file did not have context/loading/assert problems but had failures during parse or db-insert
  = Value
}

object RulesFailure extends Enumeration {
  type RulesFailure = Value
  val OtherRulesFailure = Value
}

object SolrFailure extends Enumeration {
  type SolrFailure = Value
  val OtherSolrFailure = Value
}

object FileType extends Enumeration {
  type FileType = Value
  val File, // a normal file which will be used for context eval + parse + upload to logvault
  Vault_File, // file matches skipfiles pattern - so it wont be parsed but just uploaded to Vault
  Delete_File, // file matches deletefiles pattern - so it is neither parsed nor uploaded to Vault - completely ignored
  Compressed, // file is a compressed archive
  Directory, // path is of a directory
  Streaming, // its a pseudo-file used to trigger listening of TCP/UDP stream,
  Symbolic_Link, // the file is not a real file but a symbolic link - set the file type different since these dont require parsing or vault upload
  Extensibility, // file created by extensibilty functions and not from original bundle
  Binary, // its a binary file
  UnixSpecial, // named-pipe, socket, block-device
  Reverse // File that is to be read in reverse order
  = Value
}

object BundleState extends Enumeration {
  type BundleState = Value
  val New, Parsing, Parsed, Duplicate, Failed, Partial, ZeroFiles = Value
}

object BundleType extends Enumeration {
  type BundleType = Value
  val CompressedFile, RegularFile, BundleDirectory, StreamFile = Value
}

case class GoldenConfig(val golden: Boolean, val category: String, val subcategory: String)

case class BundleDetails(val loadid: Long, val b_prop_str: Option[String], val b_prop: GoldenConfig,
                         val b_type: BundleType.BundleType, val b_compInOps: Boolean, val b_name: String, val b_comp_size: Long)
