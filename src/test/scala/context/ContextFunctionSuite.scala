package com.glassbeam.context

import java.io.File
import java.nio.file.Path

import com.glassbeam.context.Context.{ContextReason, LoaderClassArguments, LoaderEvalArguments}
import com.glassbeam.context.ContextAssertData._
import com.glassbeam.context.Dummy._
//import com.glassbeam.extensibility.traits.Parsable
//import com.glassbeam.loader.Init
import com.glassbeam.model.ContextFailure._
import com.glassbeam.model.RunEnvironment
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import scala.collection.immutable.{HashMap, List}
import scala.language.postfixOps

object Dummy {

  trait dummyRefs {
    Init.inittype = Init.Test
  }

}

object ContextAssertData {
  lazy val validContextRows: List[String] = List("""sysid=glassbeam""", """sysid1=""""")
  lazy val clsList = List(AssertUncompressionFail, Assert, Validate, CombineLines, Encoding, AssertTruthy)
  lazy val invalidContextRows: List[String] = List("""sysid""", """=""")
  lazy val dummyCR = ContextReason(HashMap[String, String]("sysid" -> "glassbeam", "mfr" -> "dummyMfr"), "")
  lazy val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
  lazy val cca = LoaderClassArguments("assert(sysid)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
}

class AssertSpec extends FlatSpec with Matchers with dummyRefs {
  val dummyCR = ContextReason(HashMap[String, String]("sysid" -> "glassbeam", "mfr" -> "dummyMfr", "node" -> "Sol42"), "")
  val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps") // ctx, file, loadid

  def getcca(context: String) = LoaderClassArguments(context, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")

  "Assert with one input parameter (context variable)" should "have empty Reason value for defined sysid 'f.assert(sysid)'" in {
    val ctxLine = "f.assert(sysid)"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should "have empty Reason value for defined sysid 'f.assert( sysid )'" in {
    val ctxLine = "f.assert( sysid )"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty())
  }
  it should "have Non-empty Reason for undefined variable as input: 'f.assert(SYSID)'" in {
    val ctxLine = "f.assert(SYSID)"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(!x.reason.isEmpty())
  }
  it should "fail for numeric value as input 'f.assert(12345)'" in {
    val ctxLine = "f.assert(12345)"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }
  "Assert with two input params (ctx variable and Email template Id)" should "extract Email templateId from input 'f.assert(sysid,99)'" in {
    val ctxLine = "f.assert(sysid,99)"
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }
  it should "extract Email templateId from input 'f.assert (  sysid ,  99 ) '" in {
    val ctxLine = "f.assert (  sysid ,  99 ) "
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }
  it should "have empty Reason value for input with defined sysid: 'f.assert(sysid, 99)'" in {
    val ctxLine = "f.assert(sysid, 99)"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty())
  }
  it should "have NON-empty Reason for input with undefined variable: 'f.assert(SYSID, 99)'" in {
    val ctxLine = "f.assert(SYSID, 99)"
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(!x.reason.isEmpty())
  }
  "Assert with three inputs (ctx var, email templateid, custom msg)" should """extract email template AND custom message from input 'f.assert(sysid, 99, "Greetings, Earthlings!")'""" in {
    val ctxLine = """f.assert(sysid, 99, "Greetings, Earthlings!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }
  it should """extract email template AND custom message from input 'f.assert (  sysid  , 99 , "Greetings, Earthlings!"  )'""" in {
    val ctxLine = """f.assert (  sysid  , 99 , "Greetings, Earthlings!"  )"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }
  it should """extract custom message with context var from input 'f.assert(sysid, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(sysid, 99, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings of system {context.node}!\"")
  }
  it should """have empty Reason value for input with defined sysid: 'f.assert(sysid, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(sysid, 99, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty())
  }
  it should """have NON-empty Reason (=custom msg) for input with undefined variable: 'f.assert(SYSID, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(SYSID, 99, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\n\"Greetings, Earthlings of system Sol42!\"") // Has newline char in code
  }
  it should """have empty Reason with undefined variable if custom msg is empty: 'f.assert(SYSID, 99,  ")'""" in {
    val ctxLine = """f.assert(SYSID, 99,  )"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && clsAssert.assertOptionalMsg.get == "" && x.reason == "\n") // Has newline char in code
  }

  "Assert with two inputs (ctx variable and custom msg)" should """extract custom message from input 'f.assert(sysid, "Greetings, Earthlings!")'""" in {
    val ctxLine = """f.assert(sysid, "Greetings, Earthlings!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }
  it should """extract custom message from input 'f.assert (  sysid  , "Greetings, Earthlings!"  )'""" in {
    val ctxLine = """f.assert (  sysid  , "Greetings, Earthlings!"  )"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }
  it should """extract custom message with context var from input 'f.assert(sysid, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(sysid, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings of system {context.node}!\"")
  }
  it should """have empty Reason value for input with defined sysid: 'f.assert(sysid, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(sysid, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty())
  }
  it should """have NON-empty Reason (=custom msg) for input with undefined variable: 'f.assert(SYSID, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """f.assert(SYSID, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\n\"Greetings, Earthlings of system Sol42!\"") // Has newline char in code
  }
  it should """have empty Reason (=custom msg) for undefined variable IF custom msg is empty: 'f.assert(SYSID,  )'""" in {
    val ctxLine = """f.assert(SYSID,  )"""
    val clsAssert = new Assert(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && clsAssert.assertOptionalMsg.get == "" && x.reason == "\n") // Has newline char in code
  }
}

class AssertFileDuplicateSpec extends FlatSpec with Matchers with dummyRefs with MockFactory {
  val fnKey = "fname"
  val fname = "test"
  val ctxfnName = "f.duplicate"
  val ctxfn = s"$fnKey=$ctxfnName($fname) "

  "AssertFileDuplicate" should "not indicate duplicate" in {
    val cnt = mockFunction[String, Long, String, Int]
    cnt expects(fname, *, *) returning 0 once

    val cca = LoaderClassArguments(ctxfn, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String](fname -> fname, "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, new File(fname), -1,"mps")
    val clsAssert = new AssertFileDuplicate(cca) {
      override def count(fname: String, loadId: Long, fullkey: String) = cnt(fname, loadId, fullkey)
    }
    val x = clsAssert.execute(cefa)
    assert(x.contextStrings.get(fnKey) === Some(fname))
  }

  it should "indicate duplicate" in {
    val cnt = mockFunction[String, Long, String, Int]
    cnt expects(*, *, *) returning 2 once

    val cca = LoaderClassArguments(ctxfn, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String](fname -> fname, "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, new File(fname), -1,"mps")
    val clsAssert = new AssertFileDuplicate(cca) {
      override def count(fname: String, loadId: Long, fullkey: String) = cnt(fname, loadId, fullkey)
    }
    val x = clsAssert.execute(cefa)
    assert(x.contextStrings.get(fnKey) === Some("None"))
  }
}

class AssertUncompressionFailSpec extends FlatSpec with Matchers with dummyRefs with MockFactory {

  val dummyCR = ContextReason(HashMap[String, String]("sysid" -> "", "mfr" -> "dummyMfr", "node" -> "Sol42"), "")
  val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
  val ctxFn = "b.assertOnUncompressionFailure"
  val onFail = mockFunction[Long, String, Unit]

  def getcca(context: String) = LoaderClassArguments(context, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")

  def getauf(ctxLine: String, compressedFiles: Seq[(String, Option[Int])]) = {
    new AssertUncompressionFail(getcca(ctxLine)) {
      override def obtainCompressedFiles(loadId: Long) = compressedFiles
      override def onFailure(loadId: Long, error_stmt: String) = onFail(loadId, error_stmt)
    }
  }

  "AssertUncompressionFail with no input params" should """fail for no CompressedFiles input: Seq(("dummy",None))""" in {
    onFail expects(*, *) once

    val clsAssert = getauf(ctxFn, Seq(("dummy", None)))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }

  it should """fail for CompressedFiles input with bad uncompression status: Seq(("dummy",Some(3)))""" in {
    onFail expects(*, *) once

    val clsAssert = getauf(ctxFn, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }

  it should """not fail for CompressedFiles input with valid uncompression status: Seq(("dummy",Some(0)))""" in {
    val clsAssert = getauf(ctxFn, Seq(("dummy", Some(0))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isEmpty)
  }

  "AssertUncompressionFail with one input parm (email templateId)" should "extract Email templateId from input 'b.assertOnUncompressionFailure(99)'" in {
    val ctxLine = "b.assertOnUncompressionFailure(99)"
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }

  it should "extract Email templateId from input 'b.assertOnUncompressionFailure ( 99 )  '" in {
    val ctxLine = "b.assertOnUncompressionFailure ( 99 )  "
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }

  "AssertUncompressionFail with two input params (email templateId and custom msg)" should """extract email template AND custom message from input 'b.assertOnUncompressionFailure(99, "Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertOnUncompressionFailure(99, "Greetings, Earthlings!")"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }

  it should """extract email template AND custom message from input 'b.assertOnUncompressionFailure (  99 , "Greetings, Earthlings! "  )'""" in {
    val ctxLine = """b.assertOnUncompressionFailure (  99 , "Greetings, Earthlings! "  )"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings! \"")
  }

  it should """extract custom message with context var from input 'b.assertOnUncompressionFailure(99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertOnUncompressionFailure(99, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings of system {context.node}!\"")
  }

  it should """have NON-empty Reason (=custom msg) for CompressedFiles input as Seq(("dummy",Some(3))), and context line: 'b.assertOnUncompressionFailure(99, "Greetings, Earthlings of system {context.node}!")'""" in {
    onFail expects(*, *) once

    val ctxLine = """b.assertOnUncompressionFailure(99, "Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\"Greetings, Earthlings of system Sol42!\"") // Doesn't have newline char in code
  }

  it should """have empty Reason (=custom msg) for a failure case Seq(("dummy",Some(3))), IF Custom msg is empty: 'b.assertOnUncompressionFailure(99, )'""" in {
    onFail expects(*, *) once

    val ctxLine = """b.assertOnUncompressionFailure(99, )"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "") // Doesn't have newline char in code
  }

  "AssertUncompressionFail with one input param (custom msg)" should """extract custom message from input 'b.assertOnUncompressionFailure("Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertOnUncompressionFailure("Greetings, Earthlings!")"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }

  it should """extract custom message from input 'b.assertOnUncompressionFailure (  "Greetings, Earthlings!"  )'""" in {
    val ctxLine = """b.assertOnUncompressionFailure (  "Greetings, Earthlings!"  )"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(0))))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }

  it should """have NON-empty Reason (=custom msg) for CompressedFiles input as Seq(("dummy",Some(3))), and context as: 'b.assertOnUncompressionFailure("Greetings, Earthlings of system {context.node}!")'""" in {
    onFail expects(*, *) once
    val ctxLine = """b.assertOnUncompressionFailure("Greetings, Earthlings of system {context.node}!")"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\"Greetings, Earthlings of system Sol42!\"") // Doesn't have newline char in code
  }

  it should """have empty Reason (=custom msg) for a failure case Seq(("dummy",Some(3))), IF custom msg is spaces: 'b.assertOnUncompressionFailure( )'""" in {
    onFail expects(*, *) once
    val ctxLine = """b.assertOnUncompressionFailure( )"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "") // Doesn't have newline char in code
  }

  it should """have empty Reason (=custom msg) for a failure case Seq(("dummy",Some(3))), IF custom msg is empty: 'b.assertOnUncompressionFailure()'""" in {
    onFail expects(*, *) once
    val ctxLine = """b.assertOnUncompressionFailure()"""
    val clsAssert = getauf(ctxLine, Seq(("dummy", Some(3))))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "") // Doesn't have newline char in code
  }
}

class AssertTruthySpec extends FlatSpec with Matchers with dummyRefs {

  val dummyCR = ContextReason(HashMap[String, String]("sysid" -> "glassbeam", "mfr" -> "dummyMfr", "node" -> "Sol42", 
    "zombie" -> "true", "human" -> "false", "emptyStr" -> "", "spaces" -> "   ", "BIG_TRUE" -> "TRUE", "BIG_FALSE" -> "FALSE"), "")
  val cefa =LoaderEvalArguments(dummyCR, null, 1L,"mps") // ctx, file, loadid

  def getcca(context: String) = LoaderClassArguments(context, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")

  "AssertTruthy with one input parameter (context variable)" should "fail for empty variable 'f.assertTruthy(emptyStr)'" in {
    val ctxLine = "f.assertTruthy(emptyStr)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.nonEmpty)
  }
  it should "fail (have non-empty Reason) for spaces 'f.assertTruthy(spaces)'" in {
    val ctxLine = "f.assertTruthy(spaces)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.nonEmpty)
  }
  it should "fail (have Non-empty Reason) for undefined variable: 'f.assertTruthy(UNDEFINED_VARIABLE)'" in {
    val ctxLine = "f.assertTruthy(UNDEFINED_VARIABLE)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.nonEmpty)
  }
  it should "fail for ctx variable having value as true: 'f.assertTruthy(zombie)'" in {
    val ctxLine = "f.assertTruthy(zombie)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }
  it should "not fail for ctx variable having value as false: 'f.assertTruthy(human)'" in {
    val ctxLine = "f.assertTruthy(human)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should "fail for ctx variable having value as TRUE: 'f.assertTruthy(BIG_TRUE)'" in {
    val ctxLine = "f.assertTruthy(BIG_TRUE)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }
  it should "not fail for ctx variable having value as FALSE: 'f.assertTruthy(BIG_FALSE)'" in {
    val ctxLine = "f.assertTruthy(BIG_FALSE)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should "fail for ctx variable having value other than true/false: 'f.assertTruthy(sysid)'" in {
    val ctxLine = "f.assertTruthy(sysid)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.nonEmpty)
  }
  it should "not fail if syntax has spaces and ctx variable is false: 'f.assertTruthy  (  human )  '" in {
    val ctxLine = "f.assertTruthy  (  human )  "
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should "fail if syntax has spaces and ctx variable has value as true: 'f.assertTruthy  (  zombie ) '" in {
    val ctxLine = "f.assertTruthy  (  zombie ) "
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined)
  }

  "AssertTruthy with two input params (ctx variable and Email template Id)" should "extract Email templateId from input 'f.assertTruthy(zombie,99)'" in {
    val ctxLine = "f.assertTruthy(zombie, 99)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }
  it should "extract Email templateId from input 'f.assertTruthy (  zombie ,  99 ) '" in {
    val ctxLine = "f.assertTruthy (  zombie ,  99 ) "
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }
  it should "have empty Reason value if ctx variable is false: 'f.assertTruthy(human, 99)'" in {
    val ctxLine = "f.assertTruthy(human, 99)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should "have NON-empty Reason if ctx variable is true: 'f.assertTruthy(zombie, 99)'" in {
    val ctxLine = "f.assertTruthy(zombie, 99)"
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(!x.reason.isEmpty)
  }

  "AssertTruthy with three inputs (ctx var, email templateid, custom msg)" should """extract email template AND custom message from input 'f.assertTruthy(zombie, 99, "Greetings, Zombies!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, 99, "Greetings, Zombies!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies!\"")
  }
  it should """extract email template AND custom message from input 'f.assertTruthy (  zombie  , 99 , "Greetings, Zombies!"  )'""" in {
    val ctxLine = """f.assertTruthy (  zombie  , 99 , "Greetings, Zombies!"  )"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies!\"")
  }
  it should """extract custom message with context var from input 'f.assertTruthy(zombie, 99, "Greetings, Zombies of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, 99, "Greetings, Zombies of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies of system {context.node}!\"")
  }
  it should """have empty Reason value if ctx variable is false: 'f.assertTruthy(human, 99, "Greetings, non-humans of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(human, 99, "Greetings, non-humans of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty)
  }
  it should """have NON-empty Reason (=custom msg) if ctx variable is true: 'f.assertTruthy(zombie, 99, "Greetings, Zombies of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, 99, "Greetings, Zombies of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason == "\n\"Greetings, Zombies of system Sol42!\"") // Has newline char in code
  }
  it should """have empty Reason if ctx variable is true AND custom msg is empty: 'f.assertTruthy(zombie, 99,   )'""" in {
    val ctxLine = """f.assertTruthy(zombie, 99,   )"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\n") // Has newline char in code
  }

  "AssertTruthy with two inputs (ctx variable and custom msg)" should """extract custom message from input 'f.assertTruthy(zombie, "Greetings, Zombies!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, "Greetings, Zombies!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies!\"")
  }
  it should """extract custom message from input 'f.assertTruthy (  zombie  , "Greetings, Zombies!"  )'""" in {
    val ctxLine = """f.assertTruthy (  zombie  , "Greetings, Zombies!"  )"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies!\"")
  }
  it should """extract custom message with context var from input 'f.assertTruthy(zombie, "Greetings, Zombies of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, "Greetings, Zombies of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Zombies of system {context.node}!\"")
  }
  it should """have empty Reason value if ctx variable is false: 'f.assertTruthy(human, "Greetings, non-humans of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(human, "Greetings, non-humans of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason.isEmpty())
  }
  it should """have NON-empty Reason (=custom msg) if ctx variable is true: 'f.assertTruthy(zombie, "Greetings, Zombies of system {context.node}!")'""" in {
    val ctxLine = """f.assertTruthy(zombie, "Greetings, Zombies of system {context.node}!")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason == "\n\"Greetings, Zombies of system Sol42!\"") // Has newline char in code
  }
  it should """have empty Reason if ctx variable is true AND custom msg is empty: 'f.assertTruthy(zombie, "")'""" in {
    val ctxLine = """f.assertTruthy(zombie, "")"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.reason == "\n\"\"") // Has newline char in code
  }
  it should """have empty Reason if ctx variable is true AND custom msg is empty: 'f.assertTruthy(zombie,  )'""" in {
    val ctxLine = """f.assertTruthy(zombie, )"""
    val clsAssert = new AssertTruthy(getcca(ctxLine))
    val x = clsAssert.execute(cefa)
    assert(x.failure.isDefined && x.reason == "\n") // Has newline char in code
  }
}

class LSchemaSpec extends FlatSpec with Matchers with dummyRefs {
  "Testing x=l.schema' " should "Be Passed" in {
    val cca = LoaderClassArguments("x=l.schema", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLschema = new Lschema(cca)
    val x = clsLschema.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(x.reason.isEmpty)
  }
  "Testing l.schema Without RHS" should "Be Failed" in {
    val cca = LoaderClassArguments("x=", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLschema = new Lschema(cca)
    val x = clsLschema.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing l.schema without LHS' " should "Be Failed" in {
    val cca = LoaderClassArguments("l.schema", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLschema = new Lschema(cca)
    val x = clsLschema.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing l.schema with  RHS=schema' " should "Be Failed" in {
    val cca = LoaderClassArguments("x=abc", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLschema = new Lschema(cca)
    val x = clsLschema.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
}

class FDateSpec extends FlatSpec with Matchers with dummyRefs {
  "Testing x=f.date' with a sample input file" should "Be Passed " in {
    val cca = LoaderClassArguments("x=f.date", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, new java.io.File("/tmp/test.txt"), -1,"mps")
    val clsFDate = new Fdate(cca)
    val x = clsFDate.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(x.reason.isEmpty)
  }
  "Testing x=f.date With NULL" should "Be Failed " in {
    val cca = LoaderClassArguments("x=", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsFDate = new Fdate(cca)
    val x = clsFDate.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing f.date without using f.date " should "Be Failed " in {
    val cca = LoaderClassArguments("x=F.Date", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsFDate = new Fdate(cca)
    val x = clsFDate.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing x=f.date on NULL" should "Be Failed " in {
    val cca = LoaderClassArguments("x=f.date", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsFDate = new Fdate(cca)
    val x = clsFDate.execute(cefa)
    println(s"Reason = ${x.reason}")
    assert(x.contextStrings.get("x") === Some("None"))
  }
}

class LoaderTestSpec extends FlatSpec with Matchers with dummyRefs {

  "Testing x=l.customer' with rhs as null" should "Be Passed " in {
    val cca = LoaderClassArguments("ec=null", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsFDate = new Lcustomer(cca)
    val x = clsFDate.execute(cefa)
    println("Context String EC = " + cefa.cr)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing x=l.customer functionality by comparision" should "Be Passed " in {
    val cca = LoaderClassArguments("ec=l.customer", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLCust = new Lcustomer(cca)
    assert(clsLCust.arg.customer == cca.customer)
  }
  "Testing l.customer without lhs" should "Be failed" in {
    val cca = LoaderClassArguments("l.customer", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLCust = new Lcustomer(cca)
    val x = clsLCust.execute(cefa)
    assert(!x.reason.isEmpty())
  }
  "Testing x=l.product' with rhs as null" should "Be Passed " in {
    val cca = LoaderClassArguments("prod=null", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLProd = new Lproduct(cca)
    val x = clsLProd.execute(cefa)
    println("Context String EC = " + cefa.cr)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing x=l.product functionality by comparision" should "Be Passed " in {
    val cca = LoaderClassArguments("ec=l.product", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLCust = new Lcustomer(cca)
    assert(clsLCust.arg.product == cca.product)
  }
  "Testing x=l.manufacturer' with rhs as null" should "Be Passed " in {
    val cca = LoaderClassArguments("prod=null", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLMfr = new Lmanufacturer(cca)
    val x = clsLMfr.execute(cefa)
    println("Context String EC = " + cefa.cr)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing x=l.manufacturer functionality by comparision" should "Be Passed " in {
    val cca = LoaderClassArguments("x=l.manufacturer", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLCust = new Lmanufacturer(cca)
    assert(clsLCust.arg.manufacturer == cca.manufacturer)
  }
  "Testing x=l.schema' with rhs as null" should "Be Passed " in {
    val cca = LoaderClassArguments("x=null", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLSch = new Lschema(cca)
    val x = clsLSch.execute(cefa)
    println("Context String EC = " + cefa.cr)
    println(s"Reason = ${x.reason}")
    assert(!x.reason.isEmpty)
  }
  "Testing x=l.schema functionality by comparision" should "Be Passed " in {
    val cca = LoaderClassArguments("x=l.schema", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, null, -1,"mps")
    val clsLCust = new Lschema(cca)
    assert(clsLCust.arg.schema == cca.schema)
  }
  //File Level Functions

  "Testing x=f.name' with rhs as null" should "Be Passed " in {
    val cca = LoaderClassArguments("x=null", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, new java.io.File("/tmp/test.txt"), -1,"mps")
    val clsFname = new Fname(cca)
    val x = clsFname.execute(cefa)
    assert(!x.reason.isEmpty)
  }
  "Testing x=f.name' Functionality " should "Be Passed " in {
    val cca = LoaderClassArguments("x=f.name", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val dummyCR = ContextReason(HashMap[String, String]("sch" -> "TEST", "mfr" -> "dummyMfr"), "")
    val cefa = LoaderEvalArguments(dummyCR, new java.io.File("/tmp/test.txt"), -1,"mps")
    val clsFname = new Fname(cca)
    val x = clsFname.execute(cefa)
    println(x.contextStrings)
    assert(x.reason.isEmpty())
  }
}

class ValidateTestSpec extends FlatSpec with Matchers with dummyRefs with RunEnvironment {
  //  ConfigSetup.init
  val cca = LoaderClassArguments("validate(mfr)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
  val emps = cca.customer + filesep + cca.manufacturer + filesep + cca.product + filesep + cca.schema

  def valueObtainerForKey(mfrKey: String, mfrValueToReturn: String): (String, String) => Seq[String] = (key, _emps) => {
    if (_emps == emps && key == mfrKey) Seq(mfrValueToReturn)
    else Seq.empty
  }

  "Validate(mfr)" should "not fail" in {
    val mfrKey = "test-mfr"
    val mfrValueToMatch = "test-mfr-Value"
    val cr = ContextReason(contextStrings = HashMap[String, String]("sysid" -> "glassbeam", "mfr" -> mfrKey), "")
    val cefa = LoaderEvalArguments(cr, null, -1,"mps")
    val responseContextReason = new Validate(cca, valueObtainerForKey(mfrKey, mfrValueToMatch)).execute(cefa)
    responseContextReason.failure shouldBe None
    responseContextReason.reason shouldBe empty
  }

  "Validate(keynotpresent)" should "fail" in {
    val responseContextReason = new Validate(cca, valueObtainerForKey("", "")).execute(cefa)
    responseContextReason.failure shouldBe Some(ValidateFailure)
    assert(!responseContextReason.reason.isEmpty)
  }
}

class BSizeTestSpec extends FlatSpec with Matchers with dummyRefs {
  "BSize" should "succeed" in {
    val cca = LoaderClassArguments("s=b.size", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cefa = LoaderEvalArguments(dummyCR, new File("/tmp/testFile"), -1,"mps")
    val bundleSizeObtainer: File => Long = file =>
      if (file.toString == "/tmp/testFile") 10L else 0L

    val responseContextReason = new Bsize(cca, bundleSizeObtainer).execute(cefa)
    responseContextReason.contextStrings("s") shouldEqual 10L.toString
    responseContextReason.failure shouldBe None
    responseContextReason.reason shouldBe empty
  }
}

class BIdTestSpec extends FlatSpec with Matchers with dummyRefs {
  "BId" should "succeed" in {
    val sysIdValue = "glassbeam"
    val obs_tsValue = "testObsts"
    val cca = LoaderClassArguments("b.id(sysid,obs_ts) ", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String]("sysid" -> sysIdValue, "obs_ts" -> obs_tsValue), "")
    val cefa = LoaderEvalArguments(cr, null, -1,"mps")
    val responseContextReason = BId.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("sysid") shouldEqual sysIdValue
    responseContextReason.contextStrings("obs_ts") shouldEqual obs_tsValue
    responseContextReason.failure shouldBe None
    responseContextReason.reason shouldBe empty
  }
}

class FcountTestSpec extends FlatSpec with Matchers with MockFactory with dummyRefs {
  "Fcount" should "succeed" in {

    val cca = LoaderClassArguments("c=f.count /.*test.*/ ", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String](), "")
    val cefa = LoaderEvalArguments(cr, null, 10L,"mps")
    val fileNamesObtainer = mockFunction[Long, Seq[String]]
    fileNamesObtainer expects 10L returning Seq("test", "a", "b", "c", "test.txt", "ddf_test.ts", "dff")

    val responseContextReason = new Fcount(cca, fileNamesObtainer).execute(cefa)
    responseContextReason.contextStrings("c") shouldBe 3L.toString
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }

  it should "also succeed" in {
    val cca = LoaderClassArguments("c=f.count /.*test1.*/ ", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String](), "")
    val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
    val fileNamesObtainer = mockFunction[Long, Seq[String]]
    fileNamesObtainer expects 1L returning Seq("test", "a", "b", "c", "test.txt", "ddf_test.ts", "dff")

    val responseContextReason = new Fcount(cca, fileNamesObtainer).execute(cefa)
    responseContextReason.contextStrings("c") shouldBe 0L.toString
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }
}

class BnameTestSpec extends FlatSpec with Matchers with MockFactory with dummyRefs {
  "Bname" should "succeed" in {
    val cca = LoaderClassArguments("bname=b.name", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String](), "")
    val cefa = LoaderEvalArguments(cr, new File("/tmp/test.txt"), -1,"mps")
    val bundleNameObtainer = mockFunction[File, String]
    bundleNameObtainer expects new File("/tmp/test.txt") returning "testBundle"

    val responseContextReason = new Bname(cca, bundleNameObtainer).execute(cefa)
    responseContextReason.contextStrings("bname") shouldBe "testBundle"
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }

  it should "throw exception" in {
    val cca = LoaderClassArguments("bname=b.name", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String](), "")
    val cefa = LoaderEvalArguments(cr, new File("/tmp/test.txt"), -1,"mps")
    val bundleNameObtainer = mockFunction[File, String]
    bundleNameObtainer expects new File("/tmp/test.txt") throwing new Exception("bundle name not found")

    intercept[Exception] {
      val responseContextReason = new Bname(cca, bundleNameObtainer).execute(cefa)
      responseContextReason.contextStrings("bname") shouldBe "testBundle"
      responseContextReason.reason shouldBe empty
      responseContextReason.failure shouldBe None
    }
  }
}

class LookupTestSpec extends FlatSpec with Matchers with MockFactory with dummyRefs with RunEnvironment {
  private def getDelimitedMPS(carg: LoaderClassArguments) = filesep + carg.manufacturer + filesep + carg.product + filesep + carg.schema

  "Bname" should "provide proper value to lValue of context function" in {
    val cca = LoaderClassArguments("lValue=lookup(test)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val delimMPS = getDelimitedMPS(cca)
    val cr = ContextReason(contextStrings = HashMap[String, String]("ec" -> "testEC", "test" -> "testKey"), "")
    val cefa = LoaderEvalArguments(cr, new File("/tmp/test.txt"), -1,"mps")
    val lookupValueObtainer = mockFunction[String, String, Option[String]]
    lookupValueObtainer expects("testKey", s"testEC$delimMPS") returning Some("testLookupValue")

    val responseContextReason = new Lookup(cca, lookupValueObtainer).execute(cefa)
    responseContextReason.contextStrings("lValue") shouldBe "testLookupValue"
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }


  it should "provide empty value to lValue of context function" in {
    val cca = LoaderClassArguments("lValue=lookup(test)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val delimMPS = getDelimitedMPS(cca)
    val cr = ContextReason(contextStrings = HashMap[String, String]("ec" -> "testEC", "test" -> "testKey"), "")
    val cefa = LoaderEvalArguments(cr, new File("/tmp/test.txt"), -1,"mps")
    val lookupValueObtainer = mockFunction[String, String, Option[String]]
    lookupValueObtainer expects("testKey", s"testEC$delimMPS") returning None

    val responseContextReason = new Lookup(cca, lookupValueObtainer).execute(cefa)
    responseContextReason.contextStrings("lValue") shouldBe ""
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }

  it should "fail" in {
    val cca = LoaderClassArguments("lValue=lookup(test)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String]("test" -> "testKey"), "")
    val cefa = LoaderEvalArguments(cr, new File("/tmp/test.txt"), -1,"mps")
    val lookupValueObtainer = mockFunction[String, String, Option[String]]

    val responseContextReason = new Lookup(cca, lookupValueObtainer).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
    responseContextReason.failure shouldBe defined
  }
}

class MGrepTestSpec extends FlatSpec with Matchers with dummyRefs {

  import com.glassbeam.context.TestHelpers._

  "MGrep" should "provide values this_testX_Value for x" in {
    val cca = LoaderClassArguments("x = m.grep /start:/ /this(.+?)value/", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val file = new File("/tmp/test.txt")
    file.write(
      """test content
        |start: start regex match here
        |some text 1
        |some text 2
        |end regex match this_testX_value as matched content
      """.stripMargin)

    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = Mgrep.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("x") shouldBe "_testX_"
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }
}

class XMLValueTestSpec extends FlatSpec with Matchers with dummyRefs {

  import com.glassbeam.context.TestHelpers._

  val xmlStr =

    """<?xml version="1.0" encoding="UTF-8"?>
      |<com.ibm.inventory version="1.0">
      |   <ResourceSet class="com.ibm.inventory.Hardware" timestamp="20131119084049000" type="Hardware" version="1.0">
      |      <Resource displayName="8205-E6C-068807T" uniqueId="8205-E6C-068807T">
      |         8205-E6C-068807T
      |         <Property displayName="Attached HMCs" name="V3">
      |            V3
      |            <Value type="string">2394-AG6_R9YH8NG</Value>
      |         </Property>
      |         <Property displayName="Brand Keyword" name="BR">
      |            BR
      |            <Value type="string">D0</Value>
      |         </Property>
      |         <Property displayName="Displayable Message" name="DS">
      |            DS
      |            <Value type="string">System VPD</Value>
      |         </Property>
      |         <Property displayName="FV" name="FV">
      |            FV
      |            <Value type="string">AL740_100</Value>
      |         </Property>
      |         </Resource>
      |         </ResourceSet>
      |         </com.ibm.inventory>""".stripMargin


  "XMLValue" should "succeed" in {
    val cca = LoaderClassArguments(
      """tzfirsttemp=xmlValue(/.*MRPD.*\.xml/,//com.ibm.inventory/ResourceSet/Resource/Property[@displayName="Displayable Message"]/Value/)""",
      1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val file = new File("/tmp/210796175BWW40.NOCUST.000.NOPMH.131119011534.cl9.MRPD.2107.xml")
    file.write(xmlStr)
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = XmlValue.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("tzfirsttemp") shouldBe "System VPD"
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None

  }
  it should "fail" in {
    val cca = LoaderClassArguments(
      """tzfirsttemp=xmlValue(/.*MRPD.*\.xml/,//invalidXpath/#$/)""",
      1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val file = new File("/tmp/nonextingfile.MRPD.2107.xml")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = XmlValue.getObject(cca).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
    responseContextReason.failure shouldBe Some(XmlError)

  }
}

object ProcessContext {

  class ParsableClass extends Parsable {
    override def processToFile(p: Path): Path = p

    override def processFileToContext(p: Path): String = p.toString

    override def processBundleToContext(p: Path): String = p.toString
  }
}

class ProcessBundleToContextTestSpec extends FlatSpec with Matchers with dummyRefs {

  val file = new File("/tmp/processBndlToCtx.txt")
  "ProcessBundleToContext" should s"provide values ${file.toString} for x" in {
    val cca = LoaderClassArguments(s"x = e.processBundleToContext /${file.getParent}/ com.glassbeam.context.ProcessContext$$ParsableClass", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = ProcessBundleToContext.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("x") shouldBe file.toString
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }

  it should "provide empty string as value for x" in {
    val cca = LoaderClassArguments(s"x = e.processBundleToContext /patternNotMatching/ com.glassbeam.context.ProcessContext$$ParsableClass", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = ProcessBundleToContext.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("x") shouldBe ""
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }
}

class ProcessFileToContextTestSpec extends FlatSpec with Matchers with dummyRefs {

  val file = new File("/tmp/processFileToCtx.txt")
  "ProcessFileToContext" should s"provide values ${file.toString} for x" in {
    val cca = LoaderClassArguments(s"x = e.processFileToContext /$file/ com.glassbeam.context.ProcessContext$$ParsableClass", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = ProcessFileToContext.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("x") shouldBe file.toString
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }

  it should "provide empty string as value for x" in {
    val cca = LoaderClassArguments(s"x = e.processFileToContext /patternNotMatching/ com.glassbeam.context.ProcessContext$$ParsableClass", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, file, -1,"mps")

    val responseContextReason = ProcessFileToContext.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("x") shouldBe ""
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }
}

class CombineLinesTestSpec extends FlatSpec with Matchers with dummyRefs {

  val regex1 = "regex1"
  val regex2 = "regex2"
  val concatDelim = "\"concat_delim\""
  val replaceDelim = "\"replace_delim\""
  val pattern1 = s"combinelines($regex1, $regex2, $concatDelim , $replaceDelim)"

  "Combine lines" should "combine given lines" in {
    val cca = LoaderClassArguments(pattern1, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, null, -1,"mps")

    val responseContextReason = CombineLines.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("combinelines") shouldBe s"$regex1,$regex2,$concatDelim,$replaceDelim"
    responseContextReason.reason shouldBe empty
    responseContextReason.failure shouldBe None
  }
}

class AssertPxFileCountTestSpec extends FlatSpec with Matchers with MockFactory with dummyRefs {

  val cr = ContextReason(HashMap[String, String]("sysid" -> "glassbeam", "mfr" -> "dummyMfr", "node" -> "Sol42"), "")
  val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
  val onFail = mockFunction[Long, String, Unit]

  def getcca(context: String) = LoaderClassArguments(context, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")

  def getapx(ctxLine: String, getPxCount: Long => Option[Long]) = {
    new AssertPxFileCount(getcca(ctxLine), getPxCount) {
      override def onFailure(loadId: Long, error_stmt: String) = onFail(loadId, error_stmt)
    }
  }

  "AssertPxFileCount with one input (max pxcount)" should "succeed for equal pxcount" in {
    val ctxLine = "b.assertPxFileCount(10)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(10L)

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    responseContextReason.reason shouldBe empty
  }

  it should "succeed for less pxcount" in {
    val ctxLine = "b.assertPxFileCount(10)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    responseContextReason.reason shouldBe empty
  }

  it should "fail for more pxcount" in {
    val ctxLine = "b.assertPxFileCount(10)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
  }

  /* // Regex match will fail in this case
  it should "fail for non-numeric pxcount" in {
    val ctxLine = "b.assertPxFileCount(abc)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
  }*/

  // Input params: pxcount, email templateid
  "AssertPxFileCount with two inputs (max pxcount and email templateid)" should "extract Email templateId from input 'b.assertPxFileCount(10, 99)'" in {
    val ctxLine = "b.assertPxFileCount(10, 99)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }

  it should "extract Email templateId from input 'b.assertPxFileCount (  10  , 99 ) '" in {
    val ctxLine = "b.assertPxFileCount (  10  , 99 ) "
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99)
  }

  it should "succeed for less pxcount with Email templateId: 'b.assertPxFileCount(10, 99)'" in {
    val ctxLine = "b.assertPxFileCount(10, 99)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    responseContextReason.reason shouldBe empty
  }

  it should "fail for more pxcount with Email templateId: 'b.assertPxFileCount(10, 99)'" in {
    val ctxLine = "b.assertPxFileCount(10, 99)"
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
  }

  // Input params: pxcount, email template, custom msg with/without context substitutions
  "AssertPxFileCount with three inputs (max pxcount, email templateId, custom msg)" should """extract Email templateId AND custom message from input 'b.assertPxFileCount(10, 99, "Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, 99, "Greetings, Earthlings!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }

  it should """extract Email templateId AND custom message from input 'b.assertPxFileCount  (  10  , 99  , "Greetings, Earthlings! "   )  '""" in {
    val ctxLine = """b.assertPxFileCount  (  10  , 99  , "Greetings, Earthlings! "   )  """
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 && 
      clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings! \"")
  }

  it should """extract custom message with context var from input 'b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings of system {context.node}!\"")
  }

  it should """succeed for less pxcount with Email templateId and Custom Msg: 'b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    responseContextReason.reason shouldBe empty
  }

  it should """fail for more pxcount with Email templateId and Custom Msg (with Reason==Custom msg): 'b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, 99, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(responseContextReason.failure.isDefined && responseContextReason.reason == "\"Greetings, Earthlings of system Sol42!\"") // Has no newline char in code
  }

  it should """fail for more pxcount but with empty reason IF custom msg is empty: 'b.assertPxFileCount(10, 99, )'""" in {
    val ctxLine = """b.assertPxFileCount(10, 99, )"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(responseContextReason.failure.isDefined && responseContextReason.reason == "") // Has no newline char in code
  }

  // Input params: pxcount, custom msg with/without context substitutions
  "AssertPxFileCount with two inputs (max pxcount, custom msg)" should """extract custom message from input 'b.assertPxFileCount(10, "Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, "Greetings, Earthlings!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\"")
  }

  it should """extract custom message from input 'b.assertPxFileCount  (  10  , "Greetings, Earthlings! "   )  '""" in {
    val ctxLine = """b.assertPxFileCount  (  10  , "Greetings, Earthlings! "   )  """
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings! \"")
  }

  it should """extract custom message with context var from input 'b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val clsAssert = getapx(ctxLine, getPxCount)
    clsAssert.execute(cefa) // DELETE
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings of system {context.node}!\"")
  }

  it should """succeed for less pxcount with Email templateId and Custom Msg: 'b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(8L)

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    responseContextReason.reason shouldBe empty
  }

  it should """fail for more pxcount with Email templateId and Custom Msg (with Reason==Custom msg): 'b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")'""" in {
    val ctxLine = """b.assertPxFileCount(10, "Greetings, Earthlings of system {context.node}!")"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(responseContextReason.failure.isDefined && responseContextReason.reason == "\"Greetings, Earthlings of system Sol42!\"") // Has no newline char in code
  }

  it should """fail for more pxcount but with empty Reason if Custom Msg is empty: 'b.assertPxFileCount(10,)'""" in {
    val ctxLine = """b.assertPxFileCount(10,)"""
    val getPxCount = mockFunction[Long, Option[Long]]
    getPxCount expects 1L returning Some(11L)
    onFail expects(*, *) once

    val responseContextReason = getapx(ctxLine, getPxCount).execute(cefa)
    assert(responseContextReason.failure.isDefined && responseContextReason.reason == "") // Has no newline char in code
  }
}

class AssertBundleDuplicateTestSpec extends FlatSpec with Matchers with dummyRefs {

  val dummyCR = ContextReason(HashMap[String, String]("sysid" -> "", "mfr" -> "dummyMfr", "node" -> "Sol42"), "")
  val cefa = LoaderEvalArguments(dummyCR, null, 1L,"mps")

  def getcca(context: String) = LoaderClassArguments(context, 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")

  "AssertBundleDuplicate with no inputs" should "succeed" in {
    val ctxLine = "b.assertBundleDuplicate"
    val ctxReason = AssertBundleDuplicate.getObject(getcca(ctxLine)).execute(cefa)
    ctxReason.contextStrings("assertBundleDuplicate") shouldBe "exists"
    ctxReason.reason shouldBe empty
  }

  "AssertBundleDuplicate with one input (email templateId)" should "succeed" in {
    val ctxLine = "b.assertBundleDuplicate(99)"
    val ctxReason = AssertBundleDuplicate.getObject(getcca(ctxLine)).execute(cefa)
    ctxReason.contextStrings("assertBundleDuplicate") shouldBe "exists"
    ctxReason.reason shouldBe empty
  }

  it should "extract Email templateId from input 'b.assertBundleDuplicate(99)'" in {
    val ctxLine = "b.assertBundleDuplicate(99)"

    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalTemplateId.isDefined 
      && clsAssert.assertOptionalTemplateId.get == 99 
      && ctxReason.contextStrings("assertBundleDuplicateOptionalTemplate") == "99")
  }

  it should "extract Email templateId from input 'b.assertBundleDuplicate( 99 )  '" in {
    val ctxLine = "b.assertBundleDuplicate( 99 )  "
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalTemplateId.isDefined 
      && clsAssert.assertOptionalTemplateId.get == 99 
      && ctxReason.contextStrings("assertBundleDuplicateOptionalTemplate") == "99")
  }

  "AssertBundleDuplicate with two inputs (email templateId and custom msg)" should "succeed" in {
    val ctxLine = """b.assertBundleDuplicate(99, "Greetings, Earthlings!")"""
    val ctxReason = AssertBundleDuplicate.getObject(getcca(ctxLine)).execute(cefa)
    ctxReason.contextStrings("assertBundleDuplicate") shouldBe "exists"
    ctxReason.reason shouldBe empty
  }

  it should """extract Email templateId AND custom msg from input 'b.assertBundleDuplicate(99, "Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertBundleDuplicate(99, "Greetings, Earthlings!")"""
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 
      && ctxReason.contextStrings("assertBundleDuplicateOptionalTemplate") == "99"
      && clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\""
      && ctxReason.contextStrings("assertBundleDuplicateOptionalMsg") == "\"Greetings, Earthlings!\"")
  }

  it should """extract Email templateId AND custom msg from input 'b.assertBundleDuplicate (  99 , "Greetings, Earthlings! ")'""" in {
    val ctxLine = """b.assertBundleDuplicate (  99 , "Greetings, Earthlings! ")"""
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalTemplateId.isDefined && clsAssert.assertOptionalTemplateId.get == 99 
      && ctxReason.contextStrings("assertBundleDuplicateOptionalTemplate") == "99"
      && clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings! \""
      && ctxReason.contextStrings("assertBundleDuplicateOptionalMsg") == "\"Greetings, Earthlings! \"")
  }

  "AssertBundleDuplicate with one input (custom msg)" should "succeed" in {
    val ctxLine = """b.assertBundleDuplicate("Greetings, Earthlings!")"""
    val ctxReason = AssertBundleDuplicate.getObject(getcca(ctxLine)).execute(cefa)
    ctxReason.contextStrings("assertBundleDuplicate") shouldBe "exists"
    ctxReason.reason shouldBe empty
  }

  it should """extract custom msg from input 'b.assertBundleDuplicate("Greetings, Earthlings!")'""" in {
    val ctxLine = """b.assertBundleDuplicate("Greetings, Earthlings!")"""
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings!\""
      && ctxReason.contextStrings("assertBundleDuplicateOptionalMsg") == "\"Greetings, Earthlings!\"")
  }

  it should """extract custom msg from input 'b.assertBundleDuplicate ( "Greetings, Earthlings! ")'""" in {
    val ctxLine = """b.assertBundleDuplicate ( "Greetings, Earthlings! ")"""
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == "\"Greetings, Earthlings! \""
      && ctxReason.contextStrings("assertBundleDuplicateOptionalMsg") == "\"Greetings, Earthlings! \"")
  }

  it should """extract empty custom msg from input 'b.assertBundleDuplicate( )'""" in {
    val ctxLine = """b.assertBundleDuplicate( )"""
    val clsAssert = AssertBundleDuplicate.getObject(getcca(ctxLine))
    val ctxReason = clsAssert.execute(cefa)
    assert(clsAssert.assertOptionalMsg.isDefined && clsAssert.assertOptionalMsg.get == ""
      && ctxReason.contextStrings("assertBundleDuplicateOptionalMsg") == "")
  }
}

class BPropertiesTestSpec extends FlatSpec with Matchers with dummyRefs {
  "BProperties" should "produce proper value for b.properties" in {
    val cca = LoaderClassArguments("b.properties(propa,propb,propc)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String]("propa" -> "valuea", "propb" -> "valueb", "propc" -> "valuec"), "")
    val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
    val responseContextReason = BProperties.getObject(cca).execute(cefa)
    responseContextReason.contextStrings("b.properties") shouldBe """{"0":{"key":"propa","value":"valuea"},"1":{"key":"propb","value":"valueb"},"2":{"key":"propc","value":"valuec"}}"""
    responseContextReason.reason shouldBe empty
  }
  it should "return context reason without b.properties" in {
    val cca = LoaderClassArguments("b.properties()", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String]("propa" -> "valuea", "propb" -> "valueb", "propc" -> "valuec"), "")
    val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
    val responseContextReason = BProperties.getObject(cca).execute(cefa)
    responseContextReason.contextStrings.contains("b.properties") shouldBe false
    responseContextReason.reason shouldBe empty
  }
  it should "fail with reason of missing key " in {
    val cca = LoaderClassArguments("b.properties(propa,propb,propc)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap.empty[String, String], "")
    val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
    val responseContextReason = BProperties.getObject(cca).execute(cefa)
    assert(!responseContextReason.reason.isEmpty)
  }

  it should "fail with reason empty value" in {
    val cca = LoaderClassArguments("b.properties(propa,propb,propc)", 1, "dummyCust", "dummymanufacturer", "dummyProduct", "dummySchema")
    val cr = ContextReason(contextStrings = HashMap[String, String]("propa" -> "", "propb" -> "valueb", "propc" -> "valuec"), "")
    val cefa = LoaderEvalArguments(cr, null, 1L,"mps")
    val responseContextReason = BProperties.getObject(cca).execute(cefa)
    assert(responseContextReason.reason.isEmpty)
  }
}

//object ConfigSetup {
//  lazy val init = TestContext.initStartupConfig(TestConfig.lCPConfigArgs.toArray)
//}

class ContextFunctionSuite extends Suites(
  new AssertSpec,
  new AssertUncompressionFailSpec,
  new AssertFileDuplicateSpec,
  new AssertTruthySpec,
  new LSchemaSpec,
  new LoaderTestSpec,
  new FDateSpec,
  new ValidateTestSpec,
  new BSizeTestSpec,
  new BIdTestSpec,
  new FcountTestSpec,
  new BnameTestSpec,
  new LookupTestSpec,
  new MGrepTestSpec,
  new XMLValueTestSpec,
  new ProcessBundleToContextTestSpec,
  new ProcessFileToContextTestSpec,
  new AssertPxFileCountTestSpec,
  new AssertBundleDuplicateTestSpec,
  new BPropertiesTestSpec,
  new CombineLinesTestSpec) //removing ParallelTestExecution as it doesn't work correctly

