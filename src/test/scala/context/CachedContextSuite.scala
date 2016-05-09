package com.glassbeam.context

import java.io.File

import com.glassbeam.context.ContextHelpers._
import com.glassbeam.context.BfnameGrep.{BFnameGrepKey, BFnameGrepValue}
import com.glassbeam.context.Bgrep.{BGrepKey, BGrepValue}
import com.glassbeam.context.Context.{ContextReason, LoaderEvalArguments}
//import com.glassbeam.context.Context.BundleCacheKey
import com.google.common.cache.{Cache, CacheBuilder}
import com.typesafe.config.ConfigFactory
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest._

import scala.collection.immutable.HashMap
import scala.language.postfixOps
import scala.util.Random


class BundleFunctionsTest extends FeatureSpec with GivenWhenThen with Matchers with ContextTest with MockFactory
with GeneratorDrivenPropertyChecks  with ParallelTestExecution {

  import com.glassbeam.context.TestHelpers._

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(workers = 2)

  val folder: String = BundleFunctionsTest.directoryToCreateFiles

  feature("b.grep caching functionality ") {
    val bGrepSamples = Gen.oneOf(BundleFunctionsTest.bGrepSamples)

    scenario("Evaluate BGrep") {
      val cache: Cache[BGrepKey, BGrepValue] = CacheBuilder.newBuilder().build()

      // prepare mock
      val bGrepFnObtainer = {
        val fileNameObtainer = mockFunction[Long, Seq[String]]("bGrepFnObtainer")
        BundleFunctionsTest.bGrepSamples foreach { sample =>
          if (sample.matchedContent.isDefined) {
            fileNameObtainer expects sample.loadId returning {
              Seq(s"$folder${sample.fileNamePrefix}${sample.fileToCreate}")
            } once
          }
        }
        fileNameObtainer
      }

      forAll(bGrepSamples) { sample =>
        val fileName = s"$folder${sample.fileNamePrefix}${sample.fileToCreate}"
        if (fileName.isEmpty) cancel("File Name provided cannot be Empty")
        val file: File = fileName toFile
        val matchedContent = sample.matchedContent
        val contentToWrite: String = sample.contentToWrite
        val loadId = sample.loadId

        Given(s"context function ${sample.contextFunction} and Cache without required Element")
        val contextFunction: String = sample.contextFunction
        val cr = ContextReason(HashMap[String, String](), "")
        val ccabg = getContextLine(contextFunction)
        val cefa = LoaderEvalArguments(cr, file, loadId,"mps")
        val bkey = new BGrepKey(loadId, ccabg.linenum)

        And(s"content to match: $contentToWrite written to file ${file.getAbsolutePath}")
        file.write(contentToWrite)

        And(s"FileNames retreiver for loadId: $loadId provided")
        info("A mock function which fails the test when called more than once")
        info("Required to test caching functionality")
        val fileNameObtainer = bGrepFnObtainer

        And("Bgrep evaluator")
        val bgrep = new Bgrep(ccabg, cache) {
          override val bGrepCacheLoader = Bgrep.bGrepCacheLoader(cache)(fileNameObtainer) _
        }

        When("Bgrep function is evaluated")
        val newcr = bgrep.execute(cefa)

        Then("Obtained ContextReason must be valid")
        val value = newcr.contextStrings.get(contextFunction.split('=')(0))
        value shouldBe matchedContent
        newcr.failure.toString shouldBe sample.failure.toString
        newcr.reason.isEmpty shouldBe sample.reason.isEmpty
      }
    }
  }

  feature("b.fname.grep caching functionality ") {

    val bFnameGrepSamples = Gen.oneOf(BundleFunctionsTest.bFnameGrepSamples)

    scenario("Evaluate BFnameGrep") {
      val cache: Cache[BFnameGrepKey, BFnameGrepValue] = CacheBuilder.newBuilder().build()
      // prepare mock
      val bFnameGrepFnObtainer = {
        val fileNameObtainer = mockFunction[Long, Seq[String]]("bFnameGrepFnObtainer")
        BundleFunctionsTest.bFnameGrepSamples foreach { sample =>
          if (sample.matchedContent.isDefined) {
            fileNameObtainer expects sample.loadId returning {
              Seq(s"$folder${sample.fileNamePrefix}${sample.fileToCreate}")
            } once
          }
        }
        fileNameObtainer
      }

      forAll(bFnameGrepSamples) { sample =>
        val fileName = s"$folder${sample.fileNamePrefix}${sample.fileToCreate}"
        if (fileName.isEmpty) cancel("File Name provided cannot be Empty")
        val file: File = fileName toFile
        val matchedContent = sample.matchedContent
        val loadId = sample.loadId

        Given(s"context function ${sample.contextFunction} and Cache without required Element")
        val contextFunction: String = sample.contextFunction
        val cr = ContextReason(HashMap[String, String](), "")
        val ccabfng = getContextLine(contextFunction)
        val cefa = LoaderEvalArguments(cr, file, loadId,"mps")
        val bkey = BundleCacheKey(loadId, ccabfng.linenum)

        And(s"file ${file.getAbsolutePath} created")
        file.createNewFile()

        And(s"FileNames retriever for loadId: $loadId provided")
        info("A mock function which fails the test when called more than once")
        info("Required to test caching functionality")
        val fileNameObtainer = bFnameGrepFnObtainer

        And("BFnameGrep evaluator")
        val bfnamegrep = new BfnameGrep(ccabfng, cache) {
          override val bFnameGrepCacheLoader = BfnameGrep.bFnameGrepCacheLoader(fileNameObtainer) _
        }

        When("BFnameGrep function is evaluated")
        val newcr = bfnamegrep.execute(cefa)

        Then("Obtained ContextReason must be valid")
        val value = newcr.contextStrings.get(contextFunction.split('=')(0))
        value shouldBe matchedContent
        newcr.failure.toString shouldBe sample.failure.toString
        newcr.reason.isEmpty shouldBe sample.reason.isEmpty
      }
    }
  }

}

object BundleFunctionsTest {

  import net.ceedubs.ficus.Ficus._
  import net.ceedubs.ficus.readers.ArbitraryTypeReader._

  case class BGrepSample
  (loadId: Long, fileToCreate: String, contentToWrite: String, matchedContent: Option[String], contextFunction: String,
   reason: String = "", failure: Option[String] = None, fileNamePrefix: String = s"${Random.nextInt(10000)}/")

  case class BFnameGrepSample
  (loadId: Long, fileToCreate: String, matchedContent: Option[String], contextFunction: String, reason: String = "",
   failure: Option[String] = None, fileNamePrefix: String = s"${Random.nextInt(10000)}")

  val testConfig = ConfigFactory.load
  val directoryToCreateFiles = testConfig.getString("bundleFunction.directoryToCreateFiles") + "/"
  val bGrepSamples = testConfig.as[Seq[BGrepSample]]("b.grep.samples")
  val bFnameGrepSamples = testConfig.as[Seq[BFnameGrepSample]]("b.fname.grep.samples")

}

class CachedContextSuite extends Suites(new BundleFunctionsTest)