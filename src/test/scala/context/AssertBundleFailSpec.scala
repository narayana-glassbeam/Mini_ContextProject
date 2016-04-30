package com.glassbeam.context

import java.io.File

import com.glassbeam.model.ContextFailure
import org.scalatest.{FeatureSpec, Matchers}

import scala.collection.immutable.HashMap

class AssertBundleFailSpec extends FeatureSpec with Matchers {

  val ctxString = "b.assertBundleFail(sysid,message,123)"
  val cca = ContextClassArguments(ctxString, 1, "", "", "", "")
  val cr = ContextReason(HashMap[String, String](), "")
  val cefa = ContextExecFnArguments(cr, new File(""), 1234)
  val assertBundleFail = AssertBundleFail.getObject(cca)


  // TODO - re-phrase the text for each feature and scenario

  feature("invoke  assertbundleFail with 3 arguments") {

    scenario("look for the first arg and see if the bundle could be processed") {
      val assertBundle = assertBundleFail.execute(cefa)
      assertBundle.failure shouldBe Some(ContextFailure.AssertBundleFailure)
    }

    scenario("when context is executed with Execution Args") {
      val crMap = HashMap[String,String]("sysid" -> "1234")
      val cr = ContextReason(crMap, "")
      val cefa = ContextExecFnArguments(cr, new File(""), 1234)
      val assertBundle = assertBundleFail.execute(cefa)
      assertBundle.failure should be(None)
    }

    scenario("create a message body using the $variables defined in the templates") {
      val assertBundle = assertBundleFail.execute(cefa)
      assertBundle.failure shouldBe Some(ContextFailure.AssertBundleFailure)
    }

    scenario("should just record the errors and not send mail when no template id is given"){
      val ctxString = "b.assertBundleFail(sysid,message)"
      val cca = ContextClassArguments(ctxString, 1, "", "", "", "")
      val cefa = ContextExecFnArguments(cr, new File(""), 1234)
      val assertBundle = assertBundleFail.execute(cefa)
      assertBundle.failure shouldBe Some(ContextFailure.AssertBundleFailure)
      // Verify Mock Email actor should not get called
    }

    scenario("should just record the errors and not send mail when both message template and message String is given") {
      val ctxString = "b.assertBundleFail(sysid)"
      val cca = ContextClassArguments(ctxString, 1, "", "", "", "")
      val cefa = ContextExecFnArguments(cr, new File(""), 1234)
      val assertBundle = assertBundleFail.execute(cefa)
      assertBundle.failure shouldBe Some(ContextFailure.AssertBundleFailure)
      // Verify Mock Email actor should not get called
    }

  }

}
