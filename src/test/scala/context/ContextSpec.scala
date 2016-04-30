package com.glassbeam.context

import org.scalatest.{ParallelTestExecution, Suites}

class ContextSuite extends Suites(new CachedContextSuite, new ContextCheckTestSuite, new  ContextFunctionSuite, new AssertBundleFailSpec) with ParallelTestExecution