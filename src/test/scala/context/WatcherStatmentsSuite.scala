package com.glassbeam.context

import com.glassbeam.context.Context.{ContextClassArguments, MatchArguments, WatcherEvalArguments}
import com.glassbeam.context.ContextSection.ContextSection
import com.glassbeam.context.Dummy.dummyRefs
import org.scalatest.{FlatSpec, Matchers, Suites}

object WatcherStatementsData {

  //WATCHER STATEMENT PATTERNS
  val deletePattern = "f.deletefiles=.*/var/log/oslog/tech-support.log".trim
  val skipPattern = "f.skipfiles=.*/var/log/oslog/tech-support.log".trim
  val includeVaultPattern = "f.includevault=.*/var/log/oslog/tech-support.log".trim
  val includeParsePattern = "f.includeparse=.*/var/log/oslog/tech-support.log".trim
  val binaryFilePattern = "f.binary=.*/var/log/oslog/tech-support.log".trim
  val textFilePattern = "f.text=.*/var/log/oslog/tech-support.log".trim
  val reverseFilePattern = "f.reversefiles=.*/var/log/oslog/tech-support.log".trim
  val mostRecentPattern = "f.selectFile(/var/log/oslog/tech-support.log)".trim
  val processFilePattern = "xyz=e.processToFile /var/log/oslog/tech-support.log/ com.glassbeam.aruba.Aruba_rconf".trim
  val unCompressPattern = "b.uncompress.level /(.+?)/ 20".trim

  val file_name = "/var/log/oslog/tech-support.log"
  val processFileName = ""
  val mps = "watcher/statements/test"
  val loadid = 1234

  val mutableSection:ContextSection = ContextSection.MutableState
  val immutableSection:ContextSection = ContextSection.ImmutableState

  val manufacturer = "manufacturer"
  val product = "product"
  val schema = "schema"
  val customer = "customer"
  val emps = "emps"

  def getContextClassArguments(contextLine:String) = ContextClassArguments(contextLine,1,customer,manufacturer,product,schema)
  def getWatcherEvalArguments(filename:String) = WatcherEvalArguments(filename,emps)



}

class DeletePatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "DeletePattern " should s" return true for file $file_name with mutable section " in {
//    val file = new File(file_name)
//    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(deletePattern)
    val file_matched = MatchArguments(deletePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" watcher pattern found for "+wp.name)
       WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"delete file pattern file matched ${file_matched}")
    assert(file_matched)
  }

  "Delete Pattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(deletePattern)
    val file_matched = MatchArguments(deletePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" watcher pattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"delete file pattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "Delete Pattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(deletePattern)
    val file_matched = MatchArguments(deletePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" watcher pattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"delete file pattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class SkipPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "SkipPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(skipPattern)
    val file_matched = MatchArguments(skipPattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" SkipPattern  found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"SkipPattern  file matched ${file_matched}")
    assert(file_matched)
  }

  "SkipPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(skipPattern)
    val file_matched = MatchArguments(skipPattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" SkipPattern  found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"SkipPattern  file matched ${file_matched}")
    assert(!file_matched)
  }

  "SkipPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(skipPattern)
    val file_matched = MatchArguments(skipPattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" watcher pattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"SkipPattern  file matched ${file_matched}")
    assert(!file_matched)
  }

}

class IncludeVaultPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "IncludeVaultPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(includeVaultPattern)
    val file_matched = MatchArguments(includeVaultPattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" IncludeVaultPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeVaultPattern file matched ${file_matched}")
    assert(file_matched)
  }

  "IncludeVaultPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(includeVaultPattern)
    val file_matched = MatchArguments(includeVaultPattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" \"IncludeVaultPattern \" found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeVaultPattern  file matched ${file_matched}")
    assert(!file_matched)
  }

  "IncludeVaultPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(includeVaultPattern)
    val file_matched = MatchArguments(includeVaultPattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" IncludeVaultPattern  found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeVaultPattern  file matched ${file_matched}")
    assert(!file_matched)
  }

}

class IncludeParsePatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "IncludeParsePattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(includeParsePattern)
    val file_matched = MatchArguments(includeParsePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" IncludeParsePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeParsePattern file matched ${file_matched}")
    assert(file_matched)
  }

  "IncludeParsePattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(includeParsePattern)
    val file_matched = MatchArguments(includeParsePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" IncludeParsePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeParsePattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "IncludeParsePattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(includeParsePattern)
    val file_matched = MatchArguments(includeParsePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" IncludeParsePattern for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"IncludeParsePattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class BinaryPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "BinaryPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(binaryFilePattern)
    val file_matched = MatchArguments(binaryFilePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println("BinaryPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"BinaryPattern file matched ${file_matched}")
    assert(file_matched)
  }

  "BinaryPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(binaryFilePattern)
    val file_matched = MatchArguments(binaryFilePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" BinaryPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"BinaryPattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "BinaryPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(binaryFilePattern)
    val file_matched = MatchArguments(binaryFilePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println("BinaryPattern for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"BinaryPattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class TextPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "TextPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(textFilePattern)
    val file_matched = MatchArguments(textFilePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" TextPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"TextPattern file matched ${file_matched}")
    assert(file_matched)
  }

  "TextPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(textFilePattern)
    val file_matched = MatchArguments(textFilePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println("TextPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"TextPattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "TextPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(textFilePattern)
    val file_matched = MatchArguments(textFilePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" TextPattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"TextPattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class ReversePatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "ReversePattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(reverseFilePattern)
    val file_matched = MatchArguments(reverseFilePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println("ReversePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ReversePattern file matched ${file_matched}")
    assert(file_matched)
  }

  "ReversePattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(reverseFilePattern)
    val file_matched = MatchArguments(reverseFilePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println(" ReversePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ReversePattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "ReversePattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(reverseFilePattern)
    val file_matched = MatchArguments(reverseFilePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" ReversePattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ReversePattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class MostRecentPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "MostRecentPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(mostRecentPattern)
    val file_matched = MatchArguments(mostRecentPattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" MostRecentPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"MostRecentPattern file matched ${file_matched}")
    assert(file_matched)
  }

  "MostRecentPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(mostRecentPattern)
    val file_matched = MatchArguments(mostRecentPattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println("MostRecentPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"MostRecentPattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "MostRecentPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(mostRecentPattern)
    val file_matched = MatchArguments(mostRecentPattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println("MostRecentPattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"MostRecentPattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class ProcessFilePatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "ProcessFilePattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(processFilePattern)
    val file_matched = MatchArguments(processFilePattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" ProcessFilePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ProcessFilePattern file matched ${file_matched}")
    assert(file_matched)
  }

  "ProcessFilePattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(processFilePattern)
    val file_matched = MatchArguments(processFilePattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println("ProcessFilePattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ProcessFilePattern file matched ${file_matched}")
    assert(!file_matched)
  }

  "ProcessFilePattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(processFilePattern)
    val file_matched = MatchArguments(processFilePattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" ProcessFilePattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalFileMatchesPattern(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
        false
    }
    println(s"ProcessFilePattern file matched ${file_matched}")
    assert(!file_matched)
  }

}

class UncompressPatternSpec extends FlatSpec with Matchers with dummyRefs {
  import WatcherStatementsData._

  "UncompressPattern " should s" return true for file $file_name with mutable section " in {
    //    val file = new File(file_name)
    //    file.write("This is test file for delete pattern ")
    val cca = getContextClassArguments(unCompressPattern)
    val bundle_depth = MatchArguments(unCompressPattern,mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" UncompressPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalUncompressLevel(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        0
    }
    println(s"UncompressPattern file matched ${bundle_depth}")
    assert(bundle_depth == 20)
  }

  "UncompressPattern " should s" return fail for file name ${file_name} with immutable section " in {
    val cca = getContextClassArguments(unCompressPattern)
    val bundle_depth = MatchArguments(unCompressPattern,immutableSection) match {
      case WatcherStatements(wp)  =>
        println("UncompressPattern found for "+wp.name)
        WatcherStatements.getObject(cca,wp).evalUncompressLevel(getWatcherEvalArguments(file_name))
      case _ =>
        println(" match not found ")
        0
    }
    println(s"UncompressPattern file matched ${bundle_depth}")
    assert(bundle_depth == 0)
  }

  "UncompressPattern " should s" return fail for wrong file name with mutable section " in  {
    val cca = getContextClassArguments(unCompressPattern)
    val bundle_depth = MatchArguments(unCompressPattern, mutableSection) match {
      case WatcherStatements(wp)  =>
        println(" UncompressPattern found for wrong file name "+wp.name)
        WatcherStatements.getObject(cca,wp).evalUncompressLevel(getWatcherEvalArguments("/var/test/tech-support.log"))
      case _ =>
        println(" match not found ")
       0
    }
    println(s"UncompressPattern file matched ${bundle_depth}")
    assert(bundle_depth == 20)
  }

}

class WatcherStatmentsSuite extends Suites(new DeletePatternSpec,new SkipPatternSpec,
  new IncludeParsePatternSpec,new IncludeVaultPatternSpec,new BinaryPatternSpec,new TextPatternSpec,new UncompressPatternSpec,new ProcessFilePatternSpec,
  new MostRecentPatternSpec,new ReversePatternSpec)