package com.glassbeam.context
import java.io.{File, PrintWriter}
import java.nio.file.Paths

import org.apache.commons.io.FileUtils

/**
  * Created by narayana on 29/4/16.
  */
object TestHelpers {

  implicit class stringToFile(val filePath: String) extends AnyVal {
    def toFile = Paths.get(filePath).normalize().toAbsolutePath.toFile
  }

  implicit class writeToFile(val filePath: File) extends AnyVal {
    def write(content: String) = {
      filePath.delete()
      FileUtils.touch(filePath)
      val p = new PrintWriter(filePath)
      p.print(content)
      p.close()
    }
  }
}
