package com.wavesenterprise

import java.io.{File, PrintWriter}

package object wavesenterprise {

  def writeTextFile(path: File, content: String): File = {
    val writer = new PrintWriter(path)
    try writer.write(content)
    finally writer.close()
    path
  }
}