package com.wavesenterprise.utils

import scala.util.control.NonFatal

/**
  * Remove after migration to Scala 2.13
  */
object ResourceUtils extends ScorexLogging {
  def withResource[T <: AutoCloseable, V](r: => T)(f: T => V): V = {
    val resource: T = r
    require(resource != null, "resource is null")
    var exception: Throwable = null
    try {
      f(resource)
    } catch {
      case NonFatal(e) =>
        exception = e
        throw e
    } finally {
      closeAndAddSuppressed(exception, resource)
    }
  }

  private def closeAndAddSuppressed(e: Throwable, resource: AutoCloseable): Unit = {
    if (e != null) {
      try {
        resource.close()
      } catch {
        case NonFatal(suppressed) =>
          e.addSuppressed(suppressed)
        case fatal: Throwable if NonFatal(e) =>
          fatal.addSuppressed(e)
          throw fatal
        case fatal: InterruptedException =>
          fatal.addSuppressed(e)
          throw fatal
        case fatal: Throwable =>
          e.addSuppressed(fatal)
      }
    } else {
      resource.close()
    }
  }

  def closeQuietly(closeable: AutoCloseable): Unit = {
    try closeable.close()
    catch {
      case NonFatal(e) =>
        log.warn("Exception is thrown while closing", e)
    }
  }
}
