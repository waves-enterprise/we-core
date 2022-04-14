package com.wavesenterprise.utils

import java.lang
import java.net.{InetAddress, SocketTimeoutException}

import monix.eval.Task
import monix.execution.{Cancelable, Scheduler}
import monix.reactive.Observable
import org.apache.commons.net.ntp.NTPUDPClient

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Random
import scala.util.control.NonFatal

trait Time {
  def correctedTime(): Long

  def getTimestamp(): Long
}

sealed trait NtpMode
object NtpMode {
  case object Normal                                           extends NtpMode
  case class Sensitive(fatalExpirationTimeout: FiniteDuration) extends NtpMode
}

case class NTP(servers: Seq[String],
               mode: NtpMode = NtpMode.Normal,
               requestTimeout: FiniteDuration = 10.seconds,
               expirationTimeout: FiniteDuration = 60.seconds)(implicit val scheduler: Scheduler)
    extends Time
    with ScorexLogging
    with AutoCloseable {

  private val ServerSwitchDelay    = 5.seconds
  private val OffsetPanicThreshold = 1000000L

  @volatile private var lastUpdate = System.nanoTime()
  @volatile private var offset     = 0L

  private val client = new NTPUDPClient()
  client.setDefaultTimeout(requestTimeout.toMillis.toInt)

  private def isFatalExpired: Boolean = mode match {
    case NtpMode.Normal             => false
    case NtpMode.Sensitive(timeout) => System.nanoTime() - lastUpdate > timeout.toNanos
  }

  private def updateOffset(server: String): Task[Option[lang.Long]] = {
    Task
      .eval {
        try {
          val address = InetAddress.getByName(server)
          client.open()
          val info = client.getTime(address)
          info.computeDetails()
          Option(info.getOffset).map { newOffset =>
            newOffset.ensuring(Math.abs(_) < OffsetPanicThreshold, "Offset is suspiciously large")
          }
        } catch {
          case NonFatal(ex) =>
            mode match {
              case NtpMode.Sensitive(timeout) if isFatalExpired =>
                log.warn(
                  s"""Problems with NTP '$server': fatal timeout '$timeout' expired.
                     |Working NTP synchronization is crucial for the node to run correctly!""".stripMargin,
                  ex
                )

              case _ if ex.isInstanceOf[SocketTimeoutException] =>
                log.debug(s"Problems with NTP '$server': response timeout ('$requestTimeout').", ex)

              case _ =>
                log.debug(s"Problems with NTP '$server':", ex)
            }

            None
        } finally {
          client.close()
        }
      }
      .flatMap {
        case Some(newOffset) =>
          log.debug(s"Adjusting time with '$newOffset' milliseconds, source: '$server'")
          offset = newOffset
          lastUpdate = System.nanoTime()
          updateOffset(server).delayExecution(expirationTimeout)
        case None =>
          Task.pure(None)
      }
  }

  private def shuffledServers = Random.shuffle(servers)

  private val syncProcess: Cancelable = {
    Observable
      .repeat(shuffledServers: _*)
      .doOnNext(server => Task(log.info(s"Used '$server' NTP server")))
      .mapEval(updateOffset)
      .delayOnNext(ServerSwitchDelay)
      .doOnComplete(Task(log.info("NTP sync stopped")))
      .logErr
      .onErrorRestartUnlimited
      .executeOn(scheduler)
      .subscribe()
  }

  override def correctedTime(): Long = System.currentTimeMillis() + offset

  @volatile private var txTime: Long = 0

  def getTimestamp(): Long = {
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }

  override def close(): Unit = syncProcess.cancel()
}
