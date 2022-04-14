package com.wavesenterprise
import com.wavesenterprise.utils.NTP
import org.scalatest.{BeforeAndAfterAll, Suite}

trait NTPTime extends BeforeAndAfterAll { _: Suite =>
  protected val ntpTime = NTP(Seq("pool.ntp.org"))(monix.execution.Scheduler.global)

  override protected def afterAll(): Unit = {
    super.afterAll()
    ntpTime.close()
  }
}
