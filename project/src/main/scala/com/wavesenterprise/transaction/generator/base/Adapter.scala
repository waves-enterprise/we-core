package src.main.scala.com.wavesenterprise.transaction.generator.base

import com.wavesenterprise.transaction.TxScheme
import com.wavesenterprise.transaction.generator.base.CodeWriter

trait Adapter {
  def packageName: String
  def objectName: String
  def buildWriter(schemes: Seq[TxScheme]): CodeWriter
}
