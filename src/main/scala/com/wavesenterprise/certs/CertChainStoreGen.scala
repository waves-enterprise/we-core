package com.wavesenterprise.certs

import com.wavesenterprise.lang.EitherExt
import org.bouncycastle.asn1.x500.X500Name
import org.scalacheck.Gen

import java.security.PrivateKey
import java.security.cert.X509Certificate
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

trait CertChainStoreGen extends TestCertBuilder {

  @tailrec
  private def buildChain(name: String,
                         chainId: Int,
                         deep: Int,
                         parentCert: X509Certificate,
                         issuerPrivateKey: PrivateKey,
                         certs: List[X509Certificate]): List[X509Certificate] = {
    if (deep == 0) {
      certs
    } else {
      val keyPair = keypairGenerator.generateKeyPair()
      val cert =
        generateCert(new X500Name(parentCert.getSubjectX500Principal.getName), issuerPrivateKey, keyPair.getPublic, s"${name}_${chainId}_$deep")
      buildChain(name, chainId, deep - 1, cert, keyPair.getPrivate, cert :: certs)
    }
  }

  private def buildChain(name: String, chainId: Int, deep: Int): List[X509Certificate] = {
    val caKeyPair = keypairGenerator.generateKeyPair()
    val caCert    = generateSelfSignedCert(caKeyPair, s"${name}_${chainId}_CA")
    buildChain(name, chainId, deep, caCert, caKeyPair.getPrivate, List(caCert))
  }

  protected def certsForChainGen(chainId: Int = 1): Gen[List[X509Certificate]] = {
    for {
      depth <- Gen.chooseNum(1, 5)
      name  <- Gen.alphaStr.filter(_.nonEmpty)
    } yield {
      buildChain(name, chainId, depth)
    }
  }

  protected val certChainStoreGen: Gen[CertChainStore] =
    Gen.chooseNum(0, 7).flatMap { num =>
      Gen.sequence {
        (1 to num).map(certsForChainGen)
      }
    } map { certs =>
      CertChainStore.fromCertificates(certs.asScala.flatten).explicitGet()
    }
}
