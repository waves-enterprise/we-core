package com.wavesenterprise.certs

import cats.implicits._
import com.wavesenterprise.crypto.internals.PkiError
import org.bouncycastle.asn1.x500.X500Name
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.security._
import java.security.cert.X509Certificate
import scala.collection.mutable
import scala.util.Random

class CertChainStoreSpec extends FreeSpec with Matchers with ScalaCheckPropertyChecks with CertChainStoreGen {

  "CertStore" - {
    "serialization round trip" in {
      forAll(certChainStoreGen) { chain =>
        val bytes         = chain.bytes
        val parsingResult = CertChainStore.fromBytes(bytes)
        parsingResult shouldBe 'right

        val Right((parsedChain, offset)) = parsingResult
        parsedChain shouldBe chain
        offset shouldBe bytes.length

        val (parsedChain2, offset2) = CertChainStore.fromBytesUnsafe(bytes)

        parsedChain shouldBe parsedChain2
        offset shouldBe offset2
      }
    }
  }

  "build CertStore" - {
    val caKeyPair                               = keypairGenerator.generateKeyPair()
    val clientKeyPair                           = keypairGenerator.generateKeyPair()
    val (caCerts, intermediateCerts, userCerts) = validCertChain(caKeyPair, clientKeyPair)
    val certificates                            = caCerts ++ intermediateCerts ++ userCerts
    val shuffledCerts                           = Random.shuffle(certificates)
    val maybeCertStore                          = CertChainStore.fromCertificates(shuffledCerts)

    "with valid CA and user certificates" in {
      maybeCertStore shouldBe 'right

      val certStore = maybeCertStore.right.get
      certStore.clientCertificates should contain theSameElementsAs userCerts.map(_.getSubjectX500Principal)
      certStore.caCertificates should contain theSameElementsAs caCerts.map(_.getSubjectX500Principal)
    }

    val certsByDN = mutable.HashMap.empty ++ certificates.map { cert =>
      cert.getSubjectX500Principal.getName -> cert
    }.toMap

    val certStore = maybeCertStore.right.get

    "with valid chains" in {
      val certFChain = certStore.getCertChain(certsByDN("CN=cF"))
      val certBChain = certStore.getCertChain(certsByDN("CN=cB"))
      val certDChain = certStore.getCertChain(certsByDN("CN=cD"))

      certFChain shouldBe Right(CertChain(certsByDN("CN=caB"), List(certsByDN("CN=cE")), certsByDN("CN=cF")))
      certBChain shouldBe Right(CertChain(certsByDN("CN=caA"), List(certsByDN("CN=cA")), certsByDN("CN=cB")))
      certDChain shouldBe Right(CertChain(certsByDN("CN=caA"), List("CN=cC", "CN=cA").map(certsByDN(_)), certsByDN("CN=cD")))
    }

    "ignore duplicated DNs" in {
      val certs = List(certsByDN("CN=cE"), certsByDN("CN=caB"), certsByDN("CN=cE"))
      CertChainStore.fromCertificates(certs) shouldBe Left(PkiError(s"Unable to build cert chain. Input certificates contain duplicated DNs"))
    }

    "fail on building chain starting intermediate cert" in {
      certStore.getCertChain(certsByDN("CN=cA")) shouldBe Left(PkiError(s"Unable to build cert chain starting from the intermediate cert 'CN=cA'"))
    }

    "add new client certificate" in {
      val certG = generateCert(new X500Name("CN=cE"), caKeyPair.getPrivate, clientKeyPair.getPublic, "cG")
      certStore.addCert(certG) shouldBe 'right
      certsByDN.put("CN=cG", certG)
      val certGChain = certStore.getCertChain(certG)
      certGChain shouldBe 'right
      certGChain.right.get shouldBe CertChain(certsByDN("CN=caB"), List(certsByDN("CN=cE")), certG)
    }

    "receive proper error on adding cert" in {
      val certH = generateCert(new X500Name("CN=cF"), caKeyPair.getPrivate, clientKeyPair.getPublic, "cH")
      certStore.addCert(certH) shouldBe Left(PkiError("Certificate 'CN=cH' is issued by a client certificate 'CN=cF' which is forbidden"))

      val certI = generateCert(new X500Name("CN=cX"), caKeyPair.getPrivate, clientKeyPair.getPublic, "cI")
      certStore.addCert(certI) shouldBe Left(PkiError(s"Issuer's certificate 'CN=cX' was not found in the CertStore"))
    }

    "remove certificates" in {
      certStore.removeCert(certsByDN("CN=cA")) shouldBe Left {
        PkiError(s"Unable to remove intermediate certificate 'CN=cA' which still has dependent certificates")
      }

      certStore.removeCert(certsByDN("CN=cG")) shouldBe 'right
      certStore.getCertChain(certsByDN("CN=cG")) shouldBe Left(PkiError("Certificate 'CN=cG' was not found in the CertChain"))
      certStore.getCertChain(certsByDN("CN=cF")) shouldBe Right(CertChain(certsByDN("CN=caB"), List(certsByDN("CN=cE")), certsByDN("CN=cF")))
      certsByDN.remove("CN=cG")

      certStore.removeCert(certsByDN("CN=cF")) shouldBe 'right
      certStore.getCertChain(certsByDN("CN=cF")) shouldBe Left(PkiError("Certificate 'CN=cF' was not found in the CertChain"))
      certStore.getCertChain(certsByDN("CN=cE")) shouldBe Left(PkiError("Certificate 'CN=cE' was not found in the CertChain"))
      certStore.getCertChain(certsByDN("CN=caB")) shouldBe Left(PkiError("Certificate 'CN=caB' was not found in the CertChain"))
      certsByDN.remove("CN=cF")
      certsByDN.remove("CN=cE")
      certsByDN.remove("CN=caB")
    }

    "add new chains" in {
      val caCertB = generateSelfSignedCert(caKeyPair, "caB")
      val certE   = generateCert(new X500Name(caCertB.getSubjectX500Principal.getName), caKeyPair.getPrivate, clientKeyPair.getPublic, "cE")
      val certF   = generateCert(new X500Name(certE.getSubjectX500Principal.getName), clientKeyPair.getPrivate, clientKeyPair.getPublic, "cF")
      val certH   = generateCert(new X500Name("CN=caA"), caKeyPair.getPrivate, clientKeyPair.getPublic, "cH")

      certStore.addCertificates(Random.shuffle(List(caCertB, certE, certF, certH, certsByDN("CN=caA")))) shouldBe 'right
      certsByDN.put("CN=caB", caCertB)
      certsByDN.put("CN=cE", certE)
      certsByDN.put("CN=cF", certF)
      certsByDN.put("CN=cH", certH)
      certStore.getCertChain(certsByDN("CN=cF")) shouldBe Right(CertChain(certsByDN("CN=caB"), List(certsByDN("CN=cE")), certsByDN("CN=cF")))
      certStore.getCertChain(certsByDN("CN=cH")) shouldBe Right(CertChain(certsByDN("CN=caA"), List.empty, certsByDN("CN=cH")))
    }

    "validate certificates" in {
      certStore.toSet should contain theSameElementsAs certsByDN.values.toSet
    }

    "get cert chains" in {
      val allCertsChains = certStore.clientCertificates.toList
        .traverse(cert => certStore.getCertChain(cert))
        .right
        .get

      certStore.getCertChains.right.get should contain theSameElementsAs allCertsChains
    }
  }

  /**
    *      caA       caB
    *       |         |
    *      cA        cE
    *     /  \       |
    *   cB   cC     cF
    *         |
    *        cD
    */
  private def validCertChain(caKeyPair: KeyPair, clientKeyPair: KeyPair): (List[X509Certificate], List[X509Certificate], List[X509Certificate]) = {
    val caCertA = generateSelfSignedCert(caKeyPair, "caA")
    val caCertB = generateSelfSignedCert(caKeyPair, "caB")
    val certA   = generateCert(new X500Name(caCertA.getSubjectX500Principal.getName), caKeyPair.getPrivate, clientKeyPair.getPublic, "cA")
    val certB   = generateCert(new X500Name(certA.getSubjectX500Principal.getName), clientKeyPair.getPrivate, clientKeyPair.getPublic, "cB")
    val certC   = generateCert(new X500Name(certA.getSubjectX500Principal.getName), clientKeyPair.getPrivate, clientKeyPair.getPublic, "cC")
    val certD   = generateCert(new X500Name(certC.getSubjectX500Principal.getName), clientKeyPair.getPrivate, clientKeyPair.getPublic, "cD")
    val certE   = generateCert(new X500Name(caCertB.getSubjectX500Principal.getName), caKeyPair.getPrivate, clientKeyPair.getPublic, "cE")
    val certF   = generateCert(new X500Name(certE.getSubjectX500Principal.getName), clientKeyPair.getPrivate, clientKeyPair.getPublic, "cF")

    (List(caCertA, caCertB), List(certA, certC, certE), List(certB, certD, certF))
  }
}
