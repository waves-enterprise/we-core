package com.wavesenterprise.certs

import org.bouncycastle.asn1.x500.X500Name
import org.bouncycastle.asn1.x509.SubjectPublicKeyInfo
import org.bouncycastle.cert.X509v3CertificateBuilder
import org.bouncycastle.cert.jcajce.JcaX509CertificateConverter
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.operator.jcajce.JcaContentSignerBuilder

import java.math.BigInteger
import java.security.cert.X509Certificate
import java.security._
import java.util.Calendar
import scala.util.Random

trait TestCertBuilder {

  protected val provider = new BouncyCastleProvider()

  protected val keypairGenerator: KeyPairGenerator = {
    val kpGen = KeyPairGenerator.getInstance("RSA")
    kpGen.initialize(512, new SecureRandom())
    kpGen
  }

  protected def generateSelfSignedCert(keyPair: KeyPair, dn: String): X509Certificate = {
    val subject = new X500Name(s"CN=$dn")
    val serial  = Random.nextInt(Short.MaxValue)
    val endTime = Calendar.getInstance()
    endTime.add(Calendar.YEAR, 10)
    val builder = new X509v3CertificateBuilder(
      subject,
      BigInteger.valueOf(serial),
      Calendar.getInstance().getTime,
      endTime.getTime,
      subject,
      SubjectPublicKeyInfo.getInstance(keyPair.getPublic.getEncoded)
    )
    val contentSigner         = new JcaContentSignerBuilder("SHA1withRSA").build(keyPair.getPrivate)
    val x509CertificateHolder = builder.build(contentSigner)

    new JcaX509CertificateConverter().setProvider(provider).getCertificate(x509CertificateHolder)
  }

  protected def generateCert(
      issuer: X500Name,
      issuerPrivateKey: PrivateKey,
      subjectPublicKey: PublicKey,
      dn: String
  ): X509Certificate = {
    val subject = new X500Name(s"CN=$dn")
    val serial  = Random.nextInt(Short.MaxValue)
    val endTime = Calendar.getInstance()
    endTime.add(Calendar.YEAR, 10)
    val builder = new X509v3CertificateBuilder(
      issuer,
      BigInteger.valueOf(serial),
      Calendar.getInstance().getTime,
      endTime.getTime,
      subject,
      SubjectPublicKeyInfo.getInstance(subjectPublicKey.getEncoded)
    )
    val contentSigner         = new JcaContentSignerBuilder("SHA1withRSA").build(issuerPrivateKey)
    val x509CertificateHolder = builder.build(contentSigner)

    new JcaX509CertificateConverter().setProvider(provider).getCertificate(x509CertificateHolder)
  }
}
