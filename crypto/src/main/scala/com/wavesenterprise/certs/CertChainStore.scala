package com.wavesenterprise.certs

import cats.implicits._
import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.crypto.internals.{CryptoError, PkiError}
import com.wavesenterprise.serialization.BinarySerializer
import com.wavesenterprise.utils.EitherUtils.EitherExt
import com.wavesenterprise.utils.{Base58, ReadWriteLocking}
import org.apache.commons.codec.digest.DigestUtils

import java.security.cert.X509Certificate
import java.util.concurrent.locks.{ReadWriteLock, ReentrantReadWriteLock}
import javax.security.auth.x500.X500Principal
import scala.annotation.tailrec
import scala.collection.mutable

/**
  * CertStore with certificate chains represented as a mutable single-directed graph data structure.
  * Chains without common certificates are represented as disjoint graphs.
  * Graph consistency implemented with Kahn's algorithm.
  * Self-signed certificates are taken as CA certificates.
  */
class CertChainStore private (
    private val caCerts: mutable.Set[X500Principal],
    private val clientCerts: mutable.Set[X500Principal],
    private val certsByDN: mutable.Map[X500Principal, X509Certificate],
    private val certsGraph: mutable.Map[X500Principal, mutable.Set[X500Principal]]
) extends ReadWriteLocking {
  override protected val lock: ReadWriteLock = new ReentrantReadWriteLock()

  def caCertificates: Set[X500Principal]     = readLock(caCerts.toSet)
  def clientCertificates: Set[X500Principal] = readLock(clientCerts.toSet)

  /**
    * @return Unordered sequence of all the certificates contained within the CertChainStore
    */
  def toSet: Set[X509Certificate] = readLock(certsByDN.values.toSet)

  def isEmpty: Boolean = readLock(certsByDN.isEmpty)

  private def getCertChainBySubject(subject: X500Principal): Either[CryptoError, CertChain] = {
    readLock {
      if (clientCerts.contains(subject)) {
        val chain = buildChain(subject)
        Right(CertChain(chain.last, chain.view.tail.init.toIndexedSeq, chain.head))
      } else if (certsByDN.contains(subject)) {
        Left(PkiError(s"Unable to build cert chain starting from the intermediate cert '$subject'"))
      } else {
        Left(PkiError(s"Certificate '$subject' was not found in the CertChain"))
      }
    }
  }

  /**
    * @return Certificates chain ordered from client to CA certificate.
    */
  def getCertChain(userCert: X509Certificate): Either[CryptoError, CertChain] = {
    val subject = userCert.getSubjectX500Principal
    getCertChainBySubject(subject)
  }

  def getCertChain(certDn: X500Principal): Either[CryptoError, CertChain] = {
    getCertChainBySubject(certDn)
  }

  def getCertChains: Either[CryptoError, List[CertChain]] =
    clientCerts.toList.traverse(cert => getCertChain(cert))

  @tailrec
  private def buildChain(certDn: X500Principal, acc: List[X509Certificate] = List.empty): Seq[X509Certificate] = {
    val cert    = certsByDN(certDn)
    val nextAcc = cert :: acc
    if (caCerts.contains(certDn)) {
      nextAcc.view.reverse.toIndexedSeq
    } else {
      buildChain(cert.getIssuerX500Principal, nextAcc)
    }
  }

  /**
    * Adds a new certificate into CertStore.
    * All the intermediate certificates must be already in the CertStore.
    * The issuer must be intermediate or CA certificate.
    */
  def addCert(cert: X509Certificate): Either[CryptoError, Unit] = {
    val subject = cert.getSubjectX500Principal
    writeLock {
      if (certsByDN.contains(subject)) {
        Right(())
      } else {
        val issuer = cert.getIssuerX500Principal
        (if (issuer == subject) {
           caCerts.add(subject)
           Right(())
         } else {
           Either.cond(
             !clientCerts.contains(issuer),
             (),
             PkiError(s"Certificate '$subject' is issued by a client certificate '$issuer' which is forbidden")
           ) >> certsByDN
             .get(issuer)
             .toRight(PkiError(s"Issuer's certificate '$issuer' was not found in the CertStore"))
             .flatMap { _ =>
               val neighbors = certsGraph.getOrElseUpdate(issuer, mutable.HashSet.empty)
               Either
                 .cond(
                   !neighbors.contains(subject), {
                     certsGraph.put(subject, mutable.HashSet.empty)
                     clientCerts.add(subject)
                     neighbors.add(subject)
                   },
                   PkiError(s"Duplicated certificate connection '$issuer' <- '$subject'")
                 )
             }
         }).map(_ => certsByDN.put(subject, cert))
      }
    }
  }

  /**
    * Adds all the certificates from the input collection.
    * Complete chains including intermediate and CA certificates must be passed within the input certificates.
    */
  def addCertificates(certs: Seq[X509Certificate]): Either[CryptoError, Unit] =
    writeLock {
      CertChainStore.buildCertStore(certs, certsByDN.clone()).map(mergeWith)
    }

  /**
    * Removes certificate form the CertStore.
    * The issuer's certificates will be removed in case no linked issued certificates remain.
    */
  def removeCert(cert: X509Certificate): Either[CryptoError, Unit] = {
    val subject = cert.getSubjectX500Principal
    writeLock {
      if (certsByDN.contains(subject)) {
        Either.cond(
          certsGraph(subject).isEmpty,
          removeCertChain(cert),
          PkiError(s"Unable to remove intermediate certificate '$subject' which still has dependent certificates")
        )
      } else {
        Right(())
      }
    }
  }

  @tailrec
  private def removeCertChain(cert: X509Certificate): Unit = {
    val subject = cert.getSubjectX500Principal
    if (certsGraph(subject).isEmpty) {
      certsByDN.remove(subject)
      certsGraph.remove(subject)
      val issuer = cert.getIssuerX500Principal
      certsGraph.get(issuer).foreach { issuersCerts =>
        issuersCerts.remove(subject)
      }
      if (subject == issuer) {
        caCerts.remove(subject)
      } else {
        clientCerts.remove(subject)
        removeCertChain(certsByDN(issuer))
      }
    }
  }

  private def mergeWith(other: CertChainStore): Unit = {
    this.caCerts ++= other.caCerts
    this.clientCerts ++= other.clientCerts
    this.certsByDN ++= other.certsByDN
    other.certsGraph.foreach {
      case (root, neighbours) => this.certsGraph.getOrElseUpdate(root, mutable.Set.empty) ++= neighbours
    }
  }

  def bytes: Array[Byte] = {
    //noinspection UnstableApiUsage
    val output = newDataOutput()
    BinarySerializer.writeBigIterable(certsByDN.values, BinarySerializer.writeX509Cert, output)
    output.toByteArray
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: CertChainStore =>
        eq(that) || certsByDN == that.certsByDN
      case _ => false
    }

  override def hashCode(): Int =
    certsByDN.hashCode()

  override def toString: String = {
    getCertChains match {
      case Left(err)     => s"CorruptedCertChainStore($err)"
      case Right(chains) => chains.mkString("CertChainStore(", ", ", ")")
    }
  }
}

object CertChainStore {

  def empty = new CertChainStore(mutable.Set.empty, mutable.Set.empty, mutable.Map.empty, mutable.Map.empty)

  def fromCertificates(certs: Seq[X509Certificate]): Either[CryptoError, CertChainStore] = {
    val certsByDN = mutable.HashMap.empty[X500Principal, X509Certificate]
    buildCertStore(certs, certsByDN)
  }

  def fromBytes(bytes: Array[Byte], position: Int = 0): Either[CryptoError, (CertChainStore, Int)] = {
    val (certs, end) = BinarySerializer.parseBigList(bytes, BinarySerializer.parseX509Cert, position)
    fromCertificates(certs).map(_ -> end)
  }

  def fromBytesUnsafe(bytes: Array[Byte], position: Int = 0): (CertChainStore, Int) =
    fromBytes(bytes, position).explicitGet()

  private def buildCertStore(
      newCerts: Seq[X509Certificate],
      certsByDN: mutable.Map[X500Principal, X509Certificate]
  ): Either[CryptoError, CertChainStore] = {
    val certsCount   = newCerts.size
    val adjacencyMap = mutable.HashMap.empty[X500Principal, mutable.Set[X500Principal]]
    val inDegree     = mutable.HashMap.empty[X500Principal, Boolean]

    @tailrec
    def buildGraph(certIterator: Iterator[X509Certificate]): Either[CryptoError, Unit] = {
      if (certIterator.hasNext) {
        val cert    = certIterator.next()
        val issuer  = cert.getIssuerX500Principal
        val subject = cert.getSubjectX500Principal

        (if (!certsByDN.contains(subject)) {
           certsByDN.put(subject, cert)
           if (issuer != subject) {
             adjacencyMap.getOrElseUpdate(subject, mutable.HashSet.empty[X500Principal])
             val neighbors = adjacencyMap.getOrElseUpdate(issuer, mutable.HashSet.empty[X500Principal])
             Either.cond(
               !neighbors.contains(subject),
               neighbors.add(subject),
               PkiError(s"Duplicated certificate connection: '$issuer' <- '$subject'")
             )
           } else {
             Right(())
           }
         } else {
           Right(())
         }) match {
          case Right(_) =>
            if (!inDegree.contains(subject)) {
              inDegree.put(subject, issuer != subject)
            }
            buildGraph(certIterator)
          case Left(err) => Left(err)
        }
      } else {
        Right(())
      }
    }

    def findRootCerts(): Either[PkiError, Set[X500Principal]] = {
      val (rootCerts, errorCerts) = inDegree
        .collect {
          case (dn, hasInDegree) if !hasInDegree => dn
        }
        .partition { dn =>
          val cert = certsByDN(dn)
          cert.getIssuerX500Principal == cert.getSubjectX500Principal
        }
      Either.cond(errorCerts.isEmpty, rootCerts.toSet, PkiError(s"Couldn't build cert chain for the following certificates: $errorCerts"))
    }

    /**
      * Validates a graph using Kahn’s algorithm.
      * Step-1: Compute in-degree (if a vertex has incoming edges) for each of the vertex present in the directed graph and initialize the count of visited nodes as 0.
      * Step-2: Pick all the vertices with in-degree as `false` and add them into a queue (Enqueue operation)
      * Step-3: Remove a vertex from the queue (Dequeue operation) and then:
      *   -Increment count of visited nodes by 1.
      *   -Set in-degree to `false` for all its neighbouring nodes and add all of them to the queue.
      * Step 4: Repeat Step 3 until the queue is empty.
      * Step 5: If count of visited nodes is not equal to the number of nodes in the graph then the topological sort is not possible for the given graph.
      */
    def validateGraph(rootCerts: Set[X500Principal]): Either[CryptoError, Unit] = {
      val queue        = mutable.Queue.empty[X500Principal] ++ rootCerts
      var visitedCount = 0
      while (queue.nonEmpty) {
        val curr = queue.dequeue()
        if (!inDegree(curr)) {
          visitedCount += 1
        }
        adjacencyMap.get(curr).foreach { neighbours =>
          neighbours.foreach { nei =>
            inDegree.put(nei, false)
            queue.enqueue(nei)
          }
        }
      }
      Either.cond(
        visitedCount == certsCount,
        (), {
          val failedCerts = inDegree.collect {
            case (dn, hasInDegree) if hasInDegree => dn
          }
          if (failedCerts.isEmpty) {
            PkiError(s"Unable to build cert chain. Input certificates contain duplicated DNs")
          } else {
            PkiError(s"Unable to build cert chain for the following certificates: [${failedCerts.mkString(", ")}]")
          }
        }
      )
    }

    def createCertStore(rootCerts: Set[X500Principal]): Either[CryptoError, CertChainStore] = {
      val clientCerts = adjacencyMap
        .collect {
          case (dn, neighbours) if neighbours.isEmpty => dn
        }
        .to[mutable.Set]
      Right(new CertChainStore(rootCerts.to[mutable.Set], clientCerts, certsByDN, adjacencyMap))
    }

    for {
      _         <- buildGraph(newCerts.iterator)
      rootCerts <- findRootCerts()
      _         <- validateGraph(rootCerts)
      certStore <- createCertStore(rootCerts)
    } yield certStore
  }
}

sealed trait X509Chain {
  def caCert: X509Certificate
  def intermediateCerts: Seq[X509Certificate]
  def userCert: X509Certificate

  lazy val toSet: Set[X509Certificate] = (caCert :: userCert :: intermediateCerts.toList).toSet

  override val toString: String = {
    val intermediateStr = intermediateCerts.map(trimmedFingerPrint) match {
      case Nil  => ""
      case list => list.mkString(" ->", ", ", "")
    }
    s"""[${trimmedFingerPrint(userCert)}$intermediateStr -> ${trimmedFingerPrint(caCert)}]"""
  }

  private def trimmedFingerPrint(cert: X509Certificate): String = {
    Base58.encode(DigestUtils.sha1(cert.getEncoded)).take(7) + "..."
  }
}

case class CertChain(caCert: X509Certificate, intermediateCerts: Seq[X509Certificate], userCert: X509Certificate) extends X509Chain
