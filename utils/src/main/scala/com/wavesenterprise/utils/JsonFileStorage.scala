package com.wavesenterprise.utils

import cats.implicits.{catsStdBitraverseForEither, toBifunctorOps}
import play.api.libs.json.{Json, Reads, Writes}
import scorex.util.encode.{Base64 => ScorexBase64}

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import java.security.InvalidKeyException
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec
import scala.io.Source
import scala.util.Try

object JsonFileStorage {
  private val encoding          = "UTF-8"
  private val keySalt           = "0495c728-1614-41f6-8ac3-966c22b4a62d"
  private val aes               = "AES"
  private val algorithm         = aes + "/ECB/PKCS5Padding"
  private val hashing           = "PBKDF2WithHmacSHA512"
  private val hashingIterations = 999999
  private val keyLength         = 128

  import java.security.NoSuchAlgorithmException
  import java.security.spec.InvalidKeySpecException
  import javax.crypto.SecretKeyFactory
  import javax.crypto.spec.PBEKeySpec

  private def hashPassword(password: Array[Char], salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] =
    try {
      val skf  = SecretKeyFactory.getInstance(hashing)
      val spec = new PBEKeySpec(password, salt, iterations, keyLength)
      val key  = skf.generateSecret(spec)
      val res  = key.getEncoded
      res
    } catch {
      case e @ (_: NoSuchAlgorithmException | _: InvalidKeySpecException) =>
        throw new RuntimeException(e)
    }

  def prepareKey(key: String): SecretKeySpec =
    new SecretKeySpec(hashPassword(key.toCharArray, keySalt.getBytes(encoding), hashingIterations, keyLength), aes)

  private def encrypt(key: SecretKeySpec, value: String): String = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    ScorexBase64.encode(cipher.doFinal(value.getBytes(encoding)))
  }

  private def decrypt(key: SecretKeySpec, encryptedValue: Array[Byte]): Either[String, String] = {
    val cipher: Cipher = Cipher.getInstance(algorithm)
    Try(cipher.init(Cipher.DECRYPT_MODE, key)).toEither
      .leftMap {
        case _: InvalidKeyException => "Incorrect password"
        case unexpected             => throw unexpected
      }
      .map { _ =>
        new String(cipher.doFinal(encryptedValue), StandardCharsets.UTF_8)
      }
  }

  def save[T](value: T, path: String, key: Option[SecretKeySpec])(implicit w: Writes[T]): Unit = {
    var file: Option[PrintWriter] = None
    try {
      val folder = new File(path).getParentFile
      if (!folder.exists())
        folder.mkdirs()
      file = Option(new PrintWriter(path))
      file.foreach {
        val json = Json.toJson(value).toString()
        val data = key.fold(json)(k => encrypt(k, json))
        _.write(data)
      }
    } finally {
      file.foreach(_.close())
    }
  }

  def save[T](value: T, path: String)(implicit w: Writes[T]): Unit =
    save(value, path, None)

  def load[T](path: String, key: Option[SecretKeySpec] = None)(implicit r: Reads[T]): Either[String, T] =
    ResourceUtils.withResource(Source.fromFile(path)) { file =>
      val data = file.mkString.trim

      key
        .fold[Either[String, String]](Right(data)) { k =>
          ScorexBase64
            .decode(data)
            .toEither
            .leftMap(err => s"Failed to decode Base64 data: ${err.getMessage}")
            .flatMap(decrypt(k, _))
        }
        .flatMap { jsonStr =>
          Try(Json.parse(jsonStr).as[T]).toEither.leftMap(err => s"Failed to parse JSON: ${err.getMessage}")
        }
    }

  def load[T](path: String)(implicit r: Reads[T]): Either[String, T] =
    load(path, None)
}
