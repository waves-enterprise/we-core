package com.wavesenterprise.transaction.docker

import cats.implicits._
import com.google.common.io.ByteStreams.newDataOutput
import com.wavesenterprise.crypto
import com.wavesenterprise.docker.StoredContract
import com.wavesenterprise.docker.StoredContract.{DockerContract, WasmContract}
import com.wavesenterprise.docker.validator.{ValidationPolicy, ValidationPolicyDescriptor}
import com.wavesenterprise.state.{ByteStr, DataEntry}
import com.wavesenterprise.transaction.ValidationError.{GenericError, InvalidContractKeys}
import com.wavesenterprise.transaction.docker.ContractTransactionEntryOps.DataEntryMap
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation
import com.wavesenterprise.transaction.docker.assets.ContractAssetOperation.ContractAssetOperationMap
import com.wavesenterprise.utils.StringUtilites.ValidateAsciiAndRussian._
import com.wavesenterprise.transaction.{Transaction, ValidationError}

/**
  * Common validations for contract transactions
  */
trait ContractTransactionValidation {

  val ImageMinLength: Int              = 1          // in symbols
  val ImageMaxLength: Int              = 200        // in symbols
  val ContractNameMaxLength            = 200        // in symbols
  val MaxExecutedTransactionBytes: Int = 300 * 1024 // 300KB

  private val Sha256HexLength = 64
  private val Sha256HexDigits = (('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F')).toSet

  def validateImage(image: String): Either[ValidationError, Unit] = {
    Either.cond(
      image.length >= ImageMinLength && image.length <= ImageMaxLength,
      (),
      GenericError(s"Incorrect image length: ${image.length}. Length must be between $ImageMinLength and $ImageMaxLength")
    )
  }

  def validateHash(imageHash: String): Either[ValidationError, Unit] = {
    Either.cond(
      imageHash.length == Sha256HexLength && imageHash.forall(Sha256HexDigits.contains),
      (),
      GenericError(s"Image hash string $imageHash is not valid SHA-256 hex string")
    )
  }

  def validateHash(storedContract: StoredContract): Either[ValidationError, Unit] = {
    storedContract match {
      case wasm: WasmContract => Either.cond(
          wasm.bytecodeHash.length == Sha256HexLength && wasm.bytecodeHash.forall(Sha256HexDigits.contains),
          (),
          GenericError(s"Bytecode hash string ${wasm.bytecodeHash} is not valid SHA-256 hex string")
        )

      case docker: DockerContract => Either.cond(
          docker.imageHash.length == Sha256HexLength && docker.imageHash.forall(Sha256HexDigits.contains),
          (),
          GenericError(s"Image hash string ${docker.imageHash} is not valid SHA-256 hex string")
        )
    }
  }

  def validateContractName(contractName: String): Either[GenericError, Unit] = {
    Either.cond(
      contractName.nonEmpty && contractName.length <= ContractNameMaxLength,
      (),
      GenericError(
        s"Incorrect contractName length: ${contractName.length}. It must be non-empty and its length must be less than $ContractNameMaxLength")
    )
  }

  def validateParams(params: List[DataEntry[_]]): Either[ValidationError, Unit] = {
    for {
      _ <- notValidMapOrRight(params.map(_.key)).leftMap(InvalidContractKeys(_))
      _ <- Either.cond(params.forall(_.key.nonEmpty), (), GenericError("Param with empty key was found"))
      _ <- Either.cond(params.map(_.key).distinct.length == params.size, (), GenericError("Params with duplicate keys were found"))
      _ <- params.map(ContractTransactionEntryOps.validate).find(_.isLeft).getOrElse(Right(()))
    } yield ()
  }

  def validateResults(results: List[DataEntry[_]]): Either[ValidationError, Unit] = {
    for {
      _ <- notValidMapOrRight(results.map(_.key)).leftMap(InvalidContractKeys(_))
      _ <- Either.cond(results.forall(_.key.nonEmpty), (), GenericError("Result with empty key was found"))
      _ <- Either.cond(results.map(_.key).distinct.length == results.size, (), GenericError("Results with duplicate keys were found"))
      _ <- results.traverse(ContractTransactionEntryOps.validate)
    } yield ()
  }

  def validateValidationPolicy(policy: ValidationPolicy): Either[GenericError, Unit] = {
    import ValidationPolicyDescriptor.{Majority, MajorityWithOneOf}

    policy match {
      case ValidationPolicy.Any | ValidationPolicy.Majority =>
        Right(())
      case ValidationPolicy.MajorityWithOneOf(addresses) =>
        Either.cond(
          addresses.nonEmpty,
          (),
          GenericError(s"Empty policy addresses. Use '${Majority.name}' instead of '${MajorityWithOneOf.name}'.")
        )
    }
  }

  def validateSize(tx: ExecutableTransaction): Either[ValidationError, Unit] = validateSize(tx, ExecutableTransaction.MaxBytes)

  def validateSize(tx: ExecutedContractTransaction): Either[ValidationError, Unit] = validateSize(tx, MaxExecutedTransactionBytes)

  private[this] def validateSize(tx: Transaction, limit: Int): Either[ValidationError.ContractTransactionTooBig, Unit] = {
    val size = tx.bytes().length
    Either.cond(size <= limit, (), ValidationError.ContractTransactionTooBig(size, limit))
  }

}

object ContractTransactionValidation {
  // noinspection UnstableApiUsage
  def resultsHash(results: Seq[DataEntry[_]], assetOps: Seq[ContractAssetOperation] = Seq()): ByteStr = {
    val output = newDataOutput()
    results.sorted.foreach(ContractTransactionEntryOps.writeBytes(_, output))
    assetOps.foreach(_.writeContractAssetOperationBytes(output))
    ByteStr(crypto.fastHash(output.toByteArray))
  }

  def resultsMapHash(results: DataEntryMap, assetOps: ContractAssetOperationMap = ContractAssetOperationMap(Map())): ByteStr = {
    val output      = newDataOutput()
    val resultsList = results.mapping.toList
    val assetList   = assetOps.mapping.toList
    resultsList.sortBy(_._1.toString).foreach(_._2.sorted.foreach(ContractTransactionEntryOps.writeBytes(_, output)))
    assetList.sortBy(_._1.toString).foreach(_._2.foreach(_.writeContractAssetOperationBytes(output)))
    ByteStr(crypto.fastHash(output.toByteArray))
  }
}
