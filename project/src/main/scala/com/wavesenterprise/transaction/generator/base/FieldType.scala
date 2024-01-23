package com.wavesenterprise.transaction.generator.base

import com.wavesenterprise.transaction.base.{Code, ProtoAdapterImport}
import com.wavesenterprise.transaction.generator.scala.AtomicInnerTxAdapterGenerator
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

case class BinarySerializationContext(output: String, field: String, forProof: Boolean = false)
case class BinaryDeserializationContext(bytes: String, offset: String, field: String)

sealed trait BinarySerializableType extends FieldType {

  type BinaryWriter = BinarySerializationContext => Code
  type BinaryReader = BinaryDeserializationContext => Code

  /**
    * Defines function that build serialization code using ByteArrayDataOutput and field value
    */
  def binaryWriter: BinaryWriter

  /**
    * Defines function that build deserialization code using bytes array, bytes offset and field name.
    * The resulting code should define 2 variable:
    * "field"    – deserialized value;
    * "fieldEnd" – offset in bytes indicates the end of the deserialized value.
    */
  def binaryReader: BinaryReader
}

sealed trait WrapperType extends FieldType {

  def underlay: FieldType

  override val isCustomProofSource: Boolean = underlay.isCustomProofSource
}

case class ProtoAdaptationContext(field: String) extends AnyVal

sealed trait ProtoCompatibleType extends FieldType {

  type ProtoAdapter = ProtoAdaptationContext => Code

  /**
    * Defines function that adapts protobuf field to vanilla field.
    * If the adapter is empty, then no conversion is required.
    */
  def protoToVanillaAdapter: Option[ProtoAdapter] = None

  /**
    * Defines function that adapts vanilla field to protobuf field.
    * If the adapter is empty, then no conversion is required.
    */
  def vanillaToProtoAdapter: Option[ProtoAdapter] = None
}

sealed abstract class FieldType(
    val scalaType: String,
    val scalaImports: Set[String] = Set.empty,
    val protoType: String,
    val protoImports: Set[String] = Set.empty,
    /** In proto 3 all fields have a default value. Therefore scala protobuf plugin wraps message type to Option monad.
      * We need to know if the field is message type to generate the correct code. */
    val isMessageProtoType: Boolean = false,
    val isCustomProofSource: Boolean = false,
    /** Not every field is used in TypeScript representation. */
    val typeScriptType: Option[String] = None,
    val isProofs: Boolean = false,
) extends EnumEntry {

  def ? : FieldType.OPTION = FieldType.OPTION(this)

  override def toString: String = scalaType
}

trait SkipForProof extends BinarySerializableType {

  abstract override val binaryWriter: BinaryWriter = { c =>
    if (c.forProof) "()" else super.binaryWriter(c)
  }

  override val isCustomProofSource: Boolean = true
}

object FieldType extends Enum[FieldType] {

  case object INT
      extends FieldType(
        scalaType = "Int",
        protoType = "int32",
        scalaImports = Set("com.google.common.primitives.Ints"),
        typeScriptType = Some("Integer")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.writeInt(${c.field})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = Ints.fromBytes(${c.bytes}(${c.offset}), ${c.bytes}(${c.offset} + 1), ${c.bytes}(${c.offset} + 2), ${c.bytes}(${c.offset} + 3)) -> (${c.offset} + Ints.BYTES)"
    }
  }

  case object LONG
      extends FieldType(
        scalaType = "Long",
        protoType = "int64",
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer"),
        typeScriptType = Some("Long")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.writeLong(${c.field})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = BinarySerializer.parseLong(${c.bytes}, ${c.offset})"
    }
  }

  case object BYTE
      extends FieldType(
        scalaType = "Byte",
        protoType = "int32",
        scalaImports = Set(ProtoAdapterImport),
        typeScriptType = Some("Byte")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.writeByte(${c.field})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ${c.bytes}(${c.offset}) -> (${c.offset} + 1)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteFromProto(${c.field})"
    }
  }

  case object BOOLEAN
      extends FieldType(
        scalaType = "Boolean",
        protoType = "bool",
        typeScriptType = Some("Bool")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.writeByte(if (${c.field}) 1 else 0)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = (${c.bytes}(${c.offset}) == 1) -> (${c.offset} + 1)"
    }
  }

  case object SCRIPT
      extends FieldType(
        scalaType = "Script",
        protoType = "bytes",
        scalaImports = Set(
          "com.wavesenterprise.transaction.smart.script.Script",
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.serialization.ModelsBinarySerializer",
          "com.wavesenterprise.transaction.smart.script.ScriptReader",
          ProtoAdapterImport
        ),
        typeScriptType = Some("Base64")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortByteArray(${c.field}.bytes().arr, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ModelsBinarySerializer.parseScript(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ScriptReader.fromBytes(${c.field}.toByteArray)"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.bytes().arr)"
    }
  }

  case object SHORT_STRING
      extends FieldType(
        scalaType = "String",
        protoType = "string",
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer"),
        typeScriptType = Some("StringWithLength")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortString(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = BinarySerializer.parseShortString(${c.bytes}, ${c.offset})"
    }
  }

  case object TRANSACTION_ID
      extends FieldType(
        scalaType = "ByteStr",
        protoType = "bytes",
        scalaImports = Set(
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.state.ByteStr",
          ProtoAdapterImport
        )
      )
      with ProtoCompatibleType {

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteStrFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { _ =>
      s"ProtoAdapter.byteArrayToByteString(id().arr)"
    }
  }

  case object SHORT_BYTE_STR
      extends FieldType(
        scalaType = "ByteStr",
        protoType = "bytes",
        scalaImports = Set(
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.state.ByteStr",
          ProtoAdapterImport
        ),
        typeScriptType = Some("Base58WithLength")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortByteStr(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = BinarySerializer.parseShortByteStr(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteStrFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.arr)"
    }
  }

  case object ASSET_ID
      extends FieldType(
        scalaType = "AssetId",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.state.ByteStr", "com.wavesenterprise.transaction.{AssetId, AssetIdLength}", ProtoAdapterImport),
        typeScriptType = Some("AssetId")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.arr)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ByteStr(bytes.slice(${c.offset}, ${c.offset} + AssetIdLength)) -> (${c.offset} + AssetIdLength)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteStrFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.arr)"
    }
  }

  case object SHORT_BYTE_ARRAY
      extends FieldType(
        scalaType = "Array[Byte]",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer", ProtoAdapterImport),
        typeScriptType = Some("ByteArrayWithSize")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortByteArray(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = BinarySerializer.parseShortByteArray(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field})"
    }
  }

  case object PUBLIC_KEY_ACCOUNT
      extends FieldType(
        scalaType = "PublicKeyAccount",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.account.PublicKeyAccount", "com.wavesenterprise.crypto.KeyLength"),
        typeScriptType = Some("Base58")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.publicKey.getEncoded)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = PublicKeyAccount(${c.bytes}.slice(${c.offset}, ${c.offset} + KeyLength)) -> (${c.offset} + KeyLength)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayFromProto(${c.field}).flatMap(PublicKeyAccount.fromBytes(_).left.map(ValidationError.fromCryptoError))"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.publicKey.getEncoded)"
    }
  }

  case object ADDRESS
      extends FieldType(
        scalaType = "Address",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.account.Address"),
        typeScriptType = Some("ArrayOfStringsWithLength")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.bytes.arr)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = Address.fromBytesUnsafe(${c.bytes}.slice(${c.offset}, ${c.offset} + Address.AddressLength)) -> (${c.offset} + Address.AddressLength)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.addressFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.bytes.arr)"
    }
  }

  case object SENDER_ADDRESS extends FieldType(scalaType = "Address", protoType = "bytes") with ProtoCompatibleType

  case object ALIAS
      extends FieldType(
        scalaType = "Alias",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.account.Alias"),
        typeScriptType = Some("Alias")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortByteArray(${c.field}.bytes.arr, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"""val (${c.field}Raw, ${c.field}End) = BinarySerializer.parseShortByteArray(${c.bytes}, ${c.offset})
         |val ${c.field} = Alias.fromBytesUnsafe(${c.field}Raw)""".stripMargin
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.aliasFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.bytes.arr)"
    }
  }

  case object ADDRESS_OR_ALIAS
      extends FieldType(
        scalaType = "AddressOrAlias",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.account.AddressOrAlias"),
        typeScriptType = Some("Recipient")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.bytes.arr)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = AddressOrAlias.fromBytesUnsafe(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.addressOrAliasFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.bytes.arr)"
    }
  }

  case object PROOFS
      extends FieldType(
        scalaType = "Proofs",
        protoType = "repeated bytes",
        scalaImports = Set("com.wavesenterprise.transaction.Proofs", ProtoAdapterImport),
        isProofs = true
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.field}.writeBytes(${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = Proofs.fromBytesUnsafe(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.proofsFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.proofsToProto(${c.field})"
    }
  }

  abstract class OPTION_BASE(override val underlay: FieldType)
      extends FieldType(
        scalaType = s"Option[${underlay.scalaType}]",
        // Relations of special protobuf 3 messages types for optional fields definition.
        protoType = underlay.protoType match {
          case "int32"                          => "google.protobuf.Int32Value"
          case "int64"                          => "google.protobuf.Int64Value"
          case "bool"                           => "google.protobuf.BoolValue"
          case "string"                         => "google.protobuf.StringValue"
          case "bytes"                          => "google.protobuf.BytesValue"
          case _ if underlay.isMessageProtoType => underlay.protoType
          case _                                => s"{Undefined message type for optional $underlay"
        },
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer") ++
          underlay.scalaImports ++
          underlayTypeImportsForProto(underlay),
        protoImports = underlay.protoImports ++ (underlay.protoType match {
          case "int32" | "int64" | "bool" | "string" | "bytes" => Set("google/protobuf/wrappers.proto")
          case _                                               => Set.empty
        }),
        typeScriptType = underlay.typeScriptType
      )
      with WrapperType
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""({
             |  def underlayWriter(value: ${underlay.scalaType}, output: ByteArrayDataOutput): Unit = {
             |    ${u.binaryWriter(BinarySerializationContext("output", "value", c.forProof))}
             |  }
             |  BinarySerializer.writeByteIterable(${c.field}, underlayWriter, ${c.output})
             |})""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val binaryReader: BinaryReader = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""val (${c.field}, ${c.field}End) = {
             |  def underlayReader(bytes: Array[Byte], pos: Int): (${underlay.scalaType}, Int) = {
             |    ${u.binaryReader(BinaryDeserializationContext("bytes", "pos", "result"))}
             |    result -> resultEnd
             |  }
             |  BinarySerializer.parseOption(${c.bytes}, underlayReader, ${c.offset})
             |}""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = {
      underlay match {
        case u: ProtoCompatibleType =>
          u.protoToVanillaAdapter.map { underlayAdapter => c =>
            s"${c.field}.traverse(f => ${underlayAdapter(ProtoAdaptationContext("f"))})"
          }

        case _ => Some(_ => s"{Underlay type $underlay not implemented ProtoCompatibleType}")
      }
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = {
      underlay match {
        case u: ProtoCompatibleType =>
          u.vanillaToProtoAdapter.map { underlayAdapter => c =>
            s"""${c.field}.map { f =>
               |  ${underlayAdapter(ProtoAdaptationContext("f"))}
               |}""".stripMargin
          }

        case _ => Some(_ => s"{Underlay type $underlay not implemented ProtoCompatibleType}")
      }
    }
  }

  case class OPTION(override val underlay: FieldType) extends OPTION_BASE(underlay)

  case class SHORT_LIST(underlay: FieldType)
      extends FieldType(
        scalaType = s"List[${underlay.scalaType}]",
        protoType = "repeated " + underlay.protoType,
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer") ++
          underlay.scalaImports ++
          underlayTypeImportsForProto(underlay),
        protoImports = underlay.protoImports,
        typeScriptType = Some("List")
      )
      with WrapperType
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""({
             |  def underlayWriter(value: ${underlay.scalaType}, output: ByteArrayDataOutput): Unit = {
             |    ${u.binaryWriter(BinarySerializationContext("output", "value", c.forProof))}
             |  }
             |  BinarySerializer.writeShortIterable(${c.field}, underlayWriter, ${c.output})
             |})""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val binaryReader: BinaryReader = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""val (${c.field}, ${c.field}End) = {
             |  def underlayReader(bytes: Array[Byte], pos: Int): (${underlay.scalaType}, Int) = {
             |    ${u.binaryReader(BinaryDeserializationContext("bytes", "pos", "result"))}
             |    result -> resultEnd
             |  }
             |  BinarySerializer.parseShortList(${c.bytes}, underlayReader, ${c.offset})
             |}""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      underlay match {
        case u: ProtoCompatibleType =>
          u.protoToVanillaAdapter
            .map { underlayAdapter =>
              s"""${c.field}.toList.traverse(f => ${underlayAdapter(ProtoAdaptationContext("f"))})""".stripMargin
            }
            .getOrElse {
              s"Right(${c.field}.toList)"
            }
        case _ => s"{Underlay type $underlay not implemented ProtoCompatibleType}"
      }
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = {
      underlay match {
        case u: ProtoCompatibleType =>
          u.vanillaToProtoAdapter.map { underlayAdapter => c =>
            s"${c.field}.map(f => ${underlayAdapter(ProtoAdaptationContext("f"))})"
          }

        case _ => Some(_ => s"{Underlay type $underlay not implemented ProtoCompatibleType}")
      }
    }
  }

  case class BIG_LIST(underlay: FieldType)
      extends FieldType(
        scalaType = s"List[${underlay.scalaType}]",
        protoType = "repeated " + underlay.protoType,
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer") ++
          underlay.scalaImports ++
          underlayTypeImportsForProto(underlay),
        protoImports = underlay.protoImports,
        typeScriptType = Some("ArrayOfStringsWithLength")
      )
      with WrapperType
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""({
             |  def underlayWriter(value: ${underlay.scalaType}, output: ByteArrayDataOutput): Unit = {
             |    ${u.binaryWriter(BinarySerializationContext("output", "value", c.forProof))}
             |  }
             |  BinarySerializer.writeBigIterable(${c.field}, underlayWriter, ${c.output})
             |})""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val binaryReader: BinaryReader = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""val (${c.field}, ${c.field}End) = {
             |  def underlayReader(bytes: Array[Byte], pos: Int): (${underlay.scalaType}, Int) = {
             |    ${u.binaryReader(BinaryDeserializationContext("bytes", "pos", "result"))}
             |    result -> resultEnd
             |  }
             |  BinarySerializer.parseBigList(${c.bytes}, underlayReader, ${c.offset})
             |}""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      underlay match {
        case u: ProtoCompatibleType =>
          u.protoToVanillaAdapter
            .map { underlayAdapter =>
              s"""${c.field}.toList.traverse(f => ${underlayAdapter(ProtoAdaptationContext("f"))})""".stripMargin
            }
            .getOrElse {
              s"Right(${c.field}.toList)"
            }
        case _ => s"{Type ${underlay.scalaType} not implemented ProtoCompatibleType}"
      }
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = {
      underlay match {
        case u: ProtoCompatibleType =>
          u.vanillaToProtoAdapter.map { underlayAdapter => c =>
            s"${c.field}.map(f => ${underlayAdapter(ProtoAdaptationContext("f"))})"
          }

        case _ => Some(_ => s"{Type ${underlay.scalaType} not implemented ProtoCompatibleType}")
      }
    }
  }

  case class SHORT_SET(underlay: FieldType)
      extends FieldType(
        scalaType = s"Set[${underlay.scalaType}]",
        protoType = "repeated " + underlay.protoType,
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer") ++
          underlay.scalaImports ++
          underlayTypeImportsForProto(underlay),
        protoImports = underlay.protoImports,
        typeScriptType = Some("ArrayOfStringsWithLength")
      )
      with WrapperType
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""({
             |  def underlayWriter(value: ${underlay.scalaType}, output: ByteArrayDataOutput): Unit = {
             |    ${u.binaryWriter(BinarySerializationContext("output", "value", c.forProof))}
             |  }
             |  BinarySerializer.writeShortIterable(${c.field}, underlayWriter, ${c.output})
             |})""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val binaryReader: BinaryReader = { c =>
      underlay match {
        case u: BinarySerializableType =>
          s"""val (${c.field}, ${c.field}End) = {
             |  def underlayReader(bytes: Array[Byte], pos: Int): (${underlay.scalaType}, Int) = {
             |    ${u.binaryReader(BinaryDeserializationContext("bytes", "pos", "result"))}
             |    result -> resultEnd
             |  }
             |  BinarySerializer.parseShortSet(${c.bytes}, underlayReader, ${c.offset})
             |}""".stripMargin
        case _ => s"{Underlay type $underlay not implemented BinarySerializableType}"
      }
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      underlay match {
        case u: ProtoCompatibleType =>
          u.protoToVanillaAdapter
            .map { underlayAdapter =>
              s"""${c.field}.toList.traverse(f => ${underlayAdapter(ProtoAdaptationContext("f"))}).map(_.toSet)""".stripMargin
            }
            .getOrElse {
              s"Right(${c.field}.toSet)"
            }
        case _ => s"{Type ${underlay.scalaType} not implemented ProtoCompatibleType}"
      }
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = {
      underlay match {
        case u: ProtoCompatibleType =>
          u.vanillaToProtoAdapter.map { underlayAdapter => c =>
            s"${c.field}.map(f => ${underlayAdapter(ProtoAdaptationContext("f"))}).toSeq"
          }

        case _ => Some(_ => s"{Type ${underlay.scalaType} not implemented ProtoCompatibleType}")
      }
    }
  }

  case object DATA_ENTRY
      extends FieldType(
        scalaType = "DataEntry[_]",
        protoType = "DataEntry",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.state.DataEntry",
          "scala.language.existentials",
          ProtoAdapterImport
        ),
        protoImports = Set("data_entry.proto"),
        typeScriptType = Some("DataEntry")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"DataTransactionEntryOps.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = DataTransactionEntryOps.parse(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object STORED_CONTRACT
      extends FieldType(
        scalaType = "StoredContract",
        protoType = "StoredContract",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.docker.ContractInfo._",
          "scala.language.existentials",
          ProtoAdapterImport
        ),
        protoImports = Set("stored_contract.proto") ,
      )
      with BinarySerializableType
      with ProtoCompatibleType {
    override val binaryWriter: BinaryWriter = { c =>
      s"StoredContractOps.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = StoredContractOps.parse(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object CONTRACT_DATA_ENTRY
      extends FieldType(
        scalaType = "DataEntry[_]",
        protoType = "DataEntry",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.transaction.docker.ContractTransactionEntryOps",
          "com.wavesenterprise.state.DataEntry",
          "scala.language.existentials",
          ProtoAdapterImport
        ),
        protoImports = Set("data_entry.proto"),
        typeScriptType = Some("DockerParamEntry")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ContractTransactionEntryOps.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ContractTransactionEntryOps.parse(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object EXECUTABLE_TRANSACTION
      extends FieldType(
        scalaType = "ExecutableTransaction",
        protoType = "ExecutableTransaction",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.transaction.docker.ExecutableTransaction",
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.transaction.TransactionParsers",
          ProtoAdapterImport
        ),
        protoImports = Set("executable_contract_transaction.proto")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeBigByteArray(${c.field}.bytes(), ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"""val (${c.field}Bytes, ${c.field}End) = BinarySerializer.parseBigByteArray(${c.bytes}, ${c.offset})
         |val ${c.field} = TransactionParsers
         |  .parseBytes(${c.field}Bytes)
         |  .map(_.asInstanceOf[ExecutableTransaction])
         |  .fold(error => throw new RuntimeException("Incorrect transaction", error), identity)""".stripMargin
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object VALIDATION_PROOF
      extends FieldType(
        scalaType = "ValidationProof",
        protoType = "ValidationProof",
        isMessageProtoType = true,
        scalaImports = Set("com.wavesenterprise.transaction.docker.ValidationProof", ProtoAdapterImport),
        protoImports = Set("validation_proof.proto")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ValidationProof.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ValidationProof.fromBytes(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object POLICY_DATA_HASH
      extends FieldType(
        scalaType = "PolicyDataHash",
        protoType = "bytes",
        scalaImports = Set("com.wavesenterprise.privacy.PolicyDataHash"),
        typeScriptType = Some("Base58WithLength")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"BinarySerializer.writeShortByteArray(${c.field}.bytes.arr, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"""val (${c.field}Raw, ${c.field}End) = BinarySerializer.parseShortByteArray(${c.bytes}, ${c.offset})
         |val ${c.field} = PolicyDataHash.deserialize(${c.field}Raw)""".stripMargin
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayFromProto(${c.field}).map(PolicyDataHash.deserialize)"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.bytes.arr)"
    }
  }

  case object PERMISSION_OP_TYPE
      extends FieldType(
        scalaType = "OpType",
        protoType = "OpType",
        scalaImports = Set("com.wavesenterprise.acl.OpType", ProtoAdapterImport),
        protoImports = Set("op_type.proto"),
        typeScriptType = Some("PermissionOpType")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.writeByte(${c.field}.byte)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = OpType.fromByteUnsafe(${c.bytes}(${c.offset})) -> (${c.offset} + 1)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object PERMISSION_OP
      extends FieldType(
        scalaType = "PermissionOp",
        protoType = "PermissionOp",
        isMessageProtoType = true,
        scalaImports = Set("com.wavesenterprise.acl.PermissionOp"),
        protoImports = Set("permission_op.proto")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.bytes)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = PermissionOp.fromBytesUnsafe(bytes.slice(${c.offset}, ${c.offset} + PermissionOp.serializedSize)) -> (${c.offset} + PermissionOp.serializedSize)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object TRANSFER_BATCH
      extends FieldType(
        scalaType = "List[ParsedTransfer]",
        scalaImports = Set(
          "com.wavesenterprise.transaction.transfer.ParsedTransfer",
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.serialization.ModelsBinarySerializer",
          ProtoAdapterImport,
          "cats.implicits._"
        ),
        protoType = "repeated Transfer",
        protoImports = Set("transfer.proto"),
        typeScriptType = Some("Transfers")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ModelsBinarySerializer.writeTransferBatch(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ModelsBinarySerializer.parseTransferBatch(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"${c.field}.toList.traverse(ProtoAdapter.fromProto)"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"${c.field}.map(ProtoAdapter.toProto)"
    }
  }

  case object NODE_NAME
      extends FieldType(
        scalaType = "Option[String]",
        scalaImports = Set("com.wavesenterprise.serialization.BinarySerializer"),
        protoType = "google.protobuf.StringValue",
        protoImports = Set("google/protobuf/wrappers.proto"),
        typeScriptType = Some("StringWithLength")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"""BinarySerializer.writeShortString(${c.field}.getOrElse(""), ${c.output})"""
    }

    override val binaryReader: BinaryReader = { c =>
      s"""val (${c.field}Raw, ${c.field}End) = BinarySerializer.parseShortString(${c.bytes}, ${c.offset})
         |val ${c.field} = Option(${c.field}Raw).filter(_.nonEmpty)""".stripMargin
    }
  }

  case object ATOMIC_INNER_TRANSACTION
      extends FieldType(
        scalaType = "AtomicInnerTransaction",
        protoType = "AtomicInnerTransaction",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.transaction.TransactionParsers",
          s"com.wavesenterprise.serialization.${AtomicInnerTxAdapterGenerator.objectName}",
          "com.wavesenterprise.transaction.docker.ExecutedContractTransaction"
        ),
        protoImports = Set("managed/atomic_inner_transaction.proto"),
        isCustomProofSource = true,
        typeScriptType = Some("AtomicInnerTransaction")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      if (c.forProof) {
        s"""val id = ${c.field} match {
           |   case executedTx: ExecutedContractTransaction => executedTx.tx.id()
           |   case tx => tx.id()
           |}
           |
           |${c.output}.write(id.arr)""".stripMargin
      } else {
        s"BinarySerializer.writeBigByteArray(${c.field}.bytes(), ${c.output})"
      }
    }

    override val binaryReader: BinaryReader = { c =>
      s"""val (${c.field}Bytes, ${c.field}End) = BinarySerializer.parseBigByteArray(${c.bytes}, ${c.offset})
         |val ${c.field} = TransactionParsers
         |  .parseBytes(${c.field}Bytes)
         |  .map(_.asInstanceOf[AtomicInnerTransaction])
         |  .fold(error => throw new RuntimeException("Incorrect transaction", error), identity)""".stripMargin
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"${AtomicInnerTxAdapterGenerator.objectName}.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"${AtomicInnerTxAdapterGenerator.objectName}.toProto(${c.field})"
    }
  }

  case object ATOMIC_BADGE
      extends FieldType(
        scalaType = "AtomicBadge",
        protoType = "AtomicBadge",
        scalaImports = Set(
          "com.wavesenterprise.transaction.AtomicBadge",
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.serialization.ModelsBinarySerializer",
          ProtoAdapterImport
        ),
        protoImports = Set("atomic_badge.proto"),
        isMessageProtoType = true,
        typeScriptType = Some("AtomicBadge")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ModelsBinarySerializer.writeAtomicBadge(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ModelsBinarySerializer.parseAtomicBadge(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object VALIDATION_POLICY
      extends FieldType(
        scalaType = "ValidationPolicy",
        protoType = "ValidationPolicy",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.docker.validator.ValidationPolicy",
          ProtoAdapterImport
        ),
        protoImports = Set("validation_policy.proto"),
        typeScriptType = Some("ValidationPolicy")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.bytes)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ValidationPolicy.fromBytesUnsafe(bytes, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object CONTRACT_API_VERSION
      extends FieldType(
        scalaType = "ContractApiVersion",
        protoType = "ContractApiVersion",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.docker.ContractApiVersion",
          ProtoAdapterImport
        ),
        protoImports = Set("contract_api_version.proto"),
        typeScriptType = Some("ContractApiVersion")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.bytes)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ContractApiVersion.fromBytesUnsafe(bytes, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object CONTRACT_TRANSFER_IN
      extends FieldType(
        scalaType = "ContractTransferInV1",
        protoType = "ContractTransferIn",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.transaction.docker.assets.ContractTransferInV1",
          "com.wavesenterprise.serialization.BinarySerializer",
          "com.wavesenterprise.transaction.TransactionParsers",
          ProtoAdapterImport
        ),
        protoImports = Set("contract_transfer_in.proto")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ContractTransferInV1.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ContractTransferInV1.fromBytes(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object CONTRACT_ASSET_OPERATION
      extends FieldType(
        scalaType = "ContractAssetOperation",
        protoType = "ContractAssetOperation",
        isMessageProtoType = true,
        scalaImports = Set(
          "com.wavesenterprise.transaction.docker.assets.ContractAssetOperation",
          "com.wavesenterprise.transaction.TransactionParsers",
          ProtoAdapterImport
        ),
        protoImports = Set("contract_asset_operation.proto")
      )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ContractAssetOperation.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ContractAssetOperation.fromBytes(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object ASSET_OPERATIONS_MAP
    extends FieldType(
      scalaType = "ContractAssetOperationMap",
      protoType = "ContractAssetOperationMap",
      isMessageProtoType = true,
      scalaImports = Set(
        "com.wavesenterprise.transaction.docker.assets.ContractAssetOperation",
        "com.wavesenterprise.transaction.TransactionParsers",
        ProtoAdapterImport
      ),
      protoImports = Set("contract_asset_operation.proto")
    )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ContractAssetOperationMap.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ContractAssetOperationMap.fromBytes(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object DATA_ENTRY_MAP
    extends FieldType(
      scalaType = "DataEntryMap",
      protoType = "DataEntryMap",
      isMessageProtoType = true,
      scalaImports = Set(
        "com.wavesenterprise.state.DataEntry",
        "scala.language.existentials",
        "com.wavesenterprise.transaction.docker.ContractTransactionEntryOps",
        ProtoAdapterImport
      ),
      protoImports = Set("data_entry.proto")
    )
      with BinarySerializableType
      with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"DataEntryMap.writeBytes(${c.field}, ${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = DataEntryMap.fromBytes(${c.bytes}, ${c.offset})"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.fromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.toProto(${c.field})"
    }
  }

  case object COMMITMENT extends FieldType(
    scalaType = "Commitment",
    protoType = "bytes",
    scalaImports = Set("com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment",
      "com.wavesenterprise.crypto.internals.confidentialcontracts.Commitment.commitmentLength",
      ProtoAdapterImport),
  )
    with BinarySerializableType
    with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.hash.arr)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = Commitment(ByteStr(bytes.slice(${c.offset}, ${c.offset} + commitmentLength))) -> (${c.offset} + commitmentLength)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.commitmentFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.hash.arr)"
    }
  }

  case object READINGS_HASH extends FieldType(
    scalaType = "ReadingsHash",
    protoType = "bytes",
    scalaImports = Set("com.wavesenterprise.transaction.docker.ReadingsHash",
      "com.wavesenterprise.transaction.docker.ReadingsHash.readingsHashLength",
      ProtoAdapterImport),
  )
    with BinarySerializableType
    with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"${c.output}.write(${c.field}.hash.arr)"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ReadingsHash(ByteStr(bytes.slice(${c.offset}, ${c.offset} + readingsHashLength))) -> (${c.offset} + readingsHashLength)"
    }

    override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.readingsHashFromProto(${c.field})"
    }

    override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
      s"ProtoAdapter.byteArrayToByteString(${c.field}.hash.arr)"
    }
  }

  case object READ_DESCRIPTOR extends FieldType(
    scalaType = "ReadDescriptor",
    protoType = "bytes",
    scalaImports = Set("com.wavesenterprise.transaction.docker.ReadDescriptor",
      ProtoAdapterImport
    ),
  )
    with BinarySerializableType
    with ProtoCompatibleType {

    override val binaryWriter: BinaryWriter = { c =>
      s"ReadDescriptor.writeBytes(${c.field},${c.output})"
    }

    override val binaryReader: BinaryReader = { c =>
      s"val (${c.field}, ${c.field}End) = ReadDescriptor.fromBytes(${c.bytes}, ${c.offset})"
    }

      override val protoToVanillaAdapter: Option[ProtoAdapter] = Some { c =>
        s"ProtoAdapter.readDescriptorFromProto(${c.field})"
      }

      override val vanillaToProtoAdapter: Option[ProtoAdapter] = Some { c =>
        s"ProtoAdapter.toProto(${c.field})"
      }
  }

  case class CUSTOM_TYPE(
      scalaName: String,
      scalaImportsSet: Set[String] = Set.empty,
      protoName: String,
      protoImportsSet: Set[String] = Set.empty
  ) extends FieldType(scalaName, scalaImportsSet, protoName, protoImportsSet)

  override def values: immutable.IndexedSeq[FieldType] = findValues

  private def underlayTypeImportsForProto(underlay: FieldType): Set[String] = {
    underlay match {
      case u: ProtoCompatibleType => u.protoToVanillaAdapter.map(_ => "cats.implicits._").toSet
      case _                      => Set.empty
    }
  }
}
