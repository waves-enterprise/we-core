import {IntProcessor} from "@wavesenterprise/signature-generator";
import {FromTxProcessor, TxProcessor} from "./types";
import {concatBytes} from "@wavesenterprise/crypto-utils";

const excludeFields = ['tx_type', 'version']

export class BaseTx<T extends TxProcessor> {
  public schema: T

  public type: number;
  public version: number;
  protected networkByte?: number

  constructor(val: T, public readonly data?: Partial<FromTxProcessor<TxProcessor>>) {
    this.schema = val;

    this.version = val.version.version;
    this.type = val.tx_type.type;

    this.data.timestamp = data.timestamp ?? Date.now();
    this.data.tx_type = val.tx_type.type;
    this.data.version = val.version.version;
  }

  getBody = () => {
    const data = {...this.data}

    delete data.tx_type

    return {
      ...data,
      version: this.version,
      type: this.type
    }
  }

  private signKeys() {
    return Object.keys(this.schema).filter(k => !excludeFields.includes(k))
  }

  async getBytes() {
    const txType = this.type;
    const version = this.version;
    const txTypeBytes = await new IntProcessor().getSignatureBytes(txType)
    const versionBytes = await new IntProcessor().getSignatureBytes(version)

    this.ensureChainId();

    const bytes = await Promise.all(this.signKeys().map(async (key) => {
      const value = this.data[key]
      const processor = this.schema[key]

      processor.setNetworkByte(this.networkByte)
      let bytes: Uint8Array;


      if (!processor.isSpecified(value) && processor.isRequired()) {
        throw new Error(`${key} is required`)
      }

      try {
        if (!processor.isRequired()) {
          if (!processor.isSpecified(value)) {
            bytes = new Uint8Array([0])
          } else {
            bytes = concatBytes(new Uint8Array([1]), await processor.getSignatureBytes(value))
          }
        } else {
          bytes = await processor.getSignatureBytes(value)
        }
      } catch (e) {
        throw new Error(`${key}: ${e.message || e}`)
      }

      return bytes
    }))
    return concatBytes(txTypeBytes, versionBytes, ...bytes)
  }

  private ensureChainId() {
    if (this.schema.chainId && !this.data.chainId) {
      this.data.chainId = this.networkByte;
    }
  }

  setNetworkByte(networkByte: number) {
    this.networkByte = networkByte;
  }
}