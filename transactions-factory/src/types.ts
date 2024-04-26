import {BaseProcessor, Byte, Long, TxType, TxVersion} from "@wavesenterprise/signature-generator";
import {BaseTx} from "./base-tx";

export type TxCommon = {
  tx_type: TxType<any>;
  version: TxVersion<any>;
  timestamp: Long;
  chainId?: Byte;
}


export type TxFactory<T extends TxProcessor> = (tx?: Partial<FromTxProcessor<T>>) => BaseTx<T>

export type TxProcessor = TxCommon & Record<string, BaseProcessor<any>>;
export type FromTxProcessor<T extends TxProcessor> = {
  [key in keyof T]?: T[key] extends BaseProcessor<infer P> ? P : never
}

export type TransactionFactory<T extends TxProcessor> = (tx?: FromTxProcessor<T>) => BaseTx<T>



