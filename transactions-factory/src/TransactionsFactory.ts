import {BaseProcessor} from '@wavesenterprise/signature-generator'
import {FromTxProcessor, TxProcessor} from "./types";
import {BaseTx} from "./base-tx";

export class Processor extends BaseProcessor {
    getSignatureBytes(_: any): Promise<Uint8Array> {
        throw new Error("Deprecated")
    }
}

export function createTransactionsFactory<T extends TxProcessor>(val: T) {
    return (tx?: Partial<FromTxProcessor<T>>): BaseTx<T> => {
        return new BaseTx(val, tx);
    }
}
