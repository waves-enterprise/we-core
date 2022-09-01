import {ByteProcessor, TxType, TxVersion, utils} from '@wavesenterprise/signature-generator'

const {concatUint8Arrays, crypto} = utils;

export interface Processor<T> {
    isValid(): boolean;

    getSignatureBytes(): Promise<Uint8Array>

    getErrors(): string[]

    getBody(): {
        version: number,
        type: number
    } & Omit<T, 'tx_type'>

    getId(): Promise<string>;
}

type TransactionFields = { tx_type: TxType<any>, version: TxVersion<any> } & Record<string, ByteProcessor<any>>
type getTxType<T> = { [key in keyof T]?: T[key] extends ByteProcessor<infer P> ? P : never }
export type TransactionType<T> = getTxType<T> & Processor<getTxType<T>>
export type TransactionFactory<T> = (tx?: Partial<getTxType<T>>) => TransactionType<T>

export class TransactionClass<T extends TransactionFields> {
    public version: number
    public tx_type: number
    public senderPublicKey?: string
    public val: T
    public id?: string
    public proofs?: string[]

    constructor(val: T, tx?: Partial<getTxType<T>>) {
        this.val = val
        this.version = val.version.version
        this.tx_type = val.tx_type.type
        if (tx) {
            Object.keys(tx).forEach(key => {
                this[key] = tx[key]
            })
        }
    }

    getSignatureBytes = async () => {
        const errors = this.getErrors()
        if (errors.length) {
            throw new Error(errors.join('\n'))
        }
        const multipleDataBytes = await Promise.all(Object.keys(this.val).map(async key => {
            let bytes: Uint8Array
            let value = this[key]
            if (key.startsWith('duplicate')) {
                value = this[key.split('_').pop()]
            }
            try {
                if (!this.val[key].required && !this.val[key].allowNull) {
                    if (!value) {
                        bytes = Uint8Array.from([0])
                    } else {
                        bytes = concatUint8Arrays(Uint8Array.from([1]), await this.val[key].getSignatureBytes(value))
                    }
                } else {
                    bytes = await this.val[key].getSignatureBytes(value)
                }
            } catch (err) {
                throw new Error(`${key}: ${err.message || err}`)
            }
            return bytes
        }))
        if (multipleDataBytes.length === 1) {
            return multipleDataBytes[0]
        } else {
            return concatUint8Arrays(...multipleDataBytes)
        }
    }

    getId = async () => {
        const dataBytes = await this.getSignatureBytes()

        return crypto.buildTransactionId(dataBytes)
    }

    isValid = () => {
        return !this.getErrors().length
    }

    getErrors = (): string[] | null => {
        const that = this
        return [].concat(...Object.keys(that.val).filter(key => !key.startsWith('duplicate')).map(key => {
            const error = that.val[key].getError(that[key])
            return error ? `${key}: ${error}` : null
        }).filter(Boolean))
    }

    getBody = () => {
        const data = {} as any
        Object.keys(this).forEach(key => {
            if (typeof this[key] !== 'function' && key !== 'val') {
                data[key] = this[key]
            }
        })
        delete data.tx_type
        return {
            ...data,
            version: this.version,
            type: this.tx_type
        }
    }
}

export const createTransactionsFactory = <T extends TransactionFields>(val: T) =>
    (tx?: Partial<getTxType<T>>): TransactionType<T> =>
        (new TransactionClass(val, tx)) as unknown as TransactionType<T>
