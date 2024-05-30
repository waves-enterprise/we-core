import { 
    BaseProcessor,
    Recipient,
    AssetId,
    ALIAS_VERSION,
    ContractParam,
    LongProcessor
} from '@wavesenterprise/signature-generator';
import { 
    strToBytes,
    concatBytes,
    numberToBytes,
    boolToBytes,
    fromBase58
} from '@wavesenterprise/crypto-utils';
import { BigNumber } from 'bignumber.js';

export class Alias extends BaseProcessor<string> {
    constructor(required: boolean) {
        super(required);
    }
  
    getSignatureBytes(val: string): Promise<Uint8Array> {
        if (!this.networkByte) {
            throw new Error('Cannot create tx signature. Network byte is required')
        }
        const aliasNameBytes = strToBytes(val)
        const aliasBytes = concatBytes(
          Uint8Array.from([ALIAS_VERSION]),
          Uint8Array.from([this.networkByte]),
          numberToBytes(aliasNameBytes.length, 2),
          aliasNameBytes,
      )
        return Promise.resolve(
            concatBytes(
                numberToBytes(aliasBytes.length, 2),
                aliasBytes,
            ),
        )
    }
  }

type FixedBase64ProcessorProps = {
    appendLength?: boolean,
    prefix?: boolean,
    bitSize?: number
}

class FixedBase64Processor extends BaseProcessor<string> {
    constructor(required: boolean, protected readonly props: FixedBase64ProcessorProps = {}) {
        super(required)
    }

    getSignatureBytes(val: string): Promise<Uint8Array> {
        const valueBytes = (this.props.prefix) ?  
            Buffer.from(val.slice(7), 'base64')  : Buffer.from(val, 'base64')

        if (this.props.appendLength) {
            return Promise.resolve(concatBytes(numberToBytes(valueBytes.length, (this.props.bitSize || 16) / 8 ), valueBytes))
        }
        return Promise.resolve(valueBytes)
    }
}

export class Base64 extends FixedBase64Processor {
    constructor (required: boolean) {
        super(required, {
            appendLength: true,
            prefix: true
        })
    }
}

const dataTypeBytes = new Map<ContractParam['type'], number>([
    ['integer', 0],
    ['boolean', 1],
    ['binary', 2],
    ['string', 3],
])

BigNumber.config({
    ROUNDING_MODE: BigNumber.ROUND_DOWN,
});

function performBitwiseAnd(a: BigNumber, b: BigNumber): number {
    const sa = a.toString(2).split('.')[0];
    const sb = b.toString(2).split('.')[0];
    const len = Math.min(sa.length, sb.length);

    const s1 = sa.slice(sa.length - len);
    const s2 = sb.slice(sb.length - len);

    let result = new Array(len);
    for (let i = len - 1; i >= 0; i--) {
        result[i] = (s1[i] === '1' && s2[i] === '1') ? '1' : '0';
    }

    return parseInt(result.join(''), 2);
}

function bigNumberToByteArray(input: BigNumber): number[] {
    if (!input.s) {
        throw new Error('BigNumber input is expected');
    }

    const isMinus = input.lt(new BigNumber(0));
    const performBitwiseAnd255 = performBitwiseAnd.bind(null, new BigNumber(255));
    if (isMinus) {
        input = input.plus(1, 10);
    }

    const bytes = new Array(8);

    for (let k = 7; k >= 0; k--) {
        bytes[k] = performBitwiseAnd255(input);
        if (isMinus) {
            bytes[k] = (~bytes[k]) & 255;
        }
        input = input.div(256);
    }
    return bytes;
}

class FixedDataParamProcessor extends BaseProcessor<ContractParam> {

    constructor(required: boolean) {
        super(required)
    }

    getSignatureBytes(val: ContractParam): Promise<Uint8Array> {
        const keyContentBytes = strToBytes(val.key)
        const keyBytes = concatBytes(numberToBytes(keyContentBytes.length, 2), keyContentBytes)
        const typeByte = numberToBytes(dataTypeBytes.get(val.type)!)
        switch (val.type) {
            case 'string': {
                const contentBytes = strToBytes(val.value)
                const lengthBytes = numberToBytes(contentBytes.length, 2)
                return Promise.resolve(concatBytes(keyBytes, typeByte, lengthBytes, contentBytes))
            }
            case 'boolean': {
                const contentBytes = boolToBytes(val.value)
                return Promise.resolve(concatBytes(keyBytes, typeByte, contentBytes))
            }
            case 'integer': {
                const contentBytes  = bigNumberToByteArray(new BigNumber(val.value))
                return Promise.resolve(concatBytes(keyBytes, typeByte, Uint8Array.from(contentBytes)))
            }
            case 'binary': {
                const base64Content = val.value.startsWith('base64:') ? val.value.slice(7) : val.value
                const contentBytes = Buffer.from(base64Content, 'base64') // Buffer polyfill or something
                const lengthBytes = numberToBytes(contentBytes.length, 2)
                return Promise.resolve(concatBytes(keyBytes, typeByte, lengthBytes, contentBytes))
            }
        }
    }
}


class FixedContractParamProcessor extends BaseProcessor<ContractParam> {

    constructor(required: boolean) {
        super(required)
    }

    getSignatureBytes(val: ContractParam): Promise<Uint8Array> {
        const keyContentBytes = strToBytes(val.key)
        const keyBytes = concatBytes(numberToBytes(keyContentBytes.length, 2), keyContentBytes)
        const typeByte = numberToBytes(dataTypeBytes.get(val.type)!)
        switch (val.type) {
            case 'string': {
                const contentBytes = strToBytes(val.value)
                const lengthBytes = numberToBytes(contentBytes.length, 4)
                return Promise.resolve(concatBytes(keyBytes, typeByte, lengthBytes, contentBytes))
            }
            case 'boolean': {
                const contentBytes = boolToBytes(val.value)
                return Promise.resolve(concatBytes(keyBytes, typeByte, contentBytes))
            }
            case 'integer': {
                const contentBytes  = bigNumberToByteArray(new BigNumber(val.value))
                return Promise.resolve(concatBytes(keyBytes, typeByte, Uint8Array.from(contentBytes)))
            }
            case 'binary': {
                const base64Content = val.value.startsWith('base64:') ? val.value.slice(7) : val.value
                const contentBytes = Buffer.from(base64Content, 'base64') // Buffer polyfill or something
                const lengthBytes = numberToBytes(contentBytes.length, 4)
                return Promise.resolve(concatBytes(keyBytes, typeByte, lengthBytes, contentBytes))
            }
        }
    }

}


export class DataEntry extends FixedDataParamProcessor {}

export class DockerParamEntry extends FixedContractParamProcessor {}

export class FixedRecipient extends BaseProcessor {
    static aliasRegex = /alias:.:/i

    networkByteCurrentClass?: number;

    constructor(required: boolean, networkByteCurrentClass?: number) {
        super(required);
        this.networkByteCurrentClass = networkByteCurrentClass
    }

    getSignatureBytes(val: string): Promise<Uint8Array> {
        const netByte = this.networkByteCurrentClass ? this.networkByteCurrentClass : this.networkByte
        if (Recipient.aliasRegex.test(val)) {
            if (!netByte) {
                throw new Error('Cannot create tx signature. Network byte is required')
            }
            const alias = val.split(':').pop()!
            const aliasBytes = strToBytes(alias)
            return Promise.resolve(
                concatBytes(
                    Uint8Array.from([ALIAS_VERSION]),
                    Uint8Array.from([netByte]),
                    numberToBytes(aliasBytes.length, 2),
                    aliasBytes,
                ),
            )
        } else {
            const valueBytes = fromBase58(val)
            return Promise.resolve(valueBytes)
        }
    }
}
export class Transfers extends BaseProcessor<Array<{
    recipient: string,
    amount: number | string,
}>> {
    constructor(required: boolean) {
        super(required);
    }

    async getSignatureBytes(values: Array<{
        recipient: string,
        amount: number | string,
    }>) {
        const recipientProcessor = new FixedRecipient(true, this.networkByte)
        const amountProcessor = new LongProcessor(true)

        const promises: Array<Promise<Uint8Array>> = []
        values.forEach((item) => {
            promises.push(recipientProcessor.getSignatureBytes((item).recipient))
            promises.push(amountProcessor.getSignatureBytes((item).amount))
        })

        const elements = await Promise.all(promises)

        return concatBytes(numberToBytes(values.length, 2), ...elements)
    }
}

export class Long8Bytes extends BaseProcessor<number | string> {
    getSignatureBytes(val: number | string): Promise<Uint8Array> {
        const contentBytes = bigNumberToByteArray(new BigNumber(val))
        return Promise.resolve(Uint8Array.from(contentBytes))
    }
}

export class PermissionDueTimestamp extends BaseProcessor<number | string> {

    isSpecified(_: string | number): boolean {
        return true  // hack that right 9 zeros, if field is absent
    }

    getSignatureBytes(val: number | string): Promise<Uint8Array> {    
        const contentBytes = super.isSpecified(val) 
        ? concatBytes(new Uint8Array([1]),  Uint8Array.from(bigNumberToByteArray(new BigNumber(val))))
        : new Uint8Array(9).fill(0)
        return Promise.resolve(Uint8Array.from(contentBytes))
    }
}

export class ListWithBigLength<V = any> extends BaseProcessor<V[]> {
    static of<T>(processor: new (...args) => BaseProcessor<T>) {
        return class extends ListWithBigLength<T> {
            constructor(_: boolean) {
                super(processor)
            }
        }
    }

    constructor(public ListItemProcessor: new (...args) => BaseProcessor<V>) {
        super(true);
    }

    async getSignatureBytes(val: V[]): Promise<Uint8Array> {
        const lengthBytes = numberToBytes(val.length, 4)
        if (val.length > 0) {
            const itemProcessor = new this.ListItemProcessor().getSignatureBytes;
            const contentBytes = await Promise.all(val.map(itemProcessor))
            return concatBytes(lengthBytes, ...contentBytes)
        }
        return new Uint8Array([0, 0, 0, 0])
    }
}

export class ArrayOfStringsWithBigLength extends ListWithBigLength.of(Recipient) {}

type FixedContractPayment = {
    assetId?: string,
    amount: number
}

class FixedContractTransferInProcessor extends BaseProcessor<FixedContractPayment> {
    async getSignatureBytes(val: FixedContractPayment): Promise<Uint8Array> {
        const assetIdProcessor = new AssetId(false)

        const existanceBytes = val.assetId
            ? new Uint8Array([1])
            : new Uint8Array([0])

        const amount = Uint8Array.from(bigNumberToByteArray(new BigNumber(val.amount)))

        if (val.assetId) {
            const assetId = await assetIdProcessor.getSignatureBytes(val.assetId)

            return Promise.resolve(concatBytes(existanceBytes, assetId, amount))
        }

        const keyBytes = concatBytes(existanceBytes, amount)

        return Promise.resolve(keyBytes)
    }
}

export class ContractTransferIn extends FixedContractTransferInProcessor {}