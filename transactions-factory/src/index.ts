import {TransactionFactory} from './types';
import {TRANSACTION_TYPES} from './constants';
import {TRANSACTIONS} from './Transactions';

export {
    ArrayOfStringsWithLength,
    AssetId,
    AtomicBadge,
    AtomicInnerTransaction,
    Base58,
    Base58WithLength,
    Bool,
    Byte,
    ByteArrayWithSize,
    ContractApiVersion,
    DataEntryMap,
    Integer,
    List,
    Long,
    PermissionOpType,
    PermissionRole,
    Recipient,
    StoredContract,
    StringWithLength,
    TxType,
    TxVersion,
    ValidationPolicy
} from '@wavesenterprise/signature-generator';
export { 
    Base64,
    Alias,
    DataEntry,
    Long8Bytes,
    ArrayOfStringsWithBigLength,
    DockerParamEntry,
    ContractTransferIn,
    Transfers
   } from './FixedTypes';
export * from './constants';
export * from './TransactionsFactory';
export * from './Transactions';

export * from './types';
export * from './base-tx';

export const getTransactionFactory = (version: number, type: number): TransactionFactory<any> => {
    const key = Object.keys(TRANSACTION_TYPES).find(key => TRANSACTION_TYPES[key] === type)
    if (!key || !TRANSACTIONS[key][`V${version}`]) {
        throw new Error(`no such transaction type: ${type} and version: ${version}`)
    }
    return TRANSACTIONS[key][`V${version}`]
}
