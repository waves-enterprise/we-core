import {TransactionFactory} from './types';
import {TRANSACTION_TYPES} from './constants';
import {TRANSACTIONS} from './FixedTransactions';

export * from '@wavesenterprise/signature-generator';
export * from './constants';
export * from './FixedTransactions';
export * from './FixedTypes';
export * from './TransactionsFactory';

export * from './types';
export * from './base-tx';

export const getTransactionFactory = (version: number, type: number): TransactionFactory<any> => {
    const key = Object.keys(TRANSACTION_TYPES).find(key => TRANSACTION_TYPES[key] === type)
    if (!key || !TRANSACTIONS[key][`V${version}`]) {
        throw new Error(`no such transaction type: ${type} and version: ${version}`)
    }
    return TRANSACTIONS[key][`V${version}`]
}
