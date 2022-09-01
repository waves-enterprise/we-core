import { TransactionFactory } from './TransactionsFactory';
import { TRANSACTION_TYPES } from './constants';
import { TRANSACTIONS } from './Transactions';

export * from '@wavesenterprise/signature-generator';
export * from './constants';
export * from './Transactions';
export * from './TransactionsFactory';

export const getTransactionFactory = (version: number, type: number) : TransactionFactory<any> => {
  const key = Object.keys(TRANSACTION_TYPES).find(key => TRANSACTION_TYPES[key] === type)
  if (!key || !TRANSACTIONS[key][`V${version}`]) {
    throw new Error(`no such transaction type: ${type} and version: ${version}`)
  }
  return TRANSACTIONS[key][`V${version}`]
}
