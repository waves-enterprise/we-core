### TRANSACTIONS-FACTORY

Using this library you can easily create transactions for Waves Enterprise blockchain.

#### Generated files:
- src/Transactions.ts - containt generated tx stubs
- src/constants.ts - contains constants: tx type and version 

### Example:

```typescript
const {TRANSACTIONS} = require('@wavesenterprise/transactions-factory')

const tx = {
    senderPublicKey: "34qsNWsKKQaysTzpsf4aTyRS6Q1BoUuBntgGVj6SHZg3",
    contractId: "DP5MggKC8GJuLZshCVNSYwBtE6WTRtMM1YPPdcmwbuNg",
    params: [{"type":"integer", "key": "height", "value": 100}],
    fee: 1000000,
    timestamp: 1598008066632,
    feeAssetId: "WAVES"
}

const callTx = TRANSACTIONS.CALL_CONTRACT.V3(tx)
callTx.contractVersion = 2
...

// get bytes
const bytes = callTx.getBytes()

// get validation errors
const errors = callTx.getErrors()


```
