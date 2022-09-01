import express = require('express');
import bodyParser = require('body-parser-bigint')
import { TRANSACTION_TYPES, TRANSACTIONS } from '../../src'
import {config} from '@wavesenterprise/signature-generator'

config.set({
    networkByte: 84
})

const app: express.Application = express();

app.use(bodyParser.json())
app.get('/', (_, res) => {
    res.send()
})
app.post('/networkByte', (req, res) => {
    try {
        const {networkByte} = req.body
        if (!networkByte) {
            res.status(400).send({error: 'networkByte is required'})
        }
        config.set({
            networkByte: parseInt(networkByte)
        })
        res.status(200).send()
    } catch (err) {
        res.send({error: err.message || err})
    }
})
app.post('/', async (req, res) => {
    try {
        const {version, type, ...tx} = req.body
        const versionKey = `V${version}`
        const typeKey = Object.keys(TRANSACTION_TYPES).find(key => TRANSACTION_TYPES[key] === type)

        const generator = TRANSACTIONS[typeKey][versionKey]

        if (!generator) {
            return res.send({error: 'No such tx type'})
        }

        const signatureGenerator = generator(tx);
        const Uint8Bytes = await signatureGenerator.getSignatureBytes();
        const Int8Bytes = Int8Array.from(Uint8Bytes)
        return res.send({bytes: Array.from(Int8Bytes)})
    } catch (err) {
        console.trace(err)
        res.send({error: err.message || err})
    }
});

app.listen(3000, () => {
    console.log('App is listening on port 3000!');
});
