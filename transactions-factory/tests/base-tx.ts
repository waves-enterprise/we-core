import * as expect from 'expect';

import {TRANSACTIONS,} from "../src";

const tx_bytes = "2MBoJcatRLKfcLmUiQLx9mTsJNzWReRK9qXeeo1Gzab2Bh9Dp6jabPP5ZCHhucnQyqWk3rsuqy8KQR2huY9ozCqvA35BsBWUVc2pvHRwh2PYAxAGE8UcKGuNoyERtADTd1CrsSLGy4xyn5A54pmgFR8g4bJRLY2sXMrEiUTU6H2B3Rp1E5GmuYBvYVN93WwdSucBYYZx3o271gDyWCNvwhsUuCdWMRe2147WFaQtfbwbnx7D81EW2Kz1CDQAqWst9NKAc5CtFKoFGruBsXtBygzugXMJvapnXS8tkFnLmv3RT7Pj3c5gySZmZkxHzM2kTX5"
const v4_bytes = '51mNYmztZTMkm1PuNfh7Z8Sxrwvy7kiR4MRpU5QFdqjRKqwDi6gSoFzQUruzSBzZV7NFmYTTuLHprmNv7xLWMApY4WKSeqCtHrRGRQva7eFj5d3zgaqVsuFANpYzfb9cqfCoqj5bqU7Tc6ct1YcyxsJZGPqCpYYA4WaJC37yhGsBJztAq8h7gkdqUTJfixLJMWni34P3cfJVfPbyHQpaBMpdnY1qgpLHLDAcHi1JAW2j2z9NZhP9v35yYWFeUCco5MRtJFEmguYkzAdtk5ApWsbns88auqePuCBoL9MB9NzWdEPBzE9RbFZYuu2zhqX5'


const keys = {
    publicKey: 'AhQHhsmx9EWrJy6juLovM5DZdYCt7cTeoyX2yaTqDdNP',
    privateKey: 'HkXPWCt8xYNFmPfqVXpPtvcNEjr9Z235TQUBZ9e6DMQd',
    address: '3NeTLphUBaPDjKmHPhNVRdQqS46Xf6AB28o',
}


describe('', () => {

    it('should create signature for call contract v4', async () => {
        const tx = TRANSACTIONS.Data.V2({
            data: [
                {
                    key: 'test',
                    value: 'test',
                    type: 'string'
                }
            ],
            timestamp: 1663935233358,
            senderPublicKey: '4znB9RB4uPWdVxzfTDPhd78KdpAQc4co8A3WVhdoZEhL',
            authorPublicKey: '4znB9RB4uPWdVxzfTDPhd78KdpAQc4co8A3WVhdoZEhL',
            fee: 0
        });


        console.log(JSON.stringify(tx.getBody()))
        const bytes = await tx.getBytes();


        expect(Buffer.from(bytes).toString('base64')).toEqual("DAI7YD8P6GskLgMC/aH/8ahjgnOKjbD1VWD7Jbu6TzhJVztgPw/oayQuAwL9of/xqGOCc4qNsPVVYPslu7pPOElXAAEABHRlc3QDAAR0ZXN0AAABg2pEvU4AAAAAAAAAAAA=")
    })

    // it('should create signature for call contract v4', async () => {
    //     const issueBytes = 'sBpdZyMU2wT3YFoyLAyyD28yptX4mqEwGSjZsG8bfHzRLztqzs4J7hCoAgAWeDX9yBFTx1oKrusbcAeP2QZ3UkNkeG539RvgMbM1'
    //
    //     const tx = TRANSACTIONS.Issue.V2({
    //         chainId: 'V'.charCodeAt(0),
    //         decimals: 8,
    //         quantity: 10000,
    //         description: "Test",
    //         reissuable: false,
    //         name: 'SWAP',
    //         timestamp: 1663935233358,
    //         senderPublicKey: '4znB9RB4uPWdVxzfTDPhd78KdpAQc4co8A3WVhdoZEhL',
    //         fee: 0
    //     });
    //
    //     const bytes = await tx.getBytes();
    //
    //
    //     expect(Buffer.from(bytes).toString('base64')).toEqual("AwJWO2A/D+hrJC4DAv2h//GoY4Jzio2w9VVg+yW7uk84SVcABFNXQVAABFRlc3QAAAAAAAAnEAgAAAAAAAAAAAAAAAGDakS9TgA=")
    // })


    // it('should create signature for call contract v4', async () => {
    //     const tx = TRANSACTIONS.CallContract.V4({
    //         timestamp: 1663510702148,
    //         params: [
    //             {
    //                 type: 'string',
    //                 key: 'action',
    //                 value: 'transfer',
    //             },
    //             {
    //                 type: "string",
    //                 key: 'asset',
    //                 value: "Cb1xsvZYjHgjZAZsdMMtbTwKhJY1kYLfpq1K65uJXJVM"
    //             },
    //             {
    //                 type: "string",
    //                 key: 'recipient',
    //                 value: "3NukhxdTpDxXPYky45EPTg1tinzZDUWcbcB"
    //             },
    //             {
    //                 type: "integer",
    //                 key: 'qty',
    //                 value: 150
    //             }
    //         ],
    //         contractId: '9TvMKZuebH6NS33it9TeEH8CxmhxKqfunTYaJJc1AMyW',
    //         senderPublicKey: 'AhQHhsmx9EWrJy6juLovM5DZdYCt7cTeoyX2yaTqDdNP',
    //         fee: 100000000,
    //         contractVersion: 1,
    //     });
    //
    //     const bytes = await tx.getBytes();
    //
    //     // expect(toBase58(bytes)).toEqual(v4_bytes)
    // })
    //
    // it('should create signature for call contract v5', async () => {
    //     const tx = TRANSACTIONS.CallContract.V5({
    //         timestamp: 1663510702148,
    //         params: [
    //             {
    //                 type: 'string',
    //                 key: 'action',
    //                 value: 'transfer',
    //             },
    //             {
    //                 type: "string",
    //                 key: 'asset',
    //                 value: "Cb1xsvZYjHgjZAZsdMMtbTwKhJY1kYLfpq1K65uJXJVM"
    //             },
    //             {
    //                 type: "string",
    //                 key: 'recipient',
    //                 value: "3NukhxdTpDxXPYky45EPTg1tinzZDUWcbcB"
    //             },
    //             {
    //                 type: "integer",
    //                 key: 'qty',
    //                 value: 150
    //             }
    //         ],
    //         contractId: '9TvMKZuebH6NS33it9TeEH8CxmhxKqfunTYaJJc1AMyW',
    //         senderPublicKey: 'AhQHhsmx9EWrJy6juLovM5DZdYCt7cTeoyX2yaTqDdNP',
    //         fee: 100000000,
    //         contractVersion: 1,
    //         payments: [{
    //             assetId: 'WEST',
    //             amount: 1000
    //         }]
    //     });
    //
    //     const bytes = await tx.getBytes();
    //
    //     expect(toBase58(bytes)).toEqual(tx_bytes)
    // })
})
