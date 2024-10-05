import { Connection, Keypair, VersionedTransaction } from "@solana/web3.js";
import axios from "axios";
import { Wallet } from "@project-serum/anchor";
import bs58 from "bs58";
import dotenv from 'dotenv';

dotenv.config();
const privateKey = process.env.PRIVATE_KEY;
const rpc = process.env.RPC;
const wallet = new Wallet(Keypair.fromSecretKey(bs58.decode(privateKey)));
const connection = new Connection(rpc, "confirmed");

console.debug = () => {}; 

const buyOnJupiter = async (tokenConfig) => {
    const { addy, slippageBps } = tokenConfig;
    try {
        const quoteResponse = await axios.get('https://quote-api.jup.ag/v6/quote', {
            params: {
                inputMint: 'EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v',
                outputMint: addy,
                amount: 25000000, // 25 bucks
                slippageBps: slippageBps
            }
        });

        const swapResponse = await axios.post('https://quote-api.jup.ag/v6/swap', {
            quoteResponse: quoteResponse.data,
            userPublicKey: wallet.publicKey.toString(),
            wrapAndUnwrapSol: true
        }, {
            headers: {'Content-Type': 'application/json'}
        });

        const swapTransactionBuf = Buffer.from(swapResponse.data.swapTransaction, 'base64');
        let transaction = VersionedTransaction.deserialize(swapTransactionBuf);
        const { blockhash } = await connection.getRecentBlockhash("finalized");
        transaction.recentBlockhash = blockhash;
        transaction.sign([wallet.payer]);

        return await sendTransactionWithRetry(transaction);
    } catch (error) {
        console.error('Error in Jupiter transaction:', error);
        return null; // Ensures that a null is returned on failure
    }
};

async function sendTransactionWithRetry(transaction) {
    let retries = 3;
    while (retries > 0) {
        try {
            const rawTransaction = transaction.serialize();
            const txid = await connection.sendRawTransaction(rawTransaction, {
                skipPreflight: false,
                preflightCommitment: "confirmed"
            });
            await connection.confirmTransaction(txid, "confirmed");
            console.log(`@@@ Transaction successful: https://solscan.io/tx/${txid} @@@`);
            return txid;
        } catch (error) {
            console.error(`Attempt to send transaction failed:`);
            retries--;
            if (retries === 0) {
                console.error('All attempts to send the transaction failed.');
                throw error;
            }
            console.log(`Retrying... (${retries} attempts left)`);
        }
    }
    return null; // Ensure a null is returned if all retries fail
}

export default buyOnJupiter;
