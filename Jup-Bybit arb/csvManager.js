import { createObjectCsvWriter } from 'csv-writer';
import csvParser from 'csv-parser';
import fs from 'fs';
import { parseTransaction } from './helius.js';

const csvFilePath = 'transactions.csv';

const csvWriter = createObjectCsvWriter({
    path: csvFilePath,
    header: [
        {id: 'txid', title: 'TXID'},
        {id: 'token', title: 'Token'},
        {id: 'timestamp', title: 'Timestamp'},
        {id: 'pricePerToken', title: 'Price Per Token'}
    ],
    append: false
});

// Function to introduce a delay
const delay = (ms) => new Promise(resolve => setTimeout(resolve, ms));

const readAndUpdateTransactions = async () => {
    const transactions = [];
    fs.createReadStream(csvFilePath)
        .pipe(csvParser())
        .on('data', (row) => transactions.push(row))
        .on('end', async () => {
            console.log('CSV file read successfully.');

            for (const transaction of transactions) {
                try {
                    // Ensure the transaction ID is treated as a string
                    const txIdString = String(transaction.txid);
                    const details = await parseTransaction([txIdString]);

                    if (details && details.length > 0) {
                        transaction.pricePerToken = details[0].pricePerToken;  // Assuming this is the structure
                    }
                } catch (error) {
                    console.error(`Failed to fetch details for transaction ${transaction.txid}:`, error);
                }
                
                // Wait for 500ms before the next API request
                await delay(1000);
            }

            // Write updated transactions back to the CSV
            await csvWriter.writeRecords(transactions);
            console.log('CSV file has been updated with transaction prices.');
        });
}

export { readAndUpdateTransactions };
