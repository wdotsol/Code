import axios from 'axios';
import dotenv from 'dotenv';

dotenv.config();

const API_URL = "https://api.helius.xyz/v0/transactions";
const API_KEY = process.env.HELIUS_KEY;

export const parseTransaction = async (transactionIds) => {
    try {
        const response = await axios.post(`${API_URL}/?api-key=${API_KEY}`, {
            transactions: transactionIds
        }, {
            headers: {
                'Content-Type': 'application/json'
            }
        });
        return handleArbitrage(response.data);
    } catch (error) {
        console.error("Error parsing transactions:", error.response ? error.response.data : error.message);
        return null;
    }
};

export const handleArbitrage = (parsedTransactions) => {
    const details = parsedTransactions.map(tx => {
        const descriptionMatch = tx.description.match(/swapped (\d+\.?\d*) (\w+) for (\d+\.?\d*) (\w+)/);
        if (descriptionMatch) {
            const [, amountSold, sellToken, amountBought, buyToken] = descriptionMatch;
            return {
                amountSold,
                sellToken,
                amountBought,
                buyToken,
                pricePerToken: (parseFloat(amountSold) / parseFloat(amountBought))
            };
        }
        return null;
    }).filter(tx => tx !== null);

    if (details.length > 0) {
        console.log("Parsed trade details:", details);
        return details;
    } else {
        console.error("No valid trading details found.");
        return [];
    }
};
