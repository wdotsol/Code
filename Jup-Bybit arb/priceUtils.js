import { RestClientV5 } from 'bybit-api';
import axios from 'axios';
import { loadConfig } from './configloader.js';

const config = loadConfig();
const bybitClient = new RestClientV5({
  testnet: false,
  key: process.env.BYBIT_KEY,
  secret: process.env.BYBIT_SECRET,
});

// Function to get the best bid price from Bybit's orderbook
export const getBestBidPrice = async (symbol) => {
  try {
    const response = await bybitClient.getOrderbook({
      category: 'spot',
      symbol: symbol,
    });
    const { b: bids } = response.result;
    if (bids.length > 0) {
      const bestBidPrice = parseFloat(bids[0][0]);
      return bestBidPrice;
    } else {
      throw new Error('No bids available');
    }
  } catch (error) {
    console.error('Error fetching Bybit orderbook:', error);
    throw error;
  }
};

// Function to get the price from Jupiter
export const getJupPrice = async (tokenID) => {
  try {
    const response = await axios.get(`https://price.jup.ag/v4/price?ids=${tokenID}`);
    if (response.data.data[tokenID]) {
      return response.data.data[tokenID].price;
    } else {
      throw new Error('Token ID not found');
    }
  } catch (error) {
    console.error('Error fetching Jup price:', error);
    throw error;
  }
};
