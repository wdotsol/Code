import { RestClientV5 } from 'bybit-api';
import dotenv from 'dotenv';

dotenv.config();

const BYBIT_KEY = process.env.BYBIT_KEY;  // Your Bybit API Key
const BYBIT_SECRET = process.env.BYBIT_SECRET;  // Your Bybit API Secret

const client = new RestClientV5({
    testnet: false,
    key: BYBIT_KEY,
    secret: BYBIT_SECRET,
});

const sellOnBybit = async (symbol, qty, price, pricePrecision, orderSizePrecision) => {
  try {
    const roundedPrice = Math.ceil(price * Math.pow(10, pricePrecision)) / Math.pow(10, pricePrecision);
    const roundedQty = Math.floor(qty * Math.pow(10, orderSizePrecision)) / Math.pow(10, orderSizePrecision);
    
    const response = await client.submitOrder({
        category: 'spot',
        symbol,
        side: 'Sell',
        orderType: 'Limit',
        qty: String(roundedQty),
        price: String(roundedPrice),
        timeInForce: 'GTC',
        isLeverage: 1,
    });
    console.log('Order placed on Bybit:', response);
    return response;
  } catch (error) {
    console.error('Error placing order on Bybit:', error);
    throw error;
  }
}

export default sellOnBybit;
