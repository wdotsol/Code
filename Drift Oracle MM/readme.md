# Oracle-Based Market Making Bot on Drift

This guide introduces a simple market maker bot for Drift that uses **oracle-based limit orders** to peg its quotes to an external price feed (PYTH). The bot's purpose is to continuously quote buy/sell orders around the oracle price and hedge any filled positions to remain market-neutral. The bot hedges the position by executing an order on the opposing spot market.

## Core Functionality

The bot's operation can be broken down into five main components:

### 1. Placing Initial Perp Oracle Orders

- The bot places both a long and a short perp oracle limit order.

### 2. Monitoring Loop

- Once orders are placed, the bot continuously monitors for any changes in positions.
- The loop ensures that any filled orders are detected within 1 second.

### 3. Hedging with Spot Orders

- When the bot detects a net delta change perp market position(due to an order fill), it hedges that exposure by placing an opposing order on the spot market.

### 4. Order Management / Re-quoting

- After hedging, the bot cancels any outstanding orders and re-quotes new orders based on the updated oracle price.

### 5. Retry Mechanism for Order Placement

- The bot implements a retry logic to handle any errors or network issues during order placement.

## Setting up the bot

### Dependencies
To run the bot, you will need:
- Node.js and Typescript
- Drift SDK @drift-labs/sdk
- Solana Web3.js, v1.92.3
- (Optional) dotevn, for managing environment variables

Install the packages:
```bash
npm install @drift-labs/sdk @solana/web3.js@1.92.3 dotenv
```
*Keep in mind that v1.92.3 is used at the time of writing*

### Wallet setup
You will need a wallet keypair in order to interact with Drift and place trades. For safety measures, ```loadKeypair``` is used to load a keypair from the .env file, however you could set the keypair directly in the code.

### RPC connection
In order to interact with Solana, the bot needs an RPC endpoint. in this example the bot uses one from Helius, but you can use any Solana RPC provider. Set the RPC URL in the code or as an env variable in .env

### Drift client initialization
After completing the above setup, the following code initializes the Drift SDK client.

```
import { Connection } from '@solana/web3.js';
import { DriftClient, Wallet, loadKeypair } from '@drift-labs/sdk';

const connection = new Connection("<YOUR_RPC_ENDPOINT>", "confirmed");
const wallet = new Wallet(loadKeypair(process.env.PRIVATE_KEY!));
const driftClient = new DriftClient({
    connection,
    wallet,
    env: "mainnet-beta",    // or "devnet", depending on target environment
    accountSubscription: { type: 'websocket' }
});

await driftClient.subscribe();
console.log("Drift client subscribed!");
```

### Important remarks
In the code, we use the market indices for Sol.

```
perpMarketIndex = 0; 
spotMarketIndex = 1;
```

These correspond to the assets you want the bot to trade. Make sure you have the right indices, which can be found in the Drift documentation.

