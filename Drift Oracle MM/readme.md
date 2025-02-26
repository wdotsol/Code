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
