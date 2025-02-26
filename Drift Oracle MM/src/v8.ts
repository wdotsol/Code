import { 
    DriftClient, 
    OrderType, 
    MarketType, 
    PostOnlyParams, 
    PositionDirection, 
    Wallet, 
    loadKeypair, 
    BN, 
    BulkAccountLoader 
  } from '@drift-labs/sdk';
  import { Connection } from '@solana/web3.js';
  import * as dotenv from 'dotenv';
  dotenv.config();
  const sleep = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));
  
  /**
   * Function to place both the long and short perp orders.
   * Retries up to 'retries' times with a one-second delay between attempts.
   */
  async function placeInitialPerpOrders(driftClient: DriftClient, perpMarketIndex: number, retries = 3): Promise<boolean> {
    // Long order: buys SOL on perp with an oracle offset of -0.3 USD.
    const longOrderParams = {
      orderType: OrderType.LIMIT,
      marketType: MarketType.PERP,
      marketIndex: perpMarketIndex,
      direction: PositionDirection.LONG,
      baseAssetAmount: driftClient.convertToPerpPrecision(0.1), //0.1 sol
      oraclePriceOffset: driftClient.convertToPricePrecision(-0.3).toNumber(),
      postOnly: PostOnlyParams.NONE,
      reduceOnly: false,
    };
  
    // Short order: sells SOL on perp with an oracle offset of +0.3 USD.
    const shortOrderParams = {
      orderType: OrderType.LIMIT,
      marketType: MarketType.PERP,
      marketIndex: perpMarketIndex,
      direction: PositionDirection.SHORT,
      baseAssetAmount: driftClient.convertToPerpPrecision(0.1), //0.1 sol
      oraclePriceOffset: driftClient.convertToPricePrecision(0.3).toNumber(),
      postOnly: PostOnlyParams.NONE,
      reduceOnly: false,
    };
  
    for (let attempt = 1; attempt <= retries; attempt++) {
      try {
        const longTxSig = await driftClient.placePerpOrder(longOrderParams);
        console.log("Perp long order placed. TxSig:", longTxSig);
        const shortTxSig = await driftClient.placePerpOrder(shortOrderParams);
        console.log("Perp short order placed. TxSig:", shortTxSig);
        return true;
      } catch (error) {
        console.error(`Error placing initial perp orders (attempt ${attempt}):`, error);
        console.log("Waiting 1 second before retrying...");
        await sleep(1000);
      }
    }
    return false;
  }
  
  /**
   * Function to continuously monitor the perp position and hedge only the delta change.
   */
  async function monitorAndHedge(
    driftClient: DriftClient, 
    perpMarketIndex: number, 
    spotMarketIndex: number
  ) {
    // Keep track of the previous net position (initialize to 0)
    let previousNetPosition = new BN(0);
  
    while (true) {
      const userAccount = await driftClient.getUserAccount();
      if (!userAccount) {
        console.log("User account not found. Retrying...");
        await sleep(1000);
        continue;
      }
  
      // Find the perp position for our market.
      const perpPosition = userAccount.perpPositions.find(
        (pos) => pos.marketIndex === perpMarketIndex
      );
      if (!perpPosition) {
        console.log("No perp position for market index", perpMarketIndex, ". Retrying...");
        await sleep(1000);
        continue;
      }
  
      // Get current net perp position.
      const currentNetPosition: BN = perpPosition.baseAssetAmount;
      console.log("Current net perp position (BN):", currentNetPosition.toString());
  
      // Calculate delta (change) since last check.
      const delta = currentNetPosition.sub(previousNetPosition);
      if (delta.eq(new BN(0))) {
        // No change – no hedge needed.
        await sleep(1000);
        continue;
      }
  
      // Get the spot market oracle data.
      const spotOracleData = driftClient.getOracleDataForSpotMarket(spotMarketIndex);
      const oraclePriceBN = spotOracleData.price;
      const offsetBN = driftClient.convertToPricePrecision(0.1); // 0.1 USD offset
      let limitPriceBN: BN;
      let spotOrderParams: any;
  
      if (delta.gt(new BN(0))) {
        // Delta > 0 means our net position increased (more long)
        console.log("Delta detected: increased long by", delta.toString(), ". Hedging by selling on spot.");
        limitPriceBN = oraclePriceBN.sub(offsetBN);
        spotOrderParams = {
          orderType: OrderType.LIMIT,
          marketIndex: spotMarketIndex,
          direction: PositionDirection.SHORT,
          baseAssetAmount: delta,  // hedge the additional long exposure
          price: limitPriceBN,
        };
      } else {
        // Delta < 0 means our net position increased in the short direction.
        console.log("Delta detected: increased short by", delta.abs().toString(), ". Hedging by buying on spot.");
        limitPriceBN = oraclePriceBN.add(offsetBN);
        spotOrderParams = {
          orderType: OrderType.LIMIT,
          marketIndex: spotMarketIndex,
          direction: PositionDirection.LONG,
          baseAssetAmount: delta.abs(),  // hedge the additional short exposure
          price: limitPriceBN,
        };
      }
  
      try {
        const spotTxSig = await driftClient.placeSpotOrder(spotOrderParams);
        console.log("Spot hedge order placed. TxSig:", spotTxSig);
        // Update previous net position after a successful hedge.
        previousNetPosition = currentNetPosition;
  
        // Cancel all outstanding orders to prevent order stacking.
        try {
          await driftClient.cancelOrders(MarketType.PERP);
          console.log("Cancelled all outstanding orders.");
        } catch (cancelError) {
          console.error("Error cancelling orders:", cancelError);
        }
  
        // Once hedged, re-place the initial perp orders with retry.
        const ordersPlaced = await placeInitialPerpOrders(driftClient, perpMarketIndex);
        if (!ordersPlaced) {
          console.error("Failed to re-place perp orders after hedging.");
        }
      } catch (hedgeError) {
        console.error("Error placing spot hedge order:", hedgeError);
      }
  
      // Wait before checking again.
      await sleep(1000);
    }
  }
  
  // Main execution
  (async () => {
    try {
      // SET UP, INPUT YOUR API KEY
      const connection = new Connection("https://mainnet.helius-rpc.com/key here", "confirmed");
      const { Wallet, loadKeypair } = await import('@drift-labs/sdk');
      const wallet = new Wallet(loadKeypair(process.env.PRIVATE_KEY!));
  
      // Create the Drift client with a WebSocket.
      const driftClient = new DriftClient({
        connection,
        wallet,
        env: "mainnet-beta", // or 'devnet'
        accountSubscription: { type: 'websocket' },
      });
      await driftClient.subscribe();
      console.log("Drift client subscribed!");
  
      const perpMarketIndex = 0;
      const spotMarketIndex = 1;
  
      // Place the initial perp orders.
      await placeInitialPerpOrders(driftClient, perpMarketIndex);
  
      // Begin monitoring for delta changes and hedge as needed.
      monitorAndHedge(driftClient, perpMarketIndex, spotMarketIndex);
  
      // Keep the process running.
      process.stdin.resume();
    } catch (error) {
      console.error("Error in bot execution:", error);
      process.exit(1);
    }
  })();
  