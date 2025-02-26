import { 
	DriftClient, 
	OrderType, 
	MarketType, 
	PositionDirection, 
	Wallet, 
	loadKeypair, 
	BN, 
	BulkAccountLoader 
  } from '@drift-labs/sdk';
  import { Connection, ConfirmOptions } from '@solana/web3.js';
  import * as dotenv from 'dotenv';
  dotenv.config();
  
  (async () => {
	try {
	  // SET UP: Replace with your RPC API key and ensure PRIVATE_KEY is set in your .env file.
	  const connection = new Connection(
		"https://mainnet.helius-rpc.com/?api-key=YOUR_API_KEY", // <-- INSERT YOUR RPC API KEY HERE
		"confirmed"
	  );
	  const wallet = new Wallet(loadKeypair(process.env.PRIVATE_KEY!)); // <-- Ensure PRIVATE_KEY is set in .env
	  const opts: ConfirmOptions = { commitment: 'confirmed' };
	  const bulkAccountLoader = new BulkAccountLoader(connection, 'confirmed', 1000);
  
	  // Create the Drift client (using WebSocket for account updates)
	  const driftClient = new DriftClient({
		connection,
		wallet,
		env: "mainnet-beta", // <-- Change to 'devnet' if needed
		accountSubscription: {
		  type: 'websocket',
		  accountLoader: bulkAccountLoader,
		},
		opts,
	  });
	  await driftClient.subscribe();
	  console.log("Drift client subscribed!");
  
	  // Define the spot market index for your asset (commonly 1 for SOL, but verify)
	  const spotMarketIndex = 1; // <-- CONFIRM THIS IS THE CORRECT SPOT MARKET INDEX
  
	  // Retrieve the oracle data for the spot market.
	  // This returns an object with the price as a BN.
	  const oracleData = driftClient.getSpotOracleData(spotMarketIndex);
	  console.log("Spot Oracle Data:", oracleData);
  
	  // Perform BN arithmetic:
	  // Get the oracle price BN (already in internal precision)
	  const oraclePriceBN = oracleData.price; // BN
	  
	  // Convert a 0.01 USD offset into BN (internal price precision)
	  const offsetBN = driftClient.convertToPricePrecision(0.01); // 0.01 USD as BN
	  
	  // Calculate the limit sell price: just below the oracle price
	  const limitPriceBN = oraclePriceBN.sub(offsetBN);
	  console.log("Calculated Limit Price (BN):", limitPriceBN.toString());
  
	  // Prepare the spot order parameters for a limit sell order.
	  // Set the order size (e.g., 0.1 of the asset)
	  const baseAssetAmount = driftClient.convertToSpotPrecision(spotMarketIndex, 0.1); // <-- Adjust size as needed
	  
	  const spotOrderParams = {
		orderType: OrderType.LIMIT,
		marketIndex: spotMarketIndex,
		direction: PositionDirection.SHORT, // SELL on spot
		baseAssetAmount,                    // Size in base asset (BN)
		price: limitPriceBN,                // Use the BN limit price calculated above
		// Optionally add postOnly, reduceOnly, etc.
	  };
  
	  // Place the spot limit sell order
	  const spotTxSig = await driftClient.placeSpotOrder(spotOrderParams);
	  console.log("Spot limit sell order placed. TxSig:", spotTxSig);
  
	} catch (error) {
	  console.error("Error in test script:", error);
	  process.exit(1);
	}
  })();
  