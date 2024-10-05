import { loadConfig } from './configloader.js';
import buyOnJupiter from './jup.js';
import sellOnBybit from './bybit.js';
import { getBestBidPrice, getJupPrice } from './priceUtils.js';
import { parseTransaction } from './helius.js';

const config = loadConfig();
const enabledTokenConfigs = config.tradingPairs.filter(pair => pair.enabled);

// Function to calculate dynamic slippage based on the arbitrage opportunity
function calculateDynamicSlippage(arbOpportunityPercent, baseSlippageBps) {
  let additionalSlippage = 0;
  const scalingFactor = 25; // Increase slippage by 25 bps for every 1% above the threshold
  if (arbOpportunityPercent > 1) {
    additionalSlippage = Math.ceil((arbOpportunityPercent - 1) * scalingFactor);
  }
  return baseSlippageBps + additionalSlippage;
}

const checkAndExecuteArbitrage = async (tokenConfig) => {
  try {
    const jupPrice = await getJupPrice(tokenConfig.jupTokenID);
    const bybitPrice = await getBestBidPrice(tokenConfig.bybitSymbol);
    const arbOpportunity = (bybitPrice - jupPrice) / jupPrice * 100;

    if (arbOpportunity > tokenConfig.arbThreshold) {
      // Calculate dynamic slippage
      const dynamicSlippage = calculateDynamicSlippage(arbOpportunity, tokenConfig.slippageBps);
      const txId = await buyOnJupiter({...tokenConfig, slippageBps: dynamicSlippage});
      console.log(`Arbitrage opportunity detected for ${tokenConfig.jupTokenID}, ${arbOpportunity}% slippage: ${dynamicSlippage}` );
      if (!txId) throw new Error('Failed to complete purchase on Jupiter.');

      if (tokenConfig.bybitSell) {
        await new Promise(resolve => setTimeout(resolve, 2500));  // Delay for processing
        const tradeDetails = await parseTransaction([txId]);
        if (!tradeDetails) throw new Error('Failed to parse transaction.');
        const result = await sellOnBybit(tokenConfig.bybitSymbol, tradeDetails.qty, tradeDetails.price);
        if (!result) throw new Error(`Failed to execute sell order on Bybit for ${tokenConfig.bybitSymbol}.`);
        console.log('Arbitrage completed successfully for', tokenConfig.jupTokenID);
      } else {
        console.log(`Acquired ${tradeDetails.qty} of ${tokenConfig.jupTokenID} without selling on Bybit.`);
      }
    }
  } catch (error) {
    console.error('Arbitrage failed for', tokenConfig.jupTokenID, ':', error.message);
  }
}

const intervalTime = 1000; 
enabledTokenConfigs.forEach(tokenConfig => {
  setInterval(() => {
    checkAndExecuteArbitrage(tokenConfig);
  }, intervalTime);
});

const statusLogInterval = 60000;
setInterval(() => {
  console.log("System running, checking arbitrage opportunities...");
}, statusLogInterval);
