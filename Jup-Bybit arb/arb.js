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
  const scalingFactor = 10; // Increase slippage by 10 bps for every 1% above the threshold
  if (arbOpportunityPercent > 1) {
    additionalSlippage = Math.ceil((arbOpportunityPercent - 1) * scalingFactor);
  }
  const totalSlippage = baseSlippageBps + additionalSlippage;
  return Math.min(totalSlippage, 100); // Caps the slippage at 100 bps
}

const checkAndExecuteArbitrage = async (tokenConfig) => {
  try {
    const jupPrice = await getJupPrice(tokenConfig.jupTokenID);
    const bybitPrice = await getBestBidPrice(tokenConfig.bybitSymbol);
    const arbOpportunity = (bybitPrice - jupPrice) / jupPrice * 100;

    if (arbOpportunity > tokenConfig.arbThreshold) {
      const dynamicSlippage = calculateDynamicSlippage(arbOpportunity, tokenConfig.slippageBps);
      buyOnJupiter({...tokenConfig, slippageBps: dynamicSlippage});
      console.log(`Arbitrage opportunity detected for ${tokenConfig.jupTokenID}, ${arbOpportunity}% slippage: ${dynamicSlippage}`);
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
