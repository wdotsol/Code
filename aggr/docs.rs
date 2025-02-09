use orca_whirlpools::{WhirlpoolsConfigInput, set_whirlpools_config_address};
use solana_client::rpc_client::RpcClient;
use solana_sdk::signature::Signer;

fn config() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
}

fn funder() {
  //code here
  set_funder(wallet.pubkey()).unwrap();
}

//  orca splash pool setup
use orca_whirlpools::{
  create_splash_pool_instructions, set_whirlpools_config_address, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::{pubkey::Pubkey, signature::Signer, signer::keypair::Keypair};
use std::str::FromStr;
use tokio;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
  let token_b = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap(); // devUSDC
  let initial_price = Some(0.01);
  let wallet = Keypair::new(); // CAUTION: This wallet is not persistent.
  let funder = Some(wallet.pubkey());

  let result =
      create_splash_pool_instructions(&rpc, token_a, token_b, initial_price, funder)
          .await
          .unwrap();

  println!("Pool Address: {:?}", result.pool_address);
  println!(
      "Initialization Cost: {} lamports",
      result.initialization_cost
  );
} 

// orca concentrated liq pool
use orca_whirlpools::{
  create_concentrated_liquidity_pool_instructions, set_whirlpools_config_address,
  WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::{pubkey::Pubkey, signature::Signer, signer::keypair::Keypair};
use std::str::FromStr;
use tokio;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
  let token_b = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap(); // devUSDC
  let tick_spacing = 64;
  let initial_price = Some(0.01);
  let wallet = Keypair::new(); // CAUTION: This wallet is not persistent.
  let funder = Some(wallet.pubkey());

  let result = create_concentrated_liquidity_pool_instructions(
      &rpc,
      token_a,
      token_b,
      tick_spacing,
      initial_price,
      funder,
  )
  .await
  .unwrap();

  println!("Pool Address: {:?}", result.pool_address);
  println!(
      "Initialization Cost: {} lamports",
      result.initialization_cost
  );
}

//Fetching splash pool
//Token Mint Addresses: Provide the mint addresses of the two tokens that make up the liquidity pool.
//Fetch Pool Details: Use the appropriate function to fetch the details of the specified Splash Pool.
use orca_whirlpools::{
  fetch_splash_pool, set_whirlpools_config_address, PoolInfo, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
  let token_b = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap(); // devUSDC

  let pool_info = fetch_splash_pool(&rpc, token_a, token_b).await.unwrap();

  match pool_info {
      PoolInfo::Initialized(pool) => println!("Pool is initialized: {:?}", pool),
      PoolInfo::Uninitialized(pool) => println!("Pool is not initialized: {:?}", pool),
  }
}

//Fetching concentrated liquidity pool
//Token Mint Addresses: Provide the mint addresses of the two tokens that make up the liquidity pool.
//Tick Spacing: Specify the tick spacing, which defines the intervals for price ticks.
//Fetch Pool Details: Use the appropriate function to fetch the details of the specified Concentrated Liquidity Pool.
use orca_whirlpools::{
  fetch_concentrated_liquidity_pool, set_whirlpools_config_address, PoolInfo, WhirlpoolsConfigInput
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
  let token_b = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap(); // devUSDC
  let tick_spacing = 64;

  let pool_info = fetch_concentrated_liquidity_pool(&rpc, token_a, token_b, tick_spacing).await.unwrap();

  match pool_info {
      PoolInfo::Initialized(pool) => println!("Pool is initialized: {:?}", pool),
      PoolInfo::Uninitialized(pool) => println!("Pool is not initialized: {:?}", pool),
  }
}

//Fetching pool by token pair
//Token Mint Addresses: Provide the mint addresses of the two tokens that make up the liquidity pool.
//Fetch Pool Details: Use the appropriate function to fetch the details of the specified pools.
use orca_whirlpools::{
  fetch_whirlpools_by_token_pair, set_whirlpools_config_address, PoolInfo, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
  let token_b = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap(); // devUSDC

  let pool_infos = fetch_whirlpools_by_token_pair(&rpc, token_a, token_b)
      .await
      .unwrap();

  for pool_info in pool_infos {
      match pool_info {
          PoolInfo::Initialized(pool) => println!("Pool is initialized: {:?}", pool),
          PoolInfo::Uninitialized(pool) => println!("Pool is not initialized: {:?}", pool),
      }
  }
}

//Opening a splash pool position
/* Pool Address: Provide the address of the Splash Pool where you want to open a position.
Liquidity Parameters: Choose how you want to provide liquidity. You only need to provide one of these parameters, and the function will compute the others in the returned quote based on the current price of the pool:
liquidity: Specify the liquidity value to provide.
tokenA: Specify the amount of token A (first token in the pool).
tokenB: Specify the amount of token B (second token in the pool).
Slippage Tolerance: Set the maximum slippage tolerance (optional, defaults to 1%). Slippage refers to the difference between the expected price and the actual price at which the transaction is executed. A lower slippage tolerance reduces the risk of price changes during the transaction but may lead to failed transactions if the market moves too quickly.
Funder: This will be your wallet, which will fund the transaction.
Create Instructions: Use the appropriate function to generate the necessary instructions. */
use orca_whirlpools::{
  open_full_range_position_instructions, set_whirlpools_config_address, IncreaseLiquidityParam, WhirlpoolsConfigInput
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;
use crate::utils::load_wallet;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let wallet = load_wallet();
  let whirlpool_address = Pubkey::from_str("3KBZiL2g8C7tiJ32hTv5v3KM7aK9htpqTw4cTXz1HvPt").unwrap();
  let param = IncreaseLiquidityParam::TokenA(10);
  
  let result = open_full_range_position_instructions(
      &rpc, 
      whirlpool_address,
      param,
      Some(100),
      Some(wallet.pubkey())
  ).await.unwrap();

  println!("Quote token mac B: {:?}", result.quote.token_max_b);
  println!("Initialization cost: {:?}", result.initialization_cost);
  println!("Position mint: {:?}", result.position_mint);
}

//Opening a concentrated liquidity pool position
/* Pool Address: Provide the address of the Concentrated Liquidity Pool where you want to open a position.
Liquidity Parameters: Choose how you want to provide liquidity. You only need to provide one of these parameters, and the function will compute the others in the returned quote based on the price range and the current price of the pool:
liquidity: Specify the liquidity value to provide.
tokenA: Specify the amount of token A (first token in the pool).
tokenB: Specify the amount of token B (second token in the pool).
Price Range: Set the lower and upper bounds of the price range within which your liquidity will be active. The current price and the specified price range will influence the quote amounts. If the current price is in the middle of your price range, the ratio of token A to token B will reflect that price. However, if the current price is outside your range, you will only deposit one token, resulting in one-sided liquidity. Note that your position will only earn fees when the price falls within your selected price range, so it’s important to choose a range where you expect the price to remain active.
Slippage Tolerance: Set the maximum slippage tolerance (optional, defaults to 1%). Slippage refers to the difference between the expected token amounts and the actual amounts deposited into the liquidity pool. A lower slippage tolerance reduces the risk of depositing more tokens than expected but may lead to failed transactions if the market moves too quickly. For example, if you expect to deposit 100 units of Token A and 1,000 units of Token B, with a 1% slippage tolerance, the maximum amounts would be 101 Token A and 1,010 Token B.
Funder: This can be your wallet, which will fund the pool initialization. If the funder is not specified, the default wallet will be used. You can configure the default wallet through the SDK.
Create Instructions: Use the appropriate function to generate the necessary instructions. */
use orca_whirlpools::{
  open_position_instructions, set_whirlpools_config_address, IncreaseLiquidityParam, WhirlpoolsConfigInput
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;
use crate::utils::load_wallet;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let wallet = load_wallet();
  let whirlpool_address = Pubkey::from_str("3KBZiL2g8C7tiJ32hTv5v3KM7aK9htpqTw4cTXz1HvPt").unwrap();
  let param = IncreaseLiquidityParam::TokenA(10);
  
  let result = open_position_instructions(
      &rpc, 
      whirlpool_address,
      0.001,
      100.0,
      param,
      Some(100),
      Some(wallet.pubkey())
  ).await.unwrap();

  println!("Quote token max B: {:?}", result.quote.token_est_b);
  println!("Initialization cost: {:?}", result.initialization_cost);
  println!("Position mint: {:?}", result.position_mint);
}

//Fetch position for a wallet
use orca_whirlpools::{
  fetch_positions_for_owner, set_whirlpools_config_address, WhirlpoolsConfigInput
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let whirlpool_address =
      Pubkey::from_str("3KBZiL2g8C7tiJ32hTv5v3KM7aK9htpqTw4cTXz1HvPt").unwrap();

  let positions = fetch_positions_for_owner(&rpc, whirlpool_address)
      .await
      .unwrap();

  println!("Positions: {:?}", positions);
}

//Fetch positions in a whirlpool
use orca_whirlpools::{
  fetch_positions_in_whirlpool, set_whirlpools_config_address, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let whirlpool_address =
      Pubkey::from_str("3KBZiL2g8C7tiJ32hTv5v3KM7aK9htpqTw4cTXz1HvPt").unwrap();

  let positions = fetch_positions_in_whirlpool(&rpc, whirlpool_address)
      .await
      .unwrap();

  println!("Positions: {:?}", positions);
}

//Adjust liquidity in a pool
/* Adjusting liquidity in an existing position can be done as follows:
RPC Client: Use a Solana RPC client to interact with the blockchain.
Position Mint: Provide the mint address of the NFT representing your position. This NFT serves as proof of ownership of the position you want to adjust.
Liquidity Parameters: Choose how you want to adjust liquidity. You only need to provide one of these parameters, and the function will compute the others in the returned quote based on the current price of the pool and the price range of the position:
liquidity: Specify the liquidity value to add or remove.
tokenA: Specify the amount of token A to add or withdraw.
tokenB: Specify the amount of token B to add or withdraw.
Slippage tolerance: Set the maximum slippage tolerance (optional, defaults to 1%). Slippage refers to the difference between the expected token amounts added or removed when adjusting liquidity and the actual amounts that are ultimately deposited or withdrawn. A lower slippage tolerance reduces the risk of depositing or withdrawing more or fewer tokens than intended, but it may lead to failed transactions if the market moves too quickly.
Funder: This can be your wallet, which will fund the pool initialization. If a funder is not specified, the default wallet will be used. You can configure the default wallet through the SDK.
Create Instructions: Use the appropriate function to generate the necessary instructions.
 */use orca_whirlpools::{
  decrease_liquidity_instructions, increase_liquidity_instructions, set_whirlpools_config_address, DecreaseLiquidityParam, IncreaseLiquidityParam, WhirlpoolsConfigInput
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;
use crate::utils::load_wallet;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let wallet = load_wallet();
  let position_mint_address = Pubkey::from_str("HqoV7Qv27REUtmd9UKSJGGmCRNx3531t33bDG1BUfo9K").unwrap();
  let increase_param = IncreaseLiquidityParam::TokenA(1_000_000);
  let decrease_param = DecreaseLiquidityParam::TokenA(1_000_000);

  let increase_result = increase_liquidity_instructions(
      &rpc,
      position_mint_address,
      increase_param,
      Some(100),
      Some(wallet.pubkey()),
  )
  .await.unwrap();

  let decrease_result = decrease_liquidity_instructions(
      &rpc,
      position_mint_address,
      decrease_param,
      Some(100),
      Some(wallet.pubkey()),
  )
  .await.unwrap();

  println!("Liquidity Increase Quote: {:?}", increase_result.quote);
  println!("Liquidity Decrease Quote: {:?}", decrease_result.quote);
  println!("Number of Instructions: {}", increase_result.instructions.len());
}

//Fee harvesting
use orca_whirlpools::{
  harvest_position_instructions, set_whirlpools_config_address, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;
use crate::utils::load_wallet;

#[tokio::main]
async fn main() {
  set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
  let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
  let wallet = load_wallet();

  let position_mint_address =
      Pubkey::from_str("HqoV7Qv27REUtmd9UKSJGGmCRNx3531t33bDG1BUfo9K").unwrap();

  let result = harvest_position_instructions(&rpc, position_mint_address, Some(wallet.pubkey()))
      .await
      .unwrap();

  println!("Fees Quote: {:?}", result.fees_quote);
  println!("Rewards Quote: {:?}", result.rewards_quote);
  println!("Number of Instructions: {}", result.instructions.len());
}

//Closing a position
/* To close a position and withdraw all liquidity, follow these steps:
RPC Client: Use a Solana RPC client to interact with the blockchain.
Position Mint: Provide the mint address of the NFT representing your position. This NFT serves as proof of ownership and represents the liquidity in the position.
Parameters for Liquidity: Define the parameters for decreasing liquidity. This can be specified as a liquidity amount or as specific token amounts.
Slippage Tolerance: Set the maximum slippage tolerance (optional, defaults to 1%). Slippage refers to the difference between the expected token amounts you receive when closing a position and the actual amounts returned to your wallet. A lower slippage tolerance reduces the risk of receiving fewer tokens than expected but may lead to failed transactions if the market moves too quickly. For example, if you expect to receive 100 units of Token A and 1,000 units of Token B when closing your position, with a 1% slippage tolerance, the minimum amounts returned would be 99 Token A and 990 Token B.
Authority: This can be your wallet, which will fund the pool initialization. If the authority is not specified, the default wallet will be used. You can configure the default wallet through the SDK.
Create Instructions: Use the appropriate function to generate the necessary instructions. */
use crate::utils::load_wallet;
use orca_whirlpools::{
    close_position_instructions, set_whirlpools_config_address, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
    set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
    let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
    let wallet = load_wallet();

    let position_mint_address =
        Pubkey::from_str("HqoV7Qv27REUtmd9UKSJGGmCRNx3531t33bDG1BUfo9K").unwrap();

    let result = close_position_instructions(
        &rpc,
        position_mint_address,
        Some(100),
        Some(wallet.pubkey()),
    )
    .await
    .unwrap();

    println!("Quote token max B: {:?}", result.quote.token_est_b);
    println!("Fees Quote: {:?}", result.fees_quote);
    println!("Rewards Quote: {:?}", result.rewards_quote);
    println!("Number of Instructions: {}", result.instructions.len());
}

//Executing token swap
/* To execute a token swap in an Orca Whirlpool, follow these steps:
RPC Client: Use a Solana RPC client to interact with the blockchain.
Pool Address: Provide the address of the Orca Whirlpool pool where the swap will take place.
Swap Parameters: Define the swap parameters. You only need to provide one of these parameters, and the function will compute the others in the returned quote based on the current price of the pool:
inputAmount: Specify the amount of tokens to swap (if exact input).
outputAmount: Specify the desired amount of tokens to receive (if exact output).
mint: Provide the mint address of the token you want to swap out.
Slippage tolerance: Set the maximum slippage tolerance (optional, defaults to 1%). Slippage refers to the difference between the expected amounts of tokens received or sent during the swap and the actual amounts executed. A lower slippage tolerance reduces the risk of receiving fewer tokens than expected, but may lead to failed transactions if the market moves too quickly. For example, if you expect to receive 1,000 units of Token B for 100 units of Token A, with a 1% slippage tolerance, the maximum Token A spent will be 101, and the minimum Token B received will be 990.
Signer: This can be your wallet, which will fund the pool initialization. If a signer is not specified, the default wallet will be used. You can configure the default wallet through the SDK.
Create Instructions: Use the appropriate function to generate the necessary instructions for the swap. */
use crate::utils::load_wallet;
use orca_whirlpools::{
    set_whirlpools_config_address, swap_instructions, SwapType, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;

#[tokio::main]
async fn main() {
    set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaDevnet).unwrap();
    let rpc = RpcClient::new("https://api.devnet.solana.com".to_string());
    let wallet = load_wallet();
    let whirlpool_address =
        Pubkey::from_str("3KBZiL2g8C7tiJ32hTv5v3KM7aK9htpqTw4cTXz1HvPt").unwrap();
    let mint_address = Pubkey::from_str("BRjpCHtyQLNCo8gqRUr8jtdAj5AjPYQaoqbvcZiHok1k").unwrap();
    let input_amount = 1_000_000;

    let result = swap_instructions(
        &rpc,
        whirlpool_address,
        input_amount,
        mint_address,
        SwapType::ExactIn,
        Some(100),
        Some(wallet.pubkey()),
    )
    .await
    .unwrap();

    println!("Quote estimated token out: {:?}", result.quote);
    println!("Number of Instructions: {}", result.instructions.len());
}

//By using the SDK, the bot can retrieve the quote object for a potential swap, which includes details about the token amounts and expected output.
//The bot can quickly compare quotes from multiple pools to identify arbitrage opportunities and execute profitable swaps.

//Sending and landing tx'es
//dependencies toml:
/* serde_json = { version = "^1.0" }
solana-client = { version = "^1.18" }
solana-sdk = { version = "^1.18" }
tokio = { version = "^1.41.1" } */

//main.rs
/* use solana_client::nonblocking::rpc_client::RpcClient;
use solana_client::rpc_config::RpcSendTransactionConfig;
use solana_sdk::commitment_config::CommitmentLevel;
use solana_sdk::compute_budget::ComputeBudgetInstruction;
use solana_sdk::message::Message;
use solana_sdk::pubkey::Pubkey;
use solana_sdk::signature::Signature;
use solana_sdk::transaction::Transaction;
use solana_sdk::{signature::Keypair, signer::Signer};
use std::fs;
use std::str::FromStr;
use tokio::time::{sleep, Duration, Instant}; */

//Create transaction message
#[tokio::main]
async fn main() {
  // ...
  let instructions_result = // get instructions from Whirlpools SDK
  let message = Message::new(
    &instructions_result.instructions,
    Some(&wallet.pubkey()),
  );
  let mut signers: Vec<&dyn Signer> = vec![&wallet];
  signers.extend(
    instructions_result
      .additional_signers
      .iter()
      .map(|kp| kp as &dyn Signer),
  );
  let recent_blockhash = rpc.get_latest_blockhash().await.unwrap();
  let transaction = Transaction::new(&signers, message, recent_blockhash);
  // ...
}

//Estimate CU and prio fee
#[tokio::main]
async fn main() {
  // ...
  let simulated_transaction = rpc.simulate_transaction(&transaction).await.unwrap();

  let mut all_instructions = vec![];
  if let Some(units_consumed) = simulated_transaction.value.units_consumed {
    let units_consumed_safe = units_consumed as u32 + 100_000;
    let compute_limit_instruction =
      ComputeBudgetInstruction::set_compute_unit_limit(units_consumed_safe);
    all_instructions.push(compute_limit_instruction);

    let prioritization_fees = rpc
      .get_recent_prioritization_fees(&[whirlpool_address])
      .await
      .unwrap();
    let mut prioritization_fees_array: Vec<u64> = prioritization_fees
      .iter()
      .map(|fee| fee.prioritization_fee)
      .collect();
    prioritization_fees_array.sort_unstable();
    let prioritization_fee = prioritization_fees_array
      .get(prioritization_fees_array.len() / 2)
      .cloned();

    if let Some(prioritization_fee) = prioritization_fee {
      let priority_fee_instruction =
        ComputeBudgetInstruction::set_compute_unit_price(prioritization_fee);
      all_instructions.push(priority_fee_instruction);
    }
  }
  // ...
}

//Sign and submit transaction
#[tokio::main]
async fn main() {
  // ...
  all_instructions.extend(open_position_instructions.instructions);
  let message = Message::new(&all_instructions, Some(&wallet.pubkey()));

  let transaction = Transaction::new(&signers ,message , recent_blockhash);
  let transaction_config = RpcSendTransactionConfig {
    skip_preflight: true,
    preflight_commitment: Some(CommitmentLevel::Confirmed),
    max_retries: Some(0),
    ..Default::default()
  };

  let start_time = Instant::now();
  let timeout = Duration::from_secs(90);
  let send_transaction_result = loop {
    if start_time.elapsed() >= timeout {
      break Err(Box::<dyn std::error::Error>::from("Transaction timed out"));
    }
    let transaction_start_time = Instant::now();
    
    let signature: Signature = rpc
      .send_transaction_with_config(&transaction, transaction_config)
      .await
      .unwrap();
    let statuses = rpc
      .get_signature_statuses(&[signature])
      .await
      .unwrap()
      .value;
    
    if let Some(status) = statuses[0].clone() {
      break Ok((status, signature));
    }

    let elapsed_time = transaction_start_time.elapsed();
    let remaining_time = Duration::from_millis(1000).saturating_sub(elapsed_time);
    if remaining_time > Duration::ZERO {
      sleep(remaining_time).await;
    }
  };

  let signature = send_transaction_result.and_then(|(status, signature)| {
    if let Some(err) = status.err {
      Err(Box::new(err))
    } else {
      Ok(signature)
    }
  });
  println!("Result: {:?}", signature);
}