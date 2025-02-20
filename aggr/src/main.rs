use orca_whirlpools::{
    fetch_whirlpools_by_token_pair, set_whirlpools_config_address, PoolInfo, WhirlpoolsConfigInput,
};
use solana_client::nonblocking::rpc_client::RpcClient;
use solana_sdk::pubkey::Pubkey;
use std::str::FromStr;
use dotenvy::dotenv;
use std::env;

#[tokio::main]
async fn main() {
    dotenv().ok();
    let rpc_url = env::var("RPC_URL").expect("RPC_URL must be set in .env");

    set_whirlpools_config_address(WhirlpoolsConfigInput::SolanaMainnet).unwrap();
    let rpc = RpcClient::new(rpc_url);

    let token_a = Pubkey::from_str("So11111111111111111111111111111111111111112").unwrap();
    let token_b = Pubkey::from_str("EPjFWdd5AufqSSqeM2qN1xzybapC8G4wEGGkZwyTDt1v").unwrap(); // USDC

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
