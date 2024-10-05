import { parseTransaction } from './helius.js';

const txId = '3Kjw3AgZooDjdaKaAqWm57qvsiQv1BiY7wn7u5A5Qpu1Z7WbBL5bqQf2AaJJai2ztJKKr9Ufags3Ve7fbt7QGrSz';

// Since parseTransaction is async, you need to wait for the promise to resolve
parseTransaction([txId])  // Make sure to pass an array as your function expects it
    .then(details => {
        console.log("Transaction details:");
    })
    .catch(error => {
        console.error("Error fetching transaction details:", error);
    });
