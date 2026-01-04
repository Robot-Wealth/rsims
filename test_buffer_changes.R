library(devtools)
load_all()

cat('Test 1: Normal case - should hold (52.6% is within 50% ± 5%)\n')
result1 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,52.6,47), c(10, 10, 10), c(0, 0.5, 0.5), 1000, 0.1)
print(result1)

cat('\nTest 2: Outside buffer - should rebalance (56% is outside 50% ± 5%)\n')
result2 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,56,44), c(10, 10, 10), c(0, 0.5, 0.5), 1000, 0.1)
print(result2)

cat('\nTest 3: Small position - should hold (3% is within 5% ± 5%, clipped at [0%, 10%])\n')
result3 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,3,97), c(10, 10, 10), c(0, 0.05, 0.95), 1000, 0.1)
print(result3)

cat('\nTest 4: Small position near zero - should hold if within clipped bounds\n')
result4 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,2,98), c(10, 10, 10), c(0, 0.02, 0.98), 1000, 0.1)
print(result4)

cat('\nTest 5: Target zero - should always exit to zero\n')
result5 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,3,97), c(10, 10, 10), c(0, 0.0, 1.0), 1000, 0.1)
print(result5)

cat('\nTest 6: Short position - should hold (-38% is within -40% ± 5%)\n')
result6 <- rsims:::positionsFromNoTradeBufferMinComm(c(0,-38,138), c(10, 10, 10), c(0, -0.40, 1.40), 1000, 0.1)
print(result6)

cat('\nTest 7: Fixed commission version - rebalances to edge\n')
result7 <- rsims:::positionsFromNoTradeBuffer(c(0,56,44), c(10, 10, 10), c(0, 0.5, 0.5), 1000, 0.1)
print(result7)
