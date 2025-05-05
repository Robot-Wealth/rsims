library(testthat)

test_that("positionsFromNoTradeBuffer handles long, short, and zero weights correctly", {
  prices <- c(10, 20, 5)
  cap_equity <- 10000
  buffer <- 0.1

  # Start with positions at 0
  pos <- c(0, 0, 0)

  # Case 1: long position with theo weight = 0.2 (outside buffer)
  theo_weights <- c(0.2, 0, 0)
  result <- positionsFromNoTradeBuffer(pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], (0.2 * (1 - buffer)) * cap_equity / prices[1])
  expect_equal(result[2], 0)
  expect_equal(result[3], 0)

  # Case 2: short position with theo weight = -0.3 (outside buffer)
  theo_weights <- c(-0.3, 0, 0)
  result <- positionsFromNoTradeBuffer(pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], (-0.3 * (1 - buffer)) * cap_equity / prices[1])


  # Case 3: weight is zero or NA — position should be exited
  theo_weights <- c(0, NA, 0.1)
  current_pos <- c(100, -50, 200)
  result <- positionsFromNoTradeBuffer(current_pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], 0)
  expect_equal(result[2], 0)
  expect_equal(result[3], current_pos[3])  # no change, because it's inside the buffer (no trade)
})

test_that("positionsFromNoTradeBufferMinComm handles long, short, and zero weights correctly", {
  prices <- c(10, 20, 5)
  cap_equity <- 10000
  buffer <- 0.1

  # Start with positions at 0
  pos <- c(0, 0, 0)

  # Case 1: long position with theo weight = 0.2 (outside buffer)
  theo_weights <- c(0.2, 0, 0)
  result <- positionsFromNoTradeBufferMinComm(pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], 0.2 * cap_equity / prices[1])
  expect_equal(result[2], 0)
  expect_equal(result[3], 0)

  # Case 2: short position with theo weight = -0.3 (outside buffer)
  theo_weights <- c(-0.3, 0, 0)
  result <- positionsFromNoTradeBufferMinComm(pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], -0.3 * cap_equity / prices[1])

  # Case 3: within buffer — no trade
  current_pos <- c((0.2 * cap_equity / prices[1]), 0, 0)
  theo_weights <- c(0.2, 0.1, -0.1)
  result <- positionsFromNoTradeBufferMinComm(current_pos, prices, theo_weights, cap_equity, buffer)
  expect_equal(result[1], current_pos[1])  # unchanged
})
