library(devtools)
load_all()

cat("Testing NA price handling in rsims\n")
cat("====================================\n\n")

# Create test data with NA prices for ticker that doesn't exist yet
# Simulate 3 tickers over 5 days, where ticker C only starts trading on day 3

dates <- as.Date("2020-01-01") + 0:4
prices_data <- matrix(
  c(
    as.numeric(dates),
    c(10, 10.5, 11, 11.5, 12),     # Ticker A - always trading
    c(20, 20.2, 20.5, 20.8, 21),   # Ticker B - always trading
    c(NA, NA, 30, 30.5, 31)        # Ticker C - starts day 3
  ),
  ncol = 4
)
colnames(prices_data) <- c("date", "A", "B", "C")

# Weights where C has zero weight when price is NA
weights_data <- matrix(
  c(
    as.numeric(dates),
    c(0.5, 0.5, 0.4, 0.4, 0.4),    # Ticker A
    c(0.5, 0.5, 0.4, 0.4, 0.4),    # Ticker B
    c(0.0, 0.0, 0.2, 0.2, 0.2)     # Ticker C - zero until day 3
  ),
  ncol = 4
)
colnames(weights_data) <- c("date", "A", "B", "C")

cat("Test 1: Valid case - NA price with zero weight (should succeed)\n")
cat("----------------------------------------------------------------\n")
result1 <- tryCatch({
  bt_result <- fixed_commission_backtest(
    prices = prices_data,
    theo_weights = weights_data,
    trade_buffer = 0.0,
    initial_cash = 10000,
    commission_pct = 0.001
  )
  cat("SUCCESS: Backtest completed with NA prices where weight=0\n")
  final_value <- tail(bt_result[bt_result$ticker == "Cash", ]$Value, 1)
  cat("Final equity:", as.numeric(final_value), "\n\n")
  TRUE
}, error = function(e) {
  cat("FAILED:", e$message, "\n\n")
  FALSE
})

cat("Test 2: Invalid case - NA price with non-zero weight (should error)\n")
cat("---------------------------------------------------------------------\n")
# Create invalid weights where we try to trade ticker C on day 2 (when price is NA)
weights_invalid <- weights_data
weights_invalid[2, "C"] <- 0.1  # Try to allocate 10% to C on day 2 when price is NA

result2 <- tryCatch({
  fixed_commission_backtest(
    prices = prices_data,
    theo_weights = weights_invalid,
    trade_buffer = 0.0,
    initial_cash = 10000,
    commission_pct = 0.001
  )
  cat("FAILED: Should have thrown an error but didn't\n\n")
  FALSE
}, error = function(e) {
  cat("SUCCESS: Correctly caught error:", e$message, "\n\n")
  TRUE
})

cat("Summary\n")
cat("=======\n")
if(result1 && result2) {
  cat("All tests PASSED ✓\n")
  cat("- NA prices with zero weights are handled correctly\n")
  cat("- NA prices with non-zero weights are caught at validation\n")
} else {
  cat("Some tests FAILED ✗\n")
}
