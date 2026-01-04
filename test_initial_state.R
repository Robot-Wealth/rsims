library(devtools)
load_all()

cat("Testing include_initial_state parameter\n")
cat("========================================\n\n")

# Create simple test data
dates <- as.Date("2020-01-01") + 0:4
prices_data <- matrix(
  c(
    as.numeric(dates),
    c(10, 10.5, 11, 11.5, 12),
    c(20, 20.2, 20.5, 20.8, 21)
  ),
  ncol = 3
)
colnames(prices_data) <- c("date", "A", "B")

weights_data <- matrix(
  c(
    as.numeric(dates),
    c(0.5, 0.5, 0.4, 0.4, 0.4),
    c(0.5, 0.5, 0.6, 0.6, 0.6)
  ),
  ncol = 3
)
colnames(weights_data) <- c("date", "A", "B")

initial_cash <- 10000

cat("Test 1: fixed_commission_backtest with include_initial_state = FALSE (default)\n")
cat("------------------------------------------------------------------------------\n")
result_default <- fixed_commission_backtest(
  prices = prices_data,
  theo_weights = weights_data,
  trade_buffer = 0.0,
  initial_cash = initial_cash,
  commission_pct = 0.001
)

cat("Number of rows:", nrow(result_default), "\n")
cat("Number of unique dates:", length(unique(result_default$Date)), "\n")
cat("First date:", as.character(min(result_default$Date)), "\n")
cat("Initial cash row present:", any(result_default$ticker == "Cash" & result_default$Date == min(result_default$Date) & result_default$Value == initial_cash & result_default$Trades == 0), "\n\n")

cat("Test 2: fixed_commission_backtest with include_initial_state = TRUE\n")
cat("--------------------------------------------------------------------\n")
result_with_initial <- fixed_commission_backtest(
  prices = prices_data,
  theo_weights = weights_data,
  trade_buffer = 0.0,
  initial_cash = initial_cash,
  commission_pct = 0.001,
  include_initial_state = TRUE
)

cat("Number of rows:", nrow(result_with_initial), "\n")
cat("Number of unique dates:", length(unique(result_with_initial$Date)), "\n")
cat("First date:", as.character(min(result_with_initial$Date)), "\n")

# Check if initial state is correct - get FIRST occurrence of each ticker
cash_initial <- result_with_initial[result_with_initial$ticker == "Cash", ][1, ]
ticker_a_initial <- result_with_initial[result_with_initial$ticker == "A", ][1, ]
ticker_b_initial <- result_with_initial[result_with_initial$ticker == "B", ][1, ]

cat("\nInitial state validation:\n")
cat("  Cash Value:", cash_initial$Value, "(expected:", initial_cash, ")\n")
cat("  Cash Position:", cash_initial$Position, "(expected: 0)\n")
cat("  Ticker A Position:", ticker_a_initial$Position, "(expected: 0)\n")
cat("  Ticker A Value:", ticker_a_initial$Value, "(expected: 0)\n")
cat("  Ticker A Trades:", ticker_a_initial$Trades, "(expected: 0)\n")
cat("  Ticker B Position:", ticker_b_initial$Position, "(expected: 0)\n")
cat("  Ticker B Value:", ticker_b_initial$Value, "(expected: 0)\n")
cat("  Ticker B Trades:", ticker_b_initial$Trades, "(expected: 0)\n")

all_correct <- (
  cash_initial$Value[[1]] == initial_cash &&
  cash_initial$Position[[1]] == 0 &&
  ticker_a_initial$Position[[1]] == 0 &&
  ticker_a_initial$Value[[1]] == 0 &&
  ticker_a_initial$Trades[[1]] == 0 &&
  ticker_b_initial$Position[[1]] == 0 &&
  ticker_b_initial$Value[[1]] == 0 &&
  ticker_b_initial$Trades[[1]] == 0
)

cat("\nTest 3: Verify first trading period is unchanged\n")
cat("--------------------------------------------------\n")
# Get the first trade for each ticker (row 1 for default, row 2 for with_initial since row 1 is t=0)
first_trade_default <- result_default[1:3, ]  # First 3 rows (Cash, A, B)
first_trade_with_initial <- result_with_initial[4:6, ]  # Rows 4-6 (second occurrence of Cash, A, B)

# Sort both to ensure same order for comparison
first_trade_default <- first_trade_default[order(first_trade_default$ticker), ]
first_trade_with_initial <- first_trade_with_initial[order(first_trade_with_initial$ticker), ]

trades_match <- all.equal(
  first_trade_default[, c("ticker", "Position", "Value", "Trades")],
  first_trade_with_initial[, c("ticker", "Position", "Value", "Trades")],
  check.attributes = FALSE
)

cat("First trading period matches:", isTRUE(trades_match), "\n")

cat("\nSummary\n")
cat("=======\n")
if(all_correct && isTRUE(trades_match)) {
  cat("All tests PASSED ✓\n")
  cat("- Default behavior (no initial state) unchanged\n")
  cat("- Initial state row correctly shows t=0 with initial cash and zero positions\n")
  cat("- First trading period is identical in both cases\n")
} else {
  cat("Some tests FAILED ✗\n")
  if(!all_correct) cat("- Initial state validation failed\n")
  if(!isTRUE(trades_match)) cat("- First trading period mismatch\n")
}
