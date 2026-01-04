#' FX Backtest
#'
#' @description Quasi event-based simulation for FX portfolios with proper
#' USD-denominated P&L conversion for all pair types (XXX/USD, USD/XXX, and crosses).
#'
#' This function works for both spot FX and FX CFDs. The P&L mechanics are
#' identical: regardless of instrument type, P&L accrues in the quote currency
#' and must be converted to your account currency (USD). For example, trading
#' AUD/JPY generates P&L in JPY whether spot or CFD - the quote_currency_rates
#' matrix handles the conversion to USD.
#'
#' @param prices Matrix of exchange rates. Column 1 must be the timestamp or index.
#' @param theo_weights Matrix of theoretical weights. Column 1 must be the timestamp or index.
#'   Positive = long base currency, negative = short base currency.
#' @param base_currency_rates Matrix of XXX/USD rates for each pair's base currency.
#'   Used for position sizing (converting USD equity to base currency units).
#'   For XXX/USD pairs, this equals the pair's price.
#'   For USD/XXX pairs, this is 1.0 (base is USD).
#'   For crosses like EUR/JPY, this is the EUR/USD rate.
#' @param quote_currency_rates Matrix of multipliers to convert quote-currency P&L to USD.
#'   For XXX/USD pairs, this is 1.0 (P&L already in USD).
#'   For USD/XXX pairs, this is 1/USDXXX (e.g., 1/USDJPY).
#'   For crosses like EUR/JPY, this is 1/USDJPY.
#' @param swap_rates Matrix of daily swap rates. Must have paired columns for each pair:
#'   {PAIR}_long and {PAIR}_short. Positive = credit, negative = debit.
#'   If NULL, no swap costs are applied.
#' @param trade_buffer Trade buffer parameter - rebalance when position deviates by more than this
#' @param initial_cash Initial cash balance in USD
#' @param commission_pct Percent commission charged on trades (as decimal, e.g., 0.0001 = 1bp)
#' @param capitalise_profits If TRUE, use current equity (including profits) for position sizing.
#'   If FALSE, use min(initial_cash, equity) - profits accrue but don't compound.
#' @param include_initial_state If TRUE, prepends an initial state row (t=0) to results.
#'
#' @return Long dataframe of results with columns:
#'   ticker, Date, Close, Position, Value, PnlQuote, PnlUsd, Swap, Trades, TradeValue, Commission
#'
#' @details
#' `theo_weights` should be date-aligned with `prices` - it is up to the user to lag
#' `theo_weights` as necessary to ensure that trades occur at appropriate prices.
#'
#' Position sizing works as follows:
#' 1. cap_equity = capitalise_profits ? equity : min(initial_cash, equity)
#' 2. target_value_usd = cap_equity * weight
#' 3. target_position = target_value_usd / base_currency_rate
#'
#' P&L is calculated as:
#' 1. pnl_quote = position * (current_price - previous_price)
#' 2. pnl_usd = pnl_quote * quote_currency_rate
#'
#' @section Spot FX vs CFDs:
#' This function handles both spot FX and FX CFDs identically because the P&L
#' calculation is the same:
#' \itemize{
#'   \item P&L always accrues in the quote currency (e.g., JPY for AUD/JPY)
#'   \item The \code{quote_currency_rates} matrix converts this to USD
#'   \item Swap rates (spot) and financing charges (CFDs) are handled via \code{swap_rates}
#' }
#' The only difference is settlement mechanism (physical vs cash), which doesn't
#' affect backtesting calculations.
#'
#' @examples
#' \dontrun{
#' results <- fx_backtest(
#'   prices = prices_matrix,
#'   theo_weights = weights_matrix,
#'   base_currency_rates = base_rates,
#'   quote_currency_rates = quote_rates,
#'   trade_buffer = 0.05,
#'   initial_cash = 100000,
#'   commission_pct = 0.00005,
#'   capitalise_profits = TRUE
#' )
#' }
#' @export
fx_backtest <- function(prices,
                        theo_weights,
                        base_currency_rates,
                        quote_currency_rates,
                        swap_rates = NULL,
                        trade_buffer = 0.,
                        initial_cash = 10000,
                        commission_pct = 0,
                        capitalise_profits = FALSE,
                        include_initial_state = FALSE) {

  # Input validation
  if (trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  # Check dimension matching FIRST (before timestamp checks)
  if (!isTRUE(all.equal(dim(prices), dim(theo_weights))))
    stop("Prices and weights matrices must have same dimensions")

  if (!isTRUE(all.equal(dim(prices), dim(base_currency_rates))))
    stop("Prices and base_currency_rates matrices must have same dimensions")

  if (!isTRUE(all.equal(dim(prices), dim(quote_currency_rates))))
    stop("Prices and quote_currency_rates matrices must have same dimensions")

  # Check timestamp alignment between prices and weights
  misaligned_timestamps <- which(prices[, 1] != theo_weights[, 1])
  if (length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with weights timestamps at indexes {paste(misaligned_timestamps, collapse=', ')}"))

  # Check timestamp alignment between prices and base_currency_rates
  misaligned_timestamps <- which(prices[, 1] != base_currency_rates[, 1])
  if (length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with base_currency_rates at indexes {paste(misaligned_timestamps, collapse=', ')}"))

  # Check timestamp alignment between prices and quote_currency_rates
  misaligned_timestamps <- which(prices[, 1] != quote_currency_rates[, 1])
  if (length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with quote_currency_rates at indexes {paste(misaligned_timestamps, collapse=', ')}"))

  # Validate that NA prices don't occur where we want to trade
  # Handle single-column case (after removing date column)
  prices_data <- prices[, -1, drop = FALSE]
  weights_data <- theo_weights[, -1, drop = FALSE]
  na_price_with_weight <- is.na(prices_data) & weights_data != 0
  if (any(na_price_with_weight, na.rm = TRUE)) {
    problem_rows <- which(apply(na_price_with_weight, 1, any))
    stop(glue::glue("NA prices detected where theo_weights is non-zero at row(s): {paste(problem_rows, collapse=', ')}. Fix upstream data."))
  }

  # Validate swap_rates if provided
  if (!is.null(swap_rates)) {
    misaligned_timestamps <- which(prices[, 1] != swap_rates[, 1])
    if (length(misaligned_timestamps) > 0)
      stop(glue::glue("Prices timestamps misaligned with swap_rates at indexes {paste(misaligned_timestamps, collapse=', ')}"))
  }

  # Get pair names for output
  pairs <- colnames(prices)[-1]
  num_pairs <- length(pairs)

  # Initialize state
  current_positions <- rep(0, num_pairs)
  previous_prices <- rep(NA, num_pairs)
  row_list <- vector(mode = "list", length = nrow(prices))
  cash <- initial_cash

  # Backtest loop
  for (i in 1:nrow(prices)) {
    current_date <- prices[i, 1]
    current_prices <- prices[i, -1]
    current_weights <- theo_weights[i, -1]
    current_base_rates <- base_currency_rates[i, -1]
    current_quote_rates <- quote_currency_rates[i, -1]

    # Get swap rates for this period (if provided)
    if (!is.null(swap_rates)) {
      # Extract swap rates - expect columns like EURUSD_long, EURUSD_short
      swap_long <- numeric(num_pairs)
      swap_short <- numeric(num_pairs)
      for (j in seq_along(pairs)) {
        long_col <- paste0(pairs[j], "_long")
        short_col <- paste0(pairs[j], "_short")
        if (long_col %in% colnames(swap_rates)) {
          swap_long[j] <- swap_rates[i, long_col]
        }
        if (short_col %in% colnames(swap_rates)) {
          swap_short[j] <- swap_rates[i, short_col]
        }
      }
    } else {
      swap_long <- rep(0, num_pairs)
      swap_short <- rep(0, num_pairs)
    }

    # Calculate P&L from price changes (in quote currency, then convert to USD)
    pnl_quote <- current_positions * (current_prices - previous_prices)
    pnl_quote <- ifelse(is.na(pnl_quote), 0, pnl_quote)
    pnl_usd <- pnl_quote * current_quote_rates
    pnl_usd <- ifelse(is.na(pnl_usd), 0, pnl_usd)

    # Calculate swap costs (direction-dependent)
    # Swap is applied to position value in quote currency, then converted to USD
    position_value_quote <- current_positions * current_prices
    swap_quote <- ifelse(current_positions > 0,
                         abs(position_value_quote) * swap_long,
                         ifelse(current_positions < 0,
                                abs(position_value_quote) * swap_short,
                                0))
    swap_quote <- ifelse(is.na(swap_quote), 0, swap_quote)
    swap_usd <- swap_quote * current_quote_rates
    swap_usd <- ifelse(is.na(swap_usd), 0, swap_usd)

    # Update cash with P&L and swap
    cash <- cash + sum(pnl_usd) + sum(swap_usd)

    # Calculate current position values in USD
    position_value_usd <- current_positions * current_prices / current_base_rates
    position_value_usd <- ifelse(is.na(position_value_usd), 0, position_value_usd)

    # Calculate equity
    equity <- sum(position_value_usd) + cash

    # Apply capitalise_profits flag
    cap_equity <- ifelse(capitalise_profits, equity, min(initial_cash, equity))

    # Calculate target positions using trade buffer
    # The C++ function works with weights, so we need to pass weights directly
    # But we need positions in base currency units, so we'll convert after
    target_positions <- positionsFromNoTradeBuffer(
      current_positions,
      current_prices,
      current_weights,
      cap_equity,
      trade_buffer
    )

    # The C++ function returns: target_weight * cap_equity / price
    # But for FX, we need: target_value_usd / base_currency_rate
    # Since target_value_usd = cap_equity * weight, and position = target_value_usd / base_rate
    # We need to adjust: multiply by (current_prices / current_base_rates)
    # Actually, let's recalculate properly for FX:
    # target_value_usd = cap_equity * current_weights
    # target_positions_fx = target_value_usd / current_base_rates

    # For now, we'll compute target positions directly for FX
    # and use the trade buffer logic ourselves
    current_weights_actual <- (current_positions * current_prices / current_base_rates) / cap_equity
    current_weights_actual <- ifelse(is.na(current_weights_actual) | !is.finite(current_weights_actual), 0, current_weights_actual)

    target_positions_fx <- numeric(num_pairs)
    for (j in seq_along(pairs)) {
      theo_weight <- current_weights[j]

      # Exit position entirely if NA or zero target weight
      if (is.na(theo_weight) || theo_weight == 0) {
        target_positions_fx[j] <- 0
        next
      }

      # If trade_buffer is zero, always rebalance to theo_weight
      if (trade_buffer == 0) {
        target_value_usd <- cap_equity * theo_weight
        target_positions_fx[j] <- target_value_usd / current_base_rates[j]
        next
      }

      # Calculate buffer bounds
      lower_bound <- theo_weight - trade_buffer / 2
      upper_bound <- theo_weight + trade_buffer / 2

      # Clip bounds at zero to prevent wrong-sign positions
      if (theo_weight > 0) {
        lower_bound <- max(0, lower_bound)
      } else {
        upper_bound <- min(0, upper_bound)
      }

      # Check if current weight is within buffer
      if (current_weights_actual[j] < lower_bound) {
        # Rebalance to lower bound (for fixed commission, rebalance to edge)
        target_value_usd <- cap_equity * lower_bound
        target_positions_fx[j] <- target_value_usd / current_base_rates[j]
      } else if (current_weights_actual[j] > upper_bound) {
        # Rebalance to upper bound
        target_value_usd <- cap_equity * upper_bound
        target_positions_fx[j] <- target_value_usd / current_base_rates[j]
      } else {
        # Within buffer, keep current position
        target_positions_fx[j] <- current_positions[j]
      }
    }

    # Calculate trades
    trades <- target_positions_fx - current_positions
    trade_value_quote <- trades * current_prices
    trade_value_usd <- trade_value_quote / current_base_rates
    trade_value_usd <- ifelse(is.na(trade_value_usd), 0, trade_value_usd)

    # Calculate commissions (on USD trade value)
    commissions <- abs(trade_value_usd) * commission_pct

    # Update cash for trades and commissions
    # When we buy base currency, we spend USD; when we sell, we receive USD
    # trade_value_usd represents the USD value of the trade
    cash <- cash - sum(trade_value_usd) - sum(commissions)

    # Update positions
    current_positions <- target_positions_fx

    # Recalculate position values after trade
    position_value_usd <- current_positions * current_prices / current_base_rates
    position_value_usd <- ifelse(is.na(position_value_usd), 0, position_value_usd)

    # Build output row
    row_mat <- matrix(
      data = c(
        rep(as.numeric(current_date), num_pairs + 1),
        c(1, current_prices),
        c(cash, current_positions),
        c(cash, position_value_usd),
        c(sum(pnl_quote), pnl_quote),
        c(sum(pnl_usd), pnl_usd),
        c(sum(swap_usd), swap_usd),
        c(0, trades),
        c(-sum(trade_value_usd), trade_value_usd),
        c(0, commissions)
      ),
      nrow = num_pairs + 1,
      ncol = 10,
      byrow = FALSE,
      dimnames = list(
        c("Cash", pairs),
        c("Date", "Close", "Position", "Value", "PnlQuote", "PnlUsd", "Swap", "Trades", "TradeValue", "Commission")
      )
    )

    row_list[[i]] <- row_mat
    previous_prices <- current_prices
  }

  # Optionally prepend initial state (t=0)
  if (include_initial_state) {
    initial_row <- matrix(
      data = c(
        rep(as.numeric(prices[1, 1]), num_pairs + 1),
        c(1, prices[1, -1]),
        c(initial_cash, rep(0, num_pairs)),
        c(initial_cash, rep(0, num_pairs)),
        rep(0, num_pairs + 1),
        rep(0, num_pairs + 1),
        rep(0, num_pairs + 1),
        rep(0, num_pairs + 1),
        rep(0, num_pairs + 1),
        rep(0, num_pairs + 1)
      ),
      nrow = num_pairs + 1,
      ncol = 10,
      byrow = FALSE,
      dimnames = list(
        c("Cash", pairs),
        c("Date", "Close", "Position", "Value", "PnlQuote", "PnlUsd", "Swap", "Trades", "TradeValue", "Commission")
      )
    )
    row_list <- c(list(initial_row), row_list)
  }

  # Combine list of matrices into dataframe
  do.call(rbind, row_list) %>%
    tibble::as_tibble(rownames = "ticker") %>%
    dplyr::mutate(
      Date = as.Date(Date, origin = "1970-01-01")
    )
}
