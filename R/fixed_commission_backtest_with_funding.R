#' Fixed Commission Backtest with Funding
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or index.
#' @param theo_weights Matrix of theoretical weights. Column 1 must be the timestamp or index.
#' @param funding_rates Matrix of funding rates applied to positions held at the end of the period in percent per period paid to long positions. Column 1 must be the timestamp or index.
#' @param trade_buffer Trade buffer parameter
#' @param initial_cash Inital cash balance
#' @param commission_pct Percent commission charged on trades
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance in determining position sizes. If FALSE, profits accrue as a cash balance and are not reinvested.
#'
#' @return long dataframe of results - dates, trades, commissions, value of portfolio components
#' @details
#' `theo_weights` should be date-aligned with `prices` - it is up to the user to lag `theo_weights` as necessary to
#' ensure that trades occur at appropriate prices. Specifically, the function assumes that
#' `target_weights` are date-aligned with `prices` such that the price at which
#' you assume you trade into a target weight has the same index value.
#'
#' `funding_rates` represents daily funding accrued to positions at the end of the period in percent per period paid to longs.
#' @examples
#' @export
fixed_commission_backtest_with_funding <- function(prices, theo_weights, funding_rates, trade_buffer = 0., initial_cash = 10000, commission_pct = 0, capitalise_profits = FALSE) {
  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != theo_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with funding timestamps at prices indexes {misaligned_timestamps}"))

  misaligned_timestamps <- which(prices[, 1] != funding_rates[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with funding rates timestamps at prices indexes {misaligned_timestamps}"))

  if(!all.equal(dim(prices), dim(theo_weights), dim(funding_rates)))
    stop("Prices, funding, and weights matrixes must have same dimensions")

  # get tickers for later
  tickers <- colnames(prices)[-1]

  # initial state
  num_assets <- ncol(prices) - 1  # -1 for date column
  current_positions <- rep(0, num_assets)
  previous_theo_weights <- rep(0, num_assets)
  row_list <- vector(mode = "list", length = nrow(prices))  # preallocate for slight speedup
  cash <- initial_cash

  # backtest loop
  for(i in 1:nrow(prices)) {
    # TODO: does wrapping in as.numeric() speed up? (creates array as opposed to named vector). Don't need the names later?
    # TODO: check that date is first column in backtest_theo_weights and backtest_prices
    # TODO: checks on data alignment, length etc
    current_date <- prices[i, 1]
    current_prices <- prices[i, -1]
    current_theo_weights <- theo_weights[i, -1]
    current_funding_rates <- funding_rates[i, -1]

    # accrue funding on current positions
    funding <- current_positions*current_prices*current_funding_rates
    cash <- cash + sum(funding, na.rm = TRUE)

    # update equity
    equity <- sum(current_positions * current_prices, na.rm = TRUE) + cash
    cap_equity <- ifelse(capitalise_profits, equity, min(initial_cash, equity))  # min reflects assumption that we don't top up strategy equity if in drawdown

    # update positions based on no-trade buffer
    # TODO: consider commissions in calculating position sizes (otherwise can go over leverage 1)
    target_positions <- positionsFromNoTradeBuffer(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer)

    # calculate position deltas, trade values and commissions
    trades <- target_positions - current_positions
    trade_value <- trades * current_prices
    commissions <- abs(trade_value) * commission_pct

    # adjust cash by value of trades
    cash <- cash - sum(trade_value, na.rm = TRUE) - sum(commissions, na.rm = TRUE)
    current_positions <- target_positions
    position_value <- current_positions * current_prices
    equity <- sum(position_value, na.rm = TRUE) + cash

    # store date in matrix as numeric (then convert date back to date format later)
    row_mat <- matrix(
      data = c(
        rep(as.numeric(current_date), num_assets + 1),
        c(1, current_prices),
        c(cash, current_positions),
        c(cash, position_value),
        c(-sum(trade_value), trades),
        c(-sum(trade_value), trade_value),
        c(0, funding),  # no interest on cash balance
        c(0, commissions)
      ),
      nrow = num_assets + 1,
      ncol = 7,
      byrow = FALSE,
      dimnames = list(
        # tickers are row names
        c("Cash", tickers),
        # column names
        c("Date", "Close", "Position", "Value", "Trades", "TradeValue", "Funding", "Commission")
      )
    )

    row_list[[i]] <- row_mat

    previous_theo_weights <- current_theo_weights
  }

  # Combine list of matrixes into dataframe
  do.call(rbind, row_list) %>%
    tibble::as_tibble(rownames = "ticker") %>%
    dplyr::mutate(
      Date = as.Date(Date, origin ="1970-01-01")
    )
}