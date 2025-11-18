#' Fixed Commission Backtest with Funding
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or index.
#' @param target_weights Matrix of theoretical weights. Column 1 must be the timestamp or index.
#' @param funding_rates Matrix of funding rates applied to positions held at the end of the period in percent per period paid to long positions. Column 1 must be the timestamp or index.
#' @param trade_buffer Trade buffer parameter
#' @param margin Percentage of exposure required as margin
#' @param initial_cash Inital cash balance
#' @param commission_pct Percent commission charged on trades
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance in determining position sizes. If FALSE, profits accrue as a cash balance and are not reinvested.
#' @param include_initial_state If TRUE, prepends an initial state row (t=0) to the results showing initial cash and zero positions. Defaults to FALSE for backward compatibility.
#'
#' @return long dataframe of results - dates, trades, commissions, value of portfolio components
#' @details
#' `target_weights` should be date-aligned with `prices` - it is up to the user to lag `target_weights` as necessary to
#' ensure that trades occur at appropriate prices. Specifically, the function assumes that
#' `target_weights` are date-aligned with `prices` such that the price at which
#' you assume you trade into a target weight has the same index value.
#'
#' `funding_rates` represents daily funding accrued to positions at the end of the period in percent per period paid to longs.
#' @examples
#' @export
fixed_commission_backtest_with_funding <- function(prices, target_weights, funding_rates, trade_buffer = 0., initial_cash = 10000, margin = 0.05, commission_pct = 0, capitalise_profits = FALSE, include_initial_state = FALSE) {
  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != target_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with funding timestamps at prices indexes {misaligned_timestamps}"))

  misaligned_timestamps <- which(prices[, 1] != funding_rates[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Prices timestamps misaligned with funding rates timestamps at prices indexes {misaligned_timestamps}"))

  if(!all.equal(dim(prices), dim(target_weights)))
    stop("Prices and weights matrixes must have same dimensions")

  if(!all.equal(dim(prices), dim(funding_rates)))
    stop("Prices and funding matrixes must have same dimensions")

  # Validate that NA prices don't occur where we want to trade
  na_price_with_weight <- is.na(prices[, -1]) & target_weights[, -1] != 0
  if(any(na_price_with_weight, na.rm = TRUE)) {
    problem_rows <- which(apply(na_price_with_weight, 1, any))
    stop(glue::glue("NA prices detected where target_weights is non-zero at row(s): {paste(problem_rows, collapse=', ')}. Fix upstream data."))
  }

  # check for NA in weights and funding matrixes
  if(any(is.na(target_weights))) {
    warning("NA present in target weights: consider replacing these values before continuing")
  }

  if(any(is.na(funding_rates))) {
    warning("NA present in funding rates: consider replacing these values before continuing")
  }

  # get tickers for later
  tickers <- colnames(prices)[-1]

  # initial state
  num_assets <- ncol(prices) - 1  # -1 for date column
  current_positions <- rep(0, num_assets)
  previous_target_weights <- rep(0, num_assets)
  previous_prices <- rep(NA, num_assets)
  row_list <- vector(mode = "list", length = nrow(prices))  # preallocate for slight speedup
  cash <- initial_cash
  maint_margin <- 0

  # backtest loop
  for(i in 1:nrow(prices)) {
    # TODO: does wrapping in as.numeric() speed up? (creates array as opposed to named vector). Don't need the names later?
    # TODO: check that date is first column in backtest_target_weights and backtest_prices
    # TODO: checks on data alignment, length etc
    current_date <- prices[i, 1]
    current_prices <- prices[i, -1]
    current_target_weights <- target_weights[i, -1]
    current_funding_rates <- funding_rates[i, -1]

    # print(glue::glue("current_prices: {current_prices}"))
    # print(glue::glue("current_rates: {current_funding_rates}"))

    # accrue funding on current positions
    funding <- current_positions*current_prices*current_funding_rates
    funding <- ifelse(is.na(funding), 0, funding)
    # print(glue::glue("funding: {funding}"))

    # pnl for the period: price change + funding
    # TODO: later add interest on cash balance here too
    period_pnl <- current_positions * (current_prices - previous_prices)  # equivalent to settled cash in CME futures
    period_pnl <- ifelse(is.na(period_pnl), 0, period_pnl) + funding  # shouldn't have any NA in funding

    # update cash balance - includes adding back yesterday's margin and deducting today's margin
    # set na.rm = TRUE in sum functions as prices can have NA value
    cash <- cash + sum(period_pnl) + maint_margin - margin*sum(abs(current_positions)*current_prices, na.rm = TRUE)

    # update margin requirements
    # TODO: assumption: each contract has same maintenance margin requirements
    position_value <- current_positions * current_prices
    maint_margin <- margin * sum(abs(position_value), na.rm = TRUE)

    # force reduce position if exceeds maintenance margin requirements
    margin_call <- FALSE
    liq_contracts <- rep(0, num_assets)
    liq_commissions <- rep(0, num_assets)
    liq_trade_value <- rep(0, num_assets)
    if(cash + maint_margin < maint_margin) {
      margin_call <- TRUE

      # liquidate equal proportions of each contract holding
      # liquidate 5% extra to requirement for bringing maint_margin into line with cash
      liquidate_factor <- 1.05*(maint_margin + cash)/maint_margin

      # liquidate equal proportions of each contract (probably not how exchange would actually do it)
      # but only liquidate a maximum amount of existing positions
      liq_contracts <- pmax(
        sign(current_positions) * liquidate_factor * abs(current_positions),
        sign(current_positions) * abs(current_positions)
      ) # sign() to account for possible short positions
      liq_trade_value <- liq_contracts*current_price
      liq_commissions <- abs(liq_trade_value) * commission_pct

      current_positions <- current_positions - liq_contracts
      position_value <- current_positions * current_prices

      # account for freed margin
      # freed_margin <- margin*sum(abs(liq_contracts)*current_price)
      cash <- cash + maint_margin -margin*sum(abs(position_value), na.rm = TRUE) - sum(liq_commissions, na.rm = TRUE)

      maint_margin <- margin*sum(abs(position_value), na.rm = TRUE)
    }

    # update equity
    # TODO: this is not correct - should be cash plus margin
    equity <- cash + maint_margin  # sum(current_positions * current_prices, na.rm = TRUE) + cash
    cap_equity <- ifelse(capitalise_profits, equity, min(initial_cash, equity))  # min reflects assumption that we don't top up strategy equity if in drawdown
    # print(glue::glue("cap eq: {cap_equity}, eq: {equity}"))

    # update positions based on no-trade buffer
    # TODO: consider commissions in calculating position sizes (otherwise can go over leverage 1)
    target_positions <- positionsFromNoTradeBuffer(current_positions, current_prices, current_target_weights, cap_equity, trade_buffer)

    # calculate position deltas, trade values and commissions
    trades <- target_positions - current_positions
    trade_value <- trades * current_prices
    commissions <- abs(trade_value) * commission_pct

    # post-trade cash:  cash freed up from closing positions, cash used as margin, commissions
    target_position_value <- target_positions * current_prices
    post_trade_cash <- cash + maint_margin - margin*sum(abs(target_position_value), na.rm = TRUE) - sum(commissions, na.rm = TRUE)
    # print(glue::glue("post_trade_cash: {post_trade_cash}"))

    reduced_target_pos <- FALSE
    if(post_trade_cash < maint_margin) {
      reduced_target_pos <- TRUE
      # adjust trade size down
      # post trade, max value of position can be found by:
      # Cash + freed_margin - post_trade_margin - commissions = maint_margin
      # setting commissions to the value calculated for full trade size above (will be conservative), we get:
      # max_post_trade_contracts_value = (cash + freed_margin - commissions)/margin_requirement
      max_post_trade_contracts_value <- 0.95*(cash + maint_margin - sum(commissions, na.rm = TRUE))/margin

      reduce_by <-  max_post_trade_contracts_value/sum(abs(target_position_value), na.rm = TRUE)
      # ensure doesn't change sign from intended, but we only reduce target contracts no further than zero
      target_positions <- sign(target_positions) * reduce_by * pmax(abs(target_positions), rep(0, num_assets))
      trades <- target_positions - current_positions
      trade_value <- trades * current_prices
      commissions <- abs(trade_value) * commission_pct

      current_positions <- target_positions
      position_value <- current_positions * current_prices
      post_trade_cash <- cash + maint_margin - margin*sum(abs(position_value), na.rm = TRUE) - sum(commissions, na.rm = TRUE)
    } else {
      current_positions <- target_positions
      position_value <- current_positions * current_prices
    }

    # update cash and maint_margin
    cash <- post_trade_cash
    # print(glue::glue("cash at end of period: {cash}"))
    maint_margin <- margin*sum(abs(position_value), na.rm = TRUE)

    # store date in matrix as numeric, then convert back to date format with as.Date(date_col, origin ="1970-01-01")
    row_mat <- matrix(
      data = c(
        rep(as.numeric(current_date), num_assets+1),
        c(current_prices, 0),
        c(current_positions, cash),
        c(position_value, cash),
        c(margin*abs(position_value), 0),
        c(funding, 0),
        c(period_pnl, 0),
        c(trades - liq_contracts, 0),  # minus because we keep sign of original position in liq_contracts
        c(trade_value-liq_trade_value, 0),
        c(commissions + liq_commissions, 0),
        rep(margin_call, num_assets+1),   # bool will be stored as int (1-0)
        rep(reduced_target_pos, num_assets+1)
      ),
      nrow = num_assets + 1,
      ncol = 12,
      byrow = FALSE,
      dimnames = list(
        # tickers are row names
        c(tickers, "Cash"),
        # column names
        c("Date", "Close", "Position", "Value", "Margin", "Funding", "PeriodPnL", "Trades", "TradeValue", "Commission", "MarginCall", "ReducedTargetPos")
      )
    )

    row_list[[i]] <- row_mat

    previous_target_weights <- current_target_weights
    previous_prices <- current_prices
  }

  # Optionally prepend initial state (t=0)
  if(include_initial_state) {
    initial_row <- matrix(
      data = c(
        rep(as.numeric(prices[1, 1]), num_assets+1),   # Use first date
        c(prices[1, -1], 0),                            # Prices at t=0
        c(rep(0, num_assets), initial_cash),            # Zero positions, initial cash
        c(rep(0, num_assets), initial_cash),            # Zero value, initial cash
        rep(0, num_assets+1),                           # No margin
        rep(0, num_assets+1),                           # No funding
        rep(0, num_assets+1),                           # No period PnL
        rep(0, num_assets+1),                           # No trades
        rep(0, num_assets+1),                           # No trade value
        rep(0, num_assets+1),                           # No commission
        rep(FALSE, num_assets+1),                       # No margin call
        rep(FALSE, num_assets+1)                        # No reduced target
      ),
      nrow = num_assets + 1,
      ncol = 12,
      byrow = FALSE,
      dimnames = list(
        c(tickers, "Cash"),
        c("Date", "Close", "Position", "Value", "Margin", "Funding", "PeriodPnL", "Trades", "TradeValue", "Commission", "MarginCall", "ReducedTargetPos")
      )
    )
    row_list <- c(list(initial_row), row_list)
  }

  # Combine list of matrixes into dataframe
  do.call(rbind, row_list) %>%
    tibble::as_tibble(rownames = "ticker") %>%
    dplyr::mutate(
      Date = as.Date(Date, origin ="1970-01-01")
    )
}
