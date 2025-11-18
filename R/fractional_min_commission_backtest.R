#' Equities Minimum Commission Backtest using Fractional Numbers of Shares
#'
#' @description Identical function to min_commission_backtest, except it allows
#' for dealing in fractional shares. This is useful when simulating assets that
#' have undergone splits that result in split-adjusted prices that are so large
#' as to prevent the granularity needed when constrained to doing integer lots
#' of shares (think UVXY).
#'
#' This is imperfect, but is a decent trade off when the price series has been
#' adjusted by orders of magnitude.
#'
#' Quasi event-based simulation based on target share weights,
#' prices, commissions and interest.
#'
#' The function is quite opinionated with respect to the data that it expects.
#' The idea is that user effort is spent in upstream data wrangling, and the
#' payoff is an extremely efficient backtesting routine, enabling fast
#' experimentation with parameters such as `trade_buffer` and different
#' commission models.
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or index.
#' @param unadjusted_prices Matrix of unadjusted prices. Column 1 must be the
#' timestamp or index. Used to calculate commission based on actual number of
#' shares traded.
#' @param target_weights Matrix of target weights. Column 1 must be the
#' timestamp or index.
#' @param interest_rates Matrix of daily interest rates applied to unused cash
#' (positive interest) and borrowed cash (negative interest). If not passed,
#' assumes constant interest rate of zero.
#' @param short_borrow_costs Named vector of annualised short borrow costs as percent. For
#' example, c("TLT" = 0.0025) is equivalent to a short borrow cost of 0.25%pa for
#' TLT. Defaults to zero.
#' @param trade_buffer Trade buffer parameter (see details)
#' @param initial_cash Inital cash balance
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance in determining position sizes. If FALSE, profits accrue as a cash balance and are not reinvested.
#' @param include_initial_state If TRUE, prepends an initial state row (t=0) to the results showing initial cash and zero positions. Defaults to FALSE for backward compatibility.
#' @param commission_fun Function for determining commissions from prices and
#' trades
#' @param ... Additional arguments passed to commission_fun. For
#' `us_tiered_commission`, this will be `max_pct_per_order` (the maximum
#' commission as a percentage of order value), `min_dollars_per_order` (the
#' minimum commission in dollars of a single order), `dollars_per_share` (the
#' per-share cost in dollars - for IB fixed tier, 0.005 is reasonable).
#'
#' @return long dataframe of results consisting of the following columns:
#'  ticker: Ticker or Cash
#'  date: In YYYY-MM-DD format
#'  close: Closing price (adjusted).
#'  shares: Number of shares held.
#'  exposure: Value of exposure to current ticker or to Cash.
#'  share_trades: Number of shares traded, including any positions that
#'  were liquidated fully or partially.
#'  trade_value: Value of shares traded, including any positions that
#'  were liquidated fully or partially.
#'  commission: Commissions paid on today's trading, including any liquidated
#'  positions.
#'  interest: Interest accrued on yesterday's cash balance and settled today.
#'  short_borrow: Borrow costs from holding yesterday's short positions through
#'  today's close.
#'  margin_call: Boolean indicating whether a margin call occurred today.
#'  reduced_target_pos: Boolean indicating whether target positions could not be
#'  executed due to insufficient margin, and were scaled back accordingly.
#'
#' @details
#' `target_weights`,  `prices`, `unadjusted_prices`, and `interest_rates` should
#' be date-aligned.
#'
#' The function will error if these matrixes are of different
#' shape, or have different values in Column 1 (the date or index column).
#' A simple approach to ensuring inputs conform with this requirement is in the
#' vignette.
#'
#' It is up to the user to lag `target_weights` as necessary to ensure that
#' trades occur at appropriate prices. Specifically, the function assumes that
#' `target_weights` are date-aligned with `prices` such that the price at which
#' you assume you trade into a target weight has the same index value.
#'
#' `interest_rates` are the daily percentage interest rate applied to the cash
#' balance. It is up to the user to ensure this is in the correct format (for
#' instance dividing by 100*365 if your raw data is in percent per-anum), and to
#' apply a broker spread if necessary.
#'
#' The function assumes a margin requirement of 0.25 the value of the shares
#' held and will liquidate positions if this requirement is not met (ie will
#' simulate a margin call).
#'
#'
#' @examples
#' \dontrun{
#' results_df <- fractional_min_commission_backtest(
#'   prices = backtest_prices,
#'   unadjusted_prices = unadjusted_prices,
#'   target_weights = target_weights,
#'   interest_rates = rates,
#'   trade_buffer = trade_buffer,
#'   initial_cash = initial_cash,
#'   capitalise_profits = FALSE,
#'   commission_fun = us_tiered_commission,
#'   max_pct_per_order = 0.01,
#'   min_dollars_per_order = 1,
#'   dollars_per_share = 0.005
#'   )
#' }
#' @export
fractional_min_commission_backtest <- function(prices, unadjusted_prices, target_weights, interest_rates = NULL, short_borrow_costs = NULL, trade_buffer = 0., initial_cash = 10000, capitalise_profits = FALSE, include_initial_state = FALSE, commission_fun, ...) {

  MAINT_MARGIN <- 0.25
  broker_interest_spread <- 0.5/(100*365)

  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != target_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Misaligned timestamps at indexes {misaligned_timestamps}"))

  if(!all.equal(dim(prices), dim(target_weights)))
    stop("Prices and weights matrixes must have same dimensions")

  # Validate that NA prices don't occur where we want to trade
  na_price_with_weight <- is.na(prices[, -1]) & target_weights[, -1] != 0
  if(any(na_price_with_weight, na.rm = TRUE)) {
    problem_rows <- which(apply(na_price_with_weight, 1, any))
    stop(glue::glue("NA prices detected where target_weights is non-zero at row(s): {paste(problem_rows, collapse=', ')}. Fix upstream data."))
  }

  if(is.null(interest_rates)) {
    interest_rates <- matrix(
      data = c(prices[, 1], rep(0, nrow(prices))),
      ncol = 2
    )
  }

  num_assets <- ncol(target_weights) - 1
  tickers <- colnames(target_weights)[-1]

  if(is.null(short_borrow_costs)) {
    short_borrow_costs <- rep(0, num_assets)
    names(short_borrow_costs) <- tickers
  } else if(! all(names(short_borrow_costs) %in% tickers)) {
    stop("short_borrow_costs must be a named vector with names corresponding to tickers")
  } else {
    # ensure short_borrow_costs ordered same as prices, weights
    short_borrow_costs <- short_borrow_costs[sort(names(short_borrow_costs))]
  }

  rowlist <- vector(mode = "list", length = nrow(target_weights))  # preallocate list to store daily backtest data

  Cash <- initial_cash
  share_pos <- rep(0, num_assets)
  share_value <- rep(0, num_assets)
  cap_equity <- initial_cash
  previous_weights <- rep(0, num_assets)

  # Iterate through prices and backtest
  for (i in 1:(nrow(target_weights))) {
    current_date <- target_weights[i, 1]  # date will be a numeric from origin
    current_price <- prices[i, -1]
    current_weights <- target_weights[i, -1]
    current_unadjprice <-unadjusted_prices[i, -1]
    current_interest_rate <- ifelse(is.na(interest_rates[i, -1]), 0, interest_rates[i, -1])

    # interest is accrued on yesterday's cash balance at today's rate
    interest <- ifelse(
      Cash > 0,
      max(0, (current_interest_rate-broker_interest_spread)) * Cash,
      (current_interest_rate+broker_interest_spread)*Cash
    )
    # short borrow is debited based on holding yesterday's positions to today's close
    short_borrow <- short_borrow_costs/365 * ifelse(share_pos >= 0, 0, share_pos*current_price)

    # update cash and total equity
    Cash <- Cash + interest + sum(short_borrow, na.rm = TRUE)
    equity <- sum(share_pos * current_price, na.rm = TRUE) + Cash

    if(equity > 0) {
      # force reduce position if exceeds maintenance margin requirements
      margin_call <- FALSE
      liq_shares <- rep(0, num_assets)
      liq_commissions <- rep(0, num_assets)
      if(equity < MAINT_MARGIN*sum(abs(share_pos) * current_price, na.rm = TRUE)) {
        margin_call <- TRUE

        # liquidate equal proportions of each share holding
        # liquidate 5% extra to requirement for bringing positions into line with margin requirement
        # liq_fac = 1 - (max_pos_val)/(current_pos_val) where max_pos_val = equity/MM
        liquidate_factor <- 1 - 1.05*(equity)/(MAINT_MARGIN)/(sum(abs(share_pos) * current_price, na.rm = TRUE))

        # liquidate equal proportions of each contract (probably not how broker would actually do it)
        # but only liquidate a maximum amount of existing positions
        liq_shares <- -sign(share_pos) * pmin(
          liquidate_factor * abs(share_pos),  # deal in fraction number of contracts
          abs(share_pos)
        ) # sign() to account for possible short positions

        liq_commissions <- commission_fun(liq_shares, current_price, current_unadjprice, ...)

        Cash <- Cash - sum(liq_shares*current_price, na.rm = TRUE) - sum(liq_commissions, na.rm = TRUE)
        share_pos <- share_pos + liq_shares
        share_value <- share_pos * current_price
        equity <- sum(share_value, na.rm = TRUE) + Cash
      }

      # Capitalise profits
      if(capitalise_profits) {
        cap_equity <- equity
      }

      # Update target shares based on signal
      target_shares <- positionsFromNoTradeBufferMinComm(share_pos, current_price, current_weights, cap_equity, trade_buffer)
      # target_shares <- trunc(target_shares)

      trades <- target_shares - share_pos
      trade_value <- trades * current_price
      commissions <- commission_fun(trades, current_price, current_unadjprice, ...)

      # can we do proposed trades?
      # Can we do proposed trades given NAV and MM? If not, adjust trade size, value, commissions etc
      post_trade_equity <- sum(target_shares*current_price, na.rm = TRUE) + Cash - sum(trade_value, na.rm = TRUE) - sum(commissions, na.rm = TRUE)
      post_trade_margin <- MAINT_MARGIN*sum(abs(target_shares)*current_price, na.rm = TRUE)
      reduced_target_pos <- FALSE
      if(post_trade_equity < post_trade_margin) {
        reduced_target_pos <- TRUE
        # adjust trade sizes
        max_post_trade_shareval <- max((equity - sum(commissions, na.rm = TRUE))/MAINT_MARGIN, 0)
        reduce_by <- 1 - max_post_trade_shareval/sum(abs(target_shares)*current_price, na.rm = TRUE)

        reduce_shares <- -sign(target_shares) * pmin(
          reduce_by * abs(target_shares),  # deal in fractional number of contracts
          abs(target_shares)
        )

        target_shares <- target_shares + reduce_shares

        trades <- target_shares - share_pos
        trade_value <- trades * current_price
        commissions <- commission_fun(trades, current_price, current_unadjprice, ...)
      }

      # Adjust cash by value of trades
      Cash <- Cash - sum(trade_value, na.rm = TRUE) - sum(commissions, na.rm = TRUE)
      share_pos <- target_shares
      share_value <- share_pos * current_price
      equity <- sum(share_value, na.rm = TRUE) + Cash

      if(equity <= 0) {  # rekt
        share_pos <- rep(0, num_assets)
        share_value <- rep(0, num_assets)
        share_value <- rep(0, num_assets)
        trades <- rep(0, num_assets)
        liq_shares <- rep(0, num_assets)
        commissions <- rep(0, num_assets)
        liq_commissions <- rep(0, num_assets)
        interest <- 0
        equity <- 0
        Cash <- 0
      }

    } else {  # rekt
      share_pos <- rep(0, num_assets)
      share_value <- rep(0, num_assets)
      trade_value <- rep(0, num_assets)
      trades <- rep(0, num_assets)
      liq_shares <- rep(0, num_assets)
      commissions <- rep(0, num_assets)
      liq_commissions <- rep(0, num_assets)
      interest <- 0
      equity <- 0
      Cash <- 0
    }

    # store date in matrix as numeric, then convert back to date format with as.Date(date_col, origin ="1970-01-01")
    row_mat <- matrix(
      data = c(
        rep(as.numeric(current_date), num_assets+1),
        c(0, current_price),
        c(0, share_pos),
        c(Cash, share_value),
        c(0, trades + liq_shares),
        c(-sum(trade_value, na.rm = TRUE), trade_value),
        c(0, commissions + liq_commissions),
        c(interest, rep(0, num_assets)),
        c(0, short_borrow),
        rep(margin_call, num_assets+1),   # bool will be stored as int (1-0)
        rep(reduced_target_pos, num_assets+1)
      ),
      nrow = num_assets + 1,
      ncol = 11,
      byrow = FALSE,
      dimnames = list(
        # tickers are row names
        c("Cash", tickers),
        # column names
        c("date", "close", "shares", "exposure", "share_trades", "trade_value", "commission", "interest", "short_borrow", "margin_call", "reduced_target_pos")
      )
    )

    rowlist[[i]] <- row_mat

    previous_weights <- current_weights
  }

  # Optionally prepend initial state (t=0)
  if(include_initial_state) {
    initial_row <- matrix(
      data = c(
        rep(as.numeric(prices[1, 1]), num_assets+1),  # Use first date
        c(0, prices[1, -1]),                           # Prices at t=0
        rep(0, num_assets+1),                          # Zero shares
        c(initial_cash, rep(0, num_assets)),           # Initial cash only
        rep(0, num_assets+1),                          # No trades
        rep(0, num_assets+1),                          # No trade value
        rep(0, num_assets+1),                          # No commission
        rep(0, num_assets+1),                          # No interest
        rep(0, num_assets+1),                          # No short borrow
        rep(FALSE, num_assets+1),                      # No margin call
        rep(FALSE, num_assets+1)                       # No reduced target
      ),
      nrow = num_assets + 1,
      ncol = 11,
      byrow = FALSE,
      dimnames = list(
        c("Cash", tickers),
        c("date", "close", "shares", "exposure", "share_trades", "trade_value", "commission", "interest", "short_borrow", "margin_call", "reduced_target_pos")
      )
    )
    rowlist <- c(list(initial_row), rowlist)
  }

  # Combine list of matrixes into dataframe
  do.call(rbind, rowlist) %>%
    tibble::as_tibble(rownames = "ticker") %>%
    dplyr::mutate(
      date = as.Date(date, origin ="1970-01-01"),
      margin_call = dplyr::if_else(margin_call != 0, TRUE, FALSE),
      reduced_target_pos = dplyr::if_else(reduced_target_pos != 0, TRUE, FALSE)
    )
}
