#' Equities Cash Accounting Backtest
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or index.
#' @param theo_weights Matrix of theoretical weights. Column 1 must be the timestamp or index.
#' @param trade_buffer Trade buffer parameter
#' @param initial_cash Inital cash balance
#' @param commission_pct Percent commission charged on trades
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance in determining position sizes. If FALSE, profits accrue as a cash balance and are not reinvested.
#'
#' @return long dataframe of results - dates, trades, commissions, value of portfolio components
#' @details
#' `theo_weights` should be date-aligned with `prices` - it is up to the user to lag `theo_weights` as necessary to
#' ensure that trades occur at appropriate prices.
#' @examples
#' @export
min_commission_backtest <- function(prices, unadjusted_prices, theo_weights, trade_buffer = 0., initial_cash = 10000, capitalise_profits = FALSE, leverage = 1, commission_fun, ...) {

  MAINT_MARGIN <- 0.25

  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != theo_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Misaligned timestamps at indexes {misaligned_timestamps}"))

  if(!all.equal(dim(prices), dim(theo_weights)))
    stop("Prices and weights matrixes must have same dimensions")

  num_assets <- ncol(theo_weights) - 1
  # get tickers for later
  tickers <- colnames(theo_weights)[-1]

  rowlist <- vector(mode = "list", length = nrow(theo_weights))  # preallocate list to store daily backtest data

  Cash <- initial_cash
  share_pos <- rep(0, num_assets)
  share_value <- rep(0, num_assets)
  cap_equity <- initial_cash
  previous_weights <- rep(0, num_assets)

  # Iterate through prices and backtest
  for (i in 1:(nrow(theo_weights))) {
    current_date <- theo_weights[i, 1]  # date will be a numeric from origin
    current_price <- wide_prices[i, -1]
    current_weights <- theo_weights[i, -1]
    current_unadjprice <-unadjusted_prices[i, -1]

    # update total equity balance
    equity <- sum(share_pos * current_price) + Cash
    if(equity > 0) {
      # force reduce position if exceeds maintenance margin requirements
      margin_call <- FALSE
      liq_shares <- rep(0, num_assets)
      liq_commissions <- rep(0, num_assets)
      if(leverage > 1 && equity < MAINT_MARGIN*sum(share_pos * current_price)) {
        margin_call <- TRUE

        # how much stock to liquidate? NOTE: this doesn't include the commission costs or other fees of liquidating in figuring out post-liquidation NAV
        liquidate_factor <- 1 - (equity/MAINT_MARGIN)/sum(share_pos * current_price) # 1 - (max share value/current share value)

        # liquidate equal proportions of stock
        liq_shares <- liquidate_factor * share_pos
        liq_commissions <- commission_fun(liq_shares, current_price, current_unadjprice, ...)

        Cash <- Cash - sum(liq_shares*current_price) - sum(liq_commissions)
        share_pos <- share_pos - liq_shares
        share_value <- share_pos * current_price
        equity <- sum(share_value) + Cash

      }

      # Capitalise profits
      if(capitalise_profits) {
        cap_equity <- equity
      }

      # Update target shares based on signal
      targetshares <- positionsFromNoTradeBufferMinComm(share_pos, current_price, current_weights, cap_equity, trade_buffer)
      # targetshares <- share_pos
      # for(j in c(1:num_assets)) {
      #   if(current_price[j] == 0 || (current_weights[j] == 0)) {
      #     targetshares[j] <- 0
      #     next  # no price/target size
      #   }
      #   this_targetshares <- trunc(current_weights[j]*cap_equity / current_price[j])
      #   if(current_weights[j] != previous_weights[j] || this_targetshares == 0 || (abs(1 - share_pos[j]/this_targetshares)) >= rebal_tolerance) {  # prevents divide by zero error as last term is not evaulated if this_targetshares is zero.
      #     targetshares[j] <- this_targetshares  # max(1, sum(current_signals != 0)) divides capital among anything with a non-zero position, while protecting against divide-by-zero errors in the case where we go from long/short to flat.
      #   }
      # }

      trades <- targetshares - share_pos
      tradevalue <- trades * current_price

      commissions <- commission_fun(trades, current_price, current_unadjprice, ...)

      # can we do proposed trades?
      if(leverage > 1) {
        # Can we do proposed trades given NAV and MM? If not, adjust trade size, value, commissions etc
        post_trade_equity <- sum(targetshares*current_price) + Cash - sum(tradevalue) - sum(commissions)
        if(post_trade_equity < MAINT_MARGIN*sum(targetshares * current_price)) {
          # adjust trade sizes
          max_post_trade_shareval <- (equity - sum(commissions))/MAINT_MARGIN
          if(max_post_trade_shareval > 0) {  # this will only work when > 0
            reduce_by <- 1 - max_post_trade_shareval/sum(targetshares*current_price)
            targetshares <- targetshares - ceiling(reduce_by*targetshares)
            trades <- targetshares - share_pos
            tradevalue <- trades * current_price
            commissions <- commission_fun(trades, current_price, current_unadjprice, ...)
          }
        }
      } else {
        # Cash can't be negative
        post_trade_cash <- Cash - sum(tradevalue) - sum(commissions)
        if(post_trade_cash < 0) {
          # adjust trade size down
          max_post_trade_shareval <- equity - sum(commissions)
          if(max_post_trade_shareval > 0) {  # this will only work when > 0
            reduce_by <- 1 - max_post_trade_shareval/sum(targetshares*current_price)
            targetshares <- targetshares - ceiling(reduce_by*targetshares)
            trades <- targetshares - share_pos
            tradevalue <- trades * current_price
            commissions <- commission_fun(trades, current_price, current_unadjprice, ...)
          }
        }
      }

      # Adjust cash by value of trades
      Cash <- Cash - sum(tradevalue) - sum(commissions)
      share_pos <- targetshares
      share_value <- share_pos * current_price
      equity <- sum(share_value) + Cash

      if(equity <= 0) {  # rekt
        share_pos <- rep(0, num_assets)
        share_value <- rep(0, num_assets)
        share_value <- rep(0, num_assets)
        trades <- rep(0, num_assets)
        liq_shares <- rep(0, num_assets)
        commissions <- rep(0, num_assets)
        liq_commissions <- rep(0, num_assets)
        interst <- rep(0, num_assets)
        short_borrow_cost <- rep(0, num_assets)
        equity <- 0
        Cash <- 0
      }

    } else {  # rekt
      share_pos <- rep(0, num_assets)
      share_value <- rep(0, num_assets)
      tradevalue <- rep(0, num_assets)
      trades <- rep(0, num_assets)
      liq_shares <- rep(0, num_assets)
      commissions <- rep(0, num_assets)
      liq_commissions <- rep(0, num_assets)
      interst <- rep(0, num_assets)
      short_borrow_cost <- rep(0, num_assets)
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
        c(-sum(tradevalue), tradevalue),
        c(0, commissions + liq_commissions),
        rep(margin_call, num_assets+1)   # bool will be stored as int (1-0)
      ),
      nrow = num_assets + 1,
      ncol = 8,
      byrow = FALSE,
      dimnames = list(
        # tickers are row names
        c("Cash", tickers),
        # column names
        c("date", "close", "shares", "exposure", "sharetrades", "tradevalue", "commission", "margin_call")
      )
    )

    rowlist[[i]] <- row_mat

    previous_weights <- current_weights
  }

  # Combine list of matrixes into dataframe
  do.call(rbind, rowlist) %>%
    as_tibble(rownames = "ticker") %>%
    mutate(
      date = as.Date(date, origin ="1970-01-01"),
      margin_call = dplyr::if_else(margin_call != 0, TRUE, FALSE)
    )
}
