# TODO:
  # handle upstream: create futures price series from a particular roll approach
  # need a dataframe of positions by ticker, where ticker is a contract
  # OR load contracts and roll within backtest loop... will be slower...
  # first approach more in line with rsims approach of calculating trades upstream of backtest loop
  # include roll in simulated trades by flagging roll dates
  # function for checking matrix inputs are at least the correct type
  # function for converting standard df to matrix with correct columns
  # min commission model for futures trading: assume we using raw prices, ie not continuous contract which would require passing adjusted and unadjusted price - commission_fun(trades, current_price, ...)
  # function for calculating futs positions from no trade buffer and min commission - can we use the share-based one?? futsPositionsFromNoTradeBufferMinComm(contract_pos, current_price, current_weights, cap_equity, trade_buffer)
  # function for including interest accrued: maybe pass a matrix of STIRs or calculate upstream

# function for mapping symbol (GC, ES etc) / date / position to contract/date/position
# where we are in a position for a symbol, figure out which contract we should be in
# do it based on open interest
# pos <- data.frame(
#   date = c("2021-01-04", "2021-01-04", "2021-01-05", "2021-01-05", "2021-01-06", "2021-01-06", "2021-01-07", "2021-01-07", "2021-01-08", "2021-01-08"),
#   symbol = rep(c("GC", "ES"), 5),
#   position = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1)
# )
# contracts <- futures %>% select(ticker, date, close, open_interest) %>% dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+"))
#
# get_contract_positions <- function(symbol_positions, contracts) {
#
# }



#' Futures Backtest, roll on days to expiry, minimum commission model
#'
#' @description Event-based simulation based on desired futures positions, raw
#' contract prices (ie not backadjusted) and user-specified roll date (dte).
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or
#' index. Column 2 must be days-to-expiry.
#' @param theo_weights Matrix of theoretical weights. Column 1 must be the
#' timestamp or index.
#' @param trade_buffer Trade buffer parameter
#' @param initial_cash Inital cash balance
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance
#' in determining position sizes. If FALSE, profits accrue as a cash balance and
#'  are not reinvested.
#' @param leverage Leverage to apply in simulation
#' @param commission_fun Function for determining commissions from prices and
#' trades
#'
#' @return long dataframe of results - dates, trades, commissions, value of portfolio components
#' @details
#' `theo_weights` should be date-aligned with `prices` - it is up to the user to lag `theo_weights` as necessary to
#' ensure that trades occur at appropriate prices.
#' @examples
#' @export
min_commission_futs_backtest <- function(prices, theo_weights, trade_buffer = 0., initial_cash = 10000, capitalise_profits = FALSE, leverage = 1, commission_fun, ...) {

  MAINT_MARGIN <- 0.25

  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != theo_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Misaligned timestamps at indexes {misaligned_timestamps}"))

  if(!all.equal(dim(prices), dim(theo_weights)))
    stop("Prices and weights matrixes must have same dimensions")

  num_assets <- ncol(theo_weights) - 1
  tickers <- colnames(theo_weights)[-1]

  rowlist <- vector(mode = "list", length = nrow(theo_weights))  # preallocate list to store daily backtest data

  Cash <- initial_cash
  contract_pos <- rep(0, num_assets)
  contract_value <- rep(0, num_assets)
  cap_equity <- initial_cash
  previous_weights <- rep(0, num_assets)

  # Iterate through prices and backtest
  for (i in 1:(nrow(theo_weights))) {
    current_date <- theo_weights[i, 1]  # date will be a numeric from origin
    current_price <- wide_prices[i, -1]
    current_weights <- theo_weights[i, -1]

    # update total equity balance
    equity <- sum(contract_pos * current_price) + Cash
    if(equity > 0) {
      # force reduce position if exceeds maintenance margin requirements
      margin_call <- FALSE
      liq_contracts <- rep(0, num_assets)
      liq_commissions <- rep(0, num_assets)
      if(leverage > 1 && equity < MAINT_MARGIN*sum(contract_pos * current_price)) {
        margin_call <- TRUE

        # how much to liquidate? NOTE: this doesn't include the commission costs or other fees of liquidating in figuring out post-liquidation NAV
        liquidate_factor <- 1 - (equity/MAINT_MARGIN)/sum(contract_pos * current_price)

        # liquidate equal proportions of each contract - not how broker would actually do it
        liq_contracts <- liquidate_factor * contract_pos
        liq_commissions <- commission_fun(liq_contracts, current_price, current_unadjprice, ...)

        Cash <- Cash - sum(liq_contracts*current_price) - sum(liq_commissions)
        contract_pos <- contract_pos - liq_contracts
        contract_value <- contract_pos * current_price
        equity <- sum(contract_value) + Cash

      }

      # Capitalise profits
      if(capitalise_profits) {
        cap_equity <- equity
      }

      # Update target contracts based on signal
      target_contracts <- futsPositionsFromNoTradeBufferMinComm(contract_pos, current_price, current_weights, cap_equity, trade_buffer)

      trades <- target_contracts - contract_pos
      tradevalue <- trades * current_price

      commissions <- commission_fun(trades, current_price, ...)

      # can we do proposed trades?
      if(leverage > 1) {
        # Can we do proposed trades given NAV and MM? If not, adjust trade size, value, commissions etc
        post_trade_equity <- sum(target_contracts*current_price) + Cash - sum(tradevalue) - sum(commissions)
        if(post_trade_equity < MAINT_MARGIN*sum(target_contracts * current_price)) {
          # adjust trade sizes
          max_post_trade_shareval <- (equity - sum(commissions))/MAINT_MARGIN
          if(max_post_trade_shareval > 0) {  # this will only work when > 0
            reduce_by <- 1 - max_post_trade_shareval/sum(target_contracts*current_price)
            target_contracts <- target_contracts - ceiling(reduce_by*target_contracts)
            trades <- target_contracts - contract_pos
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
            reduce_by <- 1 - max_post_trade_shareval/sum(target_contracts*current_price)
            target_contracts <- target_contracts - ceiling(reduce_by*target_contracts)
            trades <- target_contracts - contract_pos
            tradevalue <- trades * current_price
            commissions <- commission_fun(trades, current_price, current_unadjprice, ...)
          }
        }
      }

      # Adjust cash by value of trades
      Cash <- Cash - sum(tradevalue) - sum(commissions)
      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
      equity <- sum(contract_value) + Cash

      if(equity <= 0) {  # rekt
        contract_pos <- rep(0, num_assets)
        contract_value <- rep(0, num_assets)
        contract_value <- rep(0, num_assets)
        trades <- rep(0, num_assets)
        liq_contracts <- rep(0, num_assets)
        commissions <- rep(0, num_assets)
        liq_commissions <- rep(0, num_assets)
        interst <- rep(0, num_assets)
        short_borrow_cost <- rep(0, num_assets)
        equity <- 0
        Cash <- 0
      }

    } else {  # rekt
      contract_pos <- rep(0, num_assets)
      contract_value <- rep(0, num_assets)
      tradevalue <- rep(0, num_assets)
      trades <- rep(0, num_assets)
      liq_contracts <- rep(0, num_assets)
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
        c(0, contract_pos),
        c(Cash, contract_value),
        c(0, trades + liq_contracts),
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
        c("date", "close", "contracts", "exposure", "contracttrades", "tradevalue", "commission", "margin_call")
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
