# TODO:
  # write a vignette describing the flow:
    # wrangle contracts data into a df that you can look at and reason about
    # convert that to a numerical matrix for simulation
    # do the simulation
    # describe how the roll is handled and trade logic
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


# TODO:
  # test this function using example data below, plus another edge case
  # document: # wrangle contract data into format for backtest using open interest
  # describe contracts data: needs ticker, date, price, open_interest
  # describe output
  # example:
    # this will cause roll to show up the day after something is identified as having max OI
    # eg GC:
    # 2021-01-15 G has highest OI
    # 2021-01-19 J has highest OI (no trading 16-19)
    # 2021-01-20 we roll into J (previous_contract and close_previous_contract correspond to G, current to J)
wrangle_contracts_on_oi <- function(contracts) {
  contracts_df <- contracts %>%
    dplyr::select(ticker, date, close, open_interest) %>%
    # make new variable corresponding to base symbol (ES, GC, etc)
    # extract everything before first "-"
    dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%
    # get yesterday's open interest by ticker
    dplyr::group_by(ticker) %>%
    dplyr::mutate(lag_open_interest = dplyr::lag(open_interest)) %>%
    ungroup()

    # on the first day of the simulation, we want to be get into positions based
    # on the highest OI of that day as opposed to previous day
  first_date <- contracts_df %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(date = dplyr::first(date)) %>%
    dplyr::left_join(contracts_df %>% dplyr::select(symbol, date, ticker, close, open_interest), by = c("symbol",  "date")) %>%
    dplyr::mutate(lag_open_interest = open_interest)

  contracts_df <- contracts_df %>%
    dplyr::rows_patch(first_date, by = c("ticker", "date"))

    # get the contract with yesterday's highest open interest for each symbol for each day
    # need yesterday's highest open interest in order to get the return from the prior close
    # for the current contract
  contracts_df <- contracts_df %>%
    dplyr::group_by(date, symbol) %>%
    dplyr::mutate(current_contract = dplyr::case_when(lag_open_interest == max(lag_open_interest, na.rm = TRUE) ~ TRUE, TRUE ~ FALSE)) %>%  # remove NA because on first day of new contract, lag_open_interest will be NA
    dplyr::group_by(ticker) %>%
    dplyr::mutate(previous_contract = dplyr::case_when(dplyr::lag(current_contract) == TRUE ~ TRUE, TRUE ~ FALSE)) %>%
    dplyr::ungroup()

  current_contracts <- contracts_df %>%
    dplyr::filter(current_contract == TRUE) %>%
    dplyr::mutate(current_contract = ticker) %>%
    dplyr::select(symbol, date, close, current_contract)

  last_contracts <- contracts_df %>%
    dplyr::filter(previous_contract == TRUE) %>%
    dplyr::mutate(previous_contract = ticker) %>%
    dplyr::select(symbol, date, close, previous_contract)

  current_contracts %>%
    dplyr::left_join(last_contracts, by = c("date", "symbol"), suffix = c("_current_contract", "_previous_contract")) %>%
    dplyr::mutate(roll = case_when(
      is.na(previous_contract) ~ FALSE,  # roll should be false on first day of simulation
      current_contract == previous_contract ~ FALSE,
      TRUE ~ TRUE))
}

# transforms a wrangled contracts dataframe into a numerical matrix format for rsims simulation
# gives one wide matrix with three columns per symbol
# columns are suffixed with symbol
# Other approaches:
  # one matrix per symbol: adds complexity, reduces performance in backtest loop, but may make backtest easier to reason about
  # long dataframe rather than numerical matrix: make backtest very easy to reason about, but pay a big penalty in performance
  # what do we care about most? needs to be fast enough to be useful (long dataframes probably won't be), ideally would be easier to reason about simulation... but the data prep phase kinda takes care of that...
make_sim_prices_matrix <- function(wrangled_contracts) {
  wrangled_contracts %>%
    dplyr::select(symbol, date, close_current_contract, close_previous_contract, roll) %>%
    dplyr::arrange(date, symbol) %>%   # for consistency of column order
    tidyr::pivot_wider(
      id_cols = date,
      names_from = symbol,
      values_from = c(close_current_contract, close_previous_contract, roll)
    ) %>%
    data.matrix()
}

# takes a long dataframe of date, symbol, target weights
# returns wide matrix of date, symbol where the values of symbol are the target weights
make_sim_weights_matrix <- function(weights) {
  weights %>%
    dplyr::select(symbol, date, target_weight) %>%
    dplyr::arrange(date, symbol) %>%   # for consistency of column order
    tidyr::pivot_wider(id_cols = date,
      names_from = symbol,
      values_from = target_weight
    ) %>%
    data.matrix()
}

# example data: equal dollar weight in ES/GC/ZB
dates <- futures %>% distinct(date) %>% pull()
weights <- data.frame(
  date = sort(rep(dates[1:20], 3)),
  symbol = rep(c("ES", "GC", "ZB"), 20),
  target_weight = rep(1./3, 60)
)
target_weights <- make_sim_weights_matrix(weights)


# roll looks like this:
# cover current position in previous_contract
# put on target weight in current_contract

# each day check if we roll a contract and have a change in position:
# if we roll do the roll trades so that you're back to the ideal weight
# if we don't roll, only do the trades back to the trade buffer

# remember: dates are aligned such that prices are prices at which we trade into target weights

#' Futures Backtest, roll on days to expiry, minimum commission model
#'
#' @description Event-based simulation based on desired futures positions, raw
#' contract prices (ie not backadjusted) and user-specified roll date (dte).
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or
#' index. Column 2 must be days-to-expiry.
#' @param target_weights Matrix of theoretical weights. Column 1 must be the
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
#' `target_weights` should be date-aligned with `prices` - it is up to the user to lag `target_weights` as necessary to
#' ensure that trades occur at appropriate prices.
#' @examples
#' @export
min_commission_futs_backtest <- function(prices, target_weights, interest_rates, trade_buffer = 0., initial_cash = 10000, margin = 0.25, capitalise_profits = FALSE, commission_fun, ...) {

  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != target_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Misaligned timestamps at indexes {misaligned_timestamps}"))

  # TODO check column order matches in prices and target_weights
  # eg colnames(prices)[2:(2+num_assets-1)] should regex match symbols from colnames(target_weights)

  num_assets <- ncol(target_weights) - 1
  symbols <- colnames(target_weights)[-1]

  rowlist <- vector(mode = "list", length = nrow(target_weights))  # preallocate list to store daily backtest data

  Cash <- initial_cash
  contract_pos <- rep(0, num_assets)
  contract_value <- rep(0, num_assets)
  investable_cash <- initial_cash  # used to enable capitalisation of positions with accrued cash
  previous_weights <- rep(0, num_assets)
  previous_price <- rep(NA, num_assets)

  # Iterate through prices and backtest
  for (i in 1:(nrow(target_weights))) {
    current_date <- target_weights[i, 1]  # date will be a numeric from origin
    current_price <- prices[i, c(2:(2+num_assets-1))]
    current_weights <- target_weights[i, -1]
    current_interest_rate <- interest_rates[i, -1]

    # calculate cash settled at today's close based on yesterday's positions
    settled_cash <- sum(contract_pos * (current_price - previous_price))
    settled_cash <- ifelse(is.na(settled_cash), 0, settled_cash)

    # calculate interest paid on yesterday's cash
    interest <- current_interest_rate * Cash

    # update cash balance
    Cash <- Cash + settled_cash + interest

    # check margin requirements
    # TODO: assumption: each contract has same maintenance margin requirements
    contract_value <- contract_pos * current_price
    maint_margin <- margin * sum(contract_value)

    # force reduce position if exceeds maintenance margin requirements
    margin_call <- FALSE
    liq_contracts <- rep(0, num_assets)
    liq_commissions <- rep(0, num_assets)
    if(Cash < maint_margin) {
      margin_call <- TRUE

      # liquidate equal proportions of each contract holding
      # liquidate 5% extra to requirement for bringing maint_margin into line with cash
      liquidate_factor <- 1.05*(maint_margin - Cash)/maint_margin

      # liquidate equal proportions of each contract (probably not how broker would actually do it)
      liq_contracts <- liquidate_factor * contract_pos
      # TODO: don't need to pass unadj price to commission_fun
      liq_commissions <- commission_fun(liq_contracts, current_price, current_unadjprice, ...)

      Cash <- Cash - sum(liq_commissions)
      contract_pos <- contract_pos - liq_contracts
      contract_value <- contract_pos * current_price
      maint_margin <- margin * sum(contract_value)
    }

    # capitalise profits
    # TODO: assumption - continuous capitalisation of account with available cash if capitalise_profits = TRUE (in line with rebalance tolerance)
    if(capitalise_profits) {
      investable_cash <- Cash
    }

    # Update target contracts based on signal
    # TODO: implement this function
    target_contracts <- futsPositionsFromNoTradeBufferMinComm(contract_pos, current_price, current_weights, investable_cash, trade_buffer)

    trades <- target_contracts - contract_pos
    tradevalue <- trades * current_price

    commissions <- commission_fun(trades, current_price, ...)

    # post-trade cash:  cash freed up from closing position, cash used as margin, commissions
    contract_pos <- target_contracts
    contract_value <- contract_pos * current_price
    post_trade_cash <- Cash + maint_margin - margin*sum(contract_value) - sum(commissions)

    if(post_trade_cash < 0) {
      # adjust trade size down
      # post trade, max value of position can be found by:
      # Cash + freed_margin - post_trade_margin - commissions = 0
      # setting commissions to the value calculated for full trade size above (will be conservative), we get:
      # max_post_trade_contracts_value = (cash + freed_margin - commissions)/margin_requirement
      max_post_trade_contracts_value <- (Cash + maint_margin - sum(commissions))/margin

      reduce_by <-  (sum(contract_value) - max_post_trade_shareval)/sum(contract_value)
      target_contracts <- target_contracts - ceiling(reduce_by*target_contracts)
      trades <- target_contracts - contract_pos
      tradevalue <- trades * current_price
      commissions <- commission_fun(trades, current_price, current_unadjprice, ...)

      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
      post_trade_cash <- Cash + maint_margin - margin*sum(contract_value) - sum(commissions)
    }

    # TODO: up to here... what are the conditions that get us rekt in this case?
    # run out of cash, even after being liquidated, not enough cash to open a position

    # if(equity <= 0) {  # rekt
    #   contract_pos <- rep(0, num_assets)
    #   contract_value <- rep(0, num_assets)
    #   contract_value <- rep(0, num_assets)
    #   trades <- rep(0, num_assets)
    #   liq_contracts <- rep(0, num_assets)
    #   commissions <- rep(0, num_assets)
    #   liq_commissions <- rep(0, num_assets)
    #   interst <- rep(0, num_assets)
    #   short_borrow_cost <- rep(0, num_assets)
    #   equity <- 0
    #   Cash <- 0
    # }

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
