#' Wrangle Futures Contracts for Simulation using Open Interest
#'
#' @description This is a helper function for determining when to roll futures
#' contracts based on open interest and extracting data required for simulations
#' that include the roll.
#'
#' The roll happens the day after a new contract is identified as having the
#' maximum open interest at the close.
#'
#' Example: CME gold futures (GC)
#'  2021-01-15: G has highest OI
#'  2021-01-19: J has highest OI (no trading 16-19)
#'  2021-01-20: we roll into J (previous_contract and close_previous_contract
#'  correspond to G, current_contract and close_current_contract to J)
#'
#' @param contracts A dataframe of futures contract data including columns
#' ticker, date, close, point_value, open_interest.
#'
#' @return Long dataframe with columns:
#'   symbol: Futures symbol without contract designation (eg ES, GC, ZB, etc)
#'   date: In YYYY-MM-DD format
#'   close_current_contract: Closing USD value of the current contract
#'   (close*point_value)
#'   current_contract: Contract designation for contract with highest open
#'   interest
#'   close_previous_contract: Closing USD value of yesterday's contract with
#'   highest open interest (close*point_value)
#'   previous_contract: Contract designation for contract with highest open
#'   interest yesterday
#'   roll: Boolean specifying whether there was a roll event for the current
#'   observation
#'
#'   Note that unless there was a roll event,
#'   previous_contract == current_contract and
#'   close_previous_contract == close_current_contract
#' @export
#'
#' @examples
#' \dontrun{wrangle_contracts_on_oi(futures)}
wrangle_contracts_on_oi <- function(contracts) {
  contracts_df <- contracts %>%
    dplyr::mutate(close = close*point_value) %>%
    dplyr::select(ticker, date, close, open_interest) %>%
    # make new variable corresponding to base symbol (ES, GC, etc)
    # extract everything before first "-"
    dplyr::mutate(symbol = stringr::str_extract(ticker, "[^-]+")) %>%
    # get yesterday's open interest by ticker
    dplyr::group_by(ticker) %>%
    dplyr::mutate(lag_open_interest = dplyr::lag(open_interest)) %>%
    dplyr::ungroup()

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
    dplyr::mutate(roll = dplyr::case_when(
      is.na(previous_contract) ~ FALSE,  # roll should be false on first day of simulation
      current_contract == previous_contract ~ FALSE,
      TRUE ~ TRUE))
}

#' Make Futures Simulation Prices Matrix
#'
#' @description Transforms a wrangled contracts dataframe (the output of
#' `wrangle_contracts_on_oi`) into a wide numerical matrix for simulation.
#'
#' Returns a matrix with 3 columns per symbol plus the date. Symbols appear as
#' suffixes in the column names (the `*` below).
#'
#' Handles missing data by filling forward the last value, except for leading
#' NAs which persist.
#'
#' @param wrangled_contracts dataframe output of `wrangle_contracts_on_oi()`
#'
#' @return Wide numerical matrix with the following columns:
#'  date: numerical days since epoch
#'  close_current_contract_`*`: `n` columns designating the closing price
#'  (close*point_value) of the current contract for each symbol.
#'  close_previous_contract_`*`: `n` columns designating the closing price
#'  (close*point_value) of yesterday's contract for each symbol. Will have the
#'  same value as close_current_contract unless there was a roll.
#'  roll_`*`: `n` columns of (0,1) designating whether a roll occurs on the
#'  current date (1) or not (0).
#' @export
#'
#' @examples
#' \dontrun{
#' wrangled <- wrangle_contracts_on_oi(futures)
#' sim_prices <- make_sim_prices_matrix(wrangled)
#' }
make_sim_prices_matrix <- function(wrangled_contracts) {
  wrangled_contracts %>%
    dplyr::select(symbol, date, close_current_contract, close_previous_contract, roll) %>%
    dplyr::arrange(date, symbol) %>%   # for consistency of column order
    tidyr::pivot_wider(
      id_cols = date,
      names_from = symbol,
      values_from = c(close_current_contract, close_previous_contract, roll)
    ) %>%
    tidyr::fill(dplyr::everything(), .direction = "down") %>%
    data.matrix()
}

#' Make Futures Simulation Weights Matrix
#'
#' @description Takes a long dataframe of symbol, date, target_weight and
#' returns a wide matrix of date, and columns for target weights for each symbol.
#'
#' Helper function for wrangling data into the required shape for simulation.
#'
#' @param weights Long dataframe of symbol, date, target_weight
#'
#' @return Wide matrix of date, and columns for target weights for each symbol
#' @export
#'
#' @examples
#' /dontrun{
#' weights <- data.frame(
#'  date = dates,
#'  symbol = symbols,
#'  target_weight = target_weights
#' )
#' make_sim_weights_matrix(weights)
#' }
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

#' Futures Backtest, roll on days to expiry, fixed commission model
#'
#' @description Quasi event-based simulation based on desired futures positions, raw
#' contract prices (ie not backadjusted) and roll dates determine by maximum
#' open interest.
#'
#' The function is quite opinionated with respect to the data that it expects.
#' The idea is that user effort is spent in upstream data wrangling (the
#' `wrangle_contracts_on_oi`, `make_sim_prices_matrix`, and
#' `make_sim_weights_matrix` functions help with this), and the payoff is an
#' extremely efficient backtesting routine, enabling fast experimentation with
#' parameters such as `trade_buffer` and different commission models.
#'
#' **Assumptions and considerations**:
#' - Won't allow a position that would be rejected by the broker given user-
#' supplied margin requirements. Positions that would be rejected are
#' automatically scaled back.
#' - Similarly, force liquidates the portfolio to conform with margin
#' requirements (ie simulates a margin call). Contracts are liquidated in equal
#' proportion - which may not be how a broker actually performs this operation.
#' - Trades only integer number of contracts.
#' - User-specified leverage should be embedded in the target weights values.
#' - Currently only fixed commission models are implemented.
#' - Actual positions are determined using a trade buffer approach: when an
#' actual portfolio weight differs from its target weight by `trade_buffer` or
#' more, rebalance back to the target weight plus or minus the `trade_buffer`
#' depending on which side of the target weight the current weight lies.
#' - Uses a user-supplied constant margin requirement for all assets in the
#' portfolio (TODO: support asset-wise margin requirements)
#' - It is up to the user to align weights and prices prior to passing to this
#' function. Weights should be date-aligned with the prices at which you assume
#' you trade into them. This means that you will generally need to lag your
#' weights upstream.
#' - Works on contract value (close * point value) and assumes all contracts are
#' in USD (TODO: support contracts denominated in different currencies)
#'
#'
#' @param prices Matrix of trade prices as per output of
#' `make_sim_prices_matrix`. Column 1 must be the
#' timestamp or index in numerical format.
#' @param target_weights Matrix of target weights as per output of
#' `make_sim_weights_matrix`. Column 1 must be the timestamp or index in
#' numerical format.
#' @param interest_rates Matrix of daily interest rate applied to cash balance.
#' Ensure that the rate is adjusted to reflect daily interest. For instance,
#' many sources quote the rate in %pa - this would require dividing by (365*100).
#' @param trade_buffer Trade buffer parameter to prevent hyperactive trading
#' back to the target weights. When an actual portfolio weight differs from its
#' target weight by `trade_buffer` or more, rebalance back to the target weight
#' plus or minus the `trade_buffer` depending on which side of the target weight
#' the current weight lies. This is theoretically optimal if costs are linear.
#' @param initial_cash Inital cash balance.
#' @param margin Margin requirement as proportion of position value. Currently
#' a constant value for all assets.
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance
#' in determining position sizes. If FALSE, profits accrue as a cash balance and
#' are not reinvested.
#' @param include_initial_state If TRUE, prepends an initial state row (t=0) to the results showing initial cash and zero positions. Defaults to FALSE for backward compatibility.
#' @param commission_fun Function for determining commissions from prices and
#' trades
#' @param ... Additional arguments passed to commission_fun. For
#' `futs_per_contract_commission`, this will be `per_contract_commission` (a
#' vector of commissions per contract).
#'
#' @return long dataframe of results consisting of the following columns:
#'  symbol: Futures symbol without contract designation (eg ES, GC, ZB, etc)
#'  date: In YYYY-MM-DD format
#'  close: Closing contract USD value determined from closing_price*point_value.
#'  contracts: Number of contracts held.
#'  exposure: Value of exposure to current symbol or to Cash.
#'  margin: Dollar value of margin required to maintain exposure.
#'  interest: Interest accrued on yesterday's cash balance and settled today.
#'  settled_cash: Cash settled due to holding positions from yesterday's close
#'  through today's close.
#'  contract_trades: Number of contracts traded, including any positions that
#'  were liquidated fully or partially.
#'  trade_value: Value of contracts traded, including any positions that
#'  were liquidated fully or partially.
#'  rolled_contracts: Number of contracts that were rolled out of today.
#'  roll_price: Price at which rolled contracts were covered (NA if no contracts
#'  were rolled).
#'  commission: Commissions paid on today's trading, including any liquidated
#'  and/or rolled contracts.
#'  margin_call: Boolean indicating whether a margin call occurred today.
#'  reduced_target_pos: Boolean indicating whether target positions could not be
#'  executed due to insufficient margin, and were scaled back accordingly.
#' @export
#'
#' @examples
#' /dontrun{
#' # prices
#' futures <- readRDS(test_path("fixtures", "futures.rds"))
#' wrangled <- wrangle_contracts_on_oi(futures)
#' sim_prices <- make_sim_prices_matrix(wrangled)
#'
#' # target weights
#' target_weights <- data.frame(
#'  date = wrangled$date,
#'  symbol = wrangled$symbol,
#'  target_weight = 5*rep(1./3, nrow(wrangled))
#'  ) %>%
#'  make_sim_weights_matrix()
#'
#'  example interest rate data
#'  broker_spread <- 0.005
#'  rates <- data.frame(
#'   date = sort(unique(wrangled$date)),
#'   rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
#'   ) %>%
#'   data.matrix()
#'   per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
#'   margin <- 0.05
#'   initial_cash <- 1000000
#'   trade_buffer <- 0.2
#'
#'   results <- fixed_commission_futs_backtest(
#'     prices = sim_prices,
#'     target_weights = target_weights,
#'     interest_rates = rates,
#'     trade_buffer = trade_buffer,
#'     initial_cash = initial_cash,
#'     margin = margin,
#'     capitalise_profits = TRUE,
#'     commission_fun = futs_per_contract_commission,
#'     per_contract_commission = per_contract_commission
#'  )
#' }
fixed_commission_futs_backtest <- function(prices, target_weights, interest_rates, trade_buffer = 0., initial_cash = 10000, margin = 0.05, capitalise_profits = FALSE, include_initial_state = FALSE, commission_fun, ...) {
  num_assets <- ncol(target_weights) - 1
  symbols <- colnames(target_weights)[-1]

  if(trade_buffer < 0)
    stop("trade_buffer must be greater than or equal to zero")

  misaligned_timestamps <- which(prices[, 1] != target_weights[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Timestamps of prices and weights matrixes are misaligned"))

  misaligned_timestamps <- which(prices[, 1] != interest_rates[, 1])
  if(length(misaligned_timestamps) > 0)
    stop(glue::glue("Timestamps of prices and rates matrixes are misaligned"))

  # Validate that NA prices don't occur where we want to trade
  # For futures, check all three price columns per symbol
  price_cols_current <- grep("close_current_contract_", colnames(prices), value = TRUE)
  prices_current <- prices[, price_cols_current]
  na_price_with_weight <- is.na(prices_current) & target_weights[, -1] != 0
  if(any(na_price_with_weight, na.rm = TRUE)) {
    problem_rows <- which(apply(na_price_with_weight, 1, any))
    stop(glue::glue("NA prices detected where target_weights is non-zero at row(s): {paste(problem_rows, collapse=', ')}. Fix upstream data."))
  }

  # check for NA in weights and rates matrixes
  if(any(is.na(target_weights))) {
    warning("NA present in target weights: consider replacing these values before continuing")
  }

  if(any(is.na(interest_rates))) {
    warning("NA present in interest rates data: consider replacing these values before continuing")
  }

  # check column order matches in prices and target_weights
  prices_cols <- colnames(prices)[2:(num_assets+1)] %>% stringr::str_remove("close_current_contract_")
  if(! all(prices_cols == symbols))
      stop("Asset-wise column order mismatch in prices and weights matrixes")

  # commission_fun should be one of the fixed commission models - not a minimum
  # commission model (trade calculation function is based on a fixed comm model)
  if(! c(substitute(commission_fun)) %in% c("futs_per_contract_commission", "fixed_percent"))
    stop("Commission function not yet implemented - choose futs_per_contract_commission or fixed_percent.")

  # check that ... corresponds to correct args for commission function
  if(substitute(commission_fun) == "futs_per_contract_commission") {
    args <- list(...)

    if(!"per_contract_commission" %in% names(args) || length(args) != 1)
      stop("Wrong arguments specified. futs_per_contract_commission requires 1 argument, per_contract_commission")

    # check that per_contract_commission is a correctly named vector
    if(length(args$per_contract_commission) != num_assets || any(names(args$per_contract_commission) != symbols))
      stop("per_contract_commission must be a named vector whose names correspond to the contract symbols in target_weights")
  }
    # for futs_per_contract_commission, ... should be per_contract_commission
    # and per_contract_commission needs to be a named vector corresponding to column-order of prices matrix

  rowlist <- vector(mode = "list", length = nrow(target_weights))  # preallocate list to store daily backtest data

  Cash <- initial_cash
  contract_pos <- rep(0, num_assets)
  contract_value <- rep(0, num_assets)
  investable_cash <- initial_cash  # used to enable capitalisation of positions with accrued cash
  previous_weights <- rep(0, num_assets)
  previous_price <- rep(NA, num_assets)
  previous_roll_price <- rep(NA, num_assets)
  maint_margin <- 0

  # Iterate through prices and backtest
  for (i in 1:(nrow(target_weights))) {
    current_date <- target_weights[i, 1]  # date will be a numeric from origin
    current_price <- prices[i, c(2:(2+num_assets-1))]
    current_roll_price <- prices[i, c((2+num_assets):(2+2*num_assets-1))]  # prices of contract rolled out of today
    roll <- prices[i, c((2+2*num_assets):(2+3*num_assets-1))]  # 0 or 1 designating whether we rolled or not
    current_weights <- target_weights[i, -1]
    current_interest_rate <- ifelse(is.na(interest_rates[i, -1]), 0, interest_rates[i, -1])

    # calculate cash settled at today's close based on yesterday's positions and if we roll

    # get number of contracts to roll out of
    roll_contracts <- ifelse(roll == 0 | is.na(roll), 0, -contract_pos)
    roll_commissions <- commission_fun(roll_contracts, ...)
    # cash settled on contracts rolled out of
    settled_cash <- ifelse(roll_contracts == 0, contract_pos * (current_price - previous_price), contract_pos * (current_roll_price - previous_roll_price))  # previous_roll_price will be previous_price unless we rolled yesterday too
    settled_cash <- ifelse(is.na(settled_cash), 0, settled_cash)

    # if we rolled, adjust contract_pos to zero - will trade into target weights later
    contract_pos <- ifelse(roll_contracts == 0, contract_pos, 0)

    # if we rolled, we freed up some margin
    # freed_margin <- ifelse(roll_contracts == 0, 0, abs(roll_contracts)*current_roll_price*margin)

    # calculate interest paid on yesterday's cash
    interest <- current_interest_rate * Cash

    # update cash balance - includes adding back yesterday's margin and deducting today's margin
    # Cash <- Cash + sum(settled_cash, na.rm = TRUE) + interest + sum(freed_margin, na.rm = TRUE) - maint_margin - sum(roll_commissions, na.rm = TRUE)
    Cash <- Cash + sum(settled_cash, na.rm = TRUE) + interest + maint_margin - margin*sum(abs(contract_pos)*current_price, na.rm = TRUE) - sum(roll_commissions, na.rm = TRUE)

    # update margin requirements
    # TODO: assumption: each contract has same maintenance margin requirements
    contract_value <- contract_pos * current_price  # contract_pos is zero here for anything we rolled out of today
    maint_margin <- margin * sum(abs(contract_value), na.rm = TRUE)

    # force reduce position if exceeds maintenance margin requirements
    margin_call <- FALSE
    liq_contracts <- rep(0, num_assets)
    liq_commissions <- rep(0, num_assets)
    liq_trade_value <- rep(0, num_assets)
    if(Cash < 0) {
      margin_call <- TRUE

      # liquidate equal proportions of each contract holding
      # liquidate 5% extra to requirement for bringing maint_margin into line with cash
      liquidate_factor <- 1.05*(maint_margin + Cash)/maint_margin

      # liquidate equal proportions of each contract (probably not how broker would actually do it)
      # but only liquidate a maximum amount of existing positions
      liq_contracts <- pmax(
        trunc(sign(contract_pos) * liquidate_factor * abs(contract_pos)),  # trunc to deal in integer number of contracts
        sign(contract_pos) * abs(contract_pos)
      ) # sign() to account for possible short positions
      liq_trade_value <- liq_contracts*current_price
      liq_commissions <- commission_fun(liq_contracts, ...)

      contract_pos <- contract_pos - liq_contracts
      contract_value <- contract_pos * current_price

      # account for freed margin
      # freed_margin <- margin*sum(abs(liq_contracts)*current_price, na.rm = TRUE)
      # Cash <- Cash + freed_margin - sum(liq_commissions, na.rm = TRUE)
      Cash <- Cash + maint_margin -margin*sum(abs(contract_value), na.rm = TRUE) - sum(liq_commissions, na.rm = TRUE)

      maint_margin <- margin * sum(abs(contract_value), na.rm = TRUE)
    }

    # capitalise profits/losses
    # TODO: assumption - continuous capitalisation of account with available cash if capitalise_profits = TRUE (in line with rebalance tolerance)
    if(capitalise_profits) {
      investable_cash <- Cash + maint_margin
    } else if (Cash < initial_cash) {  # if we have losses, we don't want to keep trying to invest our inititial cash
      investable_cash <- Cash + maint_margin
    } else {
      investable_cash <- initial_cash  # if we're not capitalising profits and we're in the black, invest our initial cash
    }

    # Update target contracts based on target weights, current weights and trade buffer
    target_contracts <- positionsFromNoTradeBuffer(contract_pos, current_price, current_weights, investable_cash, trade_buffer)
    target_contracts <- trunc(target_contracts)  # deal in integer number of contracts only

    trades <- target_contracts - contract_pos
    trade_value <- trades * current_price

    commissions <- commission_fun(trades, ...)

    # post-trade cash:  cash freed up from closing position, cash used as margin, commissions
    target_contract_value <- target_contracts * current_price
    post_trade_cash <- Cash + maint_margin - margin*sum(abs(target_contract_value), na.rm = TRUE) - sum(commissions, na.rm = TRUE)

    reduced_target_pos <- FALSE
    if(post_trade_cash < 0) {
      reduced_target_pos <- TRUE
      # adjust trade size down
      # post trade, max value of position can be found by:
      # Cash + freed_margin - post_trade_margin - commissions = 0
      # setting commissions to the value calculated for full trade size above (will be conservative), we get:
      # max_post_trade_contracts_value = (cash + freed_margin - commissions)/margin_requirement
      max_post_trade_contracts_value <- 0.95*(Cash + maint_margin - sum(commissions, na.rm = TRUE))/margin

      reduce_by <-  max_post_trade_contracts_value/sum(abs(target_contract_value), na.rm = TRUE)
      # ensure doesn't change sign from intended, but we only reduce target contracts no further than zero
      target_contracts <- trunc(sign(target_contracts) * reduce_by * pmax(abs(target_contracts), rep(0, num_assets)))
      trades <- target_contracts - contract_pos
      trade_value <- trades * current_price
      commissions <- commission_fun(trades, ...)

      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
      post_trade_cash <- Cash + maint_margin - margin*sum(abs(contract_value), na.rm = TRUE) - sum(commissions, na.rm = TRUE)
    } else {
      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
    }

    # update Cash and maint_margin
    Cash <- post_trade_cash
    maint_margin <- margin*sum(abs(contract_value), na.rm = TRUE)

    # store date in matrix as numeric, then convert back to date format with as.Date(date_col, origin ="1970-01-01")
    row_mat <- matrix(
      data = c(
        rep(as.numeric(current_date), num_assets+1),
        c(0, current_price),
        c(0, contract_pos),
        c(Cash, contract_value),
        c(0, margin*abs(contract_value)),
        c(interest, rep(0, num_assets)),
        c(0, settled_cash),
        c(0, trades - liq_contracts),  # minus because we keep sign of original position in liq_contracts
        # I don't think is correct: cash value shouldn't change by value of trades. Set to zero?
        c(-sum(trade_value, -liq_trade_value, na.rm = TRUE), trade_value-liq_trade_value),
        c(0, roll_contracts),
        c(NA, ifelse(roll_contracts == 0, NA, current_roll_price)),
        c(0, commissions + liq_commissions + roll_commissions),
        rep(margin_call, num_assets+1),   # bool will be stored as int (1-0)
        rep(reduced_target_pos, num_assets+1)
      ),
      nrow = num_assets + 1,
      ncol = 14,
      byrow = FALSE,
      dimnames = list(
        # symbols are row names
        c("Cash", symbols),
        # column names
        c("date", "close", "contracts", "exposure", "margin", "interest", "settled_cash", "contract_trades", "trade_value", "rolled_contracts", "roll_price", "commission", "margin_call", "reduced_target_pos")
      )
    )

    rowlist[[i]] <- row_mat

    previous_weights <- current_weights
    previous_price <- current_price
    previous_roll_price <- current_roll_price
  }

  # Optionally prepend initial state (t=0)
  if(include_initial_state) {
    initial_row <- matrix(
      data = c(
        rep(as.numeric(prices[1, 1]), num_assets+1),   # Use first date
        c(0, prices[1, 2:(num_assets+1)]),              # Current contract prices at t=0
        rep(0, num_assets+1),                           # Zero contracts
        c(initial_cash, rep(0, num_assets)),            # Initial cash only
        rep(0, num_assets+1),                           # No margin
        rep(0, num_assets+1),                           # No interest
        rep(0, num_assets+1),                           # No settled cash
        rep(0, num_assets+1),                           # No trades
        rep(0, num_assets+1),                           # No trade value
        rep(0, num_assets+1),                           # No rolled contracts
        rep(NA, num_assets+1),                          # No roll price
        rep(0, num_assets+1),                           # No commission
        rep(FALSE, num_assets+1),                       # No margin call
        rep(FALSE, num_assets+1)                        # No reduced target
      ),
      nrow = num_assets + 1,
      ncol = 14,
      byrow = FALSE,
      dimnames = list(
        c("Cash", symbols),
        c("date", "close", "contracts", "exposure", "margin", "interest", "settled_cash", "contract_trades", "trade_value", "rolled_contracts", "roll_price", "commission", "margin_call", "reduced_target_pos")
      )
    )
    rowlist <- c(list(initial_row), rowlist)
  }

  # Combine list of matrixes into dataframe
  do.call(rbind, rowlist) %>%
    tibble::as_tibble(rownames = "symbol") %>%
    dplyr::mutate(
      date = as.Date(date, origin ="1970-01-01"),
      margin_call = dplyr::if_else(margin_call != 0, TRUE, FALSE),
      reduced_target_pos = dplyr::if_else(reduced_target_pos != 0, TRUE, FALSE)
    )
}

#' Calculate timeseries of end-of-day portfolio equity from futures backtest results.
#'
#' @description End-of-day portfolio equity for a futures simulation is
#' equivalent to the cash balance (having settled cash from existing positions)
#' plus posted margin.
#'
#' @param futs_backtest_results dataframe output of
#' `fixed_commission_futs_backtest`
#'
#' @return dataframe of date, equity
#' @export
#'
#' @examples
#' /dontrun{futs_backtest_equity(results)}
futs_backtest_equity <- function(futs_backtest_results) {
  futs_backtest_results %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(margin = sum(margin)) %>%
    dplyr::left_join(
      results %>% dplyr::select(date, symbol, exposure) %>% dplyr::filter(symbol == "Cash"),
      by = "date"
    ) %>%
    dplyr::mutate(equity = margin + exposure) %>%
    dplyr::select(date, equity)
}
