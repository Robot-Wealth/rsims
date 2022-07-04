# TODO:
  # test this function using example data below, plus another edge case
  # document: wrangle contract data into format for backtest using open interest
  # describe contracts data: needs ticker, date, price, open_interest, point_value
  # if we want to do per-contract margin, would include it here
  # describe output - prices converted to point values
  # example:
    # this will cause roll to show up the day after something is identified as having max OI
    # eg GC:
    # 2021-01-15 G has highest OI
    # 2021-01-19 J has highest OI (no trading 16-19)
    # 2021-01-20 we roll into J (previous_contract and close_previous_contract correspond to G, current to J)
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

# TODO:
  # document: transforms a wrangled contracts dataframe into a numerical matrix format for rsims simulation
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

# TODO:
  # document: takes a long dataframe of date, symbol, target weights
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

roll <- function(rolls, current_pos) {

}


# TODO:
# write a vignette describing the flow:
# describe the flow of the event loop and sequence of events
# wrangle contracts data into a df that you can look at and reason about
# convert that to a numerical matrix for simulation
# do the simulation
# describe how the roll is handled and trade logic
# fail if prices columns aren't in correct order: current prices, roll prices, roll bool

# TODO: maybe rename this to futs_backtest and handle different approaches via the
# commission model passed. eg if commission_fun == x, rebal back to ideal
# ACTUALLY... probably easier to call it fixed_comm_futs_backtest, and make the rebal function
# static (for now). Can then pass fixed_percent or fixed_per_contract commission models and get the same behaviour....
# include this in description

# TODO: currently we treat margin as the same for all products... could rejig this so that we have per-contract margin
# TODO: describe process of aligning wights and prices - should be row-aligned with the prices you trade into the weights at - means you need to lag your weights upstream. remember: dates are aligned such that prices are prices at which we trade into target weights

# TODO: describe philosophy: backtesting function is quite opinionated wrt the data that you give it. But if you put the time in to
# wrangling your data as it expects (there are functions to help with this), you get the benefit of extremely efficient backtesting code.

# describe how it force liquidates you down to your margin requirements and won't let you put on positions that your
# broker rejects.

# explain how target weights need to include leverage (ie embed leverage in weights)

# TODO: test huge leverage and try to get rekt... cash should get down to zero but no lower
# conditions that get us rekt

# TODO: document better - needs updated params, updated output df including columns and what they mean
# specify that values in results df are at the end of the period

# TODO: futures post processing stuff: NAV = Cash + margin used in existing postions
# mention that we assume we can trade fractional contracts... may want to fix this

#' Futures Backtest, roll on days to expiry, minimum commission model
#'
#' @description Event-based simulation based on desired futures positions, raw
#' contract prices (ie not backadjusted) and user-specified roll date (dte).
#'
#' Won't allow you to put on a position that would be rejected by the broker given
#' user-supplied margin requirements. Positions that would be rejected are automatically scaled back.
#'
#'
#' @param prices Matrix of trade prices. Column 1 must be the timestamp or
#' index. Column 2 must be days-to-expiry.
#' @param target_weights Matrix of theoretical weights. Column 1 must be the
#' timestamp or index.
#' @param interest_rates Matrix of daily interest rate applied to cash balance
#' @param trade_buffer Trade buffer parameter
#' @param initial_cash Inital cash balance
#' @param capitalise_profits If TRUE, utilise profits and initial cash balance
#' in determining position sizes. If FALSE, profits accrue as a cash balance and
#'  are not reinvested.
#' @param commission_fun Function for determining commissions from prices and
#' trades
#' @param ... Additional arguments passed to commission_fun. For futs_per_contract_commission,
#' this will be per_contract_commission (a vector of commissions per contract)
#'
#' @return long dataframe of results - dates, trades, commissions, value of portfolio components
#' @details
#' `target_weights` should be date-aligned with `prices` - it is up to the user to lag `target_weights` as necessary to
#' ensure that trades occur at appropriate prices.
#' @examples
#' @export
fixed_commission_futs_backtest <- function(prices, target_weights, interest_rates, trade_buffer = 0., initial_cash = 10000, margin = 0.05, capitalise_profits = FALSE, commission_fun, ...) {
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

  # Iterate through prices and backtest
  for (i in 1:(nrow(target_weights))) {
    current_date <- target_weights[i, 1]  # date will be a numeric from origin
    current_price <- prices[i, c(2:(2+num_assets-1))]
    current_roll_price <- prices[i, c((2+num_assets):(2+2*num_assets-1))]  # prices of contract rolled out of today
    roll <- prices[i, c((2+2*num_assets):(2+3*num_assets-1))]  # 0 or 1 designating whether we rolled or not
    current_weights <- target_weights[i, -1]
    current_interest_rate <- interest_rates[i, -1]

    # calculate cash settled at today's close based on yesterday's positions and if we roll

    # get number of contracts to roll out of
    roll_contracts <- ifelse(roll == 0, 0, -contract_pos)
    roll_commissions <- commission_fun(roll_contracts, ...)
    # cash settled on contracts rolled out of
    settled_cash <- ifelse(roll_contracts == 0, contract_pos * (current_price - previous_price), contract_pos * (current_roll_price - previous_roll_price))  # previous_roll_price will be previous_price unless we rolled yesterday too
    settled_cash <- ifelse(is.na(settled_cash), 0, settled_cash)

    # if we rolled, adjust contract_pos to zero - will trade into target weights later
    contract_pos <- ifelse(roll_contracts == 0, contract_pos, 0)

    # if we rolled, we freed up some margin
    freed_margin <- ifelse(roll_contracts == 0, 0, abs(roll_contracts)*current_roll_price*margin)

    # calculate interest paid on yesterday's cash
    interest <- current_interest_rate * Cash

    # update cash balance
    Cash <- Cash + sum(settled_cash) + interest + sum(freed_margin) - sum(roll_commissions)

    # check margin requirements
    # TODO: assumption: each contract has same maintenance margin requirements
    contract_value <- contract_pos * current_price  # contract_pos is zero here for anything we rolled out of today
    maint_margin <- margin * sum(abs(contract_value))

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
        sign(contract_pos) * liquidate_factor * abs(contract_pos),
        sign(contract_pos) * abs(contract_pos)
      ) # sign() to account for possible short positions
      liq_trade_value <- liq_contracts*current_price
      liq_commissions <- commission_fun(liq_contracts, ...)

      # account for freed margin
      freed_margin <- margin*sum(abs(liq_contracts)*current_price)

      Cash <- Cash + freed_margin - sum(liq_commissions)
      contract_pos <- contract_pos - liq_contracts
      contract_value <- contract_pos * current_price
      maint_margin <- margin * sum(abs(contract_value))
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

    trades <- target_contracts - contract_pos
    trade_value <- trades * current_price

    commissions <- commission_fun(trades, ...)

    # post-trade cash:  cash freed up from closing position, cash used as margin, commissions
    target_contract_value <- target_contracts * current_price
    post_trade_cash <- Cash + maint_margin - margin*sum(abs(target_contract_value)) - sum(commissions)

    reduced_target_pos <- FALSE
    if(post_trade_cash < 0) {
      reduced_target_pos <- TRUE
      # adjust trade size down
      # post trade, max value of position can be found by:
      # Cash + freed_margin - post_trade_margin - commissions = 0
      # setting commissions to the value calculated for full trade size above (will be conservative), we get:
      # max_post_trade_contracts_value = (cash + freed_margin - commissions)/margin_requirement
      max_post_trade_contracts_value <- 0.95*(Cash + maint_margin - sum(commissions))/margin

      reduce_by <-  max_post_trade_contracts_value/sum(abs(target_contract_value))
      # ensure doesn't change sign from intended, but we only reduce target contracts no further than zero
      target_contracts <- sign(target_contracts) * reduce_by * pmax(abs(target_contracts), rep(0, num_assets))
      trades <- target_contracts - contract_pos
      trade_value <- trades * current_price
      commissions <- commission_fun(trades, ...)

      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
      post_trade_cash <- Cash + maint_margin - margin*sum(abs(contract_value)) - sum(commissions)
    } else {
      contract_pos <- target_contracts
      contract_value <- contract_pos * current_price
    }

    # update Cash and maint_margin
    Cash <- post_trade_cash
    maint_margin <- margin*sum(abs(contract_value))

    # TODO: what are the conditions that get us rekt in this case?
    # run out of cash, even after being liquidated, not enough cash to open a position
    # if we get margin called, we get liquidated... not necessarily complete rekt
    # but if Cash goes negative or hits zero after being liquidated, we're rekt
    # can use if Cash + maint_margin < 0 ... rekt

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
        c(0, margin*abs(contract_value)),
        c(interest, rep(0, num_assets)),
        c(0, settled_cash),
        c(0, trades - liq_contracts),  # minus because we keep sign of original position in liq_contracts
        c(-sum(trade_value, -liq_trade_value), trade_value-liq_trade_value),
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
        c("date", "close", "contracts", "exposure", "margin", "interest", "settledcash", "contract_trades", "trade_value", "rolled_contracts", "roll_price", "commission", "margin_call", "reduced_target_pos")
      )
    )

    rowlist[[i]] <- row_mat

    previous_weights <- current_weights
    previous_price <- current_price
    previous_roll_price <- current_roll_price
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
