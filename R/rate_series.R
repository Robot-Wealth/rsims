# rate_series.R
# Coercion and validation for the two rate inputs a backtest takes: interest
# rates on cash, and short borrow costs.
#
# Both are indexed by row position inside the simulation loop, which means a
# series whose dates do not line up with the price matrix is applied to the
# wrong days and nothing complains. That is not hypothetical: a rates matrix
# built from an unfiltered price history, against weights that had lost sixty
# days to a rolling volatility window, silently applied every rate sixty
# trading days late for a twenty-year simulation.
#
# So anything indexed by row gets its dates checked once, here, before the loop.

#' Check a date-keyed rate matrix lines up with the price matrix
#'
#' @param x Matrix or data frame whose first column is a date
#' @param prices The price matrix the simulation iterates over
#' @param what Name to use in error messages
#' @return `x` as a matrix
#' @keywords internal
validate_rate_dates <- function(x, prices, what) {
  x <- data.matrix(x)

  if (nrow(x) != nrow(prices)) {
    stop(glue::glue(
      "{what} has {nrow(x)} rows but prices has {nrow(prices)}. ",
      "It is read by row position, so it must cover exactly the simulated dates."
    ))
  }

  bad <- which(x[, 1] != prices[, 1])
  if (length(bad) > 0) {
    stop(glue::glue(
      "{what} dates do not match prices at {length(bad)} row(s), first at index {bad[1]} ",
      "({as.Date(x[bad[1], 1], origin = '1970-01-01')} vs ",
      "{as.Date(prices[bad[1], 1], origin = '1970-01-01')})."
    ))
  }

  x
}

#' Build the per-day, per-asset short borrow cost matrix
#'
#' Accepts either a named vector, meaning a constant rate through time, or a
#' date-keyed matrix or data frame, meaning a rate that varies by day. Returns
#' an `nrow(prices)` by `length(tickers)` matrix in **tickers order**, so the
#' simulation loop can multiply it against `share_pos` elementwise without
#' having to think about it.
#'
#' Rates are annualised decimals, so 0.0025 is 25 basis points a year. This is
#' the borrow **fee**, the thing you pay. The rebate is not modelled separately:
#' short sale proceeds are held as collateral and earn nothing, which is what a
#' retail-sized account actually gets.
#'
#' Two behaviours here differ from doing it inline, and both are fixes:
#'
#' A named vector used to be sorted alphabetically and then multiplied against
#' `share_pos`, which is in the column order of `target_weights`. Those agree
#' only when the columns happen to be alphabetical. Now the vector is placed
#' into tickers order by name.
#'
#' A vector shorter than the number of assets used to be recycled, so naming one
#' instrument silently applied its rate to all of them. It only failed to show
#' up because borrow is zeroed on long positions. Unnamed tickers now get zero.
#'
#' @param short_borrow_costs Named vector, or date-keyed matrix/data frame
#' @param prices The price matrix the simulation iterates over
#' @param tickers Column order of `target_weights`
#' @return Numeric matrix, nrow(prices) x length(tickers)
#' @keywords internal
build_borrow_matrix <- function(short_borrow_costs, prices, tickers) {
  n <- nrow(prices)
  k <- length(tickers)
  zero <- matrix(0, nrow = n, ncol = k, dimnames = list(NULL, tickers))

  if (is.null(short_borrow_costs)) return(zero)

  if (is.matrix(short_borrow_costs) || is.data.frame(short_borrow_costs)) {
    m <- validate_rate_dates(short_borrow_costs, prices, "short_borrow_costs")
    cols <- colnames(m)[-1]
    if (is.null(cols) || !all(tickers %in% cols)) {
      stop(glue::glue(
        "short_borrow_costs matrix needs a date column plus one column per ticker. ",
        "Missing: {paste(setdiff(tickers, cols), collapse = ', ')}"
      ))
    }
    out <- m[, tickers, drop = FALSE]
    out[is.na(out)] <- 0
    return(out)
  }

  if (is.null(names(short_borrow_costs)) ||
      !all(names(short_borrow_costs) %in% tickers)) {
    stop("short_borrow_costs must be a named vector with names corresponding to tickers")
  }

  v <- rep(0, k)
  names(v) <- tickers
  v[names(short_borrow_costs)] <- short_borrow_costs

  matrix(rep(v, each = n), nrow = n, ncol = k, dimnames = list(NULL, tickers))
}
