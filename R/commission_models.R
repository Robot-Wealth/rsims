# Commission models

#' Fixed percent commission
#'
#' @description Commission calculated as a fixed percent of traded value. Suitable for many
#' crypto exchanges.
#'
#' @param trade_value vector of traded value for each asset
#' @param commission_pct percentage of traded value to be paid as commission (scalar or vector of same length as `trade_value`)
#'
#' @return vector of commissions
#' @export
#'
#' @examples
#' fixed_percent(c(150, 75, -220), 0.01)
fixed_percent <- function(trades, trade_value, commission_pct = 0.0) {
  abs(trade_value) * commission_pct
}

#' US Tiered Minimum Commission Model
#'
#' @param shares_traded number of shares traded
#' @param prices
#' @param unadjprices
#' @param max_pct_per_order
#' @param min_dollars_per_order
#' @param dollars_per_share
#'
#' @return
#' @export
#'
#' @examples
us_tiered_commission <- function(shares_traded, prices, unadjprices, max_pct_per_order, min_dollars_per_order, dollars_per_share) {
  # x cents per share
  # min y dollars per order
  # max z% of tradevalue
  tradevalue <- shares_traded * prices
  commissions <- pmin(abs(tradevalue)/unadjprices * dollars_per_share, max_pct_per_order*abs(tradevalue))  # scale by unadjusted close to get actual commissions on a given tradevalue
  commissions[commissions < min_dollars_per_order & abs(shares_traded) > 0] <- min_dollars_per_order

  commissions
}



