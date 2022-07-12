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
  # x dollars per share
  # min y dollars per order
  # max z% of tradevalue
  tradevalue <- shares_traded * prices
  commissions <- pmin(abs(tradevalue)/unadjprices * dollars_per_share, max_pct_per_order*abs(tradevalue))  # scale by unadjusted close to get actual commissions on a given tradevalue
  commissions[commissions < min_dollars_per_order & abs(shares_traded) > 0] <- min_dollars_per_order

  commissions
}

# IB futures costs
# Lowest tier: 0.85/contract, 0.25/micro-contract
# Interest earned: BM-0.5% (currently 1.08%)
#' Per-Contract Futures Commissions
#'
#' @description Calculates commission on number of contracts traded given a per-
#' contract commission structure.
#'
#' @param contracts_traded vector of number of contracts traded per symbol
#' @param per_contract_commission vector of per-contract commission per symbol
#'
#' @return vector of commissions paid per symbol
#' @export
#'
#' @examples
#' contracts_traded <- c("ES" = 5, "GC" = -2)
#' per_contract_commission <- c("ES" = 0.85, "GC" = 0.5)
#' futs_per_contract_commission(contracts_traded, per_contract_commission)
futs_per_contract_commission <- function(contracts_traded, per_contract_commission) {
  abs(contracts_traded)*per_contract_commission
}

