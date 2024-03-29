# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Calculate target positions from theoretical weights and trade buffer parameter
#' @export
positionsFromNoTradeBuffer <- function(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer) {
    .Call('_rsims_positionsFromNoTradeBuffer', PACKAGE = 'rsims', current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer)
}

#' Calculate target positions from theoretical weights and trade buffer parameter in the presence of minimum commission
#' @export
positionsFromNoTradeBufferMinComm <- function(current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer) {
    .Call('_rsims_positionsFromNoTradeBufferMinComm', PACKAGE = 'rsims', current_positions, current_prices, current_theo_weights, cap_equity, trade_buffer)
}

