#' Generate ggplot colours
#'
#' @param n number of colours to generate
#'
#' @return Character vector of n default ggplot2 colours in hex format
#' @export
#'
#' @examples
#' gg_colour_hue(10)
gg_colour_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#' Generate a set of portfolio colours
#'
#' @param tickers tickers in portfolio
#'
#' @return Named character vector of n+2 default ggplot2 colours in hex format
#' corresponding to each ticker plus "Cash" and "Portfolio"
#' @export
#'
#' @examples
#' portfolio_colours(c("ABC", "XYZ"))
portfolio_colours <- function(tickers) {
  n = length(tickers)
  my_cols <- gg_colour_hue(n)
  my_cols <- c(my_cols, "#00b0f6", "#E76BF3")
  names(my_cols) <- c(tickers, "Cash", "Portfolio")

  my_cols

}

#' Portfolio color scale
#'
#' @param cols Named character vector of colours in hex format. Names correspond
#' to tickers in the universe, plus "Cash" and "Portfolio"
#'
#' @return ggplot2 colour scale
#' @export
#'
#' @examples
#' my_cols <- portfolio_colours(7)
#' port_colour_scale <- make_port_col_scale(my_cols)
make_port_col_scale <- function(cols) {
  # colours for portfolio plots - including NAV and cash components
  port_col_scale <- ggplot2::scale_colour_manual(name = "ticker", values = cols)

  port_col_scale

}

#' Portfolio fill scale
#'
#' @param cols Named character vector of colours in hex format. Names correspond
#' to tickers in the universe, plus "Cash" and "Portfolio"
#'
#' @return ggplot2 fill scale
#' @export
#'
#' @examples
#' my_cols <- portfolio_colours(7)
#' port_fill_scale <- make_port_fill_scale(my_cols)
make_port_fill_scale <- function(cols) {
  # fills for portfolio plots - including NAV and cash components
  port_fill_scale <- ggplot2::scale_fill_manual(name = "ticker", values = cols)

  port_fill_scale

}

#' Portfolio color scale (tickers only)
#'
#' @param cols Named character vector of colours in hex format. Names correspond
#' to tickers in the universe, plus "Cash" and "Portfolio"
#'
#' @return ggplot2 colour scale
#' @export
#'
#' @examples
#' my_cols <- portfolio_colours(7)
#' colour_scale <- make_col_scale(my_cols)
make_col_scale <- function(cols) {
  # colours for plots with tickers only
  tickers_col_scale <- ggplot2::scale_colour_manual(name = "ticker", values = cols[!names(cols) %in% c('Cash', 'Portfolio')])

  tickers_col_scale
}

#' Portfolio fill scale (tickers only)
#'
#' @param cols Named character vector of colours in hex format. Names correspond
#' to tickers in the universe, plus "Cash" and "Portfolio"
#'
#' @return ggplot2 fill scale
#' @export
#'
#' @examples
#' my_cols <- portfolio_colours(7)
#' fill_scale <- make_fill_scale(my_cols)
make_fill_scale <- function(cols) {
  tickers_fill_scale <- ggplot2::scale_fill_manual(name = "ticker", values = cols[!names(cols) %in% c('Cash', 'Portfolio')])

  tickers_fill_scale
}


#' Append NAV to backtest results dataframe
#'
#' @param bt_results_df Results dataframe from rsims backtest
#'
#' @return New results data frame with a row for portfolio NAV each day
#' @export
#'
#' @examples
#' \dontrun{
#' results <- cash_backtest(prices, weights)
#' results <- append_nav_to_bt_results(results)
#' }
append_nav_to_bt_results <- function(bt_results_df) {
  port_nav <- bt_results_df %>%
    dplyr::select(date, exposure, ticker) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(exposure = sum(exposure)) %>%
    dplyr::mutate(ticker = "NAV")

  bt_results_df %>%
    dplyr::select(date, exposure, ticker) %>%
    dplyr::bind_rows(port_nav) %>%
    dplyr::arrange(date)
}

#' Plot exposures and NAV timeseries as a stacked area chart
#'
#' @param results_df Results dataframe from rsims backtest with daily NAV
#' @param title Plot title
#' @param tickers Character vector of tickers used in the simulation
#' @param colours Character vector of colours to use for tickers and Cash exposures
#'
#' @return A ggplot2 stacked area chart of timeseries of exposures and NAV
#' @export
#'
#' @examples
#' \dontrun{
#' tickers = unique(prices$ticker)
#' my_cols = portfolio_colours(length(tickers))
#' cash_backtest(prices, weights) %>%
#'   append_nav_to_bt_results() %>%
#'   stacked_area_chart("Exposures and NAV", tickers = tickers, colours = my_cols)
#' }
stacked_area_chart <- function(results_df, title, tickers, colours) {
  results_df %>%
    dplyr::filter(ticker != "NAV") %>%
    ggplot2::ggplot(aes(x = date, y = exposure, fill = ticker)) +
    ggplot2::geom_area() +
    ggplot2::scale_fill_manual(name = "ticker", values = colours, limits = c(tickers, 'Cash')) +
    ggplot2::geom_line(data = results_df %>% dplyr::filter(ticker == "NAV"), aes(x = date, y = exposure, colour = "ticker"), size = 1.5) +
    ggplot2::scale_colour_manual(values = "black", labels = "NAV") +
    ggplot2::labs(
      x = "Date",
      y = "Expsoure Value",
      title = title,
      colour = ""
    )
}

#' Timeseries portfolio returns
#'
#' @param results_df Results dataframe from rsims backtest
#'
#' @return Time series of portfolio returns
#' @export
#'
#' @examples
#' \dontrun{
#' cash_backtest(prices, weights) %>%
#'   calc_port_returns()
#' }
calc_port_returns <- function(results_df) {
  results_df %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(totalequity = sum(exposure, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(returns = totalequity / dplyr::lag(totalequity) - 1)
}

#' Annual turnover
#'
#' @param results_df Results dataframe from rsims backtest
#' @param mean_equity Mean daily portfolio equity
#' @param start_date Backtest start date
#' @param end_date Backtest end date
#'
#' @return Annual turnover as yearly average of total sell trades as pct of mean equity
#' @export
#'
#' @examples
#' \dontrun{
#' results_df <- cash_backtest(prices, weights)
#' port_returns <- results_df %>%
#'   calc_port_returns()
#' mean_equity <- mean(port_returns$totalequity)
#' results_df %>%
#'   calc_ann_turnover(mean_equity, "2000-01-01", "2021-12-31")
#' }
calc_ann_turnover <- function(results_df, mean_equity, start_date, end_date) {
  totalselltrades <- results_df %>%
    dplyr::filter(
      ticker != 'Cash',
      trade_value < 0,
      date != start_date
    ) %>%
    dplyr::summarise(sellvalue = sum(trade_value)) %>%
    dplyr::pull()

  if(length(totalselltrades) == 0) {
    return(0)
  } else {
    -100*totalselltrades / (mean_equity * (lubridate::year(end_date) - lubridate::year(start_date)))
  }
}

#' Calculate costs as percent of profits
#'
#' @param results_df Results dataframe from rsims backtest
#' @param initial_equity Starting equity
#'
#' @return Costs as a percentage of profit
#' @export
#'
#' @examples
#' \dontrun{
#' results_df <- cash_backtest(prices, weights)
#' costs_pct_profit(results_df, 20000)
#' }
costs_pct_profit <- function(results_df, initial_equity) {
  port_returns <- results_df %>%
    calc_port_returns()

  total_profit <- tail(port_returns, 1)$totalequity - initial_equity
  sum(port_returns$totalcommission) / total_profit
}

#' Summary performance table
#'
#' @param results_df Results dataframe from rsims backtest
#' @param initial_equity Starting equity
#'
#' @return dataframe of summary statistics: annualised return, annualised
#' volatility, annualised Sharpe ratio, average annual turnover, total profit,
#' and costs as a percentage of total profit.
#' @export
#'
#' @examples
#' \dontrun{
#' cash_backtest(prices, weights) %>%
#'   summary_performance(20000)
#' }
summary_performance <- function(results_df, initial_equity) {
  start_date <- head(results_df$date, 1)
  end_date <- tail(results_df$date, 1)

  port_returns <- results_df %>%
    calc_port_returns()

  mean_equity <- mean(port_returns$totalequity)
  total_profit <- tail(port_returns, 1)$totalequity - initial_equity
  total_commission <- sum(results_df$commission, na.rm = TRUE)
  costs_pct_profit <- 100*total_commission / total_profit

  perf_table <- port_returns %>%
    summarise(
      "Ann.Return(%)" = 252*mean(returns, na.rm = TRUE)*100,
      "Ann.Volatility(%)" = sqrt(252)*sd(returns, na.rm = TRUE)*100,
      "Ann.Sharpe" = `Ann.Return(%)`/`Ann.Volatility(%)`
    ) %>%
    bind_cols(c(
      results_df %>%
        calc_ann_turnover(mean_equity, start_date, end_date) %>%
        tibble::enframe(name = NULL, value = "Ave.Ann.Turnover(%)"),
      total_profit %>%
        tibble::enframe(name = NULL, value = "Tot.Profit($)"),
      costs_pct_profit %>%
        tibble::enframe(name = NULL, value = "Costs(%Profit)")
    ))

  return(perf_table)
}

#' Sharpe ratio
#'
#' @param results_df Results dataframe from rsims backtest
#'
#' @return Annualised Sharpe ratio
#' @export
#'
#' @examples
#' \dontrun{
#' cash_backtest(prices, weights) %>%
#'   calc_sharpe()
#' }
calc_sharpe <- function(results_df) {
  results_df %>%
    calc_port_returns() %>%
    dplyr::summarise(
      "Ann.Return(%)" = 252*mean(returns, na.rm = TRUE)*100,
      "Ann.Volatility(%)" = sqrt(252)*sd(returns, na.rm = TRUE)*100,
      "Ann.Sharpe" = `Ann.Return(%)`/`Ann.Volatility(%)`
    ) %>%
    dplyr::pull(Ann.Sharpe)
}

#' Trade chart
#'
#' @param results_df Results dataframe from rsims backtest
#' @param title Plot title
#' @param fill_scale ggplot2 fill scale (tickers only)
#' @param colour_scale ggplot2 colour scale (tickers only)
#'
#' @return Plot of trade value by ticker and date
#' @export
#'
#' @examples
#' \dontrun{
#' tickers = unique(prices$ticker)
#' my_cols = portfolio_colours(length(tickers))
#' col_scale <- make_col_scale(my_cols)
#' fill_scale <- make_fill_scale(my_cols)
#' results <- cash_backtest(prices, weights)
#' trades_chart(results, "Trade Value", fill_scale, col_scale)
#' }
trades_chart <- function(results_df, title, fill_scale, colour_scale) {
  # specifying colour for bar plots will render a border, without which some bars don't plot due to long x-axis
  results_df %>%
    dplyr::filter(!ticker %in% c('Cash', 'Portfolio')) %>%
    ggplot2::ggplot(aes(x = date, y = trade_value, fill = ticker, colour = ticker)) +
    ggplot2::geom_bar(stat = 'identity', position = ggplot2::position_stack(), lwd = 0.6) +
    fill_scale +
    colour_scale +
    ggplot2::labs(
      x = "Date",
      y = "Trade Value",
      title = title
    )
}

#' Commission chart
#'
#' @param results_df Results dataframe from rsims backtest
#' @param title Plot title
#' @param fill_scale ggplot2 fill scale (tickers only)
#' @param colour_scale ggplot2 colour scale (tickers only)
#'
#' @return Plot of commission cost by ticker and date
#' @export
#'
#' @examples
#' \dontrun{
#' tickers = unique(prices$ticker)
#' my_cols = portfolio_colours(length(tickers))
#' col_scale <- make_col_scale(my_cols)
#' fill_scale <- make_fill_scale(my_cols)
#' results <- cash_backtest(prices, weights)
#' comm_chart(results, "Commission Cost", fill_scale, col_scale)
#' }
comm_chart <- function(results_df, title, fill_scale, colour_scale) {
  # Trading cost as $
  results_df %>%
    dplyr::filter(!ticker %in% c('Cash', 'Portfolio')) %>%
    ggplot2::ggplot(aes(x = date, y = commission , fill = ticker, colour = ticker)) +
    ggplot2::geom_bar(stat = 'identity', lwd = 0.6, position = ggplot2::position_stack()) +
    fill_scale +
    colour_scale +
    ggplot2::labs(
      x = "Date",
      y = "Commission",
      title = title
    )
}

#' Commission as percent of exposure chart
#'
#' @param results_df Results dataframe from rsims backtest
#' @param title Plot title
#' @param fill_scale ggplot2 fill scale (tickers only)
#' @param colour_scale ggplot2 colour scale (tickers only)
#'
#' @return Plot of commission cost as percent of expsoure by ticker and date
#' @export
#'
#' @examples
#' \dontrun{
#' tickers = unique(prices$ticker)
#' my_cols = portfolio_colours(length(tickers))
#' col_scale <- make_col_scale(my_cols)
#' fill_scale <- make_fill_scale(my_cols)
#' results <- cash_backtest(prices, weights)
#' comm_pct_exp_chart(results, "Commission (% of Exposure)", fill_scale, col_scale)
#' }
comm_pct_exp_chart <- function(results_df, title, fill_scale, colour_scale) {
  # Trading cost as % of total exposure in instrument
  results_df %>%
    dplyr::filter(!ticker %in% c('Cash', 'Portfolio')) %>%
    dplyr::mutate(commissionpct = commission / abs(exposure)) %>%
    ggplot2::ggplot(aes(x = date, y = commissionpct , fill = ticker, colour = ticker)) +
    ggplot2::geom_bar(stat = 'identity', position = ggplot2::position_stack(), lwd = 0.6) +
    fill_scale +
    colour_scale +
    ggplot2::labs(
      x = "Date",
      y = "Commission",
      title = title
    )
}

#' Combine portfolio anda asset returns
#'
#' @param results_df Results dataframe from rsims backtest
#' @param returns_df Dataframe of asset returns
#'
#' @return Combined dataframe of portfolio and asset returns
#' @export
combine_port_asset_returns <- function(results_df, returns_df) {
  # create df of port and asset returns
  results_df %>%
    calc_port_returns() %>%
    select(date, returns) %>%
    mutate(ticker = "Portfolio") %>%
    bind_rows(returns_df %>% select(ticker, date, returns)) %>%
    arrange(date)
}

#' Rolling annualised portfolio performance
#'
#' @param port_returns_df Dataframe of portfolio returns
#'
#' @return Dataframe of rolling annualised portfolio statistics
#' @export
#'
rolling_ann_port_perf <- function(port_returns_df) {
  # rolling performance of portfolio

  roll_df <- port_returns_df %>%
    mutate(
      roll_ann_return = 252*roll::roll_mean(returns, width = 252, min_obs = 252),
      roll_ann_sd = sqrt(252)*roll::roll_sd(returns, width = 252, min_obs = 252),
      roll_sharpe = roll_ann_return/roll_ann_sd
    ) %>%
    select(date, roll_ann_return, roll_ann_sd, roll_sharpe) %>%
    pivot_longer(cols = c(-date), names_to = "metric", values_to = "value")

  roll_df
}

#' Rolling annualised portfolio performance plot
#'
#' @param perf_df Dataframe of rolling annualised metrics
#'
#' @return ggplot2 of rolling annualised performance
#' @export
#'
#' @examples
#' \dontrun{
#' results_df %>%
#'   calc_port_returns() %>%
#'   rolling_ann_port_perf() %>%
#'   rolling_ann_port_perf_plot()
#' }
rolling_ann_port_perf_plot <- function(perf_df) {
  metric_names <- as_labeller(c(
    `roll_ann_return` = "Return",
    `roll_ann_sd` = "Volatility",
    `roll_sharpe` = "Sharpe"
  ))

  perf_df %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~metric, scales = "free_y", ncol = 1, labeller = metric_names) +
    labs(
      x = "Date",
      y = "Volatility",
      title = "1-year Rolling Annualised Performance"
    )
}
