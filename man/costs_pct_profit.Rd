% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_processing.R
\name{costs_pct_profit}
\alias{costs_pct_profit}
\title{Calculate costs as percent of profits}
\usage{
costs_pct_profit(results_df, initial_equity)
}
\arguments{
\item{results_df}{Results dataframe from rsims backtest}

\item{initial_equity}{Starting equity}
}
\value{
Costs as a percentage of profit
}
\description{
Calculate costs as percent of profits
}
\examples{
\dontrun{
results_df <- cash_backtest(prices, weights)
costs_pct_profit(results_df, 20000)
}
}
