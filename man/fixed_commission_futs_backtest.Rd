% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixed_commission_futs_backtest.R
\name{fixed_commission_futs_backtest}
\alias{fixed_commission_futs_backtest}
\title{Futures Backtest, roll on days to expiry, fixed commission model}
\usage{
fixed_commission_futs_backtest(
  prices,
  target_weights,
  interest_rates,
  trade_buffer = 0,
  initial_cash = 10000,
  margin = 0.05,
  capitalise_profits = FALSE,
  commission_fun,
  ...
)
}
\arguments{
\item{prices}{Matrix of trade prices as per output of
\code{make_sim_prices_matrix}. Column 1 must be the
timestamp or index in numerical format.}

\item{target_weights}{Matrix of target weights as per output of
\code{make_sim_weights_matrix}. Column 1 must be the timestamp or index in
numerical format.}

\item{interest_rates}{Matrix of daily interest rate applied to cash balance.
Ensure that the rate is adjusted to reflect daily interest. For instance,
many sources quote the rate in \%pa - this would require dividing by (365*100).}

\item{trade_buffer}{Trade buffer parameter to prevent hyperactive trading
back to the target weights. When an actual portfolio weight differs from its
target weight by \code{trade_buffer} or more, rebalance back to the target weight
plus or minus the \code{trade_buffer} depending on which side of the target weight
the current weight lies. This is theoretically optimal if costs are linear.}

\item{initial_cash}{Inital cash balance.}

\item{margin}{Margin requirement as proportion of position value. Currently
a constant value for all assets.}

\item{capitalise_profits}{If TRUE, utilise profits and initial cash balance
in determining position sizes. If FALSE, profits accrue as a cash balance and
are not reinvested.}

\item{commission_fun}{Function for determining commissions from prices and
trades}

\item{...}{Additional arguments passed to commission_fun. For
\code{futs_per_contract_commission}, this will be \code{per_contract_commission} (a
vector of commissions per contract).}
}
\value{
long dataframe of results consisting of the following columns:
symbol: Futures symbol without contract designation (eg ES, GC, ZB, etc)
date: In YYYY-MM-DD format
close: Closing contract USD value determined from closing_price*point_value.
contracts: Number of contracts held.
exposure: Value of exposure to current symbol or to Cash.
margin: Dollar value of margin required to maintain exposure.
interest: Interest accrued on yesterday's cash balance and settled today.
settled_cash: Cash settled due to holding positions from yesterday's close
through today's close.
contract_trades: Number of contracts traded, including any positions that
were liquidated fully or partially.
trade_value: Value of contracts traded, including any positions that
were liquidated fully or partially.
rolled_contracts: Number of contracts that were rolled out of today.
roll_price: Price at which rolled contracts were covered (NA if no contracts
were rolled).
commission: Commissions paid on today's trading, including any liquidated
and/or rolled contracts.
margin_call: Boolean indicating whether a margin call occurred today.
reduced_target_pos: Boolean indicating whether target positions could not be
executed due to insufficient margin, and were scaled back accordingly.
}
\description{
Quasi event-based simulation based on desired futures positions, raw
contract prices (ie not backadjusted) and roll dates determine by maximum
open interest.

The function is quite opinionated with respect to the data that it expects.
The idea is that user effort is spent in upstream data wrangling (the
\code{wrangle_contracts_on_oi}, \code{make_sim_prices_matrix}, and
\code{make_sim_weights_matrix} functions help with this), and the payoff is an
extremely efficient backtesting routine, enabling fast experimentation with
parameters such as \code{trade_buffer} and different commission models.

\strong{Assumptions and considerations}:
\itemize{
\item Won't allow a position that would be rejected by the broker given user-
supplied margin requirements. Positions that would be rejected are
automatically scaled back.
\item Similarly, force liquidates the portfolio to conform with margin
requirements (ie simulates a margin call). Contracts are liquidated in equal
proportion - which may not be how a broker actually performs this operation.
\item Trades only integer number of contracts.
\item User-specified leverage should be embedded in the target weights values.
\item Currently only fixed commission models are implemented.
\item Actual positions are determined using a trade buffer approach: when an
actual portfolio weight differs from its target weight by \code{trade_buffer} or
more, rebalance back to the target weight plus or minus the \code{trade_buffer}
depending on which side of the target weight the current weight lies.
\item Uses a user-supplied constant margin requirement for all assets in the
portfolio (TODO: support asset-wise margin requirements)
\item It is up to the user to align weights and prices prior to passing to this
function. Weights should be date-aligned with the prices at which you assume
you trade into them. This means that you will generally need to lag your
weights upstream.
\item Works on contract value (close * point value) and assumes all contracts are
in USD (TODO: support contracts denominated in different currencies)
}
}
\examples{
/dontrun{
# prices
futures <- readRDS(test_path("fixtures", "futures.rds"))
wrangled <- wrangle_contracts_on_oi(futures)
sim_prices <- make_sim_prices_matrix(wrangled)

# target weights
target_weights <- data.frame(
 date = wrangled$date,
 symbol = wrangled$symbol,
 target_weight = 5*rep(1./3, nrow(wrangled))
 ) \%>\%
 make_sim_weights_matrix()

 example interest rate data
 broker_spread <- 0.005
 rates <- data.frame(
  date = sort(unique(wrangled$date)),
  rate = rep((0.05 - broker_spread)/365, nrow(wrangled))
  ) \%>\%
  data.matrix()
  per_contract_commission <- c("ES" = 0.85, "GC" = 0.85, "ZB" = 0.85)
  margin <- 0.05
  initial_cash <- 1000000
  trade_buffer <- 0.2

  results <- fixed_commission_futs_backtest(
    prices = sim_prices,
    target_weights = target_weights,
    interest_rates = rates,
    trade_buffer = trade_buffer,
    initial_cash = initial_cash,
    margin = margin,
    capitalise_profits = TRUE,
    commission_fun = futs_per_contract_commission,
    per_contract_commission = per_contract_commission
 )
}
}
