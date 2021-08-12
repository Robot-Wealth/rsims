# rsims

Tools for financial simulation

## Install and load

The easiest way to install and load `rsims` is using `pacman::p_load_current_gh` which wraps `devtools::install_github` and `require`:

`pacman::p_load_current_gh("Robot-Wealth/rsims", dependencies = TRUE)`

## Usage

The key function is `cash_backtest`, an optimised event-driven backtesting function.

`cash_backtest` simulates trading into a set of weights (calculated upstream) subject to transaction cost and other constraints.

It expects matrixes for `prices` and `theo_weights`, both with a timestamp as the first column and of the same dimensions. Further details in the function documentation.


```R
library(rsims)
results <- cash_backtest(prices, theo_weights, trade_buffer = 0., initial_cash = 10000, commission_pct = 0, capitalise_profits = FALSE)
```

## Approach to calculating position deltas

Currently there is one module implemented for calculating position deltas: the "no-trade region" approach. See the examples for dteails on how this approach works, where it is reasonable, where it shouldn't be used. 

The intent is to implement other approaches in the future, such as numerical optimisation of the return-risk-cost problem, subject to constraints.

## Cost model

Currently `rsims` implements a simplified "fixed percent of traded value" cost model. For some applications, market impact, spread, and commission might be reasonably represented by such a model. No attempt is made (yet) to explicitly account for these costs separately. Borrow, margin and funding costs are not yet implemented.

## Examples and vignettes

### Wrangling data for input to `cash_backtest`

### Finding the historically sharpe-optimal trade buffer parameter
Here's a good derivation from @macrocephalopod on Twitter: https://twitter.com/macrocephalopod/status/1373236950728052736

This leads to simple heuristic trading rule - which is theoretically optimal if your costs are linear and you don't mind holding exposures within a certain range.

It's not a good approach when your trading costs aren't approximately linear, for example, small trading with a fixed minimum commission per trade.

### Lagging signal with respect to prices (lagging theo weights is equivalent)
