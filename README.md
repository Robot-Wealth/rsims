# rsims

Tools for financial simulation

## Description

Efficient quasi-event-driven backtesting functions with opinionated input requirements.

Designed according to the following philosophy:

-   Prioritise simulation efficiency
-   Function inputs should be close to the natural outputs of the quant research process: dataframes/matrixes of prices and target weights.
-   Include helper functions for efficiently wrangling these research outputs into formats suitable for fast backtesting

## Install and load

The easiest way to install and load `rsims` is using `pacman::p_load_current_gh` which wraps `devtools::install_github` and `require`:

`pacman::p_load_current_gh("Robot-Wealth/rsims", dependencies = TRUE)`

## Usage

Depends on application:

-   `min_commission_backtest`: suitable for backtesting under the assumption of minimum commission, for instance many equities brokers.
-   `fixed_commission_backtest`: suitable for backtesting under the assumption of linear (fixed percentage) commission, for instance many crypto exchanges.
-   `fixed_commission_futs_backtest`: suitable for backtesting expiring products under the assumption of linear (fixed percentage) commission, for instance CME futures.

These functions are optimised for efficiency. They simulate trading into a set of weights (calculated upstream) subject to transaction cost and other constraints. They expect matrixes for prices and target weights.

Example usage:

``` r
library(rsims)
results <- min_commission_backtest(prices, theo_weights, trade_buffer = 0., initial_cash = 10000, commission_pct = 0, capitalise_profits = FALSE)
```

Further details in the respective function documentation.

Examples of wrangling data for input to these functions can be found in the vignettes.

## Approach to calculating position deltas

Position deltas are calculated using the trade buffer approach. Positions are rebalanced once they deviate from their target by more than a user-supplied `trade_buffer` percentage of the target weight. Rebalancing happens slightly differently depending on the commission model used:  

- for minimum commission backtesting, rebalance back to the target weight  
- for fixed commission backtesting, rebalance back to the target weight plus/minus the trade buffer  

These are heuristic rules that are theoretically optimal give the different cost models. [Here's a good derivation from \@macrocephalopod on Twitter.](https://twitter.com/macrocephalopod/status/1373236950728052736)

Other approaches to trade determination may be implemented in the future.

## Cost model

Currently `rsims` implements simplified cost models that only include commission, borrow, and funding costs.  

For some applications, the various costs (market impact, spread, and commission) might be reasonably represented by such a model. No attempt is made (yet) to explicitly account for these costs separately. 

## Examples and vignettes

TODO:

- Backtesting ETF strategies with `min_commission_backtest`  
- Finding the historically sharpe-optimal trade buffer parameter for a crypto momentum strategy with `fixed_commission_backtest`  
- Backtesting CME futures with `fixed_commission_futs_backtest`  
