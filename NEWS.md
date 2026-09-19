# rsims 1.2.08

## Short borrow costs can vary by day

`short_borrow_costs` now accepts a date-keyed matrix or data frame as well as a
named vector, mirroring `interest_rates`. A named vector still means one rate
for the whole simulation and is unchanged.

Rates are annualised **decimals**, so `c("TLT" = 0.0025)` is 0.25% a year. They
represent the borrow **fee**, the amount you pay. The rebate is not modelled
separately, because short sale proceeds are held as collateral and earn nothing
— which is what a retail-sized account actually gets. The docstring previously
said "as percent" while its own example gave a decimal.

## Rate series have their dates validated

`interest_rates` and a date-keyed `short_borrow_costs` are read by row position
inside the simulation loop, so a series whose dates do not line up with the
price matrix is applied to the wrong days. Nothing used to complain. In one
case a rates matrix built from an unfiltered price history, against weights
that had lost sixty days to a rolling volatility window, applied every interest
rate sixty trading days late for a twenty-year simulation.

Both are now checked once before the loop and error if they disagree.

**This can turn a silently wrong run into an error.** If a simulation that used
to run now stops here, it was using the wrong days' rates.

## Two fixes to named-vector borrow costs

Both change results, and in both the previous behaviour was wrong.

- The vector was sorted alphabetically by name, then multiplied elementwise
  against `share_pos`, which is in `target_weights` column order. Those agree
  only when the columns happen to be alphabetical, and `pivot_wider()` names
  columns in order of first appearance, so any ticker absent from the first
  date lands out of order and every asset after it receives a different
  ticker's rate. On a large universe this overstated borrow several-fold. The
  vector is now placed into ticker order by name, and sorting the price,
  weight and unadjusted-price matrices identically is no longer necessary.

- A vector shorter than the number of assets was recycled, so naming one
  instrument applied its rate to all of them. Tickers not named now get zero.

An **unnamed** vector used to be silently discarded, charging zero borrow for
the entire run. It now errors.

### What is unchanged

Results are identical when the vector names every ticker and the
`target_weights` columns are in alphabetical order. They are also identical
whenever the misassignment landed on a long position, since borrow is only
charged on shorts.
