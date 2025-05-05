#include <Rcpp.h>
using namespace Rcpp;

//' Calculate target positions with a no-trade buffer (supports long and short)
 //' @export
 // [[Rcpp::export]]
 NumericVector positionsFromNoTradeBuffer(NumericVector current_positions,
                                          NumericVector current_prices,
                                          NumericVector current_theo_weights,
                                          double cap_equity,
                                          double trade_buffer) {
   int num_assets = current_positions.size();
   NumericVector current_weights(num_assets);
   NumericVector target_positions = clone(current_positions); // start with current positions

   // Calculate current portfolio weights
   for (int j = 0; j < num_assets; j++) {
     current_weights[j] = current_positions[j] * current_prices[j] / cap_equity;
   }

   // Apply no-trade buffer logic
   for (int j = 0; j < num_assets; j++) {
     double theo_weight = current_theo_weights[j];

     if (R_IsNA(theo_weight) || theo_weight == 0.0) {
       target_positions[j] = 0.0;
       continue;
     }

     // Calculate buffer-aware boundaries
     double lower_bound = theo_weight * (1.0 - trade_buffer);
     double upper_bound = theo_weight * (1.0 + trade_buffer);

     // Ensure proper order regardless of sign
     if (lower_bound > upper_bound) std::swap(lower_bound, upper_bound);

     // Compare and adjust position only if outside bounds
     if (current_weights[j] < lower_bound) {
       target_positions[j] = lower_bound * cap_equity / current_prices[j];
     } else if (current_weights[j] > upper_bound) {
       target_positions[j] = upper_bound * cap_equity / current_prices[j];
     }
   }

   return target_positions;
 }


/*** R
positionsFromNoTradeBuffer(rep(0, 3), c(8, 11, 3), c(0.5, 0.3, 0.2), 1000, 3, 0.01)
*/
