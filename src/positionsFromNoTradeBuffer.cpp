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

     // If trade_buffer is zero, always rebalance to theo_weight
     if (trade_buffer == 0.0) {
       target_positions[j] = theo_weight * cap_equity / current_prices[j];
       continue;
     }

     // Calculate buffer size with minimum absolute width to avoid pathological
     // narrow buffers for small positions
     const double MIN_ABS_BUFFER = 0.005;  // 0.5% minimum buffer width
     double prop_buffer = std::abs(theo_weight) * trade_buffer;
     double buffer_size = std::max(prop_buffer, MIN_ABS_BUFFER);

     // Calculate buffer-aware boundaries
     double lower_bound = theo_weight - buffer_size/2;
     double upper_bound = theo_weight + buffer_size/2;

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
