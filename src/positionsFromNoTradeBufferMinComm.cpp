#include <Rcpp.h>
using namespace Rcpp;

//' Calculate target positions with a no-trade buffer and min commission logic
 //' Supports both long and short theoretical weights
 //'
 //' If the current weight is within [theo_weight * (1 - buffer), theo_weight * (1 + buffer)],
 //' no trade is made. If it's outside, the position is adjusted fully to theo_weight.
 //'
 //' @export
 // [[Rcpp::export]]
 NumericVector positionsFromNoTradeBufferMinComm(NumericVector current_positions,
                                                 NumericVector current_prices,
                                                 NumericVector current_theo_weights,
                                                 double cap_equity,
                                                 double trade_buffer) {
   int num_assets = current_positions.size();
   NumericVector current_weights(num_assets);
   NumericVector target_positions = clone(current_positions); // Start with current positions

   // Calculate current weights
   for (int j = 0; j < num_assets; j++) {
     current_weights[j] = current_positions[j] * current_prices[j] / cap_equity;
   }

   for (int j = 0; j < num_assets; j++) {
     double theo_weight = current_theo_weights[j];

     // Exit position entirely if NA or zero target weight
     // No buffer when target is zero - always rebalance to flat
     if (R_IsNA(theo_weight) || theo_weight == 0.0) {
       target_positions[j] = 0.0;
       continue;
     }

     // If trade_buffer is zero, always rebalance to theo_weight
     if (trade_buffer == 0.0) {
       target_positions[j] = theo_weight * cap_equity / current_prices[j];
       continue;
     }

     // Calculate buffer bounds using absolute deviation approach
     // trade_buffer represents total width, so half-width on each side
     double lower_bound = theo_weight - trade_buffer/2;
     double upper_bound = theo_weight + trade_buffer/2;

     // Clip bounds at zero to prevent holding positions with wrong sign
     if (theo_weight > 0) {
       // Long target: don't allow buffer to go negative
       lower_bound = std::max(0.0, lower_bound);
     } else {
       // Short target: don't allow buffer to go positive
       upper_bound = std::min(0.0, upper_bound);
     }

     // Trade to full target only if current weight is outside the buffer zone
     if (current_weights[j] < lower_bound || current_weights[j] > upper_bound) {
       target_positions[j] = theo_weight * cap_equity / current_prices[j];
     }
     // else: do nothing, keep current position
   }

   return target_positions;
 }


/*** R
positionsFromNoTradeBufferMinComm(rep(0, 3), c(8, 11, 3), c(0.5, 0.3, 0.2), 1000, 3, 0.01)
*/
