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
     if (R_IsNA(theo_weight) || theo_weight == 0.0) {
       target_positions[j] = 0.0;
       continue;
     }

     // Compute buffer zone edges
     double lower_bound = theo_weight * (1.0 - trade_buffer);
     double upper_bound = theo_weight * (1.0 + trade_buffer);
     if (lower_bound > upper_bound) std::swap(lower_bound, upper_bound);

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
