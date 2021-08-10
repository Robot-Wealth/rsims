#include <Rcpp.h>
using namespace Rcpp;

//' Calculate target positions from theoretical weights and trade buffer parameter
//' TODO: remove current weights calculation and instead pass as argument
//' @export
// [[Rcpp::export]]
NumericVector positionsFromNoTradeBuffer(NumericVector current_positions, NumericVector current_prices, NumericVector current_theo_weights, double cap_equity, int num_assets, double trade_buffer) {
  NumericVector current_weights(num_assets);
  NumericVector target_positions(num_assets);
  std::copy( current_positions.begin(), current_positions.end(), target_positions.begin() ) ;

  int j = 0;
  for(j = 0; j < num_assets; j++)
  {
    current_weights[j] = current_positions[j]*current_prices[j]/cap_equity;
  }

  j = 0;
  for(j = 0; j < num_assets; j++)
  {
    //Rprintf(\"%i %f %f \\n\", j, current_theo_weights[j], current_weights[j]);
    if(R_IsNA(current_theo_weights[j]) | current_theo_weights[j] == 0)
      target_positions[j] = 0;
    else if(current_weights[j] < current_theo_weights[j] - trade_buffer)
      target_positions[j] = (current_theo_weights[j] - trade_buffer)*cap_equity/current_prices[j];
    else if(current_weights[j] > current_theo_weights[j] + trade_buffer)
      target_positions[j] = (current_theo_weights[j] + trade_buffer)*cap_equity/current_prices[j];
  }

  return target_positions;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
positionsFromNoTradeBuffer(rep(0, 3), c(8, 11, 3), c(0.5, 0.3, 0.2), 1000, 3, 0.01)
*/
