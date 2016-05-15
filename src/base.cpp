#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector flag_window_cpp(NumericVector &centers,
                              const int &window,
                              const int &len,
                              const bool &flag_center) {

  LogicalVector flags(len);
  for(int h=0; h < centers.size(); h++){
    int center = centers[h] - 1; // shift for R index
    for(int i=0; i < len; i++){
      if(center - window <= i && i <= center + window){
        flags[i] = flags[i] || TRUE;
      }else{
        flags[i] = flags[i] || FALSE;
      }
    }
  }
  for(int h=0; h < centers.size(); h++){
    int j = centers[h] - 1; // shift for R index
    flags[j] = flag_center;
  }
  return flags;
}

/*** R
microbenchmark::microbenchmark(
  flag_window_cpp(c(5, 10), 3, 20, FALSE)
)
*/
