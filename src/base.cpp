#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector get_colflag_cpp(NumericVector &indices,
                              const int &window, const int &len) {

  LogicalVector flags(len);
  for(int h=0; h < indices.size(); h++){
    int index = indices[h] - 1;
    for(int i=0; i < len; i++){
      if(index - window <= i && i <= index + window && i != index){
        flags[i] = flags[i] || TRUE;
      }else{
        flags[i] = flags[i] || FALSE;
      }
    }
  }
  return flags;
}

/*** R
microbenchmark::microbenchmark(
  get_colindex(c(5, 10), 3, 20),
  get_colflag_cpp(c(5, 10), 3, 20)
)
*/
