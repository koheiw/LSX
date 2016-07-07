#include <Rcpp.h>
#include <vector>
// [[Rcpp::plugins(cpp11)]]
#include <unordered_set>

using namespace Rcpp;

void flag_collocates_cpp(const Rcpp::CharacterVector &text,
                         const std::unordered_set<Rcpp::String> set_targets,
                         const int &window,
                         std::vector<bool> &flags_target,
                         std::vector<bool> &flags_col,
                         int &g){

    int len = text.size();
    for(int i=0; i < len; i++){
      Rcpp::String token = text[i];
      bool is_in = set_targets.find(token) != set_targets.end();

      if(is_in){
        //Rcout << "Match " << token << " " << i<< "\n";
        int j_int = std::max(0, i - window);
        int j_lim = std::min(i + window + 1, len);
        for(int j = j_int; j < j_lim; j++){
          //Rcout << "Flag " << token << " " << j << "\n";
          flags_col[g + j] = flags_col[g + j] || TRUE;
        }
        flags_target[g + i] = TRUE;
      }else{
        flags_target[g + i] = FALSE;
      }
    }
    g += len; // last global index of this text
  }

// [[Rcpp::export]]
Rcpp::List flag_collocates_cppl(List texts,
                          const Rcpp::CharacterVector &targets,
                          const int &window,
                          const int &n) {

  int g = 0; // global index;
  int len = texts.size();
  //std::unordered_set<std::string> set_targets (targets.begin(), targets.end());
  std::unordered_set<Rcpp::String> set_targets;
  for (int g = 0; g < targets.size(); g++){
    set_targets.insert(targets[g]);
  }
  std::vector<bool> flags_target(n);
  std::vector<bool> flags_col(n);
  for (int h = 0; h < len; h++){
    flag_collocates_cpp(texts[h], set_targets, window, flags_target, flags_col, g);
  }

  return List::create(Rcpp::Named("target")=flags_target,
                      Rcpp::Named("col")=flags_col);
}


/*** R
#flag_collocates_cppl(list(LETTERS, letters), c('E', 'G', 'Z', 'a', 'k'), 3, 52)
*/
