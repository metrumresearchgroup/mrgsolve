
// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

//#include <Rcpp.h>
#include "RcppInclude.h"



// [[Rcpp::export]]
Rcpp::NumericMatrix EXPAND_EVENTS(Rcpp::IntegerVector idcol_,
                                  Rcpp::NumericMatrix events,
                                  Rcpp::NumericVector id) {
  
  int i,j,k;
  int crow = 0;
  
  int idcol = idcol_[0]-1;
  int ncol_new = events.ncol();
  
  Rcpp::List dimnames = events.attr("dimnames");
  Rcpp::CharacterVector names = dimnames[1];
  
  if(idcol < 0) {
    ncol_new = events.ncol() + 1;  
    names.push_back("ID");
    idcol = ncol_new-1;
    dimnames[1] = names;
  } 

  Rcpp::NumericMatrix ans(events.nrow()*id.size(),ncol_new);
  
  for(i=0; i < id.size(); ++i) {
    for(j=0; j < events.nrow(); ++j) {
      for(k=0; k < events.ncol(); ++k) {
        ans(crow,k) = events(j,k);
      }
      ans(crow,idcol) = id[i];
      ++crow;
    }
  }
  dimnames[0]  = Rcpp::CharacterVector(0);
  ans.attr("dimnames") = dimnames;
  return(ans);
}




typedef std::vector<std::string> svec;





