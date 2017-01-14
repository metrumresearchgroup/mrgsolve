// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#include "RcppInclude.h"


// Send report to console when istate returns negative after dlsoda call
void neg_istate(int  );

#include <R_ext/Rdynload.h>
extern "C"{DL_FUNC tofun(SEXP a);}

arma::mat MVGAUSS(Rcpp::NumericMatrix& OMEGA_, int n);
arma::mat MVGAUSS(arma::mat& OMEGA_,int n);
Rcpp::List SIMRE(int n1, Rcpp::NumericMatrix& OMEGA, int n2, Rcpp::NumericMatrix& SIGMA, int seed);

typedef std::map<std::string,int > si_map;
typedef std::map<std::string,double > sd_map;
typedef std::vector<std::string> svec;
typedef std::vector<int> ivec;
typedef std::map<std::string, ivec> sivec_map;


template <class T>
void sort_unique(T& a) {
  std::sort(a.begin(), a.end());
  typename T::iterator last = std::unique(a.begin(), a.end());
  a.erase(last, a.end());
}

int find_position(const Rcpp::CharacterVector& what,  const Rcpp::CharacterVector& table);

double digits(const double& a, const double& b);

void decorr(const Rcpp::NumericMatrix& x);

Rcpp::NumericMatrix SUPERMATRIX(const Rcpp::List& a);
//void match_both(svec a, svec b, ivec& ai, ivec& bi);
void from_to(const Rcpp::CharacterVector& a, 
             const Rcpp::CharacterVector& b, 
             Rcpp::IntegerVector& ai,
             Rcpp::IntegerVector& bi);

Rcpp::List get_tokens(const Rcpp::CharacterVector& code);

void set_omega(SEXP loc, Rcpp::NumericMatrix& omega_);

Rcpp::NumericMatrix EXPAND_EVENTS(const Rcpp::IntegerVector& idcol_,
                                  const Rcpp::NumericMatrix& events,
                                  const Rcpp::NumericVector& id); 

Rcpp::NumericMatrix recdata(Rcpp::NumericMatrix& dose,
                            Rcpp::NumericMatrix& obs,
                            Rcpp::IntegerVector& cols,
                            const int n_out_col,const int n_out_row,
                            const Rcpp::NumericVector& addl_,
                            const Rcpp::NumericVector& ii_,
                            const int nid, const int ntime,
                            const int namt, const int nevid, 
                            const int ncmt, const int nrate);
