// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#include "RcppInclude.h"
#include <memory>

// Send report to console when istate returns negative after dlsoda call
void neg_istate(int  );

struct var  {
  int NEWIND;
  int EVID;
};

#include <R_ext/Rdynload.h>
extern "C"{DL_FUNC tofun(SEXP a);}
// Sort an integer vector
Rcpp::IntegerVector stl_sort(Rcpp::IntegerVector x);


void Talk(std::string label, double value);
void Talk(std::string label);
void Talk(std::string label, int value);
arma::mat MVGAUSS(Rcpp::NumericMatrix OMEGA_, int n, int seed);
Rcpp::List SIMRE(int n1, Rcpp::NumericMatrix OMEGA, int n2, Rcpp::NumericMatrix SIGMA, int seed);


typedef std::map<std::string,int > si_map;
typedef std::map<std::string,double > sd_map;
typedef std::vector<std::string> svec;
typedef std::vector<int> ivec;
typedef std::map<std::string, ivec> sivec_map;



// void asSDmap(sd_map& out, Rcpp::List x);
// 
// template <class type1, class type2>
//   void asSImap(type1& a, type2 b) {
//   for(int i=0; i < b.size(); ++i) {
//     a[std::string(b[i])] = i;
//   }
// }

template <class T>
void sort_unique(T& a) {
  std::sort(a.begin(), a.end());
  typename T::iterator last = std::unique(a.begin(), a.end());
  a.erase(last, a.end());
}

int find_position(std::string what, const svec& table);

double digits(const double& a, const double& b);

Rcpp::CharacterVector colnames(Rcpp::NumericMatrix x);

void decorr(Rcpp::NumericMatrix x);

//Rcpp::NumericMatrix supermatrix(Rcpp::List mats);

typedef std::vector<Rcpp::NumericVector> mvec;

Rcpp::NumericMatrix SUPERMATRIX(Rcpp::List a);

void match_both(svec a, svec b, ivec& ai, ivec& bi);
void match_one(svec a, svec b, ivec& ret);
Rcpp::List map_data_set(Rcpp::NumericMatrix data);
Rcpp::List get_tokens(Rcpp::CharacterVector code);
Rcpp::List get_tokens_sep(Rcpp::CharacterVector code);
//Rcpp::List tokens(Rcpp::CharacterVector x, Rcpp::CharacterVector sep_);

void set_omega(SEXP loc, Rcpp::NumericMatrix& omega_);

Rcpp::List TOUCH_FUNS(Rcpp::NumericVector lparam, 
                      Rcpp::NumericVector linit,
                      Rcpp::CharacterVector capture,
                      Rcpp::List funs);
