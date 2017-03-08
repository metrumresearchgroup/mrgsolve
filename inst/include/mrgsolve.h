// Copyright (C) 2013 - 2017  Metrum Research Group, LLC
//
// This file is part of mrgsolve.
//
// mrgsolve is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// mrgsolve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.

/**
 * @file mrgsolve.h
 * 
 */

#include "RcppInclude.h"
#include <R_ext/Rdynload.h>

extern "C"{DL_FUNC tofun(SEXP a);}

// Send report to console when istate returns negative after dlsoda call
void neg_istate(int);

arma::mat MVGAUSS(Rcpp::NumericMatrix& OMEGA_, int n);

arma::mat MVGAUSS(arma::mat& OMEGA_,int n);

//! map key: string, value: integer
typedef std::map<std::string,int > si_map;

//! map key: string, value: double
typedef std::map<std::string,double > sd_map;

//! vector of strings
typedef std::vector<std::string> svec;

//! vector of integers
typedef std::vector<int> ivec;

//! map key: string, value: integer vector
typedef std::map<std::string, ivec> sivec_map;


template <class T>
void sort_unique(T& a) {
  std::sort(a.begin(), a.end());
  typename T::iterator last = std::unique(a.begin(), a.end());
  a.erase(last, a.end());
}

int find_position(const Rcpp::CharacterVector& what,  
                  const Rcpp::CharacterVector& table);

double digits(const double& a, const double& b);

void dcorr(const Rcpp::NumericMatrix& x);

Rcpp::NumericMatrix SUPERMATRIX(const Rcpp::List& a);

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

