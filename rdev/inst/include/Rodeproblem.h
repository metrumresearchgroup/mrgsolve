// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


#ifndef RODEPROBLEM_H
#define RODEPROBLEM_H
#include <math.h>
#include <memory>
#include <iostream>
#include "RcppInclude.h"

deriv_func * as_deriv_func(SEXP derivs);
init_func * as_init_func(SEXP inits);
table_func * as_table_func(SEXP table);
extern "C"{DL_FUNC tofunptr(SEXP a);}


class Rodeproblem : public odeproblem {

 public:
  Rodeproblem(int npar_, int neq_);
  void advance(double& tfrom, double& tto);
  Rodeproblem(Rcpp::NumericVector param, Rcpp::NumericVector init);
  void init_fun(SEXP ifun);
  void table_fun(SEXP tfun);
  void deriv_fun(SEXP dfun);
  void copy_parin(Rcpp::List parin);
  void copy_funs(Rcpp::List funs);

};

#endif


