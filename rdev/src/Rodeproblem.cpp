// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

/**
   @file Rodeproblem.cpp
   @brief Source code for Rodeproblem class
*/
#include "RcppInclude.h"
#include "odeproblem.h"
#include "Rodeproblem.h"
#include <string>



init_func* as_init_func(SEXP inits) {
  return(reinterpret_cast<init_func *>(tofunptr(inits))); // Known to generate compiler warning
}

deriv_func* as_deriv_func(SEXP derivs) {
    return(reinterpret_cast<deriv_func *>(tofunptr(derivs))); // Known to generate compiler warning
}

table_func* as_table_func(SEXP table) {
  return(reinterpret_cast<table_func*>(tofunptr(table))); // Known to generate compiler warning
}


Rodeproblem::Rodeproblem(int npar_,int neq_) : odeproblem(npar_, neq_) {}

Rodeproblem::Rodeproblem(Rcpp::NumericVector param,
			 Rcpp::NumericVector init):odeproblem(param.size(),init.size()) {
  size_t i=0;

  for(i=0; i < param.size(); i++) Param[i] =       double(param[i]);
  for(i=0; i < init.size(); i++)  Init_value[i] =  double(init[i]);
}


void Rodeproblem::init_fun(SEXP xifun) {
  Inits = as_init_func(xifun);
}
void Rodeproblem::table_fun(SEXP xtfun) {
  Table = as_table_func(xtfun);
}
void Rodeproblem::deriv_fun(SEXP xdfun) {
  Derivs = as_deriv_func(xdfun);
}



void Rodeproblem::advance(double& tfrom, double& tto) {

  if(Neq <= 0) return;

  if(Advan==2) {
    odeproblem* prob = this;
    prob->advan2(tfrom,tto);
    return;
  }

  if(Advan==4) {
    odeproblem* prob = this;
    prob->advan4(tfrom,tto);
    return;
  }



  F77_CALL(dlsoda) (&main_derivs,
		    &Neq,
		    this->y(),
		    &tfrom,
		    &tto,
		    &xitol,
		    &xrtol,
		    &xatol,
		    &xitask,
		    &xistate,
		    &xiopt,
		    this ->rwork(),
		    &xlrwork,
		    this->iwork(),
		    &xliwork,
		    &Neq,
		    &xjt,
		    (odeproblem *) this
		    );

  // Print a more-informative message when istate is negative
  if(this->istate() < 0) neg_istate(this->istate());

  main_derivs(&Neq, &tto,Y, Ydot, this);

}

void Rodeproblem::copy_parin(Rcpp::List parin) {
  this->tol(Rcpp::as<double>(parin["atol"]),Rcpp::as<double>(parin["rtol"]));
  this->hmax(Rcpp::as<double>(parin["hmax"]));
  this->maxsteps(Rcpp::as<double>  (parin["maxsteps"]));
  this->ixpr(Rcpp::as<double>  (parin["ixpr"]));
  this->mxhnil(Rcpp::as<double>  (parin["mxhnil"]));
}
void Rodeproblem::copy_funs(Rcpp::List funs) {
  this->Rodeproblem::init_fun(funs["init"]);
  this->Rodeproblem::table_fun(funs["table"]);
  this->Rodeproblem::deriv_fun(funs["deriv"]);
}




// void param_copy_from_data(Rodeproblem* prob,
// 			  std::vector<std::string>& copy_names,
// 			  //std::vector<std::string>& data_names,
// 			  std::map<std::string,int>& datamap,
// 			  int data_index,
// 			  Rcpp::NumericMatrix data) {
//   unsigned int k;
//   // Parameters coming in from data set:
//   for(k=0; k < copy_names.size(); k++) {
//     prob->param(copy_names[k],data(data_index,datamap[copy_names[k]]));
//   }
//   // data set items:
//   //for(k=0; k < data_names.size(); k++) {
//   //   prob->dataitems(data_names[k],data(data_index,datamap[data_names[k]]));
//   // }
// }



