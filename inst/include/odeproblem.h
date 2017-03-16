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
 * @file odeproblem.h
 * 
 */

#ifndef ODEPROBLEM_H
#define ODEPROBLEM_H
#include <math.h>
#include <vector>
#include "odepack_dlsoda.h"
#include "mrgsolv.h"
#include "RcppInclude.h"
#include "datarecord.h"

// 
// resim functor comes from mrgsolv.h
// so it can get defined in the model
// 

class odeproblem;

struct databox {
  dvec ETA; ///< vector of ETA values
  dvec EPS; ///< vector of EPS values
  unsigned int newind; ///< new individual flag
  double time; ///< current simulation time
  int evid;  ///< event ID flag
  bool SYSTEMOFF; ///< flag to stop advancing system for current ID
  dvec mtime; ///< model time values
  double id;  ///< current ID
  double amt; ///< current dosing amount value
  short int cmt; ///< current compartment value
  int nid; ///< number of IDs in the data set
  int idn; ///< current ID number
  int nrow; ///< number of rows in output data set
  int rown; ///< current output row number
  bool CFONSTOP; ///< carry forward on stop indicator
  void* envir; ///< model environment
};

//! vector of <code>datarecord</code> objects for one <code>ID</code>
typedef std::vector<rec_ptr> reclist;

//! vector of <code>reclist</code> vectors comprising  a data set
typedef std::vector<reclist> recstack;

//! <code>$MAIN</code> function
typedef void (*init_func)(MRGSOLVE_INIT_SIGNATURE);

//! <code>$TABLE</code> function
typedef void (*table_func)(MRGSOLVE_TABLE_SIGNATURE);

//! <code>$ODE</code> function
typedef void (*deriv_func)(MRGSOLVE_ODE_SIGNATURE);

//! <code>$PREAMBLE</code> function
typedef void (*config_func)(MRGSOLVE_CONFIG_SIGNATURE);

//! function to hand off to <code>DLSODA</code>
typedef void main_deriv_func(int* neq, double* t,
                             double* y,double* ydot,
                             odeproblem* prob);

#define MRGSOLVE_GET_PRED_CL  (pred[0]) ///< map CL to pred position 0 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_VC  (pred[1]) ///< map VC to pred position 1 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_KA  (pred[2]) ///< map KA to pred position 2 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_Q   (pred[3]) ///< map Q to pred position 3 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_VP  (pred[4]) ///< map VP to pred position 4 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K10 (pred[0]/pred[1]) ///< rate constants for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K12 (pred[3]/pred[1]) ///< rate constants for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K21 (pred[3]/pred[4]) ///< rate constants for <code>$PKMODEL</code>


// deriv_func*  as_deriv_func( SEXP derivs);
// init_func*   as_init_func(  SEXP inits);
// table_func*  as_table_func( SEXP table);
// config_func* as_config_func(SEXP config);

extern "C"{DL_FUNC tofunptr(SEXP a);}

main_deriv_func main_derivs;
void neg_istate(int istate);

template<typename T,typename type2> void tofunptr(T b, type2 a) {
  b = reinterpret_cast<T>(R_ExternalPtrAddr(a));
}

void dosimeta(void*);
void dosimeps(void*);

class odeproblem : public odepack_dlsoda {

public:
  odeproblem(Rcpp::NumericVector param,
             Rcpp::NumericVector init,
             Rcpp::List funs,
             int n_capture_);
  
  virtual ~odeproblem();
  
  void advance(double tfrom, double tto);
  void call_derivs(int *neq, double *t, double *y, double *ydot);
  void init(int pos, double value){Init_value[pos] = value;}
  double init(int pos){return Init_value[pos];}
  
  void init_call(const double& time);
  void init_call_record(const double& time);

  void y_init(int pos, double value);
  void y_init(Rcpp::NumericVector x);
  void y_add(const unsigned int pos, const double& value);
  
  void table_call();
  void table_init_call();
  void config_call();
  
  void set_d(rec_ptr this_rec);
  
  void omega(Rcpp::NumericMatrix& x);
  void sigma(Rcpp::NumericMatrix& x);
  
  arma::mat mv_omega(int n);
  arma::mat mv_sigma(int n);

  void pass_envir(Rcpp::Environment* x){d.envir=reinterpret_cast<void*>(x);};
  
  bool CFONSTOP(){return d.CFONSTOP;}
  
  const double* param() const {return Param;}
  void param(int pos, double value) {Param[pos] = value;}
  
  void rate(unsigned int pos, double value) {R[pos] = value;}
  double rate(unsigned int pos) {return R[pos];}
  void rate0(unsigned int pos, double value) {R0[pos] = value;}
  double rate0(unsigned int pos){return R0[pos];}
  
  int rate_count(unsigned int pos){return infusion_count[pos];}
  void rate_add(unsigned int pos, const double& value);
  void rate_rm(unsigned int pos,  const double& value);
  void rate_bump(const unsigned int pos, const double& value);
  void rate_reset();
  
  void dur(unsigned int pos, double value) {D[pos] = value;}
  double dur(unsigned int pos){return D[pos];}
  
  void fbio(unsigned int pos, double value) {F.at(pos) = value;}
  double fbio(unsigned int pos) {return F.at(pos);}
  
  double alag(int cmt){return Alag.at(abs(cmt)-1);}
  
  void reset_newid(const double id_);
  
  void eta(int pos, double value) {d.ETA[pos] =value;}
  void eps(int pos, double value) {d.EPS[pos] = value;}
  bool systemoff(){return d.SYSTEMOFF;}
  
  void on(unsigned short int cmt);
  void off(unsigned short int cmt);
  
  int is_on(unsigned int eq_n){return On[eq_n];}
  
  void time(double time_){d.time = time_;}
  void newind(unsigned int newind_){d.newind = newind_;}
  unsigned int newind(){return d.newind;}

  void advan(int x);
  int  advan(){return Advan;}
  void advan2(const double& tfrom, const double& tto);
  void advan4(const double& tfrom, const double& tto);
  
  void neta(int n);
  void neps(int n);
  
  void nid(int n) {d.nid = n;}
  void nrow(int n) {d.nrow = n;}
  void idn(int n) {d.idn = n;}
  void rown(int n) {d.rown=n;}
  
  dvec& mtime(){return d.mtime;}
  
  dvec& get_capture() {return Capture;}
  double capture(int i) {return Capture[i];}
  
  void copy_parin(const Rcpp::List& parin);
  void copy_funs(const Rcpp::List& funs);


protected:
  
  double* Param; ///< model parameters
  dvec R0; ///< acutal current infusion rate
  std::vector<unsigned int> infusion_count; ///< number of active infusions
  dvec R; ///< receive user input for infusion rate
  dvec D; ///< receive user input for infusion duration
  dvec Init_value; ///< initial conditions
  dvec Init_dummy; ///< initial conditions for user input
  dvec F; ///< bioavability
  dvec Alag; ///< dosing lag time

  std::vector<int> On; ///< compartment on/off indicator
  databox d; ///< various data passed to model functions
  
  int Advan;  ///< simulation mode: 1/2/3/4 (PK models) or 13 (odes)
  dvec a;     ///< used for advan 1/2/3/4 calculations
  dvec alpha; ///< used for advan 1/2/3/4 calculation
  
  resim simeta;  ///< functor for resimulating etas
  resim simeps; ///< functor for resimulating epsilons

  arma::mat Omega; ///< variance/covariance matrix for between-subject variability
  arma::mat Sigma; ///< variance/covariance matrix for within-subject variability
    
  dvec pred; ///< brings clearances, volumes, and kas for advan 1/2/3/4 calculations
  dvec Capture; ///< captured data items
  
  deriv_func Derivs; ///< <code>$ODE</code> function
  init_func Inits; ///< <code>$MAIN</code> function
  table_func Table; ///< <code>$TABLE</code> function
  config_func Config; ///< <code>$PREAMBLE</code> function
  
  
};


/**
 * Calculate PK model polyexponentials.
 * 
 * 
 * 
 */
double PolyExp(const double& x,
               const double& dose,
               const double& rate,
               const double& xinf,
               const double& tau,
               const bool ss,
               const dvec& a,
               const dvec& alpha,
               const int n);

Rcpp::List TOUCH_FUNS(const Rcpp::NumericVector& lparam, 
                      const Rcpp::NumericVector& linit,
                      const Rcpp::CharacterVector& capture,
                      const Rcpp::List& funs);

// SEXP ODEPTR(const Rcpp::NumericVector& lparam, 
//                               const Rcpp::NumericVector& linit,
//                               int Neta, int Neps,
//                               const Rcpp::CharacterVector& capture,
//                               const Rcpp::List& funs);

#endif
