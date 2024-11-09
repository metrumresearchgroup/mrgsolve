// Copyright (C) 2013 - 2024  Metrum Research Group
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
#include <deque>
#include "RcppInclude.h"
#include "mrgsolv.h"
#include "datarecord.h"

// 
// resim functor comes from mrgsolv.h
// so it can get defined in the model
// 

class odeproblem;

//! vector of <code>datarecord</code> objects for one <code>ID</code>
typedef std::deque<rec_ptr> reclist;

//! vector of <code>reclist</code> vectors comprising  a data set
typedef std::vector<reclist> recstack;

//! <code>$MAIN</code> function
typedef void (*init_func)(MRGSOLVE_INIT_SIGNATURE);

//! <code>$TABLE</code> function
typedef void (*table_func)(MRGSOLVE_TABLE_SIGNATURE);

//! <code>$EVENT</code> function
typedef void (*event_func)(MRGSOLVE_EVENT_SIGNATURE);

//! <code>$ODE</code> function
typedef void (*deriv_func)(MRGSOLVE_ODE_SIGNATURE);

//! <code>$PREAMBLE</code> function
typedef void (*config_func)(MRGSOLVE_CONFIG_SIGNATURE);

//! function to hand off to <code>DLSODA</code>
//typedef void main_deriv_func(int* neq, double* t,double* y,double* ydot, void* prob);
typedef void (*LSODA_ODE_SYSTEM_TYPE)(double t, double *y, double *dydt, 
              odeproblem* _data);
typedef void main_deriv_func(int* neq, double* t,double* y,double* ydot);
//typedef void (*ODE_FUNC)(double t, double *y, double *dydt, double*);
typedef void (*MRGSOLVE_ODE_FUNC)(int neq, double* t, double* y, double* ydot, std::vector<double>& param);


#define MRGSOLVE_GET_PRED_CL  (pred[0]) ///< map CL to pred position 0 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_VC  (pred[1]) ///< map VC to pred position 1 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_KA  (pred[2]) ///< map KA to pred position 2 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_Q   (pred[3]) ///< map Q to pred position 3 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_VP  (pred[4]) ///< map VP to pred position 4 for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K10 (pred[0]/pred[1]) ///< rate constants for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K12 (pred[3]/pred[1]) ///< rate constants for <code>$PKMODEL</code>
#define MRGSOLVE_GET_PRED_K21 (pred[3]/pred[4]) ///< rate constants for <code>$PKMODEL</code>

extern "C"{DL_FUNC tofunptr(SEXP a);}

main_deriv_func main_derivs;
void neg_istate(int istate);

template<typename T,typename type2> void tofunptr(T b, type2 a) {
  b = reinterpret_cast<T>(R_ExternalPtrAddr(a));
}

void dosimeta(void*);
void dosimeps(void*);


/**
 * 
 * 
 * 
 */
class odeproblem {

public:
  odeproblem(Rcpp::List param,Rcpp::NumericVector init, 
             Rcpp::List funs,
             int n_capture_);

  ~odeproblem(){};

  void do_init_calc(bool answer) {Do_Init_Calc = answer;}
  void advance(double tfrom, double tto, LSODA& solver);
  void call_derivs(double *t, double *y, double *ydot);
  void init(int pos, double value){Init_value[pos] = value;}
  double init(int pos){return Init_value[pos];}
  
  void init_call(const double& time);
  void init_call_record(const double& time);
  void init_derivs(double time);

  void y_init(int pos, double value);
  void y_init(Rcpp::NumericVector init);
  void y_add(const unsigned int pos, const double& value);
  
  void table_call();
  void table_init_call();
  void event_call();
  void config_call();
  
  void set_d(rec_ptr this_rec);

  void omega(const Rcpp::S4& mod);
  void sigma(const Rcpp::S4& mod);
  void copy_sigma_diagonals();

  arma::mat mv_omega(int n);
  arma::mat mv_sigma(int n);

  void pass_envir(Rcpp::Environment* x){d.envir=reinterpret_cast<void*>(x);};
  
  bool CFONSTOP(){return d.CFONSTOP;}
  
  const std::vector<double>& param() {return Param;}
  void param(int pos, double value) {Param[pos] = value;}
  
  void rate(unsigned int pos, double value) {R[pos] = value;}
  double rate(unsigned int pos) {return R[pos];}
  void rate0(unsigned int pos, double value) {R0[pos] = value;}
  double rate0(unsigned int pos){return R0[pos];}
  
  int rate_count(unsigned int pos){return infusion_count[pos];}
  void rate_add(unsigned int pos, const double& value);
  void rate_rm(unsigned int pos,  const double& value);
  void rate_bump(const unsigned int pos, const double& value);
  void rate_main(rec_ptr rec);
  void rate_reset();
  
  void dur(unsigned int pos, double value) {D[pos] = value;}
  double dur(unsigned int pos){return D[pos];}
  
  void reset_newid(const double id_);
  
  void eta(int pos, double value) {d.ETA[pos] = value;}
  void eps(int pos, double value) {d.EPS[pos] = value;}
  unsigned short int systemoff(){return d.SYSTEMOFF;}
  
  void on(unsigned short int cmt);
  void off(unsigned short int cmt);
  
  int is_on(unsigned int eq_n){return On[eq_n];}
  
  void fbio(unsigned int pos, double value) {F.at(pos) = value;}
  double fbio(unsigned int pos);
  double alag(int cmt);

  void time(double time_){d.time = time_;}///< sets current simulation time
  void timelast(double time_) {d.timelast=time_;}///< sets last time for current id 
  void newind(unsigned int newind_){d.newind = newind_;}
  unsigned int newind(){return d.newind;}

  void advan(int x);
  int  advan(){return Advan;}
  void advan2(const double& tfrom, const double& tto);
  void advan4(const double& tfrom, const double& tto);
  
  void set_eta();
  void set_eps();
  int neta(){return Omega.n_rows;}
  int neps(){return Sigma.n_rows;}
  
  void nid(int n) {d.nid = n;}///< sets the number of IDs
  void nrow(int n) {d.nrow = n;}///< sets the number of data set rows
  void idn(int n) {d.idn = n;}///< sets the current ID number
  void rown(int n) {d.rown=n;}///< sets the current data set row number
  void reclast(bool value) {d.reclast=value;}///< sets flag for last record
  
  dvec& get_capture() {return Capture;}
  double capture(int i) {return Capture[i];}
  
  /// copies items passed in through parin into the odeproblem object
  void copy_parin(const Rcpp::List& parin, const Rcpp::S4& mod);
  void copy_funs(const Rcpp::List& funs);
  
  bool any_mtime() {return d.mevector.size() > 0;}
  std::vector<mrgsolve::evdata> mtimes(){return d.mevector;}
  void clear_mtime(){d.mevector.clear();}
  void    y(const int pos, const double value){Y[pos] = value;}
  double  y(const int pos){return Y[pos];}
  int  istate(){return Istate;}
  void istate(int value){Istate = value;}
  void lsoda_init(){Istate=1;}
  /// returns the number of parameters
  int npar() {return Npar;}
  /// returns the number of state variables
  int neq() {return Neq;}
  /// sets the absolute and relative tolerances
  void tol(double atol, double rtol);
  

  std::vector<double> Y; ///< compartment amounts
  std::vector<double> Ydot;  ///< dxdt values
  std::vector<double> Yout; ///< used to hold Y values during solving
  std::vector<double> Param; ///< parameter vector
  std::vector<double> Capture; ///< captured data items
  double Atol; ///< absolute tolerance used by ODE solver
  double Rtol; ///< relative tolerance used by ODE solver
  double ssAtol; ///< absolute tolerance when finding steady state
  double ssRtol; //<  relative tolerance when finding steady state
  int Npar; ///< number of parameters
  int Neq; ///< number of equations
  int Istate; ///< istate value
  bool ss_fixed; ///< If true, then no warning is issued if SS not reached in ss_n doses
  int ss_n; ///< Max number of doses during SS advance before warning is issued
  bool ss_flag; ///< flag indicating when the system is advancing to SS
  std::vector<int> Ss_cmt; ///< vector of compartments to consider for SS

  std::vector<double> R0; ///< acutal current infusion rate
  std::vector<unsigned int> infusion_count; ///< number of active infusions
  std::vector<double> R; ///< receive user input for infusion rate
  std::vector<double> D; ///< receive user input for infusion duration
  std::vector<double> Init_value; ///< initial conditions
  std::vector<double> Init_dummy; ///< initial conditions for user input
  std::vector<double> F; ///< bioavability
  std::vector<double> Alag; ///< dosing lag time

  std::vector<int> On; ///< compartment on/off indicator
  databox d; ///< various data passed to model functions
  
  int Advan;  ///< simulation mode: 1/2/3/4 (PK models) or 13 (odes)
  std::vector<double> a;     ///< used for advan 1/2/3/4 calculations
  std::vector<double> alpha; ///< used for advan 1/2/3/4 calculation
  
  mrgsolve::resim simeta;///< functor for resimulating etas
  mrgsolve::resim simeps;///< functor for resimulating epsilons

  arma::mat Omega;///< variance/covariance matrix for between-subject variability
  arma::mat Sigma;///< variance/covariance matrix for within-subject variability
    
  std::vector<double> pred; ///< brings clearances, volumes, and & for advan 1/2/3/4

  deriv_func Derivs; ///< <code>$ODE</code> function
  init_func Inits; ///< <code>$MAIN</code> function
  table_func Table; ///< <code>$TABLE</code> function
  event_func Event; ///< <code>$EVENT</code> function
  config_func Config; ///< <code>$PREAMBLE</code> function
  
  bool Do_Init_Calc; ///< Flag regulating whether or not initials are taken from <code>$MAIN</code>
  int interrupt; ///< Check User Interrupt interval (number of simulation records)
};


double PolyExp(const double& x,
               const double& dose,
               const double& rate,
               const double& xinf,
               const double& tau,
               const bool ss,
               const std::vector<double>& a,
               const std::vector<double>& alpha,
               const int n);

#endif
