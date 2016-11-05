// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifndef ODEPROBLEM_H
#define ODEPROBLEM_H
#include <math.h>
#include <vector>
#include "odepack_dlsoda.h"
#include "mrgsolv.h"
#include "RcppInclude.h"
#include "datarecord.h"

typedef std::vector<rec_ptr> reclist;
typedef std::vector<reclist> recstack;


// double get_pred_CL() {return pred[0];}
// double get_pred_VC() {return pred[1];}
// double get_pred_KA() {return pred[2];}
// double get_pred_Q()  {return pred[3];}
// double get_pred_VP() {return pred[4];}
// double get_pred_k10(){return pred[0]/pred[1];}
// double get_pred_k12(){return pred[3]/pred[1];}
// double get_pred_k21(){return pred[3]/pred[4];}

#define MRGSOLVE_GET_PRED_CL  (pred[0])
#define MRGSOLVE_GET_PRED_VC  (pred[1])
#define MRGSOLVE_GET_PRED_KA  (pred[2])
#define MRGSOLVE_GET_PRED_Q   (pred[3])
#define MRGSOLVE_GET_PRED_VP  (pred[4])
#define MRGSOLVE_GET_PRED_K10 (pred[0]/pred[1])
#define MRGSOLVE_GET_PRED_K12 (pred[3]/pred[1])
#define MRGSOLVE_GET_PRED_K21 (pred[3]/pred[4])

// 
// resim functor comes from mrgsolv.h
// so it can get defined in the model
// 

class odeproblem;

struct databox {
  dvec ETA;
  dvec EPS;
  unsigned int newind;
  double time;
  int evid;
  bool SYSTEMOFF;
  dvec mtime;
  double ID;
  bool CFONSTOP;
  void* envir;
};


typedef void init_func(MRGSOLVE_INIT_SIGNATURE);

typedef void table_func(MRGSOLVE_TABLE_SIGNATURE);

typedef void deriv_func(MRGSOLVE_ODE_SIGNATURE);

typedef void config_func(MRGSOLVE_CONFIG_SIGNATURE);

typedef void main_deriv_func(int* neq, double* t,
                             double* y,double* ydot,
                             odeproblem* prob);


deriv_func*  as_deriv_func( SEXP derivs);
init_func*   as_init_func(  SEXP inits);
table_func*  as_table_func( SEXP table);
config_func* as_config_func(SEXP config);

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
  void confg_call();
  
  void omega(arma::mat x) {Omega = x;}
  void sigma(arma::mat x) {Sigma = x;}
  
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
  
  void evid(int evid_){d.evid = evid_;}
  
  void advan(int x);
  int  advan(){return Advan;}
  void advan2(const double& tfrom, const double& tto);
  void advan4(const double& tfrom, const double& tto);
  
  void neta(int n);
  void neps(int n);
  
  dvec& mtime(){return d.mtime;}
  
  dvec& get_capture() {return Capture;}
  double capture(int i) {return Capture[i];}
  
  void copy_parin(const Rcpp::List& parin);
  void copy_funs(const Rcpp::List& funs);

protected:
  
  //! parameters
  double* Param;
  
  //! Acutal curent infusion rate
  dvec R0;
  std::vector<unsigned int> infusion_count;
  
  //! User input infusion rate
  dvec R;
  //! User input infusion duration
  dvec D;
  
  //! inital conditions:
  dvec Init_value;
  dvec Init_dummy;
  
  //! Bioavailability:
  dvec F;
  
  //! ALAG:
  dvec Alag;
  
  //! cpp function to calculate derivatives
  deriv_func* Derivs;
  
  //! cpp function to calculated initial conditions based on parameters and
  init_func* Inits;
  
  //! Table
  table_func* Table;
  
  // Configure
  config_func* Config;
  
  //! Compartment on/off
  std::vector<int> On;
  databox d;
  
  // These are used for advan 1/2/3/4
  int Advan;
  dvec a;
  dvec alpha;
  
  resim simeta;
  resim simeps;

  arma::mat Omega;
  arma::mat Sigma;
    
  dvec pred;
  dvec Capture;
  

  
};


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
