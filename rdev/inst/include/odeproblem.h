// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifndef ODEPROBLEM_H
#define ODEPROBLEM_H
#include <math.h>
#include <iostream>
#include <memory>
#include <vector>
#include "odepack_dlsoda.h"
#include <string>
#include "mrgsolv.h"
#include "RcppInclude.h"


class odeproblem;
//class Rodeproblem;

struct databox {
  unsigned int newind;
  double time;
  int evid;
  dvec EPS;
  dvec ETA;
  bool SYSTEMOFF;
  bool solving;
  bool INITSOLV;
  dvec mtime;
  double ID;
  bool CFONSTOP;
  double XDOSE;
  void* omatrix;
};


typedef void init_func(MRGSOLVE_INIT_SIGNATURE);

typedef void table_func(MRGSOLVE_TABLE_SIGNATURE);

typedef void deriv_func(MRGSOLVE_ODE_SIGNATURE);

typedef void config_func(MRGSOLVE_CONFIG_SIGNATURE);

typedef void main_deriv_func(int*        neq,
                             double*     t,
                             double*     y,
                             double*     ydot,
                             odeproblem* prob);


deriv_func*  as_deriv_func( SEXP derivs);
init_func*   as_init_func(  SEXP inits);
table_func*  as_table_func( SEXP table);
config_func* as_config_func(SEXP config);

extern "C"{DL_FUNC tofunptr(SEXP a);}
extern "C" {init_func  MRGSOLVE_NO_INIT_FUN ;}
extern "C" {table_func MRGSOLVE_NO_TABLE_FUN;}
extern "C" {deriv_func MRGSOLVE_NO_ODE_FUN  ;}
extern "C" {config_func MRGSOLVE_NO_CONFIG_FUN  ;}

main_deriv_func main_derivs;
void neg_istate(int istate);

template<typename T,typename type2> void tofunptr(T b,type2 a) {
  b = reinterpret_cast<T>(R_ExternalPtrAddr(a));
}


// ODE function

// Send report to console when istate returns negative after dlsoda call

class odeproblem : public odepack_dlsoda {

  //  friend class Rodeproblem;

 public:
  odeproblem(Rcpp::NumericVector param, Rcpp::NumericVector init);

  virtual ~odeproblem();

  bool solving(){return d.solving;}
  void solving(bool in){d.solving=in;}

  void advance(double tfrom, double tto);

  // initial conditions:
  void init_fun (init_func*     fptr) {Inits  = fptr;}
  void table_fun (table_func*   fptr) {Table  = fptr;}
  void deriv_fun (deriv_func*   fptr) {Derivs = fptr;}
  void config_fun (config_func* fptr) {Config = fptr;}
  
  void init(int pos, double value){Init_value[pos] = value;}
  double init(int pos){return Init_value[pos];}
  dvec& init()  {return Init_value;}
  dvec& init_dummy(){return Init_dummy;}

  void init_copy_from_dummy();
  void init_call(const double& time);
  void init_call_record(const double& time);
  void y_init(int pos, double value);

  void table_call();
  void table_init_call();

  int ntable(){return Tabledata.size();}
  double table(std::string key){return Tabledata[key];}
  sd_map& table() {return Tabledata;}

  //void set_omatrix(Rcpp::NumericMatrix* x_);
  void pass_omega(arma::mat*);
  
  bool CFONSTOP(){return d.CFONSTOP;}

  // param:

  dvec& param() {return Param;}
  void param(int pos, double value){Param[pos] = value;}
  double param(int pos){return Param[pos];}

  void param(dvec& x) {for(size_t i = 0; i < x.size(); ++i) Param.at(i) = x[i];}
  dvec cacheparam() {return Param;}


  // rate:
  dvec& rate(){return R;}
  void rate(unsigned int pos, double value) {R[pos] = value;}
  double rate(unsigned int pos){return R[pos];}

  // dur:
  dvec& dur(){return D;}
  void dur(unsigned int pos, double value) {D[pos] = value;}
  double dur(unsigned int pos){return D[pos];}


  void rate0(unsigned int pos, double value) {R0[pos] = value;}
  double rate0(unsigned int pos){return R0[pos];}
  int rate_count(unsigned int pos){return infusion_count[pos];}

  void rate_add(unsigned int pos, const double& value);
  void rate_rm(unsigned int pos,  const double& value);
  void rate_replace(unsigned int pos, const double& value);
  void rate_reset();
  void rate_reset(unsigned short int eq_n);

  // Bioavailability:
  dvec& fbio()  {return F;}
  void fbio(unsigned int pos, double value) {F.at(pos) = value;}
  double fbio(unsigned int pos) {return F.at(pos);}

  dvec& alag()  {return Alag;}
  double alag(int cmt){return Alag.at(abs(cmt)-1);}

  void reset_newid(const double& id_);

  // ETA
  void eta(int pos, double value) {d.ETA[pos] =value;}
  double eta(int pos) {return d.ETA[pos];}
  dvec& eta() {return d.ETA;}

  // EPS
  void   eps(int pos, double value) {d.EPS[pos] = value;}
  double eps(int pos) {return d.EPS[pos];}
  dvec& eps() {return d.EPS;}

  void turnsystemoff(){d.SYSTEMOFF=true;}
  void turnsystemon(){d.SYSTEMOFF=false;}
  bool systemoff(){return d.SYSTEMOFF;}

  // Derivatives function:
  deriv_func * derivs(){return Derivs;}

  void  on(unsigned short int cmt);
  void off(unsigned short int cmt);

  int is_on(unsigned int eq_n){return On[eq_n];}

  void time(double time_){d.time = time_;}
  void newind(unsigned int newind_){d.newind = newind_;}
  unsigned int newind(){return d.newind;}

  void evid(int evid_){d.evid = evid_;}

  databox& get_d(){return d;}

  void advan(int x){Advan = x;}
  int advan(){return Advan;}
  void advan2(const double& tfrom, const double& tto);
  void advan4(const double& tfrom, const double& tto);

  void neta(int n);
  void neps(int n);

  void INITSOLV();
  dvec& mtime(){return d.mtime;}
  void clear_mtime(){d.mtime.clear();}

  double xdose(){return d.XDOSE;}
  dvec& get_pred(){return pred;}
  dvec& get_capture() {return Capture;}
  double capture(int i) {return Capture[i];}
  void  resize_capture(size_t n) {Capture.assign(int(n),0.0);}
  double get_pred_CL() {return pred[0];}
  double get_pred_VC() {return pred[1];}
  double get_pred_KA() {return pred[2];}
  double get_pred_Q()  {return pred[3];}
  double get_pred_VP() {return pred[4];}
  double get_pred_k10(){return pred[0]/pred[1];}
  double get_pred_k12(){return pred[3]/pred[1];}
  double get_pred_k21(){return pred[3]/pred[4];}

  // SAVE
  // int nRn(){return Rn.size();}
  // void add_Rn(int value){Rn.insert(value);}
  // void add_rates(double* ydot);

  // From Rodeproblem
  void init_fun(SEXP ifun);
  void table_fun(SEXP tfun);
  void deriv_fun(SEXP dfun);
  void config_fun(SEXP cfun);
  
  void copy_parin(Rcpp::List parin);
  void copy_funs(Rcpp::List funs);
  
 protected:

  //! parameters
  dvec Param;
  //! Acutal curent infusion rate
  dvec R0;
  std::vector<unsigned int> infusion_count;

  // SAVE
  //std::set<int> Rn;

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

  sd_map Tabledata;
  std::vector<std::string> Tablenames;

  //! Compartment on/off
  std::vector<char> On;

  databox d;
  int Advan;
  dvec pred;
  dvec Capture;

};



/**
    @brief DLSODA call
    @param derivs pointer to derivative function
    @param neq pointer to neq
    @param y pointer to array of state variable
    @param tfrom pointer to "from" integration time
    @param tto pointer to "to" integration time
    @param itol itol
    @param rtol rtol
    @param atol atol
    @param itask itask
    @param istate istate
    @param iopt iopt
    @param rwork rwork array
    @param lrwork length of rwork array
    @param iwork iwork array
    @param liwork lenght of iwork array
    @param dum dummy pointer to integer
    @param jt jacobian type (must be 2... no jacobian function supplied)
    @param Send

 */

extern "C" {void dlsoda_(
			 main_deriv_func* derivs,
			 int       * neq,
			 double    * y,
			 const double    * tfrom,
			 const double    * tto,
			 int       * itol,
			 double    * rtol,
			 double    * atol,
			 int       * itask,
			 int       * istate,
			 int       * iopt,
			 double    * rwork,
			 int       * lrwork,
			 int       * iwork,
			 int       * liwork,
			 int       * dum, // dummy jacobian
			 int       * jt, // jacobian type
			 odeproblem * prob
			 );
}


double PolyExp(const double& x,
	       const double& dose,
	       const double& rate,
	       const double& xinf,
	       const double& tau,
	       const bool ss,
	       const dvec& a,
	       const dvec& alpha,
	       const int n);


#endif

