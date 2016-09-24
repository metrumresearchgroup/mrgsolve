// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include <cmath>
#include <vector>
//#include <numeric>
#include "RcppInclude.h"
#include "odeproblem.h"
//#include "pkevent.h"

Rcpp::NumericMatrix OMEGADEF(1,1);
arma::mat OMGADEF(1,1,arma::fill::zeros);


#define MRGSOLVE_MAX_SS_ITER 1000


odeproblem::odeproblem(Rcpp::NumericVector param,
                       Rcpp::NumericVector init,
                       Rcpp::List funs,
                       int n_capture_) : odepack_dlsoda(param.size(),init.size()) {
  
  int npar_ = int(param.size());
  int neq_ = int(init.size());
  
  R0  = new double[neq_]();
  
  infusion_count.assign(neq_,0);
  
  // R holds the value for user input rates via _R(n)
  R.assign(neq_,0.0);
  
  // D holds the value of user input durations
  D.assign(neq_,0.0);
  
  Param = new double[npar_]();
  Init_value = new double[neq_]();
  Init_dummy = new double[neq_]();
  
  On.assign(neq_,1);
  F.assign(neq_,1.0);
  
  Alag.assign(neq_,0.0);
  
  d.evid = 0;
  d.newind = 0;
  d.time = 0.0;
  d.ID = 1.0;
  
  d.EPS.assign(50,0.0);
  d.ETA.resize(50,0.0);
  d.CFONSTOP = false;
  d.omatrix = static_cast<void*>(&OMGADEF);
  
  Advan = 13;
  pred.assign(5,0.0);
  
  for(int i=0; i < npar_; ++i) Param[i] =       double(param[i]);
  for(int i=0; i < neq_;  ++i) Init_value[i] =  double(init[i]);
  
  Inits = as_init_func(funs["main"]);
  Table = as_table_func(funs["table"]);
  Derivs = as_deriv_func(funs["ode"]);
  Config = as_config_func(funs["config"]);
  
  Capture.assign(n_capture_,0.0);
  
}


/**
 @brief Destructor for odeproblem object
 
 Upon object construction, odeproblem dynamically allocates six arrays.  These arrays are "deleted" upon destruction.
 
 @return Void
 @date January, 2014
 
 */
odeproblem::~odeproblem(){
  delete [] Param;
  delete [] Init_value;
  delete [] Init_dummy;
  delete [] R0;
}

void odeproblem::neta(int n) {
  if(n > 25) d.ETA.assign(n,0.0);
}

void odeproblem::neps(int n) {
  if(n > 25) d.EPS.assign(n,0.0);
}



/** 
 * Assigns values to both the compartment and the 
 * vector of initial conditions.
 * 
 * @param pos the compartment number (C++ indexing)
 * @param value the value for the compartment
 * 
 */
void odeproblem::y_init(int pos, double value) {
  this->y(pos,value);
  Init_value[pos] = value;
}

/** Derivative function that gets called by the solver. 
 * 
 * @param neq number of equations
 * @param t solver time
 * @param y current state
 * @param ydot left hand side of differential equations
 * @param prob an odeproblem object
 * 
 * 
 */
void main_derivs(int *neq, double *t, double *y, double *ydot, odeproblem *prob) {
  (prob->derivs())(
      t,
      y,
      ydot,
      prob->init(),
      prob->param()
  );
  prob->add_rates(ydot);
}

void odeproblem::add_rates(double* ydot) {
  for(int i = 0; i < Neq; ++i) {
    ydot[i] = (ydot[i] + R0[i])*On[i];
  }
}


/**
 * Call $MAIN to get the initial conditions.
 * 
 * @param time the time to assume for the calculation
 * 
 */
void odeproblem::init_call(const double& time) {
  
  d.time = time;
  
  this->Inits(this->init(),
              this->y(),
              this->param(),
              this->fbio(),
              this->alag(),
              this->rate(),
              this->dur(),
              this->get_d(),
              this->get_pred());
  
  for(int i=0; i < Neq; ++i) {
    this->y(i,this->init(i));
    Init_dummy[i] = this->init(i);
  }
}


/**
 * Call $MAIN with the dummy initial condition vector.
 * 
 * @param time the time to assume when making the call.
 * 
 * 
 */
void odeproblem::init_call_record(const double& time) {
  d.time = time;
  
  this->Inits(this->init_dummy(),
              this->y(),
              this->param(),
              this->fbio(),
              this->alag(),
              this->rate(),
              this->dur(),
              this->get_d(),
              this->get_pred());
}


/** Call $TABLE.
 * 
 */
void odeproblem::table_call() {
  this->Table(this->y(),
              this->init(),
              this->param(),
              this->fbio(),
              this->rate(),
              this->get_d(),
              this->get_pred(),
              this->get_capture());
}

void odeproblem::table_init_call() {
  d.time = 0.0;
  d.newind = 0;
  d.evid = 0;
  this->table_call();
}

void odeproblem::rate_reset() {
  for(int i = 0; i < Neq; ++i) {
    R0[i] = 0.0;
    infusion_count[i] = 0;
  }
}


void odeproblem::reset_newid(const double& id_=1.0) {
  
  for(int i = 0; i < Neq; ++i) {
    R0[i] = 0.0;
    R[i] = 0.0;
    D[i] = 0.0;
    infusion_count[i] = 0;
    On[i] = 1;
    F[i] = 1.0;
    Alag[i] = 0;
  }
  
  d.mtime.clear();
  d.newind = 1;
  d.time = 0.0;
  
  d.SYSTEMOFF=false;
  this->istate(1);
  d.ID = id_;
}


void odeproblem::rate_add(unsigned int pos, const double& value) {
  ++infusion_count[pos];
  R0[pos] = R0[pos] + value;
}

void odeproblem::rate_rm(unsigned int pos, const double& value) {
  if(infusion_count[pos] <= 0){
    infusion_count[pos] = 0;
    R0[pos] = 0.0;
    return;
  } else {
    --infusion_count[pos];
    R0[pos] = R0[pos] - value;
    if(R0[pos] < 0.0) R0[pos] = 0.0;
  }
}


void odeproblem::on(unsigned short int eq_n) {
  On[eq_n] = 1;
}
void odeproblem::off(unsigned short int eq_n) {
  if(infusion_count[eq_n]>0) Rcpp::stop("Attempting to turn compartment off when infusion is on.");
  On[eq_n] = 0;
  this->y(eq_n,0.0);
}


void odeproblem::pass_omega(arma::mat* x) {
  d.omatrix = reinterpret_cast<void*>(x);
}

extern "C" {
  void F77_NAME(dlsoda) (
      main_deriv_func *derivs,
      int             *neq,
      double          *y,
      const double    *tfrom,
      const double    *tto,
      int             *itol,
      double          *rtol,
      double          *atol,
      int             *itask,
      int             *istate,
      int             *iopt,
      double          *rwork,
      int             *lrwork,
      int             *iwork,
      int             *liwork,
      int             *dum, // dummy jacobian
      int             *jt, // jacobian type
      odeproblem      *prob
  );
}

void odeproblem::advance(double tfrom, double tto) {
  
  if(Neq == 0) return;
  
  if(Advan != 13) {
    if((Advan==2) | (Advan==1)) {
      this->advan2(tfrom,tto);
      return;
    }
    
    if((Advan==4) | (Advan==3)) {
      this->advan4(tfrom,tto);
      return;
    }
    // If Advan isn't 13, it needs to be 1/2/3/4
    Rcpp::stop("mrgsolve: advan has invalid value.");
  }
  
  
  F77_CALL(dlsoda)(
      &main_derivs,
      &Neq,
      //this->y(),
      Y,
      &tfrom,
      &tto,
      &xitol,
      &xrtol,
      &xatol,
      &xitask,
      &xistate,
      &xiopt,
      //this ->rwork(),
      xrwork,
      &xlrwork,
      //this->iwork(),
      xiwork,
      &xliwork,
      &Neq,
      &xjt,
      this
  );
  
  // lsoda(&main_derivs,
  //         Neq,
  //         this->y(),
  //         &tfrom,
  //         tto,
  //         xitol,
  //         &xrtol,
  //         &xatol,
  //         xitask,
  //         &xistate,
  //         xiopt,
  //         xjt,
  //         xiwork[0],
  //         xiwork[1],
  //         xiwork[4],
  //         xiwork[5],
  //         xiwork[6],
  //         xiwork[7],
  //         xiwork[8],
  //         xrwork[0],
  //         xrwork[4],
  //         xrwork[5],
  //         xrwork[6],
  //         this
  // );
  // 
  
  main_derivs(&Neq, &tto,Y, Ydot, this);
}

void odeproblem::advan2(const double& tfrom, const double& tto) {
  
  unsigned int neq = this->neq();
  
  double dt = tto-tfrom;
  if (MRGSOLVE_GET_PRED_CL <= 0) Rcpp::stop("A pred_CL has a 0 or negative value.");
  if (MRGSOLVE_GET_PRED_VC <= 0) Rcpp::stop("pred_VC has a 0 or negative  value.");
  
  double k10 = MRGSOLVE_GET_PRED_K10;
  double ka =  MRGSOLVE_GET_PRED_KA;
  
  if(k10 <= 0) Rcpp::stop("k10 has a 0 or negative value");
  
  //a and alpha are private members
  alpha[0] = k10;
  alpha[1] = ka;
  
  a[0] = ka/(ka-alpha[0]);
  a[1] = -a[0];
  
  double init0 = 0, init1 = 0;
  int eqoffset = 0;
  
  if(neq==1) {
    init0 = 0;
    init1 = this->y(0);
    eqoffset = 1;
  }
  if(neq==2) {
    init0 = this->y(0);
    init1 = this->y(1);
  }
  
  double pred0 = 0, pred1 = 0;
  
  if(neq ==2) {
    if((init0!=0) || (R0[0]!=0)) {
      
      pred0 = init0*exp(-ka*dt);//+ R0[0]*(1-exp(-ka*dt))/ka;
      
      if(ka > 0) { // new
        pred0 += R0[0]*(1.0-exp(-ka*dt))/ka; // new
        pred1 +=
          PolyExp(dt,init0,0.0  ,0.0,0.0,false,a,alpha,2) +
          PolyExp(dt,0.0  ,R0[0],dt ,0.0,false,a,alpha,2);
      } else {
        pred0 += R0[0]*dt; // new
      }
    }
  }
  
  if((init1!=0) || (R0[1-eqoffset]!=0)) {
    a[0] = 1;
    pred1 +=
      PolyExp(dt,init1,0.0  ,0.0,0.0,false,a,alpha,1) +
      PolyExp(dt,0.0  ,R0[1-eqoffset],dt ,0.0,false,a,alpha,1);
  }
  
  if(neq==2) {
    this->y(0,pred0);
    this->y(1,pred1);
  }
  if(neq==1) {
    this->y(0,pred1);
  }
}


void odeproblem::advan4(const double& tfrom, const double& tto) {
  
  double dt = tto - tfrom;
  
  unsigned int neq = this->neq();
  
  // Make sure parameters are valid
  if (MRGSOLVE_GET_PRED_VC <=  0) Rcpp::stop("pred_VC has a 0 or negative  value.");
  if (MRGSOLVE_GET_PRED_VP <=  0) Rcpp::stop("pred_VP has a 0 or negative  value.");
  if (MRGSOLVE_GET_PRED_Q  <   0) Rcpp::stop("pred_Q has a  negative  value.");
  if (MRGSOLVE_GET_PRED_CL <=  0) Rcpp::stop("pred_CL has a 0 or negative  value.");
  
  double ka =  MRGSOLVE_GET_PRED_KA;
  double k10 = MRGSOLVE_GET_PRED_K10;
  double k12 = MRGSOLVE_GET_PRED_K12;
  double k21 = MRGSOLVE_GET_PRED_K21;
  
  double ksum = k10+k12+k21;
  
  double init0 = 0, init1 = 0, init2 = 0,  pred0 = 0, pred1 = 0, pred2 = 0;
  
  int eqoffset = 0;
  
  if(neq == 2) {
    init0 = 0; init1 = this->y(0); init2 = this->y(1);
    eqoffset = 1;
  }
  if(neq ==3) {
    init0 = this->y(0); init1 = this->y(1); init2 = this->y(2);
  }
  
  //a and alpha are private members
  
  alpha[0] = (ksum + sqrt(ksum*ksum-4.0*k10*k21))/2.0;
  alpha[1] = (ksum - sqrt(ksum*ksum-4.0*k10*k21))/2.0;
  alpha[2] = ka;
  
  if(neq==3) { // only do the absorption compartment if we have 3
    if((init0 != 0) || (R0[0] != 0)) {
      
      pred0 = init0*exp(-ka*dt);// + R0[0]*(1.0-exp(-ka*dt))/ka;
      
      a[0] = ka*(k21-alpha[0])/((ka-alpha[0])*(alpha[1]-alpha[0]));
      a[1] = ka*(k21-alpha[1])/((ka-alpha[1])*(alpha[0]-alpha[1]));
      a[2] = -(a[0]+a[1]);
      
      if(ka > 0) {  // new
        pred0 += R0[0]*(1.0-exp(-ka*dt))/ka; // new
        pred1 +=
          PolyExp(dt,init0,0,0,0,false,a,alpha,3) +
          PolyExp(dt,0,R0[0],dt,0,false,a,alpha,3);
        
        a[0] = ka * k12/((ka-alpha[0])*(alpha[1]-alpha[0]));
        a[1] = ka * k12/((ka-alpha[1])*(alpha[0]-alpha[1]));
        a[2] = -(a[0] + a[1]);
        
        pred2 +=
          PolyExp(dt,init0,0,0,0,false,a,alpha,3) +
          PolyExp(dt,0,R0[0],dt,0,false,a,alpha,3);
      } else {
        pred0 += R0[0]*dt; // new
      }
    }
  }
  
  if((init1 != 0) || (R0[1-eqoffset] != 0)) {
    
    a[0] = (k21 - alpha[0])/(alpha[1]-alpha[0]) ;
    a[1] = (k21 - alpha[1])/(alpha[0]-alpha[1]) ;
    
    pred1 +=
      PolyExp(dt,init1,0,0,0,false,a,alpha,2) +
      PolyExp(dt,0,R0[1-eqoffset],dt,0,false,a,alpha,2);
    
    a[0] = k12/(alpha[1]-alpha[0]) ;
    a[1] = -a[0];
    
    pred2 +=
      PolyExp(dt,init1,0,0,0,false,a,alpha,2) +
      PolyExp(dt,0,R0[1-eqoffset],dt,0,false,a,alpha,2);
  }
  
  if((init2 != 0) || (R0[2-eqoffset] != 0)) {
    
    a[0] = k21/(alpha[1]-alpha[0]);
    a[1] = -a[0];
    
    pred1 +=
      PolyExp(dt,init2,0,0,0,false,a,alpha,2) +
      PolyExp(dt,0,R0[2-eqoffset],dt,0,false,a,alpha,2);
    
    a[0] = (k10 + k12 - alpha[0])/(alpha[1]-alpha[0]);
    a[1] = (k10 + k12 - alpha[1])/(alpha[0]-alpha[1]);
    
    pred2 +=
      PolyExp(dt,init2,0,0,0,false,a,alpha,2) +
      PolyExp(dt,0,R0[2-eqoffset],dt,0,false,a,alpha,2);
  }
  
  if(neq ==2) {
    this->y(0,pred1);
    this->y(1,pred2);
  }
  if(neq ==3) {
    this->y(0,pred0);
    this->y(1,pred1);
    this->y(2,pred2);
  }
}


double PolyExp(const double& x,
               const double& dose,
               const double& rate,
               const double& xinf,
               const double& tau,
               const bool ss,
               const dvec& a,
               const dvec& alpha,
               const int n) {
  
  double result=0, bolusResult, dx, nlntv ;
  double inf=1E9;
  //maximum value for a double in C++
  int i;
  
  //assert((alpha.size() >= n) && (a.size() >= n));
  
  //UPDATE DOSE
  if (dose>0) {
    if((tau<=0)&&(x>=0)) {
      for(i=0;i<n; ++i){result += a[i]*exp(-alpha[i]*x);}
    }
    else if(!ss)  {
      nlntv=x/tau+1;
      dx=x-trunc(x/tau)*tau; // RISKY, but preliminary results suggest that trunc
      // works on Stan variables.
      for(i=0;i<n;++i) {
        result += a[i]*exp(-alpha[i]*x)
        *(1-exp(-nlntv*alpha[i]*tau))/(1-exp(-alpha[i]*tau));
      }
    }
    
    else {
      dx = x-trunc(x/tau)*tau;
      for(i=0;i<n;++i) result += a[i]*exp(-alpha[i]*x)/(1-exp(-alpha[i]*tau));
    }
  }
  bolusResult = dose*result;
  
  //UPDATE RATE
  result=0;
  if((rate>0)&&(xinf<inf)) {
    if(tau<=0) {
      if(x>=0) {
        if(x<=xinf) {
          for(i=0;i<n;++i) result += a[i]*(1-exp(-alpha[i]*x))/alpha[i];
        }
        else {
          for(i=0;i<n;++i) {
            result += a[i]*(1-exp(-alpha[i]*xinf))*exp(-alpha[i]*(x-xinf))/alpha[i];
          }
        }
      }
    }
    
    else if(!ss) {
      if(xinf <= tau) Rcpp::stop("xinf <= tau in PolyExp");
      //assert(xinf <= tau); //and other case later, says Bill
      dx=x-trunc(x/tau)*tau;
      nlntv=trunc(x/tau)+1;
      if(dx<=xinf) {
        for(i=0;i<n;++i) {
          if(n>1) {
            result += a[i]*(1-exp(-alpha[i]*xinf))*exp(-alpha[i]*(dx-xinf+tau))
            * (1-exp(-(nlntv-1)*alpha[i]*tau))/(1-exp(-alpha[i]*tau))/alpha[i];
          }
          result += a[i]*(1-exp(-alpha[i]*dx))/alpha[i];
        }
      }
      else  {
        for(i=0;i<n;++i) {
          result += a[i] * (1 - exp(-alpha[i]*xinf))*exp(-alpha[i]*(dx-xinf)) *
            (1-exp(-nlntv*alpha[i]*tau))/(1-exp(-alpha[i]*tau)) / alpha[i];
        }
      }
    }
    
    else {
      if(xinf <= tau) Rcpp::stop("xinf <= tau in PolyExp");
      dx = x - trunc(x/tau)*tau;
      nlntv = trunc(x/tau)+1;
      if (dx <= xinf) {
        for(i=0;i<n;++i) {
          result += a[i] * (1 - exp(-alpha[i]*xinf))*exp(-alpha[i]*(dx-xinf+tau)) /
          (1-exp(-alpha[i]*tau)) / alpha[i] + a[i] * (1 - exp(-alpha[i]*dx)) / alpha[i];
        }
      }
      else {
        for(i=0;i<n;++i) {
          result += a[i] * (1 - exp(-alpha[i]*xinf))*exp(-alpha[i]*(dx-xinf)) / (1-exp(-alpha[i]*tau)) / alpha[i];
        }
      }
    }
  }
  
  else  {
    if(!ss) {
      if(x>=0) {
        for(i=0;i<n;++i) result +=a[i]*(1-exp(-alpha[i]*x))/alpha[i];
      }
    }
    else {
      for(i=0;i<n;++i) result += a[i]/alpha[i];
    }
  }
  
  return bolusResult + rate*result;
}


void odeproblem::init_fun(SEXP xifun) {
  Inits = as_init_func(xifun);
}
void odeproblem::table_fun(SEXP xtfun) {
  Table = as_table_func(xtfun);
}
void odeproblem::deriv_fun(SEXP xdfun) {
  Derivs = as_deriv_func(xdfun);
}
void odeproblem::config_fun(SEXP xcfun) {
  Config = as_config_func(xcfun);
}

init_func* as_init_func(SEXP inits) {
  return(reinterpret_cast<init_func *>(tofunptr(inits))); // Known to generate compiler warning
}

deriv_func* as_deriv_func(SEXP derivs) {
  return(reinterpret_cast<deriv_func *>(tofunptr(derivs))); // Known to generate compiler warning
}

table_func* as_table_func(SEXP table) {
  return(reinterpret_cast<table_func*>(tofunptr(table))); // Known to generate compiler warning
}

config_func* as_config_func(SEXP config) {
  return(reinterpret_cast<config_func*>(tofunptr(config))); // Known to generate compiler warning
}

void odeproblem::copy_parin(const Rcpp::List& parin) {
  this->tol(Rcpp::as<double>(parin["atol"]),Rcpp::as<double>(parin["rtol"]));
  this->hmax(Rcpp::as<double>(parin["hmax"]));
  this->maxsteps(Rcpp::as<double>  (parin["maxsteps"]));
  this->ixpr(Rcpp::as<double>  (parin["ixpr"]));
  this->mxhnil(Rcpp::as<double>  (parin["mxhnil"]));
  this->advan(Rcpp::as<int>(parin["advan"]));
}
void odeproblem::copy_funs(const Rcpp::List& funs) {
  Inits = as_init_func(funs["main"]);
  Table = as_table_func(funs["table"]);
  Derivs = as_deriv_func(funs["ode"]);
  Config = as_config_func(funs["config"]);
  
}

void odeproblem::advan(int x) {
  Advan = x;
  
  if(Advan==13) return;
  
  if((x==1) | (x ==2)) {
    a.assign(2,0.0);
    alpha.assign(2,0.0);
  }
  
  if((x==3) | (x==4)) {
    a.assign(3,0.0);
    alpha.assign(3,0.0);
  }
  
}


/**
 * Call the $MAIN function from a model object.
 * 
 * @param lparam model parameters
 * @param linit model initial contitions
 * @param Neta number of rows in OMEGA
 * @param Neps number of rows in SIGMA
 * @param capture vector of capture names
 * @param funs the model funset
 * @return list with updated initial conditions, number of paramerters,
 * and number of equations
 * 
 */
// [[Rcpp::export]]
Rcpp::List TOUCH_FUNS(const Rcpp::NumericVector& lparam, 
                      const Rcpp::NumericVector& linit,
                      int Neta, int Neps,
                      const Rcpp::CharacterVector& capture,
                      const Rcpp::List& funs) {
  
  Rcpp::List ans;
  
  odeproblem* prob  = new odeproblem(lparam, linit, funs, capture.size());
  prob->neta(Neta);
  prob->neps(Neps);
  
  double time = 0;
  prob->time(time);
  prob->newind(0);
  
  prob->init_call(time);
  
  Rcpp::NumericVector init_val(linit.size());
  
  for(int i=0; i < (prob->neq()); ++i) init_val[i] = prob->init(i);
  
  ans["init"] = init_val;
  ans["npar"] = prob->npar();
  ans["neq"] = prob->neq();
  delete prob;
  return(ans);
}
