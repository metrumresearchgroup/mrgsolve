// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifndef MRGSOLV_H
#define MRGSOLV_H

#include <vector>
#include <map>
#include <string>

typedef std::vector<double> dvec;
typedef std::vector<std::string > svec;
typedef std::vector<int > ivec;

// resim objects
typedef void refun(void*);
struct resim {
  resim(refun* x, void* y) : fun(x), prob(y){}
  resim(){}
  void operator()() {
    return fun(prob);
  }
private:
  refun* fun;
  void* prob;
};


#define MRGSOLVE_INIT_SIGNATURE  dvec& _A_0_,const double* _A_, const double* _THETA_, dvec& _F_, dvec& _ALAG_, dvec& _R_, dvec& _D_,  databox& _databox_, dvec& _pred_, resim& simeta

#define MRGSOLVE_TABLE_SIGNATURE const double* _A_, const dvec& _A_0_,  const double* _THETA_,  const dvec& _F_, const dvec& _R_,  databox& _databox_, const dvec& _pred_, dvec& _capture_, resim& simeps

#define MRGSOLVE_ODE_SIGNATURE const double* _ODETIME_, const double* _A_, double* _DADT_,  const dvec& _A_0_, const double* _THETA_

#define MRGSOLVE_CONFIG_SIGNATURE svec& capture_names, dvec& capture_values



#endif
