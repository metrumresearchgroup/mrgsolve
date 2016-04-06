// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifdef MODELINCLUDEGUARD

#endif


#ifndef MODELINCLUDEGUARD

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <math.h>
#include "mrgsolv.h"


typedef double local_double;
typedef int local_int;
typedef bool local_bool;

struct databox {
  const unsigned int newind;
  const double time;
  const int evid;
  const dvec EPS;
  const dvec ETA;
  bool SYSTEMOFF;
  bool solving;
  bool INITSOLV;
  dvec mtime;
  const double ID;
  bool CFONSTOP;
  double XDOSE;
};



#define pred_CL _pred_[0]
#define pred_V _pred_[1]
#define pred_VC _pred_[1]
#define pred_V2 _pred_[1]
#define pred_KA _pred_[2]
#define pred_Q _pred_[3]
#define pred_V3 _pred_[4]
#define pred_VP _pred_[4]


#define BEGIN_pred extern "C" {void ODEFUN___(MRGSOLVE_PRED_SIGNATURE) {

#define BEGIN_ode extern "C" {void ODEFUN___(MRGSOLVE_ODE_SIGNATURE) {
#define END_ode DONE

#define BEGIN_main extern "C" {void INITFUN___(MRGSOLVE_INIT_SIGNATURE) {
#define END_main DONE

#define BEGIN_table extern "C" {void TABLECODE___(MRGSOLVE_TABLE_SIGNATURE) {
#define END_table DONE

#define DONE }}


#define NEWIND _databox_.newind
#define TIME _databox_.time
#define SOLVERTIME _ODETIME_[0]
#define EVID _databox_.evid
#define ID _databox_.ID

#define table(a) _tabledata_[#a]
#define capture(a) _tabledata_[#a] = a


#define _F(a) _F_[a-1]
#define _R(a) _R_[a-1]
#define _D(a) _D_[a-1]
#define _ALAG(a) _ALAG_[a-1]
#define ETA(a) _databox_.ETA.at(a-1)
#define EPS(a) _databox_.EPS.at(a-1)
#define SYSTEMSTOPADVANCING() (_databox_.SYSTEMOFF=true);
#define STOPADVANCING() SYSTEMSTOPADVANCING()
#define SYSTEMNOTADVANCING (_databox_.SYSTEMOFF)
#define SOLVINGPROBLEM (_databox_.solving)
#define _NEQ (_A_0_.size())
#define INITSOLV() {_databox_.INITSOLV=true;}
#define _SETINIT if(NEWIND <=1)
#define CFONSTOP() (_databox_.CFONSTOP = true);
#define DXDTZERO() for(int _i_ = 0; _i_ < _nEQ; _i_++) _DADT_[_i_] = 0;

#define X_DOSE(a) _databox_.XDOSE = a

template <class type> void report(type a) {
  std::cout << "from report " << a << std::endl;
}
template <class type1, class type2> void report(type1 a, type2 b) {
  std::cout << a << " " << b << std::endl;
}
template <class type1, class type2> void report(type1 a, type2 b, type2 c) {
  std::cout << a << " " << b << " " << c<< std::endl;
}
template <class type1, class type2>
  void report(type1 a, type2 b, type2 c, type2 d) {
  std::cout << a << " " << b << " " << c<< " " << d<< std::endl;
}
#endif
