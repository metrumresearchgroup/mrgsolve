// Copyright (C) 2013 - 2023  Metrum Research Group
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
 * @file mrgsolv.h
 */

#ifndef MRGSOLV_H
#define MRGSOLV_H

#include <vector>
#include <iostream>

typedef void (*refun)(void*, int n);

namespace mrgsolve {
/**
 * @brief Resim functor.
 * 
 * These functors are used to re-simulate <code>ETA</code>
 * and <code>EPS</code> values.
 * 
 */
struct resim {
  //! resim constructor
  resim(refun x, void* y) : fun(x), prob(y){}
  resim(){}
  void operator()(int n = 0) {
    return fun(prob, n);
  }
  
protected: 
  refun fun; ///< function to call to re-simulate
  void* prob; ///< object to pass to re-simulated function
}; 

struct evdata {
  evdata(double a_, int b_) :  time(a_), evid(b_) {
    cmt = 1;
    amt = 0.0;
    rate = 0.0;
    now = false;
    check_unique = true;
  } 
  double time; 
  int evid;
  int cmt;
  double amt; 
  double rate;
  bool now;
  bool check_unique;
}; 

// Some functions for reporting values during a
// simulation run
template <class type> void report(type a) {
  std::cout << "from report " << a << std::endl;
}
template <class type1, class type2> void report(type1 a, type2 b) {
  std::cout << a << " " << b << std::endl;
}

}
namespace mrg = mrgsolve;

//! member functions mevent and tad come in via housemodel; see inst/base/databox.cpp


/**
 * Model data passed to the model.  
 * 
 * This data is passed to PREAMBLE, MAIN, and TABLE but not ODE.
 * 
 * 
 */
class databox {
public: 
  std::vector<double> ETA; ///< vector of ETA values
  std::vector<double> EPS; ///< vector of EPS values
  std::vector<double> SIGMA; ///< vector of on-diagonal sigma elements
  unsigned int newind; ///< new individual flag
  double time; ///< current simulation time
  int evid;  ///< event ID flag
  unsigned short int SYSTEMOFF; ///< flag to stop advancing system for current ID
  double id;  ///< current ID
  double amt; ///< current dosing amount value
  short int cmt; ///< current compartment value
  int nid; ///< number of IDs in the data set
  int idn; ///< current ID number
  int nrow; ///< number of rows in output data set
  int rown; ///< current output row number
  bool CFONSTOP; ///< carry forward on stop indicator
  void* envir; ///< model environment
  void stop() {SYSTEMOFF=9;}///< stops the problem when the next record is started
  void stop_id() {SYSTEMOFF=2;}///< stops solving for the current id, filling with NA
  void stop_id_cf(){SYSTEMOFF=1;}///< stops solving for the current id, filling last value
  std::vector<mrgsolve::evdata> mevector;///< a collection of model events to pass back
  void mevent(double time, int evid);///< constructor for evdata objects
  double mtime(double time);///< creates evdata object for simple model event time
  double tad();///< calculates time after dose
}; 

//! vector of doubles
typedef std::vector<double> dvec;

//! signature for <code>$MAIN</code>
#define MRGSOLVE_INIT_SIGNATURE  dvec& _A_0_,const dvec& _A_, const dvec& _THETA_,  dvec& _F_, dvec& _ALAG_, dvec& _R_, dvec& _D_,  databox& self, dvec& _pred_, mrgsolve::resim& simeta
#define MRGSOLVE_INIT_SIGNATURE_N 10

//! signature for <code>$TABLE</code>
#define MRGSOLVE_TABLE_SIGNATURE const dvec& _A_, const dvec& _A_0_,  dvec& _THETA_,  const dvec& _F_, const dvec& _R_,  databox& self, const dvec& _pred_, dvec& _capture_, mrgsolve::resim& simeps
#define MRGSOLVE_TABLE_SIGNATURE_N 9

//! signature for <code>$ODE</code>
#define MRGSOLVE_ODE_SIGNATURE const double* _ODETIME_, const double* _A_, double* _DADT_,  const dvec& _A_0_, const dvec& _THETA_, const bool _ss_flag_
#define MRGSOLVE_ODE_SIGNATURE_N 6

//! signature for <code>$PREAMBLE</code>
#define MRGSOLVE_CONFIG_SIGNATURE databox& self, const dvec& _THETA_, const double neq, const double npar
#define MRGSOLVE_CONFIG_SIGNATURE_N 4

#endif
