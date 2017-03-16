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
 * @file mrgsolv.h
 */

#ifndef MRGSOLV_H
#define MRGSOLV_H

#include <vector>
#include <map>
#include <string>


//! vector of doubles
typedef std::vector<double> dvec;

//! vector of strings
typedef std::vector<std::string > svec;

//! vector of integers
typedef std::vector<int> ivec;

typedef void (*refun)(void*);

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
  void operator()() {
    return fun(prob);
  }
  
protected:
  refun fun; ///< function to call to re-simulate
  void* prob; ///< object to pass to re-simulated function
};

//! signature for <code>$MAIN</code>
#define MRGSOLVE_INIT_SIGNATURE  dvec& _A_0_,const double* _A_, const double* _THETA_, dvec& _F_, dvec& _ALAG_, dvec& _R_, dvec& _D_,  databox& self, dvec& _pred_, resim& simeta
#define MRGSOLVE_INIT_SIGNATURE_N 10

//! signature for <code>$TABLE</code>
#define MRGSOLVE_TABLE_SIGNATURE const double* _A_, const dvec& _A_0_,  const double* _THETA_,  const dvec& _F_, const dvec& _R_,  databox& self, const dvec& _pred_, dvec& _capture_, resim& simeps
#define MRGSOLVE_TABLE_SIGNATURE_N 9

//! signature for <code>$ODE</code>
#define MRGSOLVE_ODE_SIGNATURE const double* _ODETIME_, const double* _A_, double* _DADT_,  const dvec& _A_0_, const double* _THETA_
#define MRGSOLVE_ODE_SIGNATURE_N 5

//! signature for <code>$PREAMBLE</code>
#define MRGSOLVE_CONFIG_SIGNATURE databox& self, const double* _THETA_, const double neq, const double npar
#define MRGSOLVE_CONFIG_SIGNATURE_N 4

#endif
