// Copyright (C) 2013 - 2019  Metrum Research Group
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

#ifndef MODELHEADER_H
#define MODELHEADER_H

#include <vector>
#include <math.h>
#include "mrgsolv.h"

typedef double local_double;
typedef int    local_int;
typedef bool   local_bool;
typedef double capture;

// pred_P definitions for $PKMODEL
// Note that V/VC/V2 are synonymous when using the pred_P construct
// Also, V3/V3 are synonyms as well
#define pred_CL _pred_[0]
#define pred_V  _pred_[1]
#define pred_VC _pred_[1]
#define pred_V2 _pred_[1]
#define pred_KA _pred_[2]
#define pred_Q  _pred_[3]
#define pred_V3 _pred_[4]
#define pred_VP _pred_[4]

// advan/trans combinations
// These definitions are added by mrgsolve at the end of $MAIN
// when $PKMODEL is called; trans 2/4 are default; when 11 is picked ... tack on a `i` at the end
#define __ADVAN1_TRANS2__  pred_CL = CL;  pred_V  = V;
#define __ADVAN2_TRANS2__  pred_CL = CL;  pred_V  = V;   pred_KA = KA;
#define __ADVAN3_TRANS4__  pred_CL = CL;  pred_V2 = V1;  pred_Q =  Q;  pred_V3 = V2;
#define __ADVAN4_TRANS4__  pred_CL = CL;  pred_V2 = V2;  pred_Q =  Q;  pred_V3 = V3; pred_KA = KA;
#define __ADVAN1_TRANS11__ pred_CL = CLi; pred_V  = Vi;
#define __ADVAN2_TRANS11__ pred_CL = CLi; pred_V  = Vi;  pred_KA = KAi;
#define __ADVAN3_TRANS11__ pred_CL = CLi; pred_V2 = V1i; pred_Q =  Qi;  pred_V3 = V2i;
#define __ADVAN4_TRANS11__ pred_CL = CLi; pred_V2 = V2i; pred_Q =  Qi;  pred_V3 = V3i; pred_KA = KAi;

// Don't need this?
#define __BEGIN_pred__ extern "C" {void __ODEFUN___(MRGSOLVE_PRED_SIGNATURE) {

// $MAIN, $ODE, and $TABLE get translated into these functions
// We need ODEFUN___ and INITFUN___ and TABLECODE___ defined in the
// .cpp.cpp model file
#define __BEGIN_config__ extern "C" { void __CONFIGFUN___(MRGSOLVE_CONFIG_SIGNATURE) {
#define __END_config__ __DONE__
#define __BEGIN_ode__ extern "C" { void __ODEFUN___(MRGSOLVE_ODE_SIGNATURE) {
#define __END_ode__ __DONE__
#define __BEGIN_main__ extern "C" { void __INITFUN___(MRGSOLVE_INIT_SIGNATURE) {
#define __END_main__ __DONE__
#define __BEGIN_table__ extern "C" { void __TABLECODE___(MRGSOLVE_TABLE_SIGNATURE) {
#define __END_table__ __DONE__
#define __BEGIN_event__ extern "C" {void __EVENTFUN___(MRGSOLVE_EVENT_SIGNATURE) {
#define __END_event__ __DONE__
#define __DONE__ }}


// New individual flag
#define NEWIND self.newind
// The data set time
#define TIME self.time
// The ode solver time
#define SOLVERTIME _ODETIME_[0]
// Event ID
#define EVID self.evid
// Data set individual
#define ID self.id
// Data set amt
#define AMT self.amt
// Data set cmt
#define CMT self.cmt
// Bool flag indicating that the system is advancing to steady-state
#define SS_ADVANCE _ss_flag_
// Bool flag indicating that an infusion is ending
#define END_OF_INFUSION (self.evid==9)
// Always accept THETA(n) as THETAn
#define THETA(a) THETA##a

// NMVARS
#ifdef _MRGSOLVE_USING_NM_VARS_
#define A(a) _A_[a-1]
#define A_0(a) _A_0_[a-1]
#define DADT(a) _DADT_[a-1]
#define T _ODETIME_[0]
#define EXP(a) exp(a)
#define DEXP(a) exp(a)
#define LOG(a) log(a)
#define LOG10(a) log10(a)
#define SQRT(a) sqrt(a)
#define COS(a) cos(a)
#define SIN(a) sin(a)
#endif

// These are the fundamental macros for
// bioavailability, infusion rate, infusion duration
// and dose lag time.  Keep these here, but
// the model spec should prefer F_CMT, R_CMT, D_CMT, ALAG_CMT
#define _F(a)    _F_[a-1]
#define _R(a)    _R_[a-1]
#define _D(a)    _D_[a-1]
#define _ALAG(a) _ALAG_[a-1]

// These are the fundamental macros for
// random effects.  These might get used,
// but users are allowed to insert labels to
// avoid directly accessing the macros.
#define ETA(a)   self.ETA.at(a-1)
#define EPS(a)   self.EPS.at(a-1)
#define SIGMA(a) self.SIGMA.at(a-1)
#define _xETA(a) self.ETA[a-1]
#define _xEPS(a) self.EPS[a-1]

// Number of equations
#define _NEQ (_A_0_.size())

// Extract objects out of $ENV
#define _MRGX_GET(a,b) b = mrgx::get<a>(self,#b);
#define _MRGX_GET_LOCAL(a,b) a b = mrgx::get<a>(self,#b);
#define _MRGX_MT_FUN(a) Rcpp::Function a = mrgx::mt_fun();

// Macros related to stopping the advance of the system
// once a condition is met
#define SYSTEMSTOPADVANCING() (self.SYSTEMOFF=1);
#define STOPADVANCING() SYSTEMSTOPADVANCING()  // Not sure why this is here
#define CFONSTOP() (self.CFONSTOP = true); // Carry forward on stop
#define SYSTEMNOTADVANCING (self.SYSTEMOFF)
#define SOLVINGPROBLEM (self.solving)
#define _SETINIT if(self.newind <= 1) // Convenience
#define _STOP_ID() (self.SYSTEMOFF = 1); // Stop this ID, log record, and fill NA after that
#define _STOP_ID_CF() (self.SYSTEMOFF = 2); // Stop this ID and carry forward
#define _STOP_ID_NA() (self.SYSTEMOFF = 3); // Fill na
#define _STOP_ERROR() (self.SYSTEMOFF = 9); // CRUMP

// Macro to insert dxdt_CMT = 0; for all compartments
#define DXDTZERO() for(int _i_ = 0; _i_ < _nEQ; ++_i_) _DADT_[_i_] = 0;

#endif
