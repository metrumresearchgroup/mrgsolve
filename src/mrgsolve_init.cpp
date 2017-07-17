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

#include "RcppInclude.h"
#include "modelheader.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

RcppExport SEXP _mrgsolve_dcorr(SEXP);
RcppExport SEXP _mrgsolve_get_tokens(SEXP);
RcppExport SEXP _mrgsolve_MVGAUSS(SEXP,SEXP);
RcppExport SEXP _mrgsolve_DEVTRAN(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,
                                 SEXP,SEXP,SEXP);
RcppExport SEXP _mrgsolve_SUPERMATRIX(SEXP,SEXP);
RcppExport SEXP _mrgsolve_TOUCH_FUNS(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
RcppExport SEXP _mrgsolve_QUICKSIM(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,
                       SEXP,SEXP,SEXP);
RcppExport SEXP _mrgsolve_EXPAND_EVENTS(SEXP,SEXP,SEXP);

RcppExport void _model_housemodel_main__(MRGSOLVE_INIT_SIGNATURE);
RcppExport void _model_housemodel_ode__(MRGSOLVE_ODE_SIGNATURE);
RcppExport void _model_housemodel_table__(MRGSOLVE_TABLE_SIGNATURE);
RcppExport void _model_housemodel_config__(MRGSOLVE_CONFIG_SIGNATURE);

static R_CallMethodDef callEntryPoints[]  = {
  CALLDEF(_mrgsolve_get_tokens,1),
  CALLDEF(_mrgsolve_MVGAUSS,2),
  CALLDEF(_mrgsolve_DEVTRAN,12),
  CALLDEF(_mrgsolve_SUPERMATRIX,2),
  CALLDEF(_mrgsolve_TOUCH_FUNS,7),
  CALLDEF(_mrgsolve_QUICKSIM,11),
  CALLDEF(_mrgsolve_EXPAND_EVENTS,3),
  CALLDEF(_mrgsolve_dcorr,1),
  CALLDEF(_model_housemodel_main__,MRGSOLVE_INIT_SIGNATURE_N),
  CALLDEF(_model_housemodel_ode__,MRGSOLVE_ODE_SIGNATURE_N),
  CALLDEF(_model_housemodel_table__,MRGSOLVE_TABLE_SIGNATURE_N),
  CALLDEF(_model_housemodel_config__,MRGSOLVE_CONFIG_SIGNATURE_N),
  {NULL, NULL, 0}
};

RcppExport void R_unload_mrgsolve(DllInfo *dll) {} 

RcppExport void R_init_mrgsolve(DllInfo* dll) {
  R_registerRoutines(dll,NULL,callEntryPoints,NULL,NULL);
  R_useDynamicSymbols(dll, FALSE);
}

#undef CALLDEF
