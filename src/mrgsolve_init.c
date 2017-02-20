#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

SEXP mrgsolve_get_tokens(SEXP);
SEXP mrgsolve_MVGAUSS(SEXP,SEXP);
SEXP mrgsolve_DEVTRAN(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,
                      SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP mrgsolve_SUPERMATRIX(SEXP,SEXP);
SEXP mrgsolve_TOUCH_FUNS(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP mrgsolve_QUICKSIM(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
SEXP mrgsolve_EXPAND_EVENTS(SEXP,SEXP,SEXP);
SEXP mrgsolve_from_to(SEXP,SEXP,SEXP,SEXP);
void mrgsolve_ZERO(SEXP);
void mrgsolve_decorr(SEXP);


static R_CallMethodDef callEntries[]  = {
  CALLDEF(mrgsolve_get_tokens,1),
  CALLDEF(mrgsolve_MVGAUSS,2),
  CALLDEF(mrgsolve_DEVTRAN,12),
  CALLDEF(mrgsolve_SUPERMATRIX,2),
  CALLDEF(mrgsolve_TOUCH_FUNS,7),
  CALLDEF(mrgsolve_QUICKSIM,11),
  CALLDEF(mrgsolve_EXPAND_EVENTS,3),
  CALLDEF(mrgsolve_from_to,4),
  CALLDEF(mrgsolve_ZERO,1),
  CALLDEF(mrgsolve_decorr,1),
  {NULL, NULL, 0}
};

void init_mrgsolve_routines(DllInfo *info){
  R_registerRoutines(info,
                     NULL,            // .C
                     callEntries,     // .Call
                     NULL,            // .Fortran
                     NULL             // .External
  );
}


void R_unload_mrgsolve(DllInfo *info) {  // #nocov start

} 

void R_init_mrgsolve(DllInfo* info) {
  init_mrgsolve_routines(info);   // init routines
  R_useDynamicSymbols(info, TRUE);
}

