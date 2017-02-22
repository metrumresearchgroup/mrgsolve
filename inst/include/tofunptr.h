// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

/**
 * @file tofunptr.h
 */


#ifndef TOFUNPTR_H
#define TOFUNPTR_H
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
//typedef void * (*DL_FUNC)();
DL_FUNC tofunptr(SEXP a);
#endif

