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
 * @file odepack_dlsoda.h
 */


#ifndef ODEPACK_DLSODA_H
#define ODEPACK_DLSODA_H
#include <math.h>

class odepack_dlsoda {

public:
  
  odepack_dlsoda(int npar_, int neq_);
  virtual ~odepack_dlsoda();
  
  void hmax(double value){xrwork[5] = value; if(value !=0) xiopt=1;}
  void hmin(double value){xrwork[6] = value;  if(value !=0) xiopt=1;}
  void ixpr(int value){xiwork[4] = value; if(value !=0) xiopt=1;}
  void maxsteps(int value){xiwork[5] = value; if(value !=0) xiopt=1;}
  void mxhnil(int value){xiwork[6] = value; if (value !=0) xiopt=1;}
  int  istate(){return xistate;}
  void istate(int value){xistate = value;}
  void lsoda_init(){xistate=1;}
  int  itask(){return xitask;}
  void itask(int itask){xitask = itask;}
  void tol(double atol, double rtol);
  
  double* rwork(){return xrwork;}
  void    rwork(int pos, double value){xrwork[pos] = value;}
  int*    iwork(){return xiwork;}
  void    iwork(int pos,int value){xiwork[pos] = value;}
  void    tcrit(double value){xrwork[0] = value;}
  double* y(){return Y;}
  void    y(const int pos, const double value){Y[pos] = value;}
  double  y(const int pos){return Y[pos];}
  double* ydot(){return Ydot;}
  
  int     npar() {return Npar;}
  int     neq(){return Neq;}
  
protected :

  int     xliwork; ///< length of iwork array
  int     xlrwork; ///< length of rwork array
  int     xistate; ///< istate value
  int     xitask; ///< itask value
  int     xiopt; ///< iopt value
  int     xitol; ///< itol value
  int     Neq; ///< number of state variables
  int     Npar; ///< number of model parameters
  int     xjt; ///< jacobian indicator
  double  xatol; ///< absolute tolerance
  double  xrtol; ///< relative tolerance
  double* xrwork; ///< rwork array
  int*    xiwork; ///< iwork array
  double* Y;  ///< current value of state variables
  double* Ydot; ///< current value of ODEs
};


#endif



