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
 * 
 * @file odepack_dlsoda.cpp
 * 
 */

#include "odepack_dlsoda.h"
#include <algorithm>

odepack_dlsoda::odepack_dlsoda(int npar_, int neq_) {

  Npar = npar_;
  Neq = neq_;

  Y = new double[neq_]();
  Ydot = new double[neq_]();

  xlrwork = std::max(int(20 + 16 * neq_),int(22 + 9  * neq_ + neq_ * neq_));
  xliwork = 20 + neq_;

  xrwork = new double[xlrwork];
  xiwork = new int [xliwork];

  xrwork[0] = 0.0;
  xrwork[4] = 0.0;      // h0
  xrwork[5] = 0.0;      // hmax
  xrwork[6] = 0.0;      // hmin
 

  xiwork[0] = 0;
  xiwork[1] = 0;
  xiwork[4] = 0;     // IXPR print if switch
  xiwork[5] = 500 ; //maxsteps_;
  xiwork[6] = 1;    //max number of prints
  xiwork[7] = 12;  // maxordn
  xiwork[8] = 5;  // maxords

  xitol = 1;
  xiopt = 0;
  xitask = 1;
  xistate = 1;
  xjt = 2;
  xatol = 1E-8;
  xrtol = 1E-8;
}


odepack_dlsoda::~odepack_dlsoda(){
  delete [] Y;
  delete [] Ydot;
  delete [] xrwork;
  delete [] xiwork;
}



void odepack_dlsoda::tol(double atol, double rtol) {
  xatol = atol;
  xrtol = rtol;
}


