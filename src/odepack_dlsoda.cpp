
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

  xlrwork = std::max(int(20 + 16 * neq_),int(22 + 9  * neq_ + pow(neq_,2)));
  xliwork = 20+neq_;

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


