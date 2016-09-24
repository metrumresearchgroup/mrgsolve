// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.



#ifndef ODEPACK_DLSODA_H
#define ODEPACK_DLSODA_H
#include <math.h>
//#include <iostream>
#include <vector>



class odepack_dlsoda {


 public:

  odepack_dlsoda(int npar_, int neq_);
  virtual ~odepack_dlsoda();

  // SOLVER SETTINGS
  void hmax(double value){xrwork[5] = value; if(value !=0) xiopt=1;}
  void hmin(double value){xrwork[6] = value;  if(value !=0) xiopt=1;}
  void ixpr(int value){xiwork[4] = value; if(value !=0) xiopt=1;}
  void maxsteps(int value){xiwork[5] = value; if(value !=0) xiopt=1;}
  void mxhnil(int value){xiwork[6] = value; if (value !=0) xiopt=1;}
  //double hmin(){return xrwork[6];}
  //double hmax(){return xrwork[5];}
  //int maxsteps(){return xiwork[5];}
  //int ixpr(){return xiwork[4];}
  //int mxhnil(){return xiwork[6];}
  int istate(){return xistate;}
  void istate(int value){xistate = value;}
  void lsoda_init(){xistate=1;}
  int itask(){return xitask;}
  void itask(int itask){xitask = itask;}
  //void usingevents(){xitask = 4; xusingevents=1;}
  //int iopt(){return xiopt;}
  //int itol(){return xitol;}
  //int jt(){return xjt;}
  void atol(double atol){xatol=atol;}
  void rtol(double rtol){xrtol=rtol;}
  //double atol(){return xatol;}
  //double rtol(){return xrtol;}
  void tol(double atol, double rtol);


  // Work arrays:
  double * rwork(){return xrwork;}
  void rwork( int pos,  double value){xrwork[pos] = value;}
  int * iwork(){return xiwork;}
  void iwork( int pos,  int value){xiwork[pos] = value;}
  void tcrit(double value){xrwork[0] = value;}
  //int liwork(){return xliwork;}
  //int lrwork(){return xlrwork;}

 // Number of parameters/equations:
  int npar() {return Npar;}
  int neq(){return Neq;}

  // y:
  //! return pointer to array current state
  double * y(){return Y;}

  //! set the values of state variables
  void y(const int pos, const double value){Y[pos] = value;}

  //! get the value of different state variables
  double y(const int pos){return Y[pos];}

  // ydot:
  double * ydot(){return Ydot;}


 protected :
  //! Number of equations
  int xliwork, xlrwork,xistate,xitask,xiopt, xitol;
  int Neq, Npar;
  int xjt,xusingevents;
  //! absolute tolerance
  double xatol,xrtol;
  double * xrwork;
  int * xiwork;
  double * Y;
  double * Ydot;
};



#endif



