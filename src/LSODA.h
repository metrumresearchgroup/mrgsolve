/***
 *        Created:  2018-08-14
 
 *         Author:  Dilawar Singh <dilawars@ncbs.res.in>
 *   Organization:  NCBS Bangalore
 *        License:  MIT License
 */

#include <RcppArmadillo.h>

#ifndef LSODE_H
#define LSODE_H

#include <array>
#include <cmath>
#include <memory>
#include <vector>
class odeproblem;
typedef odeproblem*  dtype;

using namespace std;

/* --------------------------------------------------------------------------*/
/**
 * @Synopsis  Type definition of LSODA ode system. See the file test_LSODA.cpp
 * for an example.
 *
 * @Param time, double
 * @Param y, array of double.
 * @Param dydt, array of double
 * @Param data, dtype 
 *
 * @Returns void
 */
/* ----------------------------------------------------------------------------*/
typedef void (*LSODA_ODE_SYSTEM_TYPE)(double t, double *y, double *dydt, 
              dtype _data);
typedef void main_deriv_func(int* neq, double* t,double* y,double* ydot);
//typedef void (*ODE_FUNC)(double t, double *y, double *dydt, double*);
typedef void (*MRGSOLVE_ODE_FUNC)(int neq, double* t, double* y, double* 
              ydot, std::vector<double>& param);

class LSODA
{
    
public:
    LSODA(int neq, const Rcpp::S4& parin);
    
    ~LSODA();

    int iopt; 
    int jt;
    int itask;
    double Atol; 
    double Rtol;
    int Maxsteps;
    int Neq;

    void hmax_(const double value);
    void hmin_(const double value);
    void maxsteps_(const int value);
    void ixpr_(const int value);
    void mxhnil_(const int value);

    array<int, 7> iworks;// iworks = {{0}};
    array<double, 4> rworks;// = {{0.0}};
    
    size_t idamax1(const vector<double> &dx, const size_t n, const size_t offset);
    
    void dscal1(const double da, vector<double> &dx, const size_t n,
                const size_t offset);
    
    double ddot1(const vector<double> &a, const vector<double> &b, const size_t n,
                 const size_t offsetA, const size_t offsetB);
    
    void daxpy1(const double da, const vector<double> &dx, vector<double> &dy,
                const size_t n, const size_t offsetX,
                const size_t offsetY);
    
    void dgesl1(const vector<vector<double>> &a, const size_t n, vector<int> &ipvt,
               vector<double> &b, const size_t job);
    
    void dgefa1(vector<vector<double>> &a, const size_t n, vector<int> &ipvt,
               size_t *const info);
    
    void prja(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
              dtype _data);
    
    void lsoda(LSODA_ODE_SYSTEM_TYPE f, const size_t neq, vector<double> &y,
               double *t, double tout, int itask, int *istate, int iopt, int jt,
               //array<int, 7> &iworks, array<double, 4> &rworks,
               dtype _data);
    
    void correction(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
                    size_t *corflag, double pnorm, double *del, double *delp,
                    double *told, size_t *ncf, double *rh, size_t *m,
                    dtype _data);
    
    void stoda(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
               dtype _data);
    
    // We call this function in VoxelPools::
    void lsoda_update(LSODA_ODE_SYSTEM_TYPE f, 
                      const int neq,
                      vector<double> &y, 
                      std::vector<double> &yout, 
                      double *t,
                      const double tout, 
                      int *istate,
                      //dtype const _data
                      dtype const _data
    );
    
    void terminate(int *istate);
    void terminate2(vector<double> &y, double *t);
    void successreturn(vector<double> &y, double *t, int itask, int ihit,
                       double tcrit, int *istate);
    void _freevectors(void);
    void ewset(const vector<double> &ycur);
    void resetcoeff(void);
    void solsy(vector<double> &y);
    void endstoda(void);
    void orderswitch(double *rhup, double dsm, double *pdh, double *rh,
                     size_t *orderflag);
    void intdy(double t, int k, vector<double> &dky, int *iflag);
    void corfailure(double *told, double *rh, size_t *ncf, size_t *corflag);
    void methodswitch(double dsm, double pnorm, double *pdh, double *rh);
    void cfode(int meth_);
    void scaleh(double *rh, double *pdh);
    double fnorm(int n, const vector<vector<double>> &a, const vector<double> &w);
    double vmnorm(const size_t n, const vector<double> &v,
                  const vector<double> &w);
    
    static bool abs_compare(double a, double b);
    // WAS PRIVATE    
    // const size_t neq;
    int kflag=0, jstart=0;
private:
    size_t ml=0, mu=0, imxer=0;
    double sqrteta=0.0;
    
    // NOTE: initialize in default constructor. Older compiler e.g. 4.8.4 would
    // produce error if these are initialized here. With newer compiler,
    // initialization can be done here.
    array<size_t, 3> mord;
    array<double, 13> sm1;
    
    array<double, 14> el;  // = {0};
    array<double, 13> cm1; // = {0};
    array<double, 6> cm2;  // = {0};
    
    array<array<double, 14>, 13> elco;
    array<array<double, 4>, 13> tesco;
    
    vector<double> ewt;
    vector<double> savf;
    vector<double> acor;
    vector<vector<double>> yh_;
    vector<vector<double>> wm_;
    vector<int> ipvt;
    
    size_t illin=0, init=0, ierpj=0, iersl=0, jcur=0; 
    size_t l=0, miter=0, maxord=0, maxcor=0, msbp=0, mxncf=0;

    size_t ixpr=0, jtyp=0, mused=0, mxordn=0, mxords = 12;
    size_t meth_=0;
    
    size_t n=0, nq=0, nst=0, nfe=0, nje=0, nqu=0;
    size_t mxstep=0, mxhnil=0;
    size_t nslast=0, nhnil=0, ntrep=0, nyh=0;
    
    double ccmax=0.0, el0=0.0, h_ = .0;
    double hmin=0.0, hmxi=0.0, hu=0.0, rc=0.0, tn_ = 0.0;
    double tsw=0.0, pdnorm=0.0;
    double conit=0.0, crate=0.0, hold=0.0, rmax=0.0;
    
    size_t ialth=0, ipup=0, lmax=0;
    size_t nslp=0;
    double pdest=0.0, pdlast=0.0, ratio=0.0;
    int icount=0, irflag=0;
    
private:
    int itol_ = 2;
    std::vector<double> rtol_;
    std::vector<double> atol_;
    
    // public:
    //     dtype param = nullptr;
};

#endif /* end of include guard: LSODE_H */
