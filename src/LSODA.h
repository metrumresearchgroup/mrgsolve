/***
 *        Created:  2018-08-14
 
 *         Author:  Dilawar Singh <dilawars@ncbs.res.in>
 *   Organization:  NCBS Bangalore
 *        License:  MIT License
 */

#ifndef LSODE_H
#define LSODE_H

#include <array>
#include <cmath>
#include <memory>
#include <vector>

using namespace std;

/* --------------------------------------------------------------------------*/
/**
 * @Synopsis  Type definition of LSODA ode system. See the file test_LSODA.cpp
 * for an example.
 *
 * @Param time, double
 * @Param y, array of double.
 * @Param dydt, array of double
 * @Param data, void*
 *
 * @Returns void
 */
/* ----------------------------------------------------------------------------*/
typedef void (*LSODA_ODE_SYSTEM_TYPE)(double t, double *y, double *dydt, void* _data);
typedef void main_deriv_func(int* neq, double* t,double* y,double* ydot);
//typedef void (*ODE_FUNC)(double t, double *y, double *dydt, double*);
typedef void (*MRGSOLVE_ODE_FUNC)(int neq, double* t, double* y, double* ydot, std::vector<double>& param);

class LSODA
{
    
public:
    LSODA(int neq);
    LSODA(){};
    ~LSODA();
    MRGSOLVE_ODE_FUNC odes;
    int iopt; 
    int jt;
    int itask;
    int itol;
    
    void hmax_(const double value);
    void hmin_(const double value);
    void maxsteps_(const int value);
    void ixpr_(const int value);
    void mxhnil_(const int value);
    void copy_parin(const Rcpp::List& parin);
    double Atol; 
    double Rtol;
    int Maxsteps;
    int Neq;

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
    
    void dgesl(const vector<vector<double>> &a, const size_t n, vector<int> &ipvt,
               vector<double> &b, const size_t job);
    
    void dgefa(vector<vector<double>> &a, const size_t n, vector<int> &ipvt,
               size_t *const info);
    
    void prja(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
              void *_data);
    
    void lsoda(LSODA_ODE_SYSTEM_TYPE f, const size_t neq, vector<double> &y,
               double *t, double tout, int itask, int *istate, int iopt, int jt,
               //array<int, 7> &iworks, array<double, 4> &rworks,
               void *_data);
    
    void correction(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
                    size_t *corflag, double pnorm, double *del, double *delp,
                    double *told, size_t *ncf, double *rh, size_t *m,
                    void *_data);
    
    void stoda(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
               void *_data);
    
    // We call this function in VoxelPools::
    void lsoda_update(LSODA_ODE_SYSTEM_TYPE f, 
                      const int neq,
                      vector<double> &y, 
                      std::vector<double> &yout, 
                      double *t,
                      const double tout, 
                      int *istate,
                      void *const _data
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
    
    // const size_t neq;
    
private:
    size_t ml, mu, imxer;
    double sqrteta;
    
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
    
    size_t illin, init, ierpj, iersl, jcur, l, miter, maxord, maxcor, msbp, mxncf;
    
    int kflag, jstart;
    
    size_t ixpr=0, jtyp, mused, mxordn, mxords = 12;
    size_t meth_;
    
    size_t n, nq, nst, nfe, nje, nqu;
    size_t mxstep, mxhnil;
    size_t nslast, nhnil, ntrep, nyh;
    
    double ccmax, el0, h_ = .0;
    double hmin, hmxi, hu, rc, tn_ = 0.0;
    double tsw, pdnorm;
    double conit, crate, hold, rmax;
    
    size_t ialth, ipup, lmax;
    size_t nslp;
    double pdest, pdlast, ratio;
    int icount, irflag;
    
    vector<double> ewt;
    vector<double> savf;
    vector<double> acor;
    vector<vector<double>> yh_;
    vector<vector<double>> wm_;
    
    vector<int> ipvt;
    
private:
    int itol_ = 2;
    std::vector<double> rtol_;
    std::vector<double> atol_;
    
    // public:
    //     void *param = nullptr;
};

#endif /* end of include guard: LSODE_H */
