/*
 * Please see the full history of this code, including below. 
 * 
 * LSODA.cpp and LSODA.h were obtained from
 * https://github.com/dilawar/libsoda
 * in September, 2019 and incorporated into mrgsolve with 
 * minor modifications. 
 * 
 * Modifications: 
 * - Added several members that were previously being passed in to 
 *   lsoda_update().
 * - Added several functions for setting new members.
 * - Replaced fprint statements to REfprint for CRAN compliance.
 * - Replaced output to cerr with REfprint.
 * - Added Rcpp::exception when terminate or terminate2 are called. 
 * - Removed assert stataements for CRAN compliance
 * - int and double members explicitly initialized mrgsolve #546
 * - changed dgesl to dgesl1 and dgefa to dgefa1
 * - fixed a big where pointer to ncf got incremented mrgsolve #542
 * 
 */

/*
 * HISTORY:
 * This is a CPP version of the LSODA library for integration into MOOSE
 somulator.
 * The original was aquired from
 * http://www.ccl.net/cca/software/SOURCES/C/kinetics2/index.shtml and modified
 by
 * Heng Li <lh3lh3@gmail.com>. Heng merged several C files into one and added a
 * simpler interface. [Available
 here](http://lh3lh3.users.sourceforge.net/download/lsoda.c)
 
 * The original source code came with no license or copyright
 * information. Heng Li released his modification under the MIT/X11 license. I
 * maintain the same license. I have removed quite a lot of text/comments from
 * this library. Please refer to the standard documentation.
 *
 * Contact: Dilawar Singh <dilawars@ncbs.res.in>
 */

#include <algorithm>
#include <cmath>
#include <iostream>
#include <memory>
#include <numeric>
#include <vector>

#include "LSODA.h"
#include "lsoda_functions.h"

using namespace std;

#define ETA 2.2204460492503131e-16

LSODA::~LSODA() {}

LSODA::LSODA(int neq_, const Rcpp::S4& mod) {
  if(neq_ <  0) {
    throw Rcpp::exception(
        tfm::format(
          "[lsoda] neq = %i is less than zero.\n", neq_
        ).c_str(),
        false
    );
  }
  // Initialize arrays.
  mord = {{12, 5}};
  sm1 = {{
    0., 0.5, 0.575, 0.55, 0.45, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075, 0.05,
    0.025
    }};
  el = {{0}};
  cm1 = {{0}};
  cm2 = {{0}};
  iworks = {{0}};
  iworks[6] = 5;
  iworks[5] = 12;
  rworks = {{0.0}};
  itask = 1;
  iopt = 0;
  jt = 2;
  Neq = neq_;
  hmax_(Rcpp::as<double>(mod.slot("hmax")));
  hmin_(Rcpp::as<double>(mod.slot("hmin")));
  maxsteps_(Rcpp::as<int>(mod.slot("maxsteps")));
  ixpr_(Rcpp::as<int>(mod.slot("ixpr")));
  mxhnil_(Rcpp::as<int>(mod.slot("mxhnil")));
  itol_ = 1;
  rtol_.assign(2, Rcpp::as<double>(mod.slot("rtol")));
  atol_.assign(2, Rcpp::as<double>(mod.slot("atol")));
  Rtol = rtol_[1];
  Atol = atol_[1];
  rtol_[0] = 0;
  atol_[0] = 0;
}

/* --------------------------------------------------------------------------*/
/**
 * @Synopsis  Simpler interface.
 *
 * @Param f System
 * @Param neq, size of system.
 * @Param y, init values of size neq
 * @Param yout, results vector for size neq+1, ignore yout[0]
 * @Param t, start time.
 * @Param tout, stop time.
 * @Param istate state of the system
 * @Param _data
 */
/* ----------------------------------------------------------------------------*/
void LSODA::lsoda_update(LSODA_ODE_SYSTEM_TYPE f, 
                         const int neq,
                         vector<double>& y, 
                         vector<double>& yout, 
                         double *t,
                         const double tout, 
                         int *istate,
                         dtype const _data)
{
  // array<int, 7> iworks = {{0}};
  // array<double, 4> rworks = {{0.0}};
  
  if(*t==tout || neq < 1) return;
  
  // int itask, iopt, jt;
  // 
  // itask = 1;
  // iopt = 0;
  // jt = 2;
  
  // Fill-in values.
  // for (size_t i = 1; i <= neq; ++i) {
  //   yout[i] = y[i - 1];
  // }
  std::copy(y.begin(), y.end(), 1+yout.begin());
  
  lsoda(f, neq, yout, t, tout, itask, istate, iopt, jt, _data);
  
  std::copy(1+yout.begin(), yout.end(), y.begin());
  // 
  // for(size_t i = 1; i <= neq; ++i) {
  //   y[i - 1] = yout[i];
  // }
}

void LSODA::lsoda(LSODA_ODE_SYSTEM_TYPE f, const size_t neq, vector<double> &y,
                  double *t, double tout, int itask, int *istate, int iopt,
                  int jt, dtype _data)
{
  //assert(tout > *t);
  
  int mxstp0 = 500, mxhnl0 = 10;
  
  int iflag = 0, lenyh = 0, ihit = 0;
  
  double atoli = 0, ayi = 0, big = 0, h0 = 0, hmax = 0, hmx = 0, rh = 0,
    rtoli = 0, tcrit = 0, tdist = 0, tnext = 0, tol = 0, tolsf = 0, tp = 0,
    size = 0, sum = 0, w0 = 0;
  
  /*
   Block a.
   This code block is executed on every call.
   It tests *istate and itask for legality and branches appropriately.
   If *istate > 1 but the flag init shows that initialization has not
   yet been done, an error return occurs.
   If *istate = 1 and tout = t, return immediately.
   */
  
  if (*istate < 1 || *istate > 3)
  {
    // fprintf(stderr, "[lsoda] illegal istate = %d\n", *istate);
    //cerr << "[lsoda] illegal istate = " << *istate << endl;
    REprintf("[lsoda] illegal istate = %d\n", *istate);
    terminate(istate);
    return;
  }
  if (itask < 1 || itask > 5)
  {
    //fprintf(stderr, "[lsoda] illegal itask = %d\n", itask);
    REprintf("[lsoda] illegal itask = %d\n", itask);
    terminate(istate);
    return;
  }
  if (init == 0 && (*istate == 2 || *istate == 3))
  {
    REprintf("[lsoda] istate > 1 but lsoda not initialized\n");
    terminate(istate);
    return;
  }
  
  /*
   Block b.
   The next code block is executed for the initial call ( *istate = 1 ),
   or for a continuation call with parameter changes ( *istate = 3 ).
   It contains checking of all inputs and various initializations.
   
   First check legality of the non-optional inputs neq, itol, iopt,
   jt, ml, and mu.
   */
  
  if (*istate == 1 || *istate == 3)
  {
    ntrep = 0;
    if (neq <= 0)
    {
      Rcpp::Rcerr << "[lsoda] neq = " << neq << " is less than 1." 
                  << std::endl;
      //REprintf("[lsoda] neq = %zu is less than 1.\n", neq);
      terminate(istate);
      return;
    }
    if (*istate == 3 && neq > n)
    {
      //cerr << "[lsoda] istate = 3 and neq increased" << endl;
      REprintf("[lsoda] istate = 3 and neq increased.\n");
      terminate(istate);
      return;
    }
    n = neq;
    if (itol_ < 1 || itol_ > 4)
    {
      //cerr << "[lsoda] itol = " << itol_ << " illegal" << endl;
      REprintf("[lsoda] itol = %i illegal.\n", itol_);
      terminate(istate);
      return;
    }
    if (iopt < 0 || iopt > 1)
    {
      //cerr << "[lsoda] iopt = " << iopt << " illegal" << endl;
      REprintf("[lsoda] iopt = %i illegal.\n", iopt);
      terminate(istate);
      return;
    }
    if (jt == 3 || jt < 1 || jt > 5)
    {
      //cerr << "[lsoda] jt = " << jt << " illegal" << endl;
      REprintf("[lsoda] jt = %i illegal.\n", jt);
      terminate(istate);
      return;
    }
    jtyp = jt;
    if (jt > 2)
    {
      ml = iworks[0];
      mu = iworks[1];
      if (ml >= n)
      {
        Rcpp::Rcerr << "[lsoda] ml = " << ml << " not between 1 and neq."
                    << std::endl;
        //REprintf("[lsoda] ml = %zu not between 1 and neq.\n", ml);
        terminate(istate);
        return;
      }
      if (mu >= n)
      {
        Rcpp::Rcerr << "[lsoda] mu = " << mu << " not between 1 and neq." 
                    << std::endl;
        //REprintf("[lsoda] mu = %zu not between 1 and neq.\n", mu);
        terminate(istate);
        return;
      }
    }
    
    /* Next process and check the optional inpus.   */
    /* Default options.   */
    if (iopt == 0)
    {
      ixpr = 0;
      mxstep = mxstp0;
      mxhnil = mxhnl0;
      hmxi = 0.;
      hmin = 0.;
      if (*istate == 1)
      {
        h0 = 0.;
        mxordn = mord[0];
        mxords = mord[1];
      }
    }
    /* end if ( iopt == 0 )   */
    /* Optional inputs.   */
    else /* if ( iopt = 1 )  */
    {
      ixpr = iworks[2];
      if (ixpr > 1)
      {
        Rcpp::Rcerr << "[lsoda] ixpr = " << ixpr << " is illegal." << std::endl;
        //REprintf("[lsoda] ixpr =%zu is illegal.\n", ixpr);
        terminate(istate);
        return;
      }
      
      mxstep = iworks[3];
      if (mxstep == 0)
        mxstep = mxstp0;
      mxhnil = iworks[4];
      
      if (*istate == 1)
      {
        h0 = rworks[1];
        mxordn = iworks[5];
        
        if (mxordn == 0)
          mxordn = 100; // 12
        //mxordn = 100;
        
        mxordn = min(mxordn, mord[0]);
        mxords = iworks[6];
        
        // if mxords is not given use 100.
        if (mxords == 0)
          mxords = 100; // 5
        //mxords = 100;
        mxords = min(mxords, mord[1]);
        
        if ((tout - *t) * h0 < 0.)
        {
          // cerr << "[lsoda] tout = " << tout << " behind t = " << *t
          //      << ". integration direction is given by " << h0 << endl;
          REprintf("[lsoda] tout = %f behind t = %f integration direction is given by %f.\n", 
                   tout, *t, h0);
          terminate(istate);
          return;
        }
      } /* end if ( *istate == 1 )  */
    hmax = rworks[2];
      if (hmax < 0.)
      {
        //cerr << "[lsoda] hmax < 0." << endl;
        REprintf("[lsoda] hmax < 0.\n");
        terminate(istate);
        return;
      }
      hmxi = 0.;
      if (hmax > 0)
        hmxi = 1. / hmax;
      
      hmin = rworks[3];
      if (hmin < 0.)
      {
        //cerr << "[lsoda] hmin < 0." << endl;
        REprintf("[lsoda] hmin < 0.\n");
        terminate(istate);
        return;
      }
    } /* end else   */ /* end iopt = 1   */
  }                    /* end if ( *istate == 1 || *istate == 3 )   */
    /*
     If *istate = 1, meth_ is initialized to 1.
     
     Also allocate memory for yh_, wm_, ewt, savf, acor, ipvt.
     */
    if (*istate == 1)
    {
      /*
       If memory were not freed, *istate = 3 need not reallocate memory.
       Hence this section is not executed by *istate = 3.
       */
      sqrteta = sqrt(ETA);
      meth_ = 1;
      
      nyh = n;
      lenyh = 1 + max(mxordn, mxords);
      
      yh_.resize(lenyh + 1, std::vector<double>(nyh + 1, 0.0));
      wm_.resize(nyh + 1, std::vector<double>(nyh + 1, 0.0));
      ewt.resize(1 + nyh, 0);
      savf.resize(1 + nyh, 0);
      acor.resize(nyh + 1, 0.0);
      ipvt.resize(nyh + 1, 0.0);
    }
    /*
     Check rtol and atol for legality.
     */
    if (*istate == 1 || *istate == 3)
    {
      rtoli = rtol_[1];
      atoli = atol_[1];
      for (size_t i = 1; i <= n; ++i)
      {
        if (itol_ >= 3)
          rtoli = rtol_[i];
        if (itol_ == 2 || itol_ == 4)
          atoli = atol_[i];
        if (rtoli < 0.)
        {
          //fprintf(stderr, "[lsoda] rtol = %g is less than 0.\n", rtoli);
          REprintf("[lsoda] rtol = %g is less than 0.\n", rtoli);
          terminate(istate);
          return;
        }
        if (atoli < 0.)
        {
          //fprintf(stderr, "[lsoda] atol = %g is less than 0.\n", atoli);
          REprintf("[lsoda] atol = %g is less than 0.\n", atoli);
          terminate(istate);
          return;
        }
      } /* end for   */
    }   /* end if ( *istate == 1 || *istate == 3 )   */
    
    /* If *istate = 3, set flag to signal parameter changes to stoda. */
    if (*istate == 3)
    {
      jstart = -1;
    }
    /*
     Block c.
     The next block is for the initial call only ( *istate = 1 ).
     It contains all remaining initializations, the initial call to f,
     and the calculation of the initial step size.
     The error weights in ewt are inverted after being loaded.
     */
    if (*istate == 1)
    {
      tn_ = *t;
      tsw = *t;
      maxord = mxordn;
      if (itask == 4 || itask == 5)
      {
        tcrit = rworks[0];
        if ((tcrit - tout) * (tout - *t) < 0.)
        {
          //fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tout\n");
          REprintf("[lsoda] itask = 4 or 5 and tcrit behind tout\n");
          terminate(istate);
          return;
        }
        if (h0 != 0. && (*t + h0 - tcrit) * h0 > 0.)
          h0 = tcrit - *t;
      }
      
      jstart = 0;
      nhnil = 0;
      nst = 0;
      nje = 0;
      nslast = 0;
      hu = 0.;
      nqu = 0;
      mused = 0;
      miter = 0;
      ccmax = 0.3;
      maxcor = 3;
      msbp = 20;
      mxncf = 10;
      
      /* Initial call to f.  */
      if(int(yh_.size()) != (lenyh + 1)) {
        Rcpp::stop("[lsoda] inputs are not the right size.");  
      }
      if(yh_[0].size() != (nyh + 1)) {
        Rcpp::stop("[lsoda] inputs are not the right size."); 
      }
      
      (*f)(*t, &y[1], &yh_[2][1], _data);
      nfe = 1;
      
      /* Load the initial value vector in yh_.  */
      for (size_t i = 1; i <= n; ++i)
        yh_[1][i] = y[i];
      
      /* Load and invert the ewt array.  ( h_ is temporarily set to 1. ) */
      nq = 1;
      h_ = 1.;
      ewset(y);
      for (size_t i = 1; i <= n; ++i)
      {
        if (ewt[i] <= 0.)
        {
          Rcpp::Rcerr << "[lsoda] ewt[" << i << "] = " << ewt[i] << " <= 0." 
                      << std::endl;
          //REprintf("[lsoda] ewt[%zu] = %g <= 0.\n", i, ewt[i]);
          terminate2(y, t);
          return;
        }
        ewt[i] = 1. / ewt[i];
      }
      
      /*
       The coding below computes the step size, h0, to be attempted on the
       first step, unless the user has supplied a value for this.
       First check that tout - *t differs significantly from zero.
       A scalar tolerance quantity tol is computed, as max(rtol[i])
       if this is positive, or max(atol[i]/fabs(y[i])) otherwise, adjusted
       so as to be between 100*ETA and 0.001.
       Then the computed value h0 is given by
       
       h0^(-2) = 1. / ( tol * w0^2 ) + tol * ( norm(f) )^2
       
       where   w0     = max( fabs(*t), fabs(tout) ),
       f      = the initial value of the vector f(t,y), and
       norm() = the weighted vector norm used throughout, given by
       the vmnorm function routine, and weighted by the
       tolerances initially loaded into the ewt array.
       
       The sign of h0 is inferred from the initial values of tout and *t.
       fabs(h0) is made < fabs(tout-*t) in any case.
       */
      if (h0 == 0.)
      {
        tdist = fabs(tout - *t);
        w0 = max(fabs(*t), fabs(tout));
        if (tdist < 2. * ETA * w0)
        {
          //fprintf(stderr, "[lsoda] tout too close to t to start integration\n ");
          REprintf("[lsoda] tout too close to t to start integration\n ");
          terminate(istate);
          return;
        }
        tol = rtol_[1];
        if (itol_ > 2)
        {
          for (size_t i = 2; i <= n; ++i)
            tol = max(tol, rtol_[i]);
        }
        if (tol <= 0.)
        {
          atoli = atol_[1];
          for (size_t i = 1; i <= n; ++i)
          {
            if (itol_ == 2 || itol_ == 4)
              atoli = atol_[i];
            ayi = fabs(y[i]);
            if (ayi != 0.)
              tol = max(tol, atoli / ayi);
          }
        }
        tol = max(tol, 100. * ETA);
        tol = min(tol, 0.001);
        sum = vmnorm(n, yh_[2], ewt);
        sum = 1. / (tol * w0 * w0) + tol * sum * sum;
        h0 = 1. / sqrt(sum);
        h0 = min(h0, tdist);
        h0 = h0 * ((tout - *t >= 0.) ? 1. : -1.);
      } /* end if ( h0 == 0. )   */
      /*
       Adjust h0 if necessary to meet hmax bound.
       */
      rh = fabs(h0) * hmxi;
      if (rh > 1.)
        h0 /= rh;
      
      /*
       Load h_ with h0 and scale yh_[2] by h0.
       */
      h_ = h0;
      for (size_t i = 1; i <= n; ++i)
        yh_[2][i] *= h0;
    } /* if ( *istate == 1 )   */
      /*
       Block d.
       The next code block is for continuation calls only ( *istate = 2 or 3 )
       and is to check stop conditions before taking a step.
       */
      if (*istate == 2 || *istate == 3)
      {
        nslast = nst;
        switch (itask)
        {
        case 1:
          if ((tn_ - tout) * h_ >= 0.)
          {
            intdy(tout, 0, y, &iflag);
            if (iflag != 0)
            {
              // fprintf(stderr, "[lsoda] trouble from intdy, itask = %d, tout = %g\n",
              //         itask, tout);
              REprintf("[lsoda] trouble from intdy, itask = %d, tout = %g\n",
                       itask, tout);
              terminate(istate);
              return;
            }
            *t = tout;
            *istate = 2;
            illin = 0;
            return;
          }
          break;
        case 2:
          break;
        case 3:
          tp = tn_ - hu * (1. + 100. * ETA);
          if ((tp - tout) * h_ > 0.)
          {
            // fprintf(stderr, "[lsoda] itask = %d and tout behind tcur - hu\n",
            //         itask);
            REprintf("[lsoda] itask = %d and tout behind tcur - hu\n",
                     itask);
            terminate(istate);
            return;
          }
          if ((tn_ - tout) * h_ < 0.)
            break;
          successreturn(y, t, itask, ihit, tcrit, istate);
          return;
        case 4:
          tcrit = rworks[0];
          if ((tn_ - tcrit) * h_ > 0.)
          {
            //fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
            REprintf("[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
            terminate(istate);
            return;
          }
          if ((tcrit - tout) * h_ < 0.)
          {
            //fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tout\n");
            REprintf("[lsoda] itask = 4 or 5 and tcrit behind tout\n");
            terminate(istate);
            return;
          }
          if ((tn_ - tout) * h_ >= 0.)
          {
            intdy(tout, 0, y, &iflag);
            if (iflag != 0)
            {
              // fprintf(stderr, "[lsoda] trouble from intdy, itask = %d, tout = %g\n",
              //         itask, tout);
              REprintf("[lsoda] trouble from intdy, itask = %d, tout = %g\n",
                       itask, tout);
              terminate(istate);
              return;
            }
            *t = tout;
            *istate = 2;
            illin = 0;
            return;
          }
        case 5:
          if (itask == 5)
          {
            tcrit = rworks[0];
            if ((tn_ - tcrit) * h_ > 0.)
            {
              //fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
              REprintf("[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
              terminate(istate);
              return;
            }
          }
          hmx = fabs(tn_) + fabs(h_);
          ihit = fabs(tn_ - tcrit) <= (100. * ETA * hmx);
          if (ihit)
          {
            *t = tcrit;
            successreturn(y, t, itask, ihit, tcrit, istate);
            return;
          }
          tnext = tn_ + h_ * (1. + 4. * ETA);
          if ((tnext - tcrit) * h_ <= 0.)
            break;
          h_ = (tcrit - tn_) * (1. - 4. * ETA);
          if (*istate == 2)
            jstart = -2;
          break;
        } /* end switch   */
      }   /* end if ( *istate == 2 || *istate == 3 )   */
      /*
       Block e.
       The next block is normally executed for all calls and contains
       the call to the one-step core integrator stoda.
       
       This is a looping point for the integration steps.
       
       First check for too many steps being taken, update ewt ( if not at
       start of problem).  Check for too much accuracy being requested, and
       check for h_ below the roundoff level in *t.
       */
      while (1)
      {
        if (*istate != 1 || nst != 0)
        {
          if ((nst - nslast) >= mxstep)
          {
            Rcpp::Rcerr << "[lsoda] " << mxstep 
                        << " steps taken before reaching tout;" 
                        << " consider increasing maxsteps."
                        << std::endl;
            //REprintf("[lsoda] %zu steps taken before reaching tout; consider increasing maxsteps.\n", 
            //         mxstep);
            *istate = -1;
            terminate2(y, t);
            return;
          }
          
          ewset(yh_[1]);
          for (size_t i = 1; i <= n; ++i)
          {
            if (ewt[i] <= 0.)
            {
              Rcpp::Rcerr << "[lsoda] ewt[" << i << "] = " << ewt[i] 
                          << " <= 0." << std::endl;
              //REprintf("[lsoda] ewt[%zu] = %g <= 0.\n", i, ewt[i]);
              *istate = -6;
              terminate2(y, t);
              return;
            }
            ewt[i] = 1. / ewt[i];
          }
        }
        tolsf = ETA * vmnorm(n, yh_[1], ewt);
        if (tolsf > 0.01)
        {
          tolsf = tolsf * 200.;
          if (nst == 0)
          {
            // fprintf(stderr, "lsoda -- at start of problem, too much accuracy\n");
            // fprintf(stderr, "         requested for precision of machine,\n");
            // fprintf(stderr, "         suggested scaling factor = %g\n", tolsf);
            REprintf("[lsoda] at start of problem, too much accuracy\n");
            REprintf("        requested for precision of machine,\n");
            REprintf("        suggested scaling factor = %g\n", tolsf);
            terminate(istate);
            return;
          }
          // fprintf(stderr, "lsoda -- at t = %g, too much accuracy requested\n", *t);
          // fprintf(stderr, "         for precision of machine, suggested\n");
          // fprintf(stderr, "         scaling factor = %g\n", tolsf);
          REprintf("[lsoda] at t = %g, too much accuracy requested\n", *t);
          REprintf("        for precision of machine, suggested\n");
          REprintf("        scaling factor = %g\n", tolsf);
          *istate = -2;
          terminate2(y, t);
          return;
        }
        
        if ((tn_ + h_) == tn_)
        {
          nhnil++;
          if (nhnil <= mxhnil)
          {
            // fprintf(stderr, "lsoda -- warning..internal t = %g and h_ = %g are\n",
            //         tn_, h_);
            // fprintf(
            //     stderr,
            //     "         such that in the machine, t + h_ = t on the next step\n");
            // fprintf(stderr, "         solver will continue anyway.\n");
            REprintf("[lsoda] warning..internal t = %g and h_ = %g are\n",  tn_, h_);
            REprintf("        such that in the machine, t + h_ = t on the next step\n");
            REprintf("        solver will continue anyway.\n");
            
            
            if (nhnil == mxhnil)
            {
              Rcpp::Rcerr << "[lsoda] above warning has been issued " << nhnil
                          << " times, " << std::endl
                          << "        it will not be issued again for this problem." 
                          << std::endl;
              // REprintf("[lsoda] above warning has been issued %zu times\n", nhnil);
              // REprintf("        it will not be issued again for this problem.\n");
            }
          }
        }
        
        /* Call stoda */
        stoda(neq, y, f, _data);
        if (kflag == 0)
        {
          /*
           Block f.
           The following block handles the case of a successful return from the
           core integrator ( kflag = 0 ).
           If a method switch was just made, record tsw, reset maxord,
           set jstart to -1 to signal stoda to complete the switch,
           and do extra printing of data if ixpr = 1.
           Then, in any case, check for stop conditions.
           */
          init = 1;
          if (meth_ != mused)
          {
            tsw = tn_;
            maxord = mxordn;
            if (meth_ == 2)
              maxord = mxords;
            jstart = -1;
            if (ixpr)
            {
              if (meth_ == 2)
                // cerr << "[lsoda] a switch to the stiff method has occurred "
                //      << endl;
                REprintf("[lsoda] a switch to the stiff method has occurred.\n");
              if (meth_ == 1)
                // cerr << "[lsoda] a switch to the nonstiff method has occurred"
                //      << endl;
                REprintf("[lsoda] a switch to the nonstiff method has occurred.\n");
            }
          } /* end if ( meth_ != mused )   */
          /*
           itask = 1.
           If tout has been reached, interpolate.
           */
          if (1 == itask)
          {
            if ((tn_ - tout) * h_ < 0.)
              continue;
            
            intdy(tout, 0, y, &iflag);
            *t = tout;
            *istate = 2;
            illin = 0;
            return;
          }
          /*
           itask = 2.
           */
          if (itask == 2)
          {
            successreturn(y, t, itask, ihit, tcrit, istate);
            return;
          }
          /*
           itask = 3.
           Jump to exit if tout was reached.
           */
          if (itask == 3)
          {
            if ((tn_ - tout) * h_ >= 0.)
            {
              successreturn(y, t, itask, ihit, tcrit, istate);
              return;
            }
            continue;
          }
          /*
           itask = 4.
           See if tout or tcrit was reached.  Adjust h_ if necessary.
           */
          if (itask == 4)
          {
            if ((tn_ - tout) * h_ >= 0.)
            {
              intdy(tout, 0, y, &iflag);
              *t = tout;
              *istate = 2;
              illin = 0;
              return;
            }
            else
            {
              hmx = fabs(tn_) + fabs(h_);
              ihit = fabs(tn_ - tcrit) <= (100. * ETA * hmx);
              if (ihit)
              {
                successreturn(y, t, itask, ihit, tcrit, istate);
                return;
              }
              tnext = tn_ + h_ * (1. + 4. * ETA);
              if ((tnext - tcrit) * h_ <= 0.)
                continue;
              h_ = (tcrit - tn_) * (1. - 4. * ETA);
              jstart = -2;
              continue;
            }
          } /* end if ( itask == 4 )   */
          /*
           itask = 5.
           See if tcrit was reached and jump to exit.
           */
          if (itask == 5)
          {
            hmx = fabs(tn_) + fabs(h_);
            ihit = fabs(tn_ - tcrit) <= (100. * ETA * hmx);
            successreturn(y, t, itask, ihit, tcrit, istate);
            return;
          }
        } /* end if ( kflag == 0 )   */
          /*
           kflag = -1, error test failed repeatedly or with fabs(h_) = hmin.
           kflag = -2, convergence failed repeatedly or with fabs(h_) = hmin.
           */
          if (kflag == -1 || kflag == -2)
          {
            REprintf("[lsoda] at t = %g and step size h_ = %g, the\n", tn_, h_);
            // fprintf(stderr, "lsoda -- at t = %g and step size h_ = %g, the\n", tn_,
            //        h_);
            if (kflag == -1)
            {
              // fprintf(stderr, "         error test failed repeatedly or\n");
              // fprintf(stderr, "         with fabs(h_) = hmin\n");
              REprintf("      error test failed repeatedly or\n");
              REprintf("      with fabs(h_) = hmin.\n");
              *istate = -4;
            }
            if (kflag == -2)
            {
              // fprintf(stderr,
              //         "         corrector convergence failed repeatedly or\n");
              // fprintf(stderr, "         with fabs(h_) = hmin\n");
              REprintf("      corrector convergence failed repeatedly or\n");
              REprintf("      with fabs(h_) = hmin.\n");
              *istate = -5;
            }
            big = 0.;
            imxer = 1;
            for (size_t i = 1; i <= n; ++i)
            {
              size = fabs(acor[i]) * ewt[i];
              if (big < size)
              {
                big = size;
                imxer = i;
              }
            }
            terminate2(y, t);
            return;
          } /* end if ( kflag == -1 || kflag == -2 )   */
      }   /* end while   */
} /* end lsoda   */
          
          
          void LSODA::_freevectors(void) {
            // Does nothing. USE c++ memory mechanism here.
          }

