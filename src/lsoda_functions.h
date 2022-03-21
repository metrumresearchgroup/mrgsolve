#include <R.h>
typedef odeproblem* dtype;
  
#ifndef LSODA_FUNCTIONS_H
#define LSODA_FUNCTIONS_H
#define ETA 2.2204460492503131e-16

void LSODA::hmax_(const double value) {
  rworks[2] = value;
  if(value !=0) iopt = 1;  
}

void LSODA::hmin_(const double value) {
  rworks[3] = value;
  if(value !=0) iopt = 1;  
}

void LSODA::maxsteps_(const int value) {
  Maxsteps = value;
  iworks[3] = value;
  if(value !=0) iopt=1;
}

void LSODA::ixpr_(const int value) {
  iworks[2] = value;
  if(value!=0) iopt = 1;
}

void LSODA::mxhnil_(const int value) {
  iworks[4] = value;
  if(value!=0) iopt = 1;
}

bool LSODA::abs_compare(double a, double b)
{
    return (std::abs(a) < std::abs(b));
}

/* Purpose : Find largest component of double vector dx */
size_t LSODA::idamax1(const vector<double> &dx, const size_t n, const size_t offset = 0)
{

    size_t v = 0, vmax = 0;
    size_t idmax = 1;
    for (size_t i = 1; i <= n; ++i)
    {
        v = abs(dx[i + offset]);
        if (v > vmax)
        {
            vmax = v;
            idmax = i;
        }
    }
    return idmax;

    // Following has failed with seg-fault. Probably issue with STL.
    // return std::max_element( dx.begin()+1+offset, dx.begin()+1+n, LSODA::abs_compare) - dx.begin() - offset;
}

/* Purpose : scalar vector multiplication
   dx = da * dx
*/
void LSODA::dscal1(const double da, vector<double> &dx, const size_t n,
                   const size_t offset = 0)
{
    std::transform(dx.begin() + 1 + offset, dx.end(), dx.begin() + 1 + offset,
                   [&da](double x) -> double { return da * x; });
}

/* Purpose : Inner product dx . dy */
double LSODA::ddot1(const vector<double> &a, const vector<double> &b,
                    const size_t n, const size_t offsetA = 0,
                    const size_t offsetB = 0)
{
    double sum = 0.0;
    for (size_t i = 1; i <= n; ++i)
        sum += a[i + offsetA] * b[i + offsetB];
    return sum;
}

void LSODA::daxpy1(const double da, const vector<double> &dx,
                   vector<double> &dy, const size_t n, const size_t offsetX = 0,
                   const size_t offsetY = 0)
{

    for (size_t i = 1; i <= n; ++i)
        dy[i + offsetY] = da * dx[i + offsetX] + dy[i + offsetY];
}

// See BLAS documentation. The first argument has been changed to vector.
void LSODA::dgesl1(const vector<vector<double>> &a, const size_t n,
                  vector<int> &ipvt, vector<double> &b, const size_t job)
{
    size_t k, j;
    double t;

    /*
       Job = 0, solve a * x = b.
    */
    if (job == 0)
    {
        /*
           First solve L * y = b.
        */
        for (k = 1; k <= n; ++k)
        {
            t = ddot1(a[k], b, k - 1);
            b[k] = (b[k] - t) / a[k][k];
        }
        /*
           Now solve U * x = y.
        */
        for (k = n - 1; k >= 1; k--)
        {
            b[k] = b[k] + ddot1(a[k], b, n - k, k, k);
            j = ipvt[k];
            if (j != k)
            {
                t = b[j];
                b[j] = b[k];
                b[k] = t;
            }
        }
        return;
    }
    /*
       Job = nonzero, solve Transpose(a) * x = b.

       First solve Transpose(U) * y = b.
    */
    for (k = 1; k <= n - 1; ++k)
    {
        j = ipvt[k];
        t = b[j];
        if (j != k)
        {
            b[j] = b[k];
            b[k] = t;
        }
        daxpy1(t, a[k], b, n - k, k, k);
    }
    /*
       Now solve Transpose(L) * x = y.
    */
    for (k = n; k >= 1; k--)
    {
        b[k] = b[k] / a[k][k];
        t = -b[k];
        daxpy1(t, a[k], b, k - 1);
    }
}

// See BLAS documentation. All double* has been changed to std::vector .
void LSODA::dgefa1(vector<vector<double>> &a, const size_t n, vector<int> &ipvt,
                  size_t *const info)
{
    size_t j = 0, k = 0, i = 0;
    double t = 0.0;

    /* Gaussian elimination with partial pivoting.   */

    *info = 0;
    for (k = 1; k <= n - 1; ++k)
    {
        /*
           Find j = pivot index.  Note that a[k]+k-1 is the address of
           the 0-th element of the row vector whose 1st element is a[k][k].
        */
        j = idamax1(a[k], n - k + 1, k - 1) + k - 1;
        ipvt[k] = j;
        /*
           Zero pivot implies this row already triangularized.
        */
        if (a[k][j] == 0.)
        {
            *info = k;
            continue;
        }
        /*
           Interchange if necessary.
        */
        if (j != k)
        {
            t = a[k][j];
            a[k][j] = a[k][k];
            a[k][k] = t;
        }
        /*
           Compute multipliers.
        */
        t = -1. / a[k][k];
        dscal1(t, a[k], n - k, k);

        /*
           Column elimination with row indexing.
        */
        for (i = k + 1; i <= n; ++i)
        {
            t = a[i][j];
            if (j != k)
            {
                a[i][j] = a[i][k];
                a[i][k] = t;
            }
            daxpy1(t, a[k], a[i], n - k, k, k);
        }
    } /* end k-loop  */

    ipvt[n] = n;
    if (a[n][n] == 0.)
        *info = n;
}

/* Terminate lsoda due to illegal input. */
void LSODA::terminate(int *istate)
{
    if (illin == 5) {
        // cerr << "[lsoda] repeated occurrence of illegal input. run aborted.. "
        //         "apparent infinite loop."
        //      << endl;
        REprintf("[lsoda] repeated occurrence of illegal input. run aborted;\n");
        REprintf("        apparent infinite loop.\n");
    } else
    {
        illin++;
        *istate = -3;
    }
    return;
}

/* Terminate lsoda due to various error conditions. */
void LSODA::terminate2(vector<double> &y, double *t)
{
    for (size_t i = 1; i <= n; ++i)
        y[i] = yh_[1][i];
    *t = tn_;
    illin = 0;
    return;
}

/*
   The following block handles all successful returns from lsoda.
   If itask != 1, y is loaded from yh_ and t is set accordingly.
   *Istate is set to 2, the illegal input counter is zeroed, and the
   optional outputs are loaded into the work arrays before returning.
*/

void LSODA::successreturn(vector<double> &y, double *t, int itask, int ihit,
                          double tcrit, int *istate)
{
    for (size_t i = 1; i <= n; ++i)
        y[i] = yh_[1][i];
    *t = tn_;
    if (itask == 4 || itask == 5)
        if (ihit)
            *t = tcrit;
    *istate = 2;
    illin = 0;
}

/*
c references..
c 1.  alan c. hindmarsh,  odepack, a systematized collection of ode
c     solvers, in scientific computing, r. s. stepleman et al. (eds.),
c     north-holland, amsterdam, 1983, pp. 55-64.
c 2.  linda r. petzold, automatic selection of methods for solving
c     stiff and nonstiff systems of ordinary differential equations,
c     siam j. sci. stat. comput. 4 (1983), pp. 136-148.
c-----------------------------------------------------------------------
*/

void LSODA::stoda(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
                  dtype _data)
{
    //assert(neq + 1 == y.size());
    if((neq+1) != y.size()) {
      Rcpp::stop("[lsoda] y is not the right size in stoda.");    
    }

    size_t corflag = 0, orderflag = 0;
    size_t i = 0, i1 = 0, j = 0, m = 0, ncf = 0;
    double del = 0.0, delp = 0.0, dsm = 0.0, dup = 0.0, exup = 0.0, r = 0.0,
           rh = 0.0, rhup = 0.0, told = 0.0;
    double pdh = 0.0, pnorm = 0.0;

    /*
       stoda performs one step of the integration of an initial value
       problem for a system of ordinary differential equations.
       Note.. stoda is independent of the value of the iteration method
       indicator miter, when this is != 0, and hence is independent
       of the type of chord method used, or the Jacobian structure.
       Communication with stoda is done with the following variables:

       jstart = an integer used for input only, with the following
                values and meanings:

                   0  perform the first step,
                 > 0  take a new step continuing from the last,
                  -1  take the next step with a new value of h_,
                      n, meth_, miter, and/or matrix parameters.
                  -2  take the next step with a new value of h_,
                      but with other inputs unchanged.

       kflag = a completion code with the following meanings:

                 0  the step was successful,
                -1  the requested error could not be achieved,
                -2  corrector convergence could not be achieved,
                -3  fatal error in prja or solsy.

       miter = corrector iteration method:

                 0  functional iteration,
                >0  a chord method corresponding to jacobian type jt.

    */
    kflag = 0;
    told = tn_;
    ncf = 0;
    ierpj = 0;
    iersl = 0;
    jcur = 0;
    delp = 0.;

    /*
       On the first call, the order is set to 1, and other variables are
       initialized.  rmax is the maximum ratio by which h_ can be increased
       in a single step.  It is initially 1.e4 to compensate for the small
       initial h_, but then is normally equal to 10.  If a filure occurs
       (in corrector convergence or error test), rmax is set at 2 for
       the next increase.
       cfode is called to get the needed coefficients for both methods.
    */
    if (jstart == 0)
    {
        lmax = maxord + 1;
        nq = 1;
        l = 2;
        ialth = 2;
        rmax = 10000.;
        rc = 0.;
        el0 = 1.;
        crate = 0.7;
        hold = h_;
        nslp = 0;
        ipup = miter;
        /*
           Initialize switching parameters.  meth_ = 1 is assumed initially.
        */
        icount = 20;
        irflag = 0;
        pdest = 0.;
        pdlast = 0.;
        ratio = 5.;
        cfode(2);
        for (i = 1; i <= 5; ++i)
            cm2[i] = tesco[i][2] * elco[i][i + 1];
        cfode(1);
        for (i = 1; i <= 12; ++i)
            cm1[i] = tesco[i][2] * elco[i][i + 1];
        resetcoeff();
    } /* end if ( jstart == 0 )   */
    /*
       The following block handles preliminaries needed when jstart = -1.
       ipup is set to miter to force a matrix update.
       If an order increase is about to be considered ( ialth = 1 ),
       ialth is reset to 2 to postpone consideration one more step.
       If the caller has changed meth_, cfode is called to reset
       the coefficients of the method.
       If h_ is to be changed, yh_ must be rescaled.
       If h_ or meth_ is being changed, ialth is reset to l = nq + 1
       to prevent further changes in h_ for that many steps.
    */
    if (jstart == -1)
    {
        ipup = miter;
        lmax = maxord + 1;
        if (ialth == 1)
            ialth = 2;
        if (meth_ != mused)
        {
            cfode(meth_);
            ialth = l;
            resetcoeff();
        }
        if (h_ != hold)
        {
            rh = h_ / hold;
            h_ = hold;
            scaleh(&rh, &pdh);
        }
    } /* if ( jstart == -1 )   */
    if (jstart == -2)
    {
        if (h_ != hold)
        {
            rh = h_ / hold;
            h_ = hold;
            scaleh(&rh, &pdh);
        }
    } /* if ( jstart == -2 )   */

    /*
       Prediction.
       This section computes the predicted values by effectively
       multiplying the yh_ array by the pascal triangle matrix.
       rc is the ratio of new to old values of the coefficient h_ * el[1].
       When rc differs from 1 by more than ccmax, ipup is set to miter
       to force pjac to be called, if a jacobian is involved.
       In any case, prja is called at least every msbp steps.
    */
    while (1)
    {
        while (1)
        {
            if (fabs(rc - 1.) > ccmax)
                ipup = miter;
            if (nst >= nslp + msbp)
                ipup = miter;
            tn_ += h_;
            for (size_t j = nq; j >= 1; j--)
                for (size_t i1 = j; i1 <= nq; ++i1)
                    for (i = 1; i <= n; ++i)
                        yh_[i1][i] += yh_[i1 + 1][i];

            pnorm = vmnorm(n, yh_[1], ewt);
            correction(neq, y, f, &corflag, pnorm, &del, &delp, &told, &ncf, &rh, &m,
                       _data);
            if (corflag == 0)
                break;
            if (corflag == 1)
            {
                rh = max(rh, hmin / fabs(h_));
                scaleh(&rh, &pdh);
                continue;
            }
            if (corflag == 2)
            {
                kflag = -2;
                hold = h_;
                jstart = 1;
                return;
            }
        } /* end inner while ( corrector loop )   */

        /*
           The corrector has converged.  jcur is set to 0
           to signal that the Jacobian involved may need updating later.
           The local error test is done now.
        */
        jcur = 0;
        if (m == 0)
            dsm = del / tesco[nq][2];
        if (m > 0)
            dsm = vmnorm(n, acor, ewt) / tesco[nq][2];

        if (dsm <= 1.)
        {
            /*
               After a successful step, update the yh_ array.
               Decrease icount by 1, and if it is -1, consider switching methods.
               If a method switch is made, reset various parameters,
               rescale the yh_ array, and exit.  If there is no switch,
               consider changing h_ if ialth = 1.  Otherwise decrease ialth by 1.
               If ialth is then 1 and nq < maxord, then acor is saved for
               use in a possible order increase on the next step.
               If a change in h_ is considered, an increase or decrease in order
               by one is considered also.  A change in h_ is made only if it is by
               a factor of at least 1.1.  If not, ialth is set to 3 to prevent
               testing for that many steps.
            */
            kflag = 0;
            nst++;
            hu = h_;
            nqu = nq;
            mused = meth_;
            for (size_t j = 1; j <= l; ++j)
            {
                r = el[j];
                for (i = 1; i <= n; ++i)
                    yh_[j][i] += r * acor[i];
            }
            icount--;
            if (icount < 0)
            {
                methodswitch(dsm, pnorm, &pdh, &rh);
                if (meth_ != mused)
                {
                    rh = max(rh, hmin / fabs(h_));
                    scaleh(&rh, &pdh);
                    rmax = 10.;
                    endstoda();
                    break;
                }
            }
            /*
               No method switch is being made.  Do the usual step/order selection.
            */
            ialth--;
            if (ialth == 0)
            {
                rhup = 0.;
                if (l != lmax)
                {
                    for (i = 1; i <= n; ++i)
                        savf[i] = acor[i] - yh_[lmax][i];
                    dup = vmnorm(n, savf, ewt) / tesco[nq][3];
                    exup = 1. / (double)(l + 1);
                    rhup = 1. / (1.4 * pow(dup, exup) + 0.0000014);
                }

                orderswitch(&rhup, dsm, &pdh, &rh, &orderflag);

                /*
                   No change in h_ or nq.
                */
                if (orderflag == 0)
                {
                    endstoda();
                    break;
                }
                /*
                   h_ is changed, but not nq.
                */
                if (orderflag == 1)
                {
                    rh = max(rh, hmin / fabs(h_));
                    scaleh(&rh, &pdh);
                    rmax = 10.;
                    endstoda();
                    break;
                }
                /*
                   both nq and h_ are changed.
                */
                if (orderflag == 2)
                {
                    resetcoeff();
                    rh = max(rh, hmin / fabs(h_));
                    scaleh(&rh, &pdh);
                    rmax = 10.;
                    endstoda();
                    break;
                }
            } /* end if ( ialth == 0 )   */
            if (ialth > 1 || l == lmax)
            {
                endstoda();
                break;
            }

            for (size_t i = 1; i <= n; ++i)
                yh_[lmax][i] = acor[i];

            endstoda();
            break;
        }
        /* end if ( dsm <= 1. )   */
        /*
           The error test failed.  kflag keeps track of multiple failures.
           Restore tn_ and the yh_ array to their previous values, and prepare
           to try the step again.  Compute the optimum step size for this or
           one lower.  After 2 or more failures, h_ is forced to decrease
           by a factor of 0.2 or less.
         */
        else
        {
            kflag--;
            tn_ = told;
            for (j = nq; j >= 1; j--)
            {
                for (i1 = j; i1 <= nq; ++i1)
                    for (i = 1; i <= n; ++i)
                        yh_[i1][i] -= yh_[i1 + 1][i];
            }
            rmax = 2.;
            if (fabs(h_) <= hmin * 1.00001)
            {
                kflag = -1;
                hold = h_;
                jstart = 1;
                break;
            }
            if (kflag > -3)
            {
                rhup = 0.;
                orderswitch(&rhup, dsm, &pdh, &rh, &orderflag);
                if (orderflag == 1 || orderflag == 0)
                {
                    if (orderflag == 0)
                        rh = min(rh, 0.2);
                    rh = max(rh, hmin / fabs(h_));
                    scaleh(&rh, &pdh);
                }
                if (orderflag == 2)
                {
                    resetcoeff();
                    rh = max(rh, hmin / fabs(h_));
                    scaleh(&rh, &pdh);
                }
                continue;
            }
            /* if ( kflag > -3 )   */
            /*
               Control reaches this section if 3 or more failures have occurred.
               If 10 failures have occurred, exit with kflag = -1.
               It is assumed that the derivatives that have accumulated in the
               yh_ array have errors of the wrong order.  Hence the first
               derivative is recomputed, and the order is set to 1.  Then
               h_ is reduced by a factor of 10, and the step is retried,
               until it succeeds or h_ reaches hmin.
             */
            else
            {
                if (kflag == -10)
                {
                    kflag = -1;
                    hold = h_;
                    jstart = 1;
                    break;
                }
                else
                {
                    rh = 0.1;
                    rh = max(hmin / fabs(h_), rh);
                    h_ *= rh;
                    for (i = 1; i <= n; ++i)
                        y[i] = yh_[1][i];
                    (*f)(tn_, &y[1], &savf[1], _data);
                    nfe++;
                    for (i = 1; i <= n; ++i)
                        yh_[2][i] = h_ * savf[i];
                    ipup = miter;
                    ialth = 5;
                    if (nq == 1)
                        continue;
                    nq = 1;
                    l = 2;
                    resetcoeff();
                    continue;
                }
            } /* end else -- kflag <= -3 */
        }     /* end error failure handling   */
    }         /* end outer while   */

} /* end stoda   */

void LSODA::ewset(const vector<double> &ycur)
{
    switch (itol_)
    {
    case 1:
        for (size_t i = 1; i <= n; ++i)
            ewt[i] = rtol_[1] * fabs(ycur[i]) + atol_[1];
        break;
    case 2:
        for (size_t i = 1; i <= n; ++i)
            ewt[i] = rtol_[1] * fabs(ycur[i]) + atol_[i];
        break;
    case 3:
        for (size_t i = 1; i <= n; ++i)
            ewt[i] = rtol_[i] * fabs(ycur[i]) + atol_[1];
        break;
    case 4:
        for (size_t i = 1; i <= n; ++i)
            ewt[i] = rtol_[i] * fabs(ycur[i]) + atol_[i];
        break;
    }

} /* end ewset   */

/*
   Intdy computes interpolated values of the k-th derivative of the
   dependent variable vector y, and stores it in dky.  This routine
   is called within the package with k = 0 and *t = tout, but may
   also be called by the user for any k up to the current order.
   ( See detailed instructions in the usage documentation. )

   The computed values in dky are gotten by interpolation using the
   Nordsieck history array yh_.  This array corresponds uniquely to a
   vector-valued polynomial of degree nqcur or less, and dky is set
   to the k-th derivative of this polynomial at t.
   The formula for dky is

             q
   dky[i] = sum c[k][j] * ( t - tn_ )^(j-k) * h_^(-j) * yh_[j+1][i]
            j=k

   where c[k][j] = j*(j-1)*...*(j-k+1), q = nqcur, tn_ = tcur, h_ = hcur.
   The quantities nq = nqcur, l = nq+1, n = neq, tn_, and h_ are declared
   static globally.  The above sum is done in reverse order.
   *iflag is returned negative if either k or t is out of bounds.
*/
void LSODA::intdy(double t, int k, vector<double> &dky, int *iflag)
{
    int ic, jp1 = 0;
    double c, r, s, tp;

    *iflag = 0;
    if (k < 0 || k > (int)nq)
    {
        //fprintf(stderr, "[intdy] k = %d illegal\n", k);
        REprintf("[intdy] k = %d illegal.\n", k);
        *iflag = -1;
        return;
    }
    tp = tn_ - hu - 100. * ETA * (tn_ + hu);
    if ((t - tp) * (t - tn_) > 0.)
    {
        // fprintf(stderr,
        //         "intdy -- t = %g illegal. t not in interval tcur - hu to tcur\n",
        //         t);
      REprintf("[intdy]  t = %g illegal. t not in interval tcur - hu to tcur.\n\n",t);
        *iflag = -2;
        return;
    }
    s = (t - tn_) / h_;
    ic = 1;
    for (size_t jj = l - k; jj <= nq; ++jj)
        ic *= jj;
    c = (double)ic;
    for (size_t i = 1; i <= n; ++i)
        dky[i] = c * yh_[l][i];

    for (int j = nq - 1; j >= k; j--)
    {
        jp1 = j + 1;
        ic = 1;
        for (int jj = jp1 - k; jj <= j; ++jj)
            ic *= jj;
        c = (double)ic;

        for (size_t i = 1; i <= n; ++i)
            dky[i] = c * yh_[jp1][i] + s * dky[i];
    }
    if (k == 0)
        return;
    r = pow(h_, (double)(-k));

    for (size_t i = 1; i <= n; ++i)
        dky[i] *= r;

} /* end intdy   */

void LSODA::cfode(int meth_)
{
    int i, nq, nqm1, nqp1;
    double agamq, fnq, fnqm1, pc[13], pint, ragq, rqfac, rq1fac, tsign, xpin;
    /*
       cfode is called by the integrator routine to set coefficients
       needed there.  The coefficients for the current method, as
       given by the value of meth_, are set for all orders and saved.
       The maximum order assumed here is 12 if meth_ = 1 and 5 if meth_ = 2.
       ( A smaller value of the maximum order is also allowed. )
       cfode is called once at the beginning of the problem, and
       is not called again unless and until meth_ is changed.

       The elco array contains the basic method coefficients.
       The coefficients el[i], 1 < i < nq+1, for the method of
       order nq are stored in elco[nq][i].  They are given by a generating
       polynomial, i.e.,

          l(x) = el[1] + el[2]*x + ... + el[nq+1]*x^nq.

       For the implicit Adams method, l(x) is given by

          dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),   l(-1) = 0.

       For the bdf methods, l(x) is given by

          l(x) = (x+1)*(x+2)*...*(x+nq)/k,

       where   k = factorial(nq)*(1+1/2+...+1/nq).

       The tesco array contains test constants used for the
       local error test and the selection of step size and/or order.
       At order nq, tesco[nq][k] is used for the selection of step
       size at order nq-1 if k = 1, at order nq if k = 2, and at order
       nq+1 if k = 3.
    */
    if (meth_ == 1)
    {
        elco[1][1] = 1.;
        elco[1][2] = 1.;
        tesco[1][1] = 0.;
        tesco[1][2] = 2.;
        tesco[2][1] = 1.;
        tesco[12][3] = 0.;
        pc[1] = 1.;
        rqfac = 1.;
        for (nq = 2; nq <= 12; nq++)
        {
            /*
               The pc array will contain the coefficients of the polynomial

                  p(x) = (x+1)*(x+2)*...*(x+nq-1).

               Initially, p(x) = 1.
            */
            rq1fac = rqfac;
            rqfac = rqfac / (double)nq;
            nqm1 = nq - 1;
            fnqm1 = (double)nqm1;
            nqp1 = nq + 1;
            /*
               Form coefficients of p(x)*(x+nq-1).
            */
            pc[nq] = 0.;
            for (i = nq; i >= 2; i--)
                pc[i] = pc[i - 1] + fnqm1 * pc[i];
            pc[1] = fnqm1 * pc[1];
            /*
               Compute integral, -1 to 0, of p(x) and x*p(x).
            */
            pint = pc[1];
            xpin = pc[1] / 2.;
            tsign = 1.;
            for (i = 2; i <= nq; ++i)
            {
                tsign = -tsign;
                pint += tsign * pc[i] / (double)i;
                xpin += tsign * pc[i] / (double)(i + 1);
            }
            /*
               Store coefficients in elco and tesco.
            */
            elco[nq][1] = pint * rq1fac;
            elco[nq][2] = 1.;
            for (i = 2; i <= nq; ++i)
                elco[nq][i + 1] = rq1fac * pc[i] / (double)i;
            agamq = rqfac * xpin;
            ragq = 1. / agamq;
            tesco[nq][2] = ragq;
            if (nq < 12)
                tesco[nqp1][1] = ragq * rqfac / (double)nqp1;
            tesco[nqm1][3] = ragq;
        } /* end for   */
        return;
    } /* end if ( meth_ == 1 )   */

    /* meth_ = 2. */
    pc[1] = 1.;
    rq1fac = 1.;

    /*
       The pc array will contain the coefficients of the polynomial
          p(x) = (x+1)*(x+2)*...*(x+nq).
       Initially, p(x) = 1.
    */
    for (nq = 1; nq <= 5; ++nq)
    {
        fnq = (double)nq;
        nqp1 = nq + 1;
        /*
           Form coefficients of p(x)*(x+nq).
        */
        pc[nqp1] = 0.;
        for (i = nq + 1; i >= 2; i--)
            pc[i] = pc[i - 1] + fnq * pc[i];
        pc[1] *= fnq;
        /*
           Store coefficients in elco and tesco.
        */
        for (i = 1; i <= nqp1; ++i)
            elco[nq][i] = pc[i] / pc[2];
        elco[nq][2] = 1.;
        tesco[nq][1] = rq1fac;
        tesco[nq][2] = ((double)nqp1) / elco[nq][1];
        tesco[nq][3] = ((double)(nq + 2)) / elco[nq][1];
        rq1fac /= fnq;
    }
    return;

} /* end cfode   */

void LSODA::scaleh(double *rh, double *pdh)
{
    double r;
    /*
       If h_ is being changed, the h_ ratio rh is checked against rmax, hmin,
       and hmxi, and the yh_ array is rescaled.  ialth is set to l = nq + 1
       to prevent a change of h_ for that many steps, unless forced by a
       convergence or error test failure.
    */
    *rh = min(*rh, rmax);
    *rh = *rh / max(1., fabs(h_) * hmxi * *rh);
    /*
       If meth_ = 1, also restrict the new step size by the stability region.
       If this reduces h_, set irflag to 1 so that if there are roundoff
       problems later, we can assume that is the cause of the trouble.
    */
    if (meth_ == 1)
    {
        irflag = 0;
        *pdh = max(fabs(h_) * pdlast, 0.000001);
        if ((*rh * *pdh * 1.00001) >= sm1[nq])
        {
            *rh = sm1[nq] / *pdh;
            irflag = 1;
        }
    }
    r = 1.;
    for (size_t j = 2; j <= l; ++j)
    {
        r *= *rh;
        for (size_t i = 1; i <= n; ++i)
            yh_[j][i] *= r;
    }
    h_ *= *rh;
    rc *= *rh;
    ialth = l;

} /* end scaleh   */

void LSODA::prja(const size_t neq, vector<double> &y, LSODA_ODE_SYSTEM_TYPE f,
                 dtype _data)
{
    size_t i = 0, ier = 0, j = 0;
    double fac = 0.0, hl0 = 0.0, r = 0.0, r0 = 0.0, yj = 0.0;
    /*
       prja is called by stoda to compute and process the matrix
       P = I - h_ * el[1] * J, where J is an approximation to the Jacobian.
       Here J is computed by finite differencing.
       J, scaled by -h_ * el[1], is stored in wm_.  Then the norm of J ( the
       matrix norm consistent with the weighted max-norm on vectors given
       by vmnorm ) is computed, and J is overwritten by P.  P is then
       subjected to LU decomposition in preparation for later solution
       of linear systems with p as coefficient matrix.  This is done
       by dgefa1 if miter = 2, and by dgbfa if miter = 5.
    */
    nje++;
    ierpj = 0;
    jcur = 1;
    hl0 = h_ * el0;
    /*
       If miter = 2, make n calls to f to approximate J.
    */
    if (miter != 2)
    {
        //fprintf(stderr, "[prja] miter != 2\n");
        REprintf("[prja] miter != 2.\n");
        return;
    }
    if (miter == 2)
    {
        fac = vmnorm(n, savf, ewt);
        r0 = 1000. * fabs(h_) * ETA * ((double)n) * fac;
        if (r0 == 0.)
            r0 = 1.;
        for (j = 1; j <= n; ++j)
        {
            yj = y[j];
            r = max(sqrteta * fabs(yj), r0 / ewt[j]);
            y[j] += r;
            fac = -hl0 / r;
            (*f)(tn_, &y[1], &acor[1], _data);
            for (i = 1; i <= n; ++i)
                wm_[i][j] = (acor[i] - savf[i]) * fac;
            y[j] = yj;
        }
        nfe += n;
        /*
           Compute norm of Jacobian.
        */
        pdnorm = fnorm(n, wm_, ewt) / fabs(hl0);
        /*
           Add identity matrix.
        */
        for (i = 1; i <= n; ++i)
            wm_[i][i] += 1.;
        /*
           Do LU decomposition on P.
        */
        dgefa1(wm_, n, ipvt, &ier);
        if (ier != 0)
            ierpj = 1;
        return;
    }
} /* end prja   */

/*
   This function routine computes the weighted max-norm
   of the vector of length n contained in the array v, with weights
   contained in the array w of length n.

   vmnorm = max( i = 1, ..., n ) fabs( v[i] ) * w[i].
*/
double LSODA::vmnorm(const size_t n, const vector<double> &v,
                     const vector<double> &w)
{
    double vm = 0.;
    for (size_t i = 1; i <= n; ++i)
        vm = max(vm, fabs(v[i]) * w[i]);
    return vm;
}

double LSODA::fnorm(int n, const vector<vector<double>> &a,
                    const vector<double> &w)

/*
   This subroutine computes the norm of a full n by n matrix,
   stored in the array a, that is consistent with the weighted max-norm
   on vectors, with weights stored in the array w.

      fnorm = max(i=1,...,n) ( w[i] * sum(j=1,...,n) fabs( a[i][j] ) / w[j] )
*/

{
    double an = 0, sum = 0;

    for (size_t i = 1; i <= (size_t)n; ++i)
    {
        sum = 0.;
        for (size_t j = 1; j <= (size_t)n; ++j)
            sum += fabs(a[i][j]) / w[j];
        an = max(an, sum * w[i]);
    }
    return an;
}

/*
 *corflag = 0 : corrector converged,
1 : step size to be reduced, redo prediction,
2 : corrector cannot converge, failure flag.
*/
void LSODA::correction(const size_t neq, vector<double> &y,
                       LSODA_ODE_SYSTEM_TYPE f, size_t *corflag, double pnorm,
                       double *del, double *delp, double *told,size_t *ncf,
                       double *rh, size_t *m, dtype _data)
{
    double rm = 0.0, rate = 0.0, dcon = 0.0;

    /*
       Up to maxcor corrector iterations are taken.  A convergence test is
       made on the r.m.s. norm of each correction, weighted by the error
       weight vector ewt.  The sum of the corrections is accumulated in the
       vector acor[i].  The yh_ array is not altered in the corrector loop.
    */

    *m = 0;
    *corflag = 0;
    *del = 0.;

    for (size_t i = 1; i <= n; ++i)
        y[i] = yh_[1][i];

    (*f)(tn_, &y[1], &savf[1], _data);

    nfe++;
    /*
       If indicated, the matrix P = I - h_ * el[1] * J is reevaluated and
       preprocessed before starting the corrector iteration.  ipup is set
       to 0 as an indicator that this has been done.
    */
    while (1)
    {
        if (*m == 0)
        {
            if (ipup > 0)
            {
                prja(neq, y, f, _data);
                ipup = 0;
                rc = 1.;
                nslp = nst;
                crate = 0.7;
                if (ierpj != 0)
                {
                    corfailure(told, rh, ncf, corflag);
                    return;
                }
            }
            for (size_t i = 1; i <= n; ++i)
                acor[i] = 0.;
        } /* end if ( *m == 0 )   */
        if (miter == 0)
        {
            /*
               In case of functional iteration, update y directly from
               the result of the last function evaluation.
            */
            for (size_t i = 1; i <= n; ++i)
            {
                savf[i] = h_ * savf[i] - yh_[2][i];
                y[i] = savf[i] - acor[i];
            }
            *del = vmnorm(n, y, ewt);
            for (size_t i = 1; i <= n; ++i)
            {
                y[i] = yh_[1][i] + el[1] * savf[i];
                acor[i] = savf[i];
            }
        }
        /* end functional iteration   */
        /*
           In the case of the chord method, compute the corrector error,
           and solve the linear system with that as right-hand side and
           P as coefficient matrix.
         */
        else
        {
            for (size_t i = 1; i <= n; ++i)
                y[i] = h_ * savf[i] - (yh_[2][i] + acor[i]);

            solsy(y);
            *del = vmnorm(n, y, ewt);

            for (size_t i = 1; i <= n; ++i)
            {
                acor[i] += y[i];
                y[i] = yh_[1][i] + el[1] * acor[i];
            }
        } /* end chord method   */
        /*
           Test for convergence.  If *m > 0, an estimate of the convergence
           rate constant is stored in crate, and this is used in the test.

           We first check for a change of iterates that is the size of
           roundoff error.  If this occurs, the iteration has converged, and a
           new rate estimate is not formed.
           In all other cases, force at least two iterations to estimate a
           local Lipschitz constant estimate for Adams method.
           On convergence, form pdest = local maximum Lipschitz constant
           estimate.  pdlast is the most recent nonzero estimate.
        */
        if (*del <= 100. * pnorm * ETA)
            break;
        if (*m != 0 || meth_ != 1)
        {
            if (*m != 0)
            {
                rm = 1024.0;
                if (*del <= (1024. * *delp))
                    rm = *del / *delp;
                rate = max(rate, rm);
                crate = max(0.2 * crate, rm);
            }
            dcon = *del * min(1., 1.5 * crate) / (tesco[nq][2] * conit);
            if (dcon <= 1.)
            {
                pdest = max(pdest, rate / fabs(h_ * el[1]));
                if (pdest != 0.)
                    pdlast = pdest;
                break;
            }
        }
        /*
           The corrector iteration failed to converge.
           If miter != 0 and the Jacobian is out of date, prja is called for
           the next try.   Otherwise the yh_ array is retracted to its values
           before prediction, and h_ is reduced, if possible.  If h_ cannot be
           reduced or mxncf failures have occured, exit with corflag = 2.
        */
        (*m)++;
        if (*m == maxcor || (*m >= 2 && *del > 2. * *delp))
        {
            if (miter == 0 || jcur == 1)
            {
                corfailure(told, rh, ncf, corflag);
                return;
            }
            ipup = miter;
            /*
               Restart corrector if Jacobian is recomputed.
            */
            *m = 0;
            rate = 0.;
            *del = 0.;
            for (size_t i = 1; i <= n; ++i)
                y[i] = yh_[1][i];

            (*f)(tn_, &y[1], &savf[1], _data);

            nfe++;
        }
        /*
           Iterate corrector.
        */
        else
        {
            *delp = *del;
            (*f)(tn_, &y[1], &savf[1], _data);
            nfe++;
        }
    } /* end while   */
} /* end correction   */

void LSODA::corfailure(double *told, double *rh, size_t *ncf, size_t *corflag)
{
    (*ncf)++;
    rmax = 2.;
    tn_ = *told;
    for (size_t j = nq; j >= 1; j--)
        for (size_t i1 = j; i1 <= nq; ++i1)
            for (size_t i = 1; i <= n; ++i)
                yh_[i1][i] -= yh_[i1 + 1][i];

    if (fabs(h_) <= hmin * 1.00001 || *ncf == mxncf)
    {
        *corflag = 2;
        return;
    }
    *corflag = 1;
    *rh = 0.25;
    ipup = miter;
}

/*
   This routine manages the solution of the linear system arising from
   a chord iteration.  It is called if miter != 0.
   If miter is 2, it calls dgesl1 to accomplish this.
   If miter is 5, it calls dgbsl.

   y = the right-hand side vector on input, and the solution vector
       on output.
*/
void LSODA::solsy(vector<double> &y)
{
    iersl = 0;
    if (miter != 2)
    {
        //printf("solsy -- miter != 2\n");
        REprintf("solsy -- miter != 2.\n");
        return;
    }
    if (miter == 2)
        dgesl1(wm_, n, ipvt, y, 0);
    return;
}

void LSODA::methodswitch(double dsm, double pnorm, double *pdh, double *rh)
{
    int lm1, lm1p1, lm2, lm2p1, nqm1, nqm2;
    double rh1, rh2, rh1it, exm2, dm2, exm1, dm1, alpha, exsm;
    
    /*
       We are current using an Adams method.  Consider switching to bdf.
       If the current order is greater than 5, assume the problem is
       not stiff, and skip this section.
       If the Lipschitz constant and error estimate are not polluted
       by roundoff, perform the usual test.
       Otherwise, switch to the bdf methods if the last step was
       restricted to insure stability ( irflag = 1 ), and stay with Adams
       method if not.  When switching to bdf with polluted error estimates,
       in the absence of other information, double the step size.

       When the estimates are ok, we make the usual test by computing
       the step size we could have (ideally) used on this step,
       with the current (Adams) method, and also that for the bdf.
       If nq > mxords, we consider changing to order mxords on switching.
       Compare the two step sizes to decide whether to switch.
       The step size advantage must be at least ratio = 5 to switch.
    */
    if (meth_ == 1)
    {
        if (nq > 5)
            return;
        if (dsm <= (100. * pnorm * ETA) || pdest == 0.)
        {
            if (irflag == 0)
                return;
            rh2 = 2.;
            nqm2 = min(nq, mxords);
        }
        else
        {
            exsm = 1. / (double)l;
            rh1 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
            rh1it = 2. * rh1;
            *pdh = pdlast * fabs(h_);
            if ((*pdh * rh1) > 0.00001)
                rh1it = sm1[nq] / *pdh;
            rh1 = min(rh1, rh1it);
            if (nq > mxords)
            {
                nqm2 = mxords;
                lm2 = mxords + 1;
                exm2 = 1. / (double)lm2;
                lm2p1 = lm2 + 1;
                dm2 = vmnorm(n, yh_[lm2p1], ewt) / cm2[mxords];
                rh2 = 1. / (1.2 * pow(dm2, exm2) + 0.0000012);
            }
            else
            {
                dm2 = dsm * (cm1[nq] / cm2[nq]);
                rh2 = 1. / (1.2 * pow(dm2, exsm) + 0.0000012);
                nqm2 = nq;
            }
            if (rh2 < ratio * rh1)
                return;
        }
        /*
           The method switch test passed.  Reset relevant quantities for bdf.
        */
        *rh = rh2;
        icount = 20;
        meth_ = 2;
        miter = jtyp;
        pdlast = 0.;
        nq = nqm2;
        l = nq + 1;
        return;
    } /* end if ( meth_ == 1 )   */

    /*
       We are currently using a bdf method, considering switching to Adams.
       Compute the step size we could have (ideally) used on this step,
       with the current (bdf) method, and also that for the Adams.
       If nq > mxordn, we consider changing to order mxordn on switching.
       Compare the two step sizes to decide whether to switch.
       The step size advantage must be at least 5/ratio = 1 to switch.
       If the step size for Adams would be so small as to cause
       roundoff pollution, we stay with bdf.
    */
    exsm = 1. / (double)l;
    if (mxordn < nq)
    {
        nqm1 = mxordn;
        lm1 = mxordn + 1;
        exm1 = 1. / (double)lm1;
        lm1p1 = lm1 + 1;
        dm1 = vmnorm(n, yh_[lm1p1], ewt) / cm1[mxordn];
        rh1 = 1. / (1.2 * pow(dm1, exm1) + 0.0000012);
    }
    else
    {
        dm1 = dsm * (cm2[nq] / cm1[nq]);
        rh1 = 1. / (1.2 * pow(dm1, exsm) + 0.0000012);
        nqm1 = nq;
        exm1 = exsm;
    }
    rh1it = 2. * rh1;
    *pdh = pdnorm * fabs(h_);
    if ((*pdh * rh1) > 0.00001)
        rh1it = sm1[nqm1] / *pdh;
    rh1 = min(rh1, rh1it);
    rh2 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
    if ((rh1 * ratio) < (5. * rh2))
        return;
    alpha = max(0.001, rh1);
    dm1 *= pow(alpha, exm1);
    if (dm1 <= 1000. * ETA * pnorm)
        return;
    /*
       The switch test passed.  Reset relevant quantities for Adams.
    */
    *rh = rh1;
    icount = 20;
    meth_ = 1;
    miter = 0;
    pdlast = 0.;
    nq = nqm1;
    l = nq + 1;
} /* end methodswitch   */

/*
   This routine returns from stoda to lsoda.  Hence freevectors() is
   not executed.
*/
void LSODA::endstoda()
{
    double r = 1. / tesco[nqu][2];
    for (size_t i = 1; i <= n; ++i)
        acor[i] *= r;
    hold = h_;
    jstart = 1;
}

/*
   Regardless of the success or failure of the step, factors
   rhdn, rhsm, and rhup are computed, by which h_ could be multiplied
   at order nq - 1, order nq, or order nq + 1, respectively.
   In the case of a failure, rhup = 0. to avoid an order increase.
   The largest of these is determined and the new order chosen
   accordingly.  If the order is to be increased, we compute one
   additional scaled derivative.

   orderflag = 0  : no change in h_ or nq,
               1  : change in h_ but not nq,
               2  : change in both h_ and nq.
*/
void LSODA::orderswitch(double *rhup, double dsm, double *pdh, double *rh,
                        size_t *orderflag)
{
    size_t newq = 0;
    double exsm, rhdn, rhsm, ddn, exdn, r;

    *orderflag = 0;

    exsm = 1. / (double)l;
    rhsm = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);

    rhdn = 0.;
    if (nq != 1)
    {
        ddn = vmnorm(n, yh_[l], ewt) / tesco[nq][1];
        exdn = 1. / (double)nq;
        rhdn = 1. / (1.3 * pow(ddn, exdn) + 0.0000013);
    }
    /*
       If meth_ = 1, limit rh accordinfg to the stability region also.
    */
    if (meth_ == 1)
    {
        *pdh = max(fabs(h_) * pdlast, 0.000001);
        if (l < lmax)
            *rhup = min(*rhup, sm1[l] / *pdh);
        rhsm = min(rhsm, sm1[nq] / *pdh);
        if (nq > 1)
            rhdn = min(rhdn, sm1[nq - 1] / *pdh);
        pdest = 0.;
    }
    if (rhsm >= *rhup)
    {
        if (rhsm >= rhdn)
        {
            newq = nq;
            *rh = rhsm;
        }
        else
        {
            newq = nq - 1;
            *rh = rhdn;
            if (kflag < 0 && *rh > 1.)
                *rh = 1.;
        }
    }
    else
    {
        if (*rhup <= rhdn)
        {
            newq = nq - 1;
            *rh = rhdn;
            if (kflag < 0 && *rh > 1.)
                *rh = 1.;
        }
        else
        {
            *rh = *rhup;
            if (*rh >= 1.1)
            {
                r = el[l] / (double)l;
                nq = l;
                l = nq + 1;
                for (size_t i = 1; i <= n; ++i)
                    yh_[l][i] = acor[i] * r;

                *orderflag = 2;
                return;
            }
            else
            {
                ialth = 3;
                return;
            }
        }
    }
    /*
       If meth_ = 1 and h_ is restricted by stability, bypass 10 percent test.
    */
    if (1 == meth_)
    {
        if ((*rh * *pdh * 1.00001) < sm1[newq])
            if (kflag == 0 && *rh < 1.1)
            {
                ialth = 3;
                return;
            }
    }
    else
    {
        if (kflag == 0 && *rh < 1.1)
        {
            ialth = 3;
            return;
        }
    }
    if (kflag <= -2)
        *rh = min(*rh, 0.2);
    /*
       If there is a change of order, reset nq, l, and the coefficients.
       In any case h_ is reset according to rh and the yh_ array is rescaled.
       Then exit or redo the step.
    */
    if (newq == nq)
    {
        *orderflag = 1;
        return;
    }
    nq = newq;
    l = nq + 1;
    *orderflag = 2;

} /* end orderswitch   */

void LSODA::resetcoeff()
/*
   The el vector and related constants are reset
   whenever the order nq is changed, or at the start of the problem.
*/
{
    array<double, 14> ep1;

    ep1 = elco[nq];
    for (size_t i = 1; i <= l; ++i)
        el[i] = ep1[i];
    rc = rc * el[1] / el0;
    el0 = el[1];
    conit = 0.5 / (double)(nq + 2);
}

#endif
