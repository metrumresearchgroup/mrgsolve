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
 * @file mrgx.h
 */

#ifndef MRGX_H
#define MRGX_H

#include "modelheader.h"

/** 
 * @defgroup mrgx mrgx functions
 * Extra C++ functions provided by mrgsolve.  To use these functions, use `$PLUGIN mrgx` in your model file.
 * 
 */

namespace mrgx {

/**
 * @brief Return the model environment.  
 * 
 * With each mrgsolve model object,
 * there is an R environment that can be used to maintain arbitrary 
 * R objects, potentially for use in the model.
 * 
 * @ingroup mrgx
 * @param self the model databox object
 * @return the model environment
 */
Rcpp::Environment get_envir(const databox& self) {
  return (*(reinterpret_cast<Rcpp::Environment*>(self.envir)));
}

/**
 * Simulate random normal variate between lower and upper boundaries.  An 
 * error is generated if a variate between lower and upper bounds cannot be
 * generated in 50 tries.
 * 
 * Example:
 * @code
 * $MAIN
 * double WT = mrgx::rnorm(80,40,40,140);
 * @endcode
 * 
 * @ingroup mrgx
 * @param mean normal distribution mean
 * @param sd normal distribution standard deviation
 * @param lower lower bound for variates
 * @param upper upper bound for variates
 * @return the simulated variate
 * 
 * 
 */
double rnorm(const double mean, const double sd, const double lower, 
             const double upper) {
  double x = 0;
  for(int i=0; i < 50; ++i) {
    x = R::rnorm(mean,sd);
    if(x >= lower && x <= upper) return(x);
  }
  Rcpp::stop("Could not simulate normal variate within specified bounds");
}
/**
 * Simulate random lognormal variate between lower and upper boundaries.  An 
 * error is generated if a variate between lower and upper bounds cannot be
 * generated in 50 tries.
 * 
 * Example:
 * @code
 * $MAIN
 * double WT = mrgx::rlnorm(log(80),1,40,140);
 * @endcode
 * 
 * @ingroup mrgx
 * @param mean normal distribution mean
 * @param sd normal distribution standard deviation
 * @param lower lower bound for variates
 * @param upper upper bound for variates
 * @return the simulated variate
 */
double rlognorm(const double mean, const double sd, const double lower, 
                const double upper) {
  double x = 0;
  for(int i=0; i < 50; ++i) {
    x = exp(R::rnorm(mean,sd));
    if(x >= lower && x <= upper) {
      return(x);
    }
  }
  Rcpp::stop("Could not simulate log normal variate within specified bounds");
}

/**
 * Get an R object from the model environment.
 * 
 * Example:
 * @code
 * $GLOBAL
 * Rcpp::NumericMatrix mat;
 * 
 * $PREAMBLE
 * mat = mrgx::get<Rcpp::NumericMatrix>("mymatrix", self); 
 * 
 * $ENV
 * mat <- dmat(1,2,3)
 * @endcode
 * Note: for this to work, you must define a numeric matrix called `mat`
 * in `$ENV`.
 * 
 * @ingroup mrgx
 * @param name name of the R object to get
 * @param self the model data object
 * @return an object from the model environment
 */
template<typename T>
T get(const std::string name, const databox& self) {
  Rcpp::Environment env = get_envir(self);
  return env[name];
}

/**
 * Get an R object from the global environment.
 * 
 * Example:
 * @code
 * Rcpp::NumericVector x = mrgx::get<Rcpp::NumericVector("x");
 * @endcode
 * Note: for this to work, `x` must be a numeric vector in `.GlobalEnv`.
 * 
 * @ingroup mrgx
 * @param name name of the R object to get
 * @return an object from the global environment
 */
template<typename T>
T get(const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return env[name];
} 

/**
 * Get an R object from a package namespace.  This is 
 * typically used to get a function from a 
 * specific package.
 * 
 * Example:
 * 
 * To get a function, use this code:
 * @code
 * $GLOBAL
 * Rcpp::Function myroot("uniroot");
 * @endcode
 * and then call `uniroot` as you please.
 * 
 * To use the `mrgx::get` function with `myroot` available in `$MAIN`:
 * 
 * @code
 * $MAIN
 * Rcpp::Function myroot = mrgx::get("stats", "uniroot");
 * @endcode
 * 
 * 
 * @ingroup mrgx
 * @param package name of the package
 * @param name name of the object to get
 * @return an object from the package namespace
 */
template<typename T>
T get(const std::string package, const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::namespace_env(package);
  T ans = env[name];
  return ans;
}

/**
 * Read an RDS file.
 * 
 * @ingroup mrgx
 * @param filename the name of the RDS file to read
 * @return an object saved in the RDS file
 */
template<typename T>
T readRDS(const std::string filename) {
  Rcpp::Function readRDS = get<Rcpp::Function>("base", "readRDS");
  return readRDS(filename);
}

/**
 * An empty R function.  This is typically used
 * as a placeholder when declaring an <code>Rcpp::Function</code> object.
 * 
 * @ingroup mrgx
 * @return the function <code>mt_fun</code> from the mrgsolve namespace
 */
Rcpp::Function mt_fun() {
  Rcpp::Environment env = Rcpp::Environment::namespace_env("mrgsolve");
  Rcpp::Function ans = env["mt_fun"];
  return ans;
}

}

#endif

