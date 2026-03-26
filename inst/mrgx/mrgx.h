// Copyright (C) 2013 - 2026  Metrum Research Group
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

#include "mrgsolv.h"

/** 
 * @defgroup mrgx mrgx functions
 * Extra C++ functions provided by mrgsolve.  To use these functions, use 
 * `$PLUGIN mrgx` in your model file. Using `mrgx` will also invoke 
 * the `Rcpp` plugin.
 * 
 */

namespace mrgx {

/**
 * @brief Return the model environment.  
 * 
 * With each mrgsolve model object, there is an R environment that can be used 
 * to maintain arbitrary R objects, potentially for use in the model.
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
 * $PLUGIN mrgx
 * 
 * $GLOBAL
 * Rcpp::NumericMatrix mat;
 * 
 * $PREAMBLE
 * mat = mrgx::get<Rcpp::NumericMatrix>("mymatrix", self); 
 * 
 * $ENV
 * mat <- dmat(1,2,3)
 * @endcode
 * 
 * Note: for this to work, you must define a numeric matrix called `mat`
 * in `$ENV`.
 * 
 * @ingroup mrgx
 * @param name name of the R object to get
 * @param self the model data object
 * @return an object from the model environment
 */
template<typename _T___>
_T___ get(const std::string name, const databox& self) {
  Rcpp::Environment env = get_envir(self);
  return env[name];
}

/**
 * Assign an object to the model environment.
 * 
 * Example:
 * @code
 * $PLUGIN mrgx
 *
 * $GLOBAL Rcpp::NumericVector ans(100);
 *
 * $PREAMBLE
 * for(R_xlen_t i = 0; i < ans.size(); ++i) {
 *   ans[i] = R::rnorm(0,1);
 * }
 * 
 * $PK
 * if(FINAL_ROW) {
 *   mrgx::assign("vec", ans, self);
 * }
 * @endcode
 * 
 * @ingroup mrgx
 * @param name name of the R object to to be assigned in the model evironment.
 * @param object the object to assign in the model environment.
 * @param self the model object.
 * @return `void`. 
 */
template<typename _T___>
void assign(const std::string name, const _T___& object, const databox& self) {
  Rcpp::Environment env = get_envir(self);
  env.assign(name, object);
  return;
}

/**
 * Get an R object from the global environment.
 * 
 * Example:
 * @code
 * Rcpp::NumericVector x = mrgx::get<Rcpp::NumericVector>("x");
 * @endcode
 * Note: for this to work, `x` must be a numeric vector in `.GlobalEnv`.
 * 
 * @ingroup mrgx
 * @param name name of the R object to get
 * @return an object from the global environment
 */
template<typename _T___>
_T___ get(const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return env[name];
} 

/**
 * Get an R object from a package namespace.  This is typically used to get a 
 * function from a specific package.
 * 
 * Example:
 * 
 * To get a function, use this code:
 * @code
 * $PLUGIN mrgx
 * 
 * $GLOBAL 
 * Rcpp::Function Rnorm = mrgx::get<Rcpp::Function>("stats", "rnorm"); 
 * Rcpp::Function Seq = mrgx::get<Rcpp::Function>("base", "seq");
 * 
 * $ERROR 
 * if(FINAL_ROW) {
 *   mrgx::assign("double", Rnorm(10), self); 
 *   mrgx::assign("int", seq(10), self);
 * }
 * @endcode
 * 
 * @ingroup mrgx
 * @param package name of the package
 * @param name name of the object to get
 * @return an object from the package namespace
 */
template<typename _T___>
_T___ get(const std::string package, const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::namespace_env(package);
  _T___ ans = env[name];
  return ans;
}

/**
 * Read an RDS file.
 * 
 * @code
 * $PLUGIN mrgx
 * 
 * $ENV 
 * file <- file.path(tempdir(), "file-read-rds.RDS")
 * x <- list(a = 1.23, b = c(1,2,3,99))
 * saveRDS(object = x, file = file)
 * 
 * $PREAMBLE
 * std::string file = mrgx::get<std::string>("file", self);
 * 
 * Rcpp::List y = mrgx::readRDS<Rcpp::List>(file);
 * 
 * capture a = y["a"];
 * capture b = Rcpp::as<Rcpp::NumericVector>(y["b"])[3];
 * @endcode
 * 
 * @ingroup mrgx
 * @param filename the name of the RDS file to read.
 * @return an object saved in the RDS file
 */
template<typename _T___>
_T___ readRDS(const std::string filename) {
  Rcpp::Function readRDS = get<Rcpp::Function>("base", "readRDS");
  return readRDS(filename);
}

/**
 * An empty R function.  This is typically used as a placeholder when declaring 
 * an `Rcpp::Function` object in `$GLOBAL`. This pattern of declaring in 
 * `$GLOBAL` and (re)defining in `$PREAMBLE` could be used for efficiency when 
 * the function must be called many times.
 * 
 * @code
 * $PLUGIN mrgx
 * 
 * $GLOBAL 
 * Rcpp::Function fun = mrgx::mt_fun(); 
 * 
 * $PREAMBLE 
 * fun = mrgx::get<Rcpp::Function>("SEQ");
 * 
 * $ERROR 
 * if(FINAL_ROW) {
 *   Rcpp::IntegerVector ans = fun(34);    
 *   mrgx::assign("vec", ans, self);
 * }
 * @endcode
 * 
 * @ingroup mrgx
 * @return the function `mt_fun` from the mrgsolve namespace.
 */
Rcpp::Function mt_fun() {
  // See R/Aaaa.R
  Rcpp::Environment env = Rcpp::Environment::namespace_env("mrgsolve");
  Rcpp::Function ans = env["mrgx_mt_fun"];
  return ans;
}

}

#endif
