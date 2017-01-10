#ifndef MRGX_H
#define MRGX_H

#include "modelheader.h"

namespace mrgx {
Rcpp::Environment get_envir(databox& self) {
  return (*(reinterpret_cast<Rcpp::Environment*>(self.envir)));
}
double rnorm(const double mean, const double sd, const double lower, const double upper) {
  double x = 0;
  for(int i=0; i < 50; ++i) {
    x = R::rnorm(mean,sd);
    if(x >= lower && x <= upper) return(x);
  }
  Rcpp::stop("Could not simulate normal variate within specified bounds");
}
double rlognorm(const double mean, const double sd, const double lower, const double upper) {
  double x = 0;
  for(int i=0; i < 50; ++i) {
    x = exp(R::rnorm(mean,sd));
    if(x >= lower && x <= upper) {
      return(x);
    }
  }
  Rcpp::stop("Could not simulate log normal variate within specified bounds");
}

template<typename T>
T get(const std::string name, databox& self) {
  Rcpp::Environment env = get_envir(self);
  return env[name];
}
template<typename T>
T get(const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::global_env();
  return env[name];
} 
template<typename T>
T get(const std::string package, const std::string name) {
  Rcpp::Environment env = Rcpp::Environment::namespace_env(package);
  T ans = env[name];
  return ans;
}
template<typename T>
T readRDS(std::string filename) {
  Rcpp::Function readRDS = get<Rcpp::Function>("base", "readRDS");
  return readRDS(filename);
}
Rcpp::Function mt_fun() {
  Rcpp::Environment env = Rcpp::Environment::namespace_env("mrgsolve");
  Rcpp::Function ans = env["mt_fun"];
  return ans;
}

}

#endif

