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
T get(databox& self, std::string name) {
  Rcpp::Environment env = get_envir(self);
  return env[name];
}

}

#endif

