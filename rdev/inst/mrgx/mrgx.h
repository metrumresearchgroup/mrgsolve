#ifndef MRGX_H
#define MRGX_H
namespace mrgx {


double rbinom(double p) {
  Rcpp::stop("Please use R::rbinom rather than mrgx::rbinom.");
  return 1.0;
}
double rbeta(double a, double b) {
  Rcpp::stop("Please use R::rbeta rather than mrgx::rbeta.");
  return 1.0;
  
}
double rweibull(double shape, double scale) {
  Rcpp::stop("Please use R::rweibull rather than mrgx::rweibull.");
  return 1.0;
}
double rnorm(double mean, double sd) {
  Rcpp::stop("Please use R::rnorm rather than mrgx::rnorm.");
  return 1.0;
}
double rpois(double lambda) {
  Rcpp::stop("Please use R::rpois rather than mrgx::rpois.");
  return 1.0;
}
double rexp(double rate) {
  Rcpp::stop("Please use R::rexp rather than mrgx::rexp.");
  return 1.0;
}
double rgamma(double shape, double scale) {
  Rcpp::stop("Please use R::rgamma rather than mrgx::rgamma.");
  return 1.0;
}
double runif(double a, double b) {
  Rcpp::stop("Please use R::runif rather than mrgx::runif.");
  return 1.0;
}
}

#endif
