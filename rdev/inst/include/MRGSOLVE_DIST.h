#include <Rcpp.h>


namespace DIST {
  double rbinom(double p) {
    Rcpp::NumericVector ans = Rcpp::rbinom(1,1,p);
    return(double(ans[0]));
  }
  double rbeta(double a, double b) {
    Rcpp::NumericVector ans = Rcpp::rbeta(1,a,b);
    return(double(ans[0]));
  }
  double rweibull(double shape, double scale) {
    Rcpp::NumericVector ans = Rcpp::rweibull(1,shape,scale);
    return(double(ans[0]));
  }
  double rnorm(double mean, double sd) {
    Rcpp::NumericVector ans = Rcpp::rnorm(1,mean,sd);
    return(double(ans[0]));
  }
  double rpois(double lambda) {
    Rcpp::NumericVector ans = Rcpp::rpois(1,lambda);
    return(double(ans[0]));
  }
  double rexp(double rate) {
    Rcpp::NumericVector ans = Rcpp::rexp(1,rate);
    return(double(ans[0]));
  }
  double rgamma(double shape, double scale) {
    Rcpp::NumericVector ans = Rcpp::rgamma(1,shape,1.0/scale);
    return(double(ans[0]));
  }
  double runif(double a, double b) {
    Rcpp::NumericVector ans = Rcpp::runif(1,a,b);
    return(double(ans[0]));
  }
}


