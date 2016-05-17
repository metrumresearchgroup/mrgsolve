#include <Rcpp.h>


namespace DIST {
  Rcpp::NumericVector ans;

  double rbinom(double p) {
    ans = Rcpp::rbinom(1,1,p);
    return(double(ans[0]));
  }
  double rbeta(double a, double b) {
    ans = Rcpp::rbeta(1,a,b);
    return(double(ans[0]));
  }
  double rweibull(double shape, double scale) {
    ans = Rcpp::rweibull(1,shape,scale);
    return(double(ans[0]));
  }
  double rnorm(double mean, double sd) {
    ans = Rcpp::rnorm(1,mean,sd);
    return(double(ans[0]));
  }
  double rpois(double lambda) {
    ans = Rcpp::rpois(1,lambda);
    return(double(ans[0]));
  }
  double rexp(double rate) {
    ans = Rcpp::rexp(1,rate);
    return(double(ans[0]));
  }
  double rgamma(double shape, double scale) {
    ans = Rcpp::rgamma(1,shape,1.0/scale);
    return(double(ans[0]));
  }
  double runif(double a, double b) {
    ans = Rcpp::runif(1,a,b);
    return(double(ans[0]));
  }
}


