#ifndef MRGX_H
#define MRGX_H
namespace mrgx {

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

void simeta(void* m_, std::vector<double>&ETA ) {

  arma::mat* m = reinterpret_cast<arma::mat*>(m_);
  arma::vec eigval;
  arma::mat eigvec;
  arma::eig_sym(eigval,eigvec, *m);
  
  int ncol = (*m).n_cols;
  
  arma::mat X = arma::randn<arma::mat>(1,ncol);
  
  eigval = arma::sqrt(eigval);
  arma::mat Z = arma::diagmat(eigval);
  X = eigvec * Z * X.t();
  X = X.t();
  for(int i=0; i < ncol; i++) ETA[i] = X(0,i);
}

}

#endif
