arma::mat MVNORM(arma::mat OMEGA) {
  
  //  std::srand(12523);
  
  int n = 1;
  
  arma::vec eigval;
  arma::mat eigvec;
  arma::eig_sym(eigval,eigvec, OMEGA);
  
  int ncol = OMEGA.n_cols;
  
  arma::mat X = arma::randn<arma::mat>(n,ncol);
  
  eigval = arma::sqrt(eigval);
  arma::mat Z = arma::diagmat(eigval);
  X = eigvec * Z * X.t();
  return(X.t());
}
