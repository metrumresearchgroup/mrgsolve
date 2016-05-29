
void _SIMETA_(void* m_, std::vector<double>&ETA ) {
  
  int n = 1;
  arma::mat* m = reinterpret_cast<arma::mat*>(m_);
  arma::vec eigval;
  arma::mat eigvec;
  arma::eig_sym(eigval,eigvec, *m);
  
  int ncol = (*m).n_cols;
  
  arma::mat X = arma::randn<arma::mat>(n,ncol);
  
  eigval = arma::sqrt(eigval);
  arma::mat Z = arma::diagmat(eigval);
  X = eigvec * Z * X.t();
  X = X.t();
  for(int i=0; i < ncol; i++) ETA[i] = X(0,i);
}
