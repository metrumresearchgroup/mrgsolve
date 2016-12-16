// #ifndef SIMETA_H
// #define SIMETA_H
// namespace mrgx {
// 
// void simeta(arma::mat* m, std::vector<double>&ETA) {
//   
//   //arma::mat* m = reinterpret_cast<arma::mat*>(m_);
//   arma::vec eigval;
//   arma::mat eigvec;
//   arma::eig_sym(eigval,eigvec, *m);
//   
//   int ncol = (*m).n_cols;
//   
//   arma::mat X = arma::randn<arma::mat>(1,ncol);
//   
//   eigval = arma::sqrt(eigval);
//   arma::mat Z = arma::diagmat(eigval);
//   X = eigvec * Z * X.t();
//   X = X.t();
//   for(int i=0; i < ncol; i++) ETA[i] = X(0,i);
//   
// }
// 
// arma::mat cast_matrix(void* m_) {
//   arma::mat* m = reinterpret_cast<arma::mat*>(m_); 
//   return *m;
// }
// }
// #endif
