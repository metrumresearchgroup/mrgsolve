

namespace mrgx {
Rcpp::NumericMatrix as_matrix(const std::vector<double>& value_, 
                              const std::vector<std::string>& cols) {
  
  Rcpp::NumericVector value = Rcpp::wrap(value_);
  
  size_t ncol = cols.size();
  size_t nrow = value_.size()/ncol;
  
  if(ncol * nrow != value.size()) {
    Rcpp::stop(
      "the number of cols is not consistent with the size of `value_`."
    );  
  }
  
  value.attr("dim") = Rcpp::Dimension(ncol, nrow);
  Rcpp::NumericMatrix mat = Rcpp::as<Rcpp::NumericMatrix>(value);
  mat = Rcpp::transpose(mat);
  
  Rcpp::colnames(mat) = Rcpp::wrap(cols);
  
  return mat;
}
};

