// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include "RcppInclude.h"
#include "mrgsolve.h"
#include "odeproblem.h"
#include "Rodeproblem.h"
#include "pkevent.h"
#include <vector>
#include <string>
#include "dataobject.h"
#include "boost/tokenizer.hpp"
#include "boost/foreach.hpp"

typedef boost::tokenizer<boost::escaped_list_separator<char> > so_tokenizer;

double digits(double a, double b) {
  return std::floor(a*b)/b;
}


int find_position(std::string what, svec& table) {
  svec::iterator svit = std::find(table.begin(), table.end(), what);
  if(svit==table.end()) return(-1);
  return(svit - table.begin());
}



// [[Rcpp::export]]
void test_stop() {
  Rcpp::stop("This is an error message from Rcpp::stop.");
}


Rcpp::IntegerVector stl_sort(Rcpp::IntegerVector x) {
  //http://gallery.rcpp.org/articles/sorting/
  Rcpp::IntegerVector y = clone(x);
  std::sort(y.begin(), y.end());
  return y;
}


extern "C" {void mrgsolve_no_init_function(double *y,  std::vector<double>& param) {}}

void neg_istate(int istate) {
  Rcpp::Rcout << std::endl << "mrgsolve: DLSODA returned with istate " << istate << std::endl;
  /*
   ISTATE = 2  if DLSODA was successful, negative otherwise.
   -1 means excess work done on this call (perhaps wrong JT).
   -2 means excess accuracy requested (tolerances too small).
   -3 means illegal input detected (see printed message).
   -4 means repeated error test failures (check all inputs).
   -5 means repeated convergence failures (perhaps bad Jacobian
   supplied or wrong choice of JT or tolerances).
   -6 means error weight became zero during problem. (Solution
   component i vanished, and ATOL or ATOL(i) = 0.)
   -7 means work space insufficient to finish (see messages).
   */
  
  switch (istate) {
  case -1:
    Rcpp::Rcout << "  excess work done on this call; check the model or increase maxsteps." << std::endl << std::endl;
    break;
  case -2:
    Rcpp::Rcout << "  excess accuracy requested; reduce atol and/or rtol." << std::endl  << std::endl;
    break;
  case -3:
    Rcpp::Rcout << "  illegal input detected (see printed message)." << std::endl  << std::endl;
    break;
  case -4:
    Rcpp::Rcout << "  repeated error test failures (check all inputs)." << std::endl  << std::endl;
    break;
  case -5:
    Rcpp::Rcout << "  means repeated convergence failures "<< std::endl;
    Rcpp::Rcout << "  (perhaps bad Jacobian supplied or wrong choice of JT or tolerances)." << std::endl  << std::endl;
    break;
  case -6:
    Rcpp::Rcout << "  error weight became zero during problem." << std::endl  << std::endl;
    Rcpp::Rcout << "  (Solution component i vanished, and ATOL or ATOL(i) = 0.)" << std::endl  << std::endl;
    break;
    //   case -7:
    // Rcpp::Rcout << "  work space insufficient to finish (see messages)." << std::endl;
    // break;
  default:
    break;
  }
}



// [[Rcpp::export]]
Rcpp::NumericVector CALLINIT(Rcpp::NumericVector Nparam, Rcpp::NumericVector Ninit, SEXP xifun) {
  
  Rodeproblem *prob = new Rodeproblem(Nparam,Ninit);
  
  int i=0;
  int neq = prob->neq();
  
  prob->Rodeproblem::init_fun(xifun);
  
  Rcpp::NumericVector ans(neq);
  
  double time =0;
  prob->time(0);
  prob->evid(0);
  
  prob->init_call(time);
  
  for(i=0; i < neq; i++) ans[i] = prob->init(i);
  delete prob;
  return(ans);
}


// [[Rcpp::export]]
Rcpp::List TOUCH_FUNS(Rcpp::NumericVector lparam, 
                      Rcpp::NumericVector linit,
                      int Neta, int Neps,
                      Rcpp::CharacterVector capture,
                      SEXP xifun, SEXP xtfun, SEXP xdfun) {
  
  
  int i;
  
  Rcpp::List ans;
  
  Rodeproblem *prob  = new Rodeproblem(lparam, linit);
  
  prob->Rodeproblem::init_fun(xifun);
  prob->Rodeproblem::table_fun(xtfun);
  prob->resize_capture(capture.size());
  prob->neta(Neta);
  prob->neps(Neps);
  
  double time = 0;
  prob->time(time);
  prob->newind(0);
  
  prob->init_call(time);
  prob->table_init_call();
  
  std::vector<std::string> tablenames;
  const sd_map& Tabledata = prob->table();
  for(tablemap::const_iterator it=Tabledata.begin(); it !=Tabledata.end(); ++it) {
    tablenames.push_back(it->first);
  }
  
  const dvec& init = prob->init();
  
  Rcpp::NumericVector init_val(prob->neq());
  
  for(i=0; i < (prob->neq()); i++) init_val[i] = init[i];
  
  ans["tnames"] = tablenames;
  ans["init"] = init_val;
  ans["npar"] = prob->npar();
  ans["neq"] = prob->neq();
  delete prob;
  return(ans);
}



// [[Rcpp::export]]
arma::mat MVGAUSS(Rcpp::NumericMatrix OMEGA_, int n, int seed) {
  
  //  std::srand(12523);
  
  arma::mat OMEGA( OMEGA_.begin(), OMEGA_.nrow(), OMEGA_.ncol(), false );
  
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


// [[Rcpp::export]]
Rcpp::List SIMRE(int n1, Rcpp::NumericMatrix OMEGA, int n2, Rcpp::NumericMatrix SIGMA, int seed) {
  
  arma::mat eta;
  arma::mat eps;
  if(OMEGA.nrow() > 0) eta = MVGAUSS(OMEGA,n1,-1);
  if(SIGMA.nrow() > 0) eps = MVGAUSS(SIGMA,n2,-1);
  
  Rcpp::List ans;
  ans["eta"] = eta;
  ans["eps"] = eps;
  
  return(ans);
}


Rcpp::CharacterVector colnames(Rcpp::NumericMatrix x) {
  Rcpp::List dimnames = x.attr("dimnames");
  return(dimnames[1]);
}

void asSDmap(sd_map& out, Rcpp::List x) {
  Rcpp::CharacterVector names = x.attr("names");
  for(int i=0; i < x.size(); i++) {
    out[std::string(names[i])] = Rcpp::as<double>(x[i]);
  }
}

//[[Rcpp::export]]
void decorr(Rcpp::NumericMatrix x) {
  int i = 1, j = 1, n = x.nrow();
  if(n != x.ncol()) Rcpp::stop("matrix is not square");
  for(i=0; i < n; i++) {
    for(j=0; j < n; j++) {
      if(j!=i) x(i,j) = x(i,j)*sqrt(x(i,i)*x(j,j));
    }
  }
}


//[[Rcpp::export]]
Rcpp::NumericMatrix ZERO(Rcpp::NumericMatrix x) {
  int i=0, j=0;
  for(i=0; i < x.ncol(); i++) {
    for(j=0; j < x.nrow(); j++) {
      x(i,j) = 0;
    }
  }
  return(x);
}


//[[Rcpp::export]]
Rcpp::NumericMatrix SUPERMATRIX(Rcpp::List a,bool keep_names) {
  
  int j,k;
  Rcpp::NumericMatrix mat;
  
  int tot=0;
  
  Rcpp::CharacterVector rnam;
  Rcpp::CharacterVector cnam;
  
  Rcpp::CharacterVector this_nam;
  Rcpp::List dnames(2);
 
  
  for(int i=0, n = a.size(); i < n; i++) {
    mat = Rcpp::as<Rcpp::NumericMatrix>(a[i]);
    if(mat.nrow() ==0) continue;
    if(mat.nrow() != mat.ncol()) Rcpp::stop("Not all matrices are square");
    
    tot = tot + mat.nrow();
    
    if(!keep_names) continue;
    
    dnames = mat.attr("dimnames");
    
    if(dnames.size()==0) {
      for(j=0; j < mat.nrow(); j++) {
        rnam.push_back(".");
        cnam.push_back(".");
      }
      continue;
    }
    
    if(!Rf_isNull(dnames[0])) {
      this_nam = dnames[0];
      for(int j=0, n=this_nam.size(); j < n; j++) rnam.push_back(this_nam[j]);
    } else {
      for(j=0; j < mat.nrow(); j++) rnam.push_back(".");
    }
    if(!Rf_isNull(dnames[1])) {
      this_nam = dnames[1];
      for(int j=0, n=this_nam.size(); j < n; j++) cnam.push_back(this_nam[j]);
    } else {
      for(j=0; j < mat.ncol(); j++) cnam.push_back(".");
    }
  }
  
  
  int totrow = 0;
  int totcol = 0;
  
  Rcpp::NumericMatrix ret(tot,tot);
  for(int i=0, n=a.size(); i < n; i++) {
    mat = Rcpp::as<Rcpp::NumericMatrix>(a[i]);
    
    for(j=0; j < mat.nrow(); j++) {
      for(k=0; k < mat.ncol(); k++) {
        ret(totrow+j,totcol+k) = mat(j,k);
      }
    }
    totrow = totrow + mat.nrow();
    totcol = totcol + mat.ncol();
  }
  
  if(keep_names) {
    Rcpp::List dn = Rcpp::List::create(rnam,cnam);
    ret.attr("dimnames") = dn;
  }
  
  return(ret);
}




void match_both (svec a, svec b, ivec& ai, ivec& bi) {
  
  si_map A;
  si_map B;
  svec inter;

  for(size_t i=0; i < a.size(); i++) A[a[i]] = i;
  for(size_t i=0; i < b.size(); i++) B[b[i]] = i;
  
  std::sort(a.begin(), a.end());
  std::sort(b.begin(), b.end());
  
  std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                        std::back_inserter(inter));
  
  ai.resize(inter.size());
  for(size_t i=0; i < inter.size(); i++) ai[i] = A[inter[i]];
  
  bi.resize(inter.size());
  for(size_t i=0; i < inter.size(); i++) bi[i] = B[inter[i]];
  
}


void match_one (svec a, svec b, ivec& ret) {
  
  si_map A;
  svec inter;
  
  for(size_t i=0; i < a.size(); i++) A[a[i]] = i;
  
  std::sort(a.begin(), a.end());
  std::sort(b.begin(), b.end());
  
  std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                        std::back_inserter(inter));
  ret.resize(inter.size());
  for(size_t i=0; i < inter.size(); i++) ret[i] = A[inter[i]];
}

//[[Rcpp::export]]
Rcpp::List map_data_set(Rcpp::NumericMatrix data_, Rcpp::NumericVector inpar,bool lc_) {
  
  svec parnames = Rcpp::as<svec>(inpar.attr("names"));
  
  dataobject* data = new dataobject(data_,parnames);
  
  data->map_uid();
  data->locate_tran();
  data -> idata_row();
  Rcpp::List ans = data->ex_port();
  
  delete data;
  
  return(ans);
}



//[[Rcpp::export]]
Rcpp::List get_tokens(Rcpp::CharacterVector code) {
  
  Rcpp::List ret(code.size());
  
  for(int i = 0; i < code.size(); i++) {
    Rcpp::CharacterVector tokens;
    std::string s = Rcpp::as<std::string>(code[i]);
    boost::tokenizer<> tok(s);
    for(boost::tokenizer<>::iterator beg=tok.begin(); beg!=tok.end();++beg){
      tokens.push_back(*beg);
    }
    ret[i] = tokens;
  }
  
  
  Rcpp::List ans;
  ans["tokens"] = ret;
  return ans;
}



//[[Rcpp::export]]
Rcpp::List get_sep_tokens(Rcpp::CharacterVector code) {
  
  Rcpp::List ret(code.size());
  
  std::string sep1("\\");
  std::string sep2(";,");
  std::string sep3("\"\'");//let it have quoted arguments
  boost::escaped_list_separator<char>sep(sep1,sep2,sep3);
  for(int i = 0; i < code.size(); i++) {
    Rcpp::CharacterVector tokens;
    std::string s = Rcpp::as<std::string>(code[i]);
    
    so_tokenizer tok(s,sep);
    for(so_tokenizer::iterator beg=tok.begin(); beg!=tok.end();++beg){
      tokens.push_back(*beg);
    }
    ret[i] = tokens;
  }
  
  
  Rcpp::List ans;
  ans["tokens"] = ret;
  return ans;
}


//[[Rcpp::export]]
void set_omega(SEXP loc, Rcpp::NumericMatrix& omega_) {
  Rcpp::NumericMatrix* a = reinterpret_cast<Rcpp::NumericMatrix*>(R_ExternalPtrAddr(loc));
  *a = omega_;
  return; 
}






