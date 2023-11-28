// Copyright (C) 2013 - 2023  Metrum Research Group
//
// This file is part of mrgsolve.
//
// mrgsolve is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// mrgsolve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


/**
 * @file devtran.cpp
 *
 */

#include <string>
#include "mrgsolve.h"
#include "odeproblem.h"
#include "dataobject.h"
#include "RcppInclude.h"
#include "LSODA.h"

#define CRUMP(a) throw Rcpp::exception(a,false)
#define REP(a)   Rcpp::Rcout << #a << std::endl;
#define nREP(a)  Rcpp::Rcout << a << std::endl;
#define say(a)   Rcpp::Rcout << a << std::endl;
#define __ALAG_POS -1200

// [[Rcpp::export]]
Rcpp::List ADVANCE(const Rcpp::List parin,
                   const Rcpp::List& funs,
                   const Rcpp::NumericVector times,
                   const Rcpp::S4& mod) {
  
  // passed along
  Rcpp::Environment envir = mod.slot("envir");
  
  // We need to decrement capture indices; this needs to be cloned
  Rcpp::CharacterVector cap = mod.slot("capture");
  Rcpp::IntegerVector capture = mod.slot("Icap");
  capture = Rcpp::clone(capture); 
  capture = capture - 1;
  
  // request is compartments to bring into output; decrement --> clone
  Rcpp::IntegerVector request = mod.slot("Icmt");
  request = Rcpp::clone(request);
  request = request - 1;
  
  // Parameters; clone names
  const Rcpp::S4 ParamS4 = mod.slot("param");
  const Rcpp::List Param = ParamS4.slot("data");
  Rcpp::CharacterVector paramnames(Param.names());
  paramnames = Rcpp::clone(paramnames);
  
  // Compartments; clone names
  const Rcpp::S4 InitS4 = mod.slot("init");
  const Rcpp::List Init = InitS4.slot("data");
  Rcpp::CharacterVector cmtnames(Init.names());
  cmtnames = Rcpp::clone(cmtnames);
  Rcpp::NumericVector init(Init.size());
  for(int i = 0; i < init.size(); ++i) {
    init[i] = Init[i];  
  }
  
  unsigned int crow = 0;
  
  // Create odeproblem object
  odeproblem prob(Param, init, funs, cap.size());
  prob.omega(mod);
  prob.sigma(mod);
  prob.copy_parin(parin, mod);
  prob.pass_envir(&envir);
  const unsigned int neq = prob.neq();
  LSODA solver(neq, mod);
  
  // Requested compartments
  const unsigned int nreq = request.size();
  
  // Captures
  const unsigned int n_capture  = capture.size();
  
  // Create results matrix:
  //  rows: ntime*nset
  //  cols: rep, time, eq[0], eq[1], ..., yout[0], yout[1],...
  const unsigned int NN = times.size();
  const unsigned int n_out_col  = 1 + nreq + n_capture;
  Rcpp::NumericMatrix ans(NN,n_out_col);
  const unsigned int req_start = 1;
  const unsigned int capture_start = req_start + nreq;
  
  crow = 0; // current output row
  int ic = prob.interrupt; // interrupt counter
  
  prob.nid(1);
  prob.nrow(NN);
  prob.idn(0);
  prob.rown(0);
  
  prob.config_call();
  reclist mtimehx;
  bool used_mtimehx = false;
  
  const bool do_interrupt = prob.interrupt > 0;
  
  double tfrom = times[0];
  double tto = tfrom;
  
  prob.y_init(init);
  
  //prob.set_d(a[i][0]);
  prob.init_call(tfrom);
  
  // i is indexing the subject, j is the record
  for(size_t i=0; i < times.size(); ++i) {
    
    tto = times[i];
    
    if(crow == NN) continue;
    
    prob.rown(crow);
    prob.advance(tfrom,tto,solver);
    prob.table_call();
    ans(crow,0) = tto;
    int k = 0;
    for(unsigned int i=0; i < n_capture; ++i) {
      ans(crow,k+capture_start) = prob.capture(capture[i]);
      ++k;
    }
    for(unsigned int k=0; k < nreq; ++k) {
      ans(crow,(k+req_start)) = prob.y(request[k]);
    }
    tfrom = tto;
    ++crow;
  }
  
  return Rcpp::List::create(Rcpp::Named("data") = mat2df(ans));
}
