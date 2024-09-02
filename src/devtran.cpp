// Copyright (C) 2013 - 2024  Metrum Research Group
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

#include <deque>
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

/** Perform a simulation run.
 *
 * @param parin list of options for the simulation
 * @param funs list of pointer addresses to model functions generated by
 *   getNativeSymbolInfo()
 * @param data the main data set
 * @param idata the idata data set
 * @param mod the model object for simulating
 * 
 * @return list containing matrix of simulated data and a character vector of
 * tran names that may have been carried into the output
 *
 */
// [[Rcpp::export]]
Rcpp::List DEVTRAN(const Rcpp::List parin,
                   const Rcpp::List& funs,
                   const Rcpp::NumericMatrix& data,
                   const Rcpp::NumericMatrix& idata,
                   const Rcpp::S4& mod) {
  
  const bool obsonly          = Rcpp::as<bool>   (parin["obsonly"]);
  const int  recsort          = Rcpp::as<int>    (parin["recsort"]);
  const bool filbak           = Rcpp::as<bool>   (parin["filbak"]);
  const bool tad              = Rcpp::as<bool>   (parin["tad"]);
  const bool nocb             = Rcpp::as<bool>   (parin["nocb"]);
  bool obsaug                 = Rcpp::as<bool>   (parin["obsaug"] );
  obsaug = obsaug & (data.nrow() > 0);
  
  // Grab items from the model object --------------------
  // No code for Model Matrices here; mod gets passed to odeproblem methods
  
  // Used here 
  const double digits = Rcpp::as<double>(mod.slot("digits"));
  const double tscale = Rcpp::as<double>(mod.slot("tscale"));
  const double mindt  = Rcpp::as<double>(mod.slot("mindt"));
  const bool   debug  = Rcpp::as<bool>(mod.slot("debug"));
  const bool  verbose = Rcpp::as<bool>(mod.slot("verbose")); 
  
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
  
  // Create data objects from data and idata
  dataobject dat(data, paramnames);
  dat.map_uid();
  dat.locate_tran();
  
  dataobject idat(idata, paramnames, cmtnames);
  idat.idata_row();
  
  // Number of individuals in the data set
  int NID = dat.nid();
  const int nidata = idat.nrow();
  
  unsigned int crow = 0;
  
  bool put_ev_first = false;
  bool addl_ev_first = true;
  
  switch (recsort) {
  case 1:
    break;
  case 2:
    put_ev_first = false;
    addl_ev_first = false;
    break;
  case 3:
    put_ev_first = true;
    addl_ev_first = true;
    break;
  case 4:
    put_ev_first = true;
    addl_ev_first = false;
    break;
  default:
    CRUMP("recsort must be 1, 2, 3, or 4.");
  }
  
  // Create odeproblem object
  odeproblem prob(Param, init, funs, cap.size());
  prob.omega(mod);
  prob.sigma(mod);
  prob.copy_parin(parin, mod);
  prob.pass_envir(&envir);
  const unsigned int neq = prob.neq();
  LSODA solver(neq, mod);
  
  recstack a(NID);
  
  unsigned int obscount = 0;
  unsigned int evcount = 0;
  dat.get_records(a, NID, neq, obscount, evcount, obsonly, debug);
  
  // Requested compartments
  const unsigned int nreq = request.size();
  
  // Columns from the data set to carry:
  Rcpp::CharacterVector data_carry_ = 
    Rcpp::as<Rcpp::CharacterVector >(parin["carry_data"]);
  const Rcpp::IntegerVector data_carry =  dat.get_col_n(data_carry_);
  const unsigned int n_data_carry = data_carry.size();
  
  // Columns from the idata set to carry:
  unsigned int n_idata_carry=0;
  Rcpp::IntegerVector idata_carry;
  if(nidata > 0) {
    Rcpp::CharacterVector idata_carry_ = 
      Rcpp::as<Rcpp::CharacterVector >(parin["carry_idata"]);
    idata_carry =  idat.get_col_n(idata_carry_);
    n_idata_carry = idata_carry.size();
    dat.check_idcol(idat);
  }
  
  // Tran Items to carry:
  Rcpp::CharacterVector tran_carry = 
    Rcpp::as<Rcpp::CharacterVector >(parin["carry_tran"]);
  const unsigned int n_tran_carry = tran_carry.size();
  
  // Captures
  const unsigned int n_capture  = capture.size();
  
  // Find tofd
  std::vector<double> tofd;
  if(tad) {
    tofd.reserve(a.size());
    for(recstack::const_iterator it = a.begin(); it !=a.end(); ++it) {
      for(reclist::const_iterator itt = it->begin(); itt != it->end(); ++itt) {
        if((*itt)->is_dose()) {
          tofd.push_back((*itt)->time());
          break;
        }
      }
    }
    if(tofd.size()==0) {
      tofd.resize(a.size(),0.0);
    }
    if(tofd.size() != a.size()) {
      CRUMP("There was a problem finding time of first dose.");
    }
  }
  
  // Need this for later
  int nextpos = put_ev_first ?  (1000000) : -10000;
  
  if((obscount == 0) || (obsaug)) {
    
    Rcpp::NumericMatrix tgrid = 
      Rcpp::as<Rcpp::NumericMatrix>(parin["tgridmatrix"]);
    
    if((tgrid.nrow() == 0) && (obscount==0) && (evcount==0)) {
      tgrid = Rcpp::NumericMatrix(1,1);  
    }
    
    bool multiple_tgrid = tgrid.ncol() > 1;
    
    // Already has C indexing
    Rcpp::IntegerVector tgridi = 
      Rcpp::as<Rcpp::IntegerVector>(parin["whichtg"]);
    
    // Number of non-na times in each design
    std::vector<int> tgridn;
    
    if(multiple_tgrid) {
      if(tgridi.size() < idata.nrow()) {
        CRUMP("Length of design indicator less than NID.");
      }
      if(max(tgridi) >= tgrid.ncol()) {
        CRUMP("Insufficient number of designs specified for this problem.");
      } 
      for(int i = 0; i < tgrid.ncol(); ++i) {
        tgridn.push_back(Rcpp::sum(!Rcpp::is_na(tgrid(Rcpp::_,i))));
      }
    } else {
      tgridn.push_back(tgrid.nrow());
      if(tgridi.size() == 0) {
        tgridi = Rcpp::rep(0,NID);
      }
    }
    
    // Create a common dictionary of observation events
    // Vector of vectors
    // Outer vector: length = number of designs
    // Inner vector: length = number of times in that design
    recstack designs;
    
    for(size_t i = 0; i < tgridn.size(); ++i) {
      
      reclist z;
      
      //z.reserve(tgridn[i]); // TODO: remove
      
      for(int j = 0; j < tgridn[i]; ++j) {
        rec_ptr obs = NEWREC(tgrid(j,i),nextpos,true);
        z.push_back(obs);
      }
      designs.push_back(z);
    }
    
    // We have to look up the design from the idata set
    for(recstack::iterator it = a.begin(); it != a.end(); ++it) {
      int  j, n;
      double id;
      if(multiple_tgrid) {
        id = dat.get_uid(it-a.begin());
        j  = idat.get_idata_row(id);
        n  = tgridn.at(tgridi.at(j));
      } else {
        j = 0;
        n = tgridn.at(0);
      } 
      
      //it->reserve((it->size() + n)); // TODO: remove
      for(int h=0; h < n; ++h) {
        it->push_back(designs[tgridi[j]][h]);
        ++obscount;
      } 
      std::sort(it->begin(), it->end(), CompRec());
    }
  }
  
  // Create results matrix:
  //  rows: ntime*nset
  //  cols: rep, time, eq[0], eq[1], ..., yout[0], yout[1],...
  const unsigned int NN = obsonly ? obscount : (obscount + evcount);
  int precol = 2 + int(tad);
  const unsigned int n_out_col  = precol + n_tran_carry
  + n_data_carry + n_idata_carry + nreq + n_capture;
  Rcpp::NumericMatrix ans(NN,n_out_col);
  const unsigned int tran_carry_start = precol;
  const unsigned int data_carry_start = tran_carry_start + n_tran_carry;
  const unsigned int idata_carry_start = data_carry_start + n_data_carry;
  const unsigned int req_start = idata_carry_start+n_idata_carry;
  const unsigned int capture_start = req_start+nreq;
  
  arma::mat eta;
  const int neta = prob.neta();
  if(neta > 0) {
    const std::string etasrc = Rcpp::as<std::string> (parin["etasrc"]);
    prob.set_eta();
    eta = prob.mv_omega(NID); 
    if(etasrc=="omega") {
      // Nothing else to do; we always simulate
      // from omega if neta > 0
    } else if(etasrc=="data") {
      eta = dat.get_etas(neta, false, etasrc);
    } else if(etasrc=="data.all") {
      eta = dat.get_etas(neta, true, etasrc);
    } else if(etasrc=="idata") {
      eta = idat.get_etas(neta, false, etasrc); 
    } else if(etasrc=="idata.all") {
      eta = idat.get_etas(neta, true, etasrc);
    } else {
      std::string msg = 
        R"(`etasrc` must be either:
              "omega"     = ETAs simulated from OMEGA
              "data"      = ETAs imported from the data set
              "idata"     = ETAs imported from the idata set
              "data.all"  = strict ETA import from data set
              "idata.all" = strict ETA import from idata set)";
      CRUMP(msg.c_str());  
    }
  }
  
  arma::mat eps;  
  const int neps = prob.neps();
  if(neps > 0) {
    prob.set_eps();
    eps = prob.mv_sigma(NN);
    prob.copy_sigma_diagonals();
  }
  
  Rcpp::CharacterVector tran_names;
  if(n_tran_carry > 0) {
    
    Rcpp::CharacterVector::iterator tcbeg  = tran_carry.begin();
    Rcpp::CharacterVector::iterator tcend  = tran_carry.end();
    
    const bool carry_evid = std::find(tcbeg,tcend, "evid")  != tcend;
    const bool carry_cmt =  std::find(tcbeg,tcend, "cmt")   != tcend;
    const bool carry_amt =  std::find(tcbeg,tcend, "amt")   != tcend;
    const bool carry_ii =   std::find(tcbeg,tcend, "ii")    != tcend;
    const bool carry_addl = std::find(tcbeg,tcend, "addl")  != tcend;
    const bool carry_ss =   std::find(tcbeg,tcend, "ss")    != tcend;
    const bool carry_rate = std::find(tcbeg,tcend, "rate")  != tcend;
    const bool carry_aug  = std::find(tcbeg,tcend, "a.u.g") != tcend;
    
    if(carry_evid) tran_names.push_back("evid");
    if(carry_amt)  tran_names.push_back("amt");
    if(carry_cmt)  tran_names.push_back("cmt");
    if(carry_ss)   tran_names.push_back("ss");
    if(carry_ii)   tran_names.push_back("ii");
    if(carry_addl) tran_names.push_back("addl");
    if(carry_rate) tran_names.push_back("rate");
    if(carry_aug)  tran_names.push_back("a.u.g");
    
    
    crow = 0;
    int n = 0;
    for(recstack::const_iterator it = a.begin(); it !=a.end(); ++it) {
      for(reclist::const_iterator itt = it->begin(); itt != it->end(); ++itt) {
        if(!(*itt)->output()) continue;
        n = 0;
        if(carry_evid) {ans(crow,n+precol) = (*itt)->evid();                     ++n;}
        if(carry_amt)  {ans(crow,n+precol) = (*itt)->amt();                      ++n;}
        if(carry_cmt)  {ans(crow,n+precol) = (*itt)->cmt();                      ++n;}
        if(carry_ss)   {ans(crow,n+precol) = (*itt)->ss();                       ++n;}
        if(carry_ii)   {ans(crow,n+precol) = (*itt)->ii();                       ++n;}
        if(carry_addl) {ans(crow,n+precol) = (*itt)->addl();                     ++n;}
        if(carry_rate) {ans(crow,n+precol) = (*itt)->rate();                     ++n;}
        if(carry_aug)  {ans(crow,n+precol) = ((*itt)->pos()==nextpos) && obsaug; ++n;}
        ++crow;
      }
    }
  }
  
  if(((n_idata_carry > 0) || (n_data_carry > 0)) ) {
    dat.carry_out(a,ans,idat,data_carry,data_carry_start,idata_carry,
                  idata_carry_start,nocb);
  }
  
  crow = 0; // current output row
  int ic = prob.interrupt; // interrupt counter
  
  prob.nid(dat.nid());
  prob.nrow(NN);
  prob.idn(0);
  prob.rown(0);
  
  prob.config_call();
  reclist mtimehx;
  bool used_mtimehx = false;
  
  bool has_idata = idat.nrow() > 0;
  int this_idata_row = 0;
  const bool do_interrupt = prob.interrupt > 0;
  
  if(verbose) say("starting the simulation ...");
  
  // i is indexing the subject, j is the record
  for(size_t i=0; i < a.size(); ++i) {
    
    double id = dat.get_uid(i);
    dat.next_id(i);
    prob.idn(i);
    prob.reset_newid(id);
    if(used_mtimehx) mtimehx.clear();  
    
    if(i==0) {
      prob.newind(0);
    }
    
    int this_cmtn = 0;
    double told = -1;
    
    double tfrom = a[i].front()->time();
    double tto = tfrom;
    double maxtime = a[i].back()->time();
    
    for(int k=0; k < neta; ++k) prob.eta(k,eta(i,k));
    for(int k=0; k < neps; ++k) prob.eps(k,eps(crow,k));
    
    prob.y_init(init);
    
    if(has_idata) {
      this_idata_row = idat.get_idata_row(id);
      idat.copy_parameters(this_idata_row,&prob);
      idat.copy_inits(this_idata_row,&prob);
    }
    
    if(dat.any_copy) {
      if(a[i][0]->from_data()) {
        dat.copy_parameters(a[i][0]->pos(),&prob);
      } else {
        if(filbak) {
          dat.copy_parameters(dat.start(i),&prob);
        }
      }
    }
    
    prob.set_d(a[i][0]);
    prob.init_call(tfrom);
    
    for(size_t j=0; j < a[i].size(); ++j) {
      
      if(do_interrupt && (!(--ic))) {
        Rcpp::checkUserInterrupt();
        ic = prob.interrupt;
      }
      
      if(crow == NN) continue;
      
      prob.rown(crow);
      
      rec_ptr this_rec = a[i][j];
      
      this_rec->id(id);
      
      if(prob.systemoff()) {
        // This starts a loop that will finish the remaining records 
        // for an individual; no other calls to any model functions will
        // be made
        // SYSTEMOFF = 1 --> copy model results to the line
        // SYSTEMOFF != 0, !=1 --> fill NA
        unsigned short int status = prob.systemoff();
        if(status==9) CRUMP("the problem was stopped at user request.");
        if(status==999) CRUMP("999 sent from the model.");
        if(this_rec->output()) {
          if(status==1) {
            ans(crow,0) = this_rec->id();
            ans(crow,1) = this_rec->time();
            for(unsigned int k=0; k < n_capture; ++k) {
              ans(crow,(k+capture_start)) = prob.capture(capture[k]);
            }
            for(unsigned int k=0; k < nreq; ++k) {
              ans(crow,(k+req_start)) = prob.y(request[k]);
            }
          } else {
            for(int k=0; k < ans.ncol(); ++k) {
              ans(crow,k) = NA_REAL;
            } 
          }
          ++crow;
        }
        continue;
      }
      
      if(dat.any_copy && nocb) {
        // will call lsoda_init if parameters are copied
        dat.copy_next_parameters(
          i, 
          this_rec->from_data(), 
          this_rec->pos(), 
          &prob
        );
      }
      
      tto = this_rec->time();
      
      double dt  = (tto-tfrom)/(tfrom == 0.0 ? 1.0 : tfrom);
      
      if((dt > 0.0) && (dt < mindt)) {
        tto = tfrom;
      }
      
      for(int k = 0; k < neps; ++k) {
        prob.eps(k,eps(crow,k));
      }
      
      if(j != 0) {
        prob.newind(2);
        prob.set_d(this_rec);
        if(!this_rec->is_lagged()) {
          // TODO: what other records should we skip?
          prob.init_call_record(tto);
        }
      }
      
      // Some non-observation event happening
      if(this_rec->is_event()) {
        
        this_cmtn = this_rec->cmtn();
        
        if(!this_rec->is_lagged()) {
          this_rec->fn(prob.fbio(this_cmtn));
        }
        
        if(this_rec->fn() < 0) {
          CRUMP("[mrgsolve] bioavailability fraction is less than zero.");
        }
        
        if(this_rec->fn()==0) {
          if(this_rec->is_dose()) {
            prob.on(this_cmtn);
            prob.lsoda_init();
            this_rec->unarm();
          }
        }
        
        bool sort_recs = false;
        
        if(this_rec->rate() < 0) {
          prob.rate_main(this_rec);
        }
        // Checking 
        if(!this_rec->is_lagged()) {
          
          if(prob.alag(this_cmtn) > mindt && this_rec->is_dose()) { // there is a valid lagtime
            
            if(this_rec->ss() > 0) {
              this_rec->steady(&prob, a[i], solver);
              tfrom = tto;
              this_rec->ss(0);
            }
            rec_ptr newev = NEWREC(*this_rec);
            newev->pos(__ALAG_POS);
            newev->phantom_rec();
            newev->lagged();
            newev->time(this_rec->time() + prob.alag(this_cmtn));
            newev->ss(0);
            insert_record(a[i], j, newev, put_ev_first);
            newev->schedule(a[i], maxtime, put_ev_first, NN, prob.alag(this_cmtn));
            this_rec->unarm();
            sort_recs = newev->needs_sorting();
          } else { // no valid lagtime
            this_rec->schedule(a[i], maxtime, addl_ev_first, NN, 0.0);
            sort_recs = this_rec->needs_sorting();
          }
        } // from data
        
        // This block gets hit for any and all infusions; sometimes the
        // infusion just got started and we need to add the lag time
        // sometimes it is an infusion via addl and lag time is already there
        if(this_rec->int_infusion() && this_rec->armed()) {
          rec_ptr evoff = NEWREC(this_rec->cmt(),
                                 9,
                                 this_rec->amt(),
                                 this_rec->time() + this_rec->dur(),
                                 this_rec->rate(),
                                 -299,
                                 id);
          
          if(this_rec->from_data()) {
            evoff->time(evoff->time() + prob.alag(this_cmtn));
          } 
          // Infusion off always happens first
          insert_record(a[i], j, evoff, true);
        }
        
        // SORT
        if(sort_recs) {
          std::sort(a[i].begin()+j+1,a[i].end(),CompRec());
        }
        
        if(tad) { // Adjusts told for lagtime
          if(!this_rec->is_lagged() && this_rec->is_dose()) {
            told = tto;
          }
        }
      } // is_dose
      
      prob.advance(tfrom,tto,solver);
      
      if(this_rec->evid() != 2) {
        this_rec->steady(&prob,a[i],solver);
        this_rec->implement(&prob);
      }
      
      if(!nocb && dat.any_copy) {
        if(this_rec->from_data()) {
          // will call lsoda_init
          dat.copy_parameters(this_rec->pos(),&prob);
        }
      }
      
      if(!this_rec->is_lagged()) {
        prob.table_call();
      }
      
      if(prob.any_mtime()) {
        // Will set used_mtimehx only if we push back
        std::vector<mrgsolve::evdata> mt  = prob.mtimes();
        for(size_t mti = 0; mti < mt.size(); ++mti) {
          // Unpack and check
          double this_time = (mt[mti]).time;
          if(this_time < tto && !mt[mti].now) continue;
          unsigned int this_evid = (mt[mti]).evid;
          if(this_evid==0) continue;
          double this_amt = mt[mti].amt;
          int this_cmt = (mt[mti]).cmt;
          double this_rate = (mt[mti]).rate;
          if(neq!=0 && this_evid !=0) {
            if((this_cmt == 0) || (abs(this_cmt) > int(neq))) {
              Rcpp::Rcout << this_cmt << std::endl;
              CRUMP("Compartment number in modeled event out of range.");
            }
          }
          // Create the record
          rec_ptr new_ev = NEWREC(this_cmt,this_evid,this_amt,this_time,
                                  this_rate,1.0);    
          new_ev->phantom_rec();
          new_ev->ss((mt[mti]).ss);
          new_ev->ii((mt[mti]).ii);
          new_ev->addl((mt[mti]).addl);
          // if doses happen "later" we never schedule here; it will
          //   get handled later
          // if the dose happens "now", we schedule now, even if there is lag
          //   time in play
          bool schedule_addl = false;
          if(mt[mti].now) {
            schedule_addl  = new_ev->addl() > 0;
            new_ev->fn(prob.fbio(new_ev->cmtn()));
            if(new_ev->fn() < 0) {
              CRUMP("[mrgsolve] bioavailability fraction is less than zero.");
            }
            if(new_ev->fn() ==0) {
              if(new_ev->is_dose()) {
                prob.on(new_ev->cmtn());
                prob.lsoda_init();
                new_ev->unarm();
              }
            }
            if(new_ev->rate() < 0) {
              prob.rate_main(new_ev);    
            }
            if(prob.alag(new_ev->cmtn()) > mindt && new_ev->is_dose()) {
              if(new_ev->ss() > 0) {
                new_ev->steady(&prob, a[i], solver);
                tfrom = tto;
                new_ev->ss(0);
              }
              new_ev->time(new_ev->time() + prob.alag(new_ev->cmtn()));
              new_ev->lagged();
              new_ev->pos(__ALAG_POS);
              mt[mti].now = false;
            }
          }
          // If the event is still happening now
          if(mt[mti].now) {
            new_ev->time(tto);
            new_ev->steady(&prob,a[i],solver);
            new_ev->implement(&prob);
            told = new_ev->time();
            if(new_ev->int_infusion() && new_ev->armed()) {
              rec_ptr evoff = NEWREC(new_ev->cmt(), 
                                     9, 
                                     new_ev->amt(), 
                                     new_ev->time() + new_ev->dur(), 
                                     new_ev->rate(), 
                                     -299, 
                                     id);
              insert_record(a[i], j, evoff, true);
            }
          } else {
            bool do_mt_ev = true;
            if((mt[mti].check_unique)) {
              bool found = CompEqual(mtimehx,this_time,this_evid,this_cmt,
                                     this_amt);   
              do_mt_ev = do_mt_ev && !found;
            }
            if(do_mt_ev) {
              insert_record(a[i], j, new_ev, put_ev_first);
              mtimehx.push_back(new_ev);
            }
          } // Done processing "this" event
          if(schedule_addl) {
            // For parent doses happening "now", with or without lag time
            // There is *no* unique check for additional doses
            new_ev->schedule(a[i], maxtime, addl_ev_first, NN, 0.0);
            std::sort(a[i].begin()+j+1,a[i].end(),CompRec());
          }
        } // Closes iteration across vector of events
        used_mtimehx = mtimehx.size() > 0;
        prob.clear_mtime();
      } // Close handling of modeled events
      
      if(this_rec->output()) {
        ans(crow,0) = id;
        ans(crow,1) = tto;
        if(tad) {
          ans(crow,2) = (told > -1) ? (tto - told) : tto - tofd.at(i);
        }
        int k = 0;
        for(unsigned int i=0; i < n_capture; ++i) {
          ans(crow,k+capture_start) = prob.capture(capture[i]);
          ++k;
        }
        for(unsigned int k=0; k < nreq; ++k) {
          ans(crow,(k+req_start)) = prob.y(request[k]);
        }
        ++crow;
      } 
      if(this_rec->evid()==2) {
        this_rec->implement(&prob);
        if(this_rec->cmt() < 0 && prob.infusion_count[this_cmtn] > 0) {
          int n_inf = prob.infusion_count[this_cmtn];
          int n_end = a[i].size();
          for(int ii = j; (n_inf > 0 && ii < n_end); ++ii) {
            if(a[i].at(ii)->evid()==9) {
              prob.rate_rm(this_cmtn, a[i].at(ii)->rate());
              a[i].erase(a[i].begin() + ii);
              --n_inf;
              --n_end;
              --ii;
            }
          }
        }
      }
      tfrom = tto;
    }
  }
  if(digits > 0) {
    for(int i=req_start; i < ans.ncol(); ++i) {
      ans(Rcpp::_, i) = signif(ans(Rcpp::_,i), digits);
    }
  }
  if((tscale != 1) && (tscale >= 0)) {
    ans(Rcpp::_,1) = ans(Rcpp::_,1) * tscale;
  }
  return Rcpp::List::create(Rcpp::Named("data") = mat2df(ans),
                            Rcpp::Named("trannames") = tran_names);
}
