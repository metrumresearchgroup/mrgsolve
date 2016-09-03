// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


#include <boost/shared_ptr.hpp>
#include <boost/pointer_cast.hpp>
#include <iostream>
#include "mrgsolve.h"
#include "odeproblem.h"
//#include "Rodeproblem.h"
#include "pkevent.h"
#include "dataobject.h"
#include <map>
#include <string>

#include <RcppArmadillo.h>


#define CRUMP(a) Rcpp::stop(a)
#define REP(a)   Rcpp::Rcout << #a << std::endl;
#define nREP(a)  Rcpp::Rcout << a << std::endl;
#define say(a)   Rcpp::Rcout << a << std::endl;

// [[Rcpp::export]]
Rcpp::List DEVTRAN(Rcpp::List parin,
                   Rcpp::NumericVector inpar,
                   Rcpp::CharacterVector parnames_,
                   Rcpp::NumericVector init,
                   Rcpp::CharacterVector cmtnames_,
                   Rcpp::IntegerVector capture,
                   Rcpp::List funs,
                   Rcpp::NumericMatrix data,
                   Rcpp::NumericMatrix idata,
                   Rcpp::NumericMatrix OMEGA,
                   Rcpp::NumericMatrix SIGMA) {
  
  svec parnames = Rcpp::as<svec>(parnames_);
  svec cmtnames = Rcpp::as<svec>(cmtnames_);
  for(size_t i=0; i < cmtnames.size(); ++i) cmtnames[i] += "_0";
  
  unsigned int verbose  = Rcpp::as<int>    (parin["verbose"]);
  bool debug            = Rcpp::as<bool>   (parin["debug"]  );
  int digits            = Rcpp::as<int>    (parin["digits"] );
  double tscale         = Rcpp::as<double> (parin["tscale"] );
  bool obsonly          = Rcpp::as<bool>   (parin["obsonly"]);
  bool obsaug           = Rcpp::as<bool>   (parin["obsaug"] );
  int  recsort          = Rcpp::as<int>    (parin["recsort"]);
  bool filbak           = Rcpp::as<bool>   (parin["filbak"]);
  int advan             = Rcpp::as<int>    (parin["advan"]);
  double mindt          = Rcpp::as<double> (parin["mindt"]);
  int t2advance         = Rcpp::as<int>    (parin["t2advance"]);
  
  if((t2advance < 0) || (t2advance > 1)) Rcpp::stop("Invalid value for t2advance.");
  t2advance = 0;
  
  
  if(mindt > 1E-4) Rcpp::Rcout << "Warning: mindt may be too large (" << mindt << ")" << std::endl;
  
  // Create data objects from data and idata
  dataobject *dat = new dataobject(data,parnames);
  dat->map_uid();
  dat->locate_tran();
  
  dataobject *idat = new dataobject(idata, parnames,cmtnames);
  idat -> map_uid();
  idat -> idata_row();
  
  // Number of individuals in the data set
  unsigned int NID = dat->nid();
  int nidata = idat->nrow();
  
  Rcpp::List ret;  // list for returning stuff
  int i=0,j=0,k=0;
  double time0 = 0.0;
  int crow =0,  neq=0; //
  odeproblem *prob;
  size_t h=0;
  
  
  obsaug  = obsaug & (data.nrow() > 0);
  
  bool ev_before_table = true;
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
    addl_ev_first  = true;
    break;
  case 4:
    put_ev_first = true;
    addl_ev_first = false;
    break;
  default:
    CRUMP("recsort must be 1, 2, 3, or 4.");
  }
  
  // stime is a matrix, not a vector, with multiple columns to specify multiple designs
  // Matrix of observations (stime), one column per design
  
  Rcpp::NumericMatrix tgrid = Rcpp::as<Rcpp::NumericMatrix>(parin["tgridmatrix"]);
  // Vector of length idata.nrow() that maps each ID to a design
  // Already has C indexing
  Rcpp::IntegerVector tgridi = Rcpp::as<Rcpp::IntegerVector>(parin["whichtg"]);
  if(tgridi.size() == 0) tgridi = Rcpp::rep(0,NID);
  if(tgridi.size() < NID) CRUMP("Length of design indicator less than NID.");
  
  if(max(tgridi) >= tgrid.ncol()) Rcpp::stop("Insufficient number of designs specified for this problem.");
  
  // Number of designs
  unsigned int ntgrid = tgrid.ncol();
  
  // Number of non-na times in each design
  std::vector<int> tgridn;
  if(ntgrid > 1) {
    for(i = 0; i < tgrid.ncol(); ++i) {
      tgridn.push_back(Rcpp::sum(!Rcpp::is_na(tgrid(Rcpp::_,i))));
    }
  } else {
    tgridn.push_back(tgrid.nrow());
  }
  
  // These are the requested columns.
  Rcpp::IntegerVector request;
  ivec data_carry, idata_carry;
  svec tran_carry;
  
  // Number of requested compartments
  // Number of items from data carried into answer
  // Number of items in idata matrix carried into answer
  // Number of tran data items carried into answer
  int nreq=0, n_data_carry=0, n_idata_carry=0, n_tran_carry=0;
  
  request = parin["request"];
  nreq = request.size();
  
  // Columns from the data set to carry:
  data_carry = Rcpp::as<ivec >(parin["carry_data"]);
  n_data_carry = data_carry.size();
  
  // Tran Items to carry:
  tran_carry = Rcpp::as<svec >(parin["carry_tran"]);
  n_tran_carry = tran_carry.size();
  
  if(idata.nrow()>0) {
    idata_carry = Rcpp::as<ivec>(parin["carry_idata"]);
    n_idata_carry = idata_carry.size();
  }
  
  // Vector of simulation times
  // only active if no evid=0 records in data (cleared out in that case).
  dvec stimes = Rcpp::as<dvec>(parin["stimes"]);
  dvec ptimes = Rcpp::as<dvec>(parin["ptimes"]);
  dvec mtimes = Rcpp::as<dvec>(parin["mtime"]);
  
  svec tablenames = Rcpp::as<svec> (parin["table_names"]);
  int ntable = tablenames.size();
  int size_capture = capture.at(0);
  int n_capture  = capture.size()-1;
  
  if(debug) say("Creating odeproblem object");
  
  prob  = new odeproblem(inpar, init);
  arma::mat OMEGA_(OMEGA.begin(), OMEGA.nrow(), OMEGA.ncol(),false);
  prob->pass_omega(&OMEGA_);
  prob->copy_parin(parin);
  prob->copy_funs(funs);
  neq = prob->neq();
  prob->advan(advan);
  prob->resize_capture(size_capture);
  
  
  switch(advan) {
  case 13:
    break;
  case 2:
    break;
  case 4:
    break;
  case 1:
    break;
  case 3:
    break;
  default:
    CRUMP("advan must be either 13, 2, or 4");
  }
  
  // Every ID in the data set needs to be found in idata if supplied:
  // dataobject.cpp
  if(nidata > 0) dat->check_idcol(idat);
  
  // Allocate the record list and resize for each ID:
  // stimes will get push_backed for now;
  if(debug) say("Allocating the record stack.");
  
  recstack a(NID);
  if(data.ncol()>1) {
    // if data is full, this will be all observations and events;
    // if data is condensed, this will be just events and observations from stimes
    if(debug) say("Resizing ...");
    for(recstack::iterator it = a.begin(); it !=a.end(); ++it) {
      i = it - a.begin();
      (*it).resize((dat->end(i) - dat->start(i))+1);
    }
    if(debug) say("done.");
  }
  
  double tto, tfrom;
  
  int obscount = 0;
  int evcount  = 0;
  
  // dataobject.cpp
  // Extract data records from the data set
  dat->get_records(a, NID, neq, obscount, evcount,obsonly,debug);
  // Offset for getting parameters from the data set (not idata)
  //if((obscount == 0) && (t2advance > 0)) Rcpp::stop("The data set must have observations to use t2advance.");
  //int posoff = t2cov && obscount > 0 ? 1 : 0;
  
  // Deal with stimes:
  
  // Observations from stime will always come after events;
  //unsigned int nextpos = 0; warnings
  int nextpos = 0;
  if((obscount > 0) && (!obsaug)) {
    if(debug) say("Clearing stimes ...");
    stimes.clear();
    ptimes.clear();
  }
  
  // bool extra_times = (stimes.size()>0) || (ptimes.size()>0); // Move down 9.2.16
  
  if((stimes.size()>0) || (ptimes.size()>0)) {
    
    nextpos =  put_ev_first ?  (data.nrow() + 10) : -100;
    
    double id;
    size_t n = stimes.size();
    size_t m = ptimes.size();
    unsigned int thisind = 0;
    
    if(debug) Rcpp::Rcout << "Adding observations from stime and sorting " << std::endl;
  
    for(recstack::iterator it = a.begin(); it != a.end(); ++it) {
      
      thisind = it-a.begin();
      
      id = dat->get_uid(thisind);
      
      j = idat->get_idata_row(id);
      n = tgridn[tgridi[j]];
      
      for(h=0; h < n; h++) {
        rec_ptr obs(new datarecord(0,tgrid(h,tgridi[j]),0,nextpos,id));
        (*it).push_back(obs);
        ++obscount;
      } // done adding stimes;
      
      for(h=0; h < m; h++) {
        rec_ptr obs(new datarecord(0,ptimes[h], 0, nextpos, id));
        obs->output(false);
        (*it).push_back(obs);
      }
      // sort the records by time and original position 
      std::sort((*it).begin(), (*it).end(), CompByTimePosRec);
    }
  }
  
  int NN = obscount;
  if(!obsonly) NN = NN + evcount;
  
  // Create results matrix:
  //  rows: ntime*nset
  //  cols: rep, time, eq[0], eq[1], ..., yout[0], yout[1],...
  
  int neta = 0;
  arma::mat eta;
  if(OMEGA.nrow() > 0) {
    eta = MVGAUSS(OMEGA,NID,-1);
    neta = eta.n_cols;
  }
  prob->neta(OMEGA.ncol());
  
  int neps = 0;
  arma::mat eps;
  if(SIGMA.nrow() > 0) {
    eps = MVGAUSS(SIGMA, NN, -1);
    neps = eps.n_cols;
  }
  prob->neps(SIGMA.ncol());
  prob->init_call_record(time0);
  
  
  // Figure out the output data set:
  const unsigned int n_out_col  = 2 + n_tran_carry + n_data_carry + n_idata_carry + nreq + ntable + n_capture;
  Rcpp::NumericMatrix ans(NN,n_out_col);
  const unsigned int tran_carry_start = 2;
  const unsigned int data_carry_start = tran_carry_start + n_tran_carry;
  const unsigned int idata_carry_start = data_carry_start + n_data_carry;
  const unsigned int req_start = idata_carry_start+n_idata_carry;
  const unsigned int table_start = req_start+nreq;
  const unsigned int capture_start = table_start+ntable;
  
  // Fill in id and time:
  /// This happens no matter what
  //if(debug) Rcpp::Rcout << "Filling in time and ID ... " << std::endl;
  //crow = 0; // current row counter:
  // for(recstack::const_iterator it = a.begin(); it !=a.end(); ++it) {
  //   for(reclist::const_iterator itt = (*it).begin(); itt != (*it).end(); ++itt) {
  //     // Only if this is an output record:
  //     // may not be output record if obsonly was TRUEs
  //     if((*itt)->output()) {
  //       ans(crow, 0) = (*itt)->id();
  //       ans(crow,1) = (*itt)->time();
  //       crow++;
  //     }
  //   }
  // }
  
  
  // Carry along TRAN data items (evid, amt, ii, ss, rate)
  Rcpp::CharacterVector tran_names;
  if(n_tran_carry > 0) {
    
    //if(debug) say("Filling in carried items ...");
    
    svec::const_iterator tcbeg  = tran_carry.begin();
    svec::const_iterator tcend  = tran_carry.end();
    // items in tran_carry are always lc
    bool carry_evid = std::find(tcbeg,tcend, "evid")  != tcend;
    bool carry_cmt =  std::find(tcbeg,tcend, "cmt")   != tcend;
    bool carry_amt =  std::find(tcbeg,tcend, "amt")   != tcend;
    bool carry_ii =   std::find(tcbeg,tcend, "ii")    != tcend;
    bool carry_addl = std::find(tcbeg,tcend, "addl")  != tcend;
    bool carry_ss =   std::find(tcbeg,tcend, "ss")    != tcend;
    bool carry_rate = std::find(tcbeg,tcend, "rate")  != tcend;
    bool carry_aug  = std::find(tcbeg,tcend, "a.u.g") != tcend;
    
    if(carry_evid) tran_names.push_back("evid");
    if(carry_amt)  tran_names.push_back("amt");
    if(carry_cmt)  tran_names.push_back("cmt");
    if(carry_ss)   tran_names.push_back("ss");
    if(carry_ii)   tran_names.push_back("ii");
    if(carry_addl) tran_names.push_back("addl");
    if(carry_rate) tran_names.push_back("rate");
    if(carry_aug)  tran_names.push_back("a.u.g");
    
    
    crow = 0; // current output row
    for(recstack::const_iterator it = a.begin(); it !=a.end(); ++it) {
      for(reclist::const_iterator itt = (*it).begin(); itt != (*it).end(); ++itt) {
        if(!(*itt)->output()) continue;
        int n = 0;
        if(carry_evid) {ans(crow,n+2) = (*itt)->evid();                     ++n;}
        if(carry_amt)  {ans(crow,n+2) = (*itt)->amt();                      ++n;}
        if(carry_cmt)  {ans(crow,n+2) = (*itt)->cmt();                      ++n;}
        if(carry_ss)   {ans(crow,n+2) = (*itt)->ss();                       ++n;}
        if(carry_ii)   {ans(crow,n+2) = (*itt)->ii();                       ++n;}
        if(carry_addl) {ans(crow,n+2) = (*itt)->addl();                     ++n;}
        if(carry_rate) {ans(crow,n+2) = (*itt)->rate();                     ++n;}
        if(carry_aug)  {ans(crow,n+2) = ((*itt)->pos()==nextpos) && obsaug; ++n;}
        ++crow;
      }
    }
  }
  
  
  // Carry items from data or idata
  if(((n_idata_carry > 0) || (n_data_carry > 0)) ) {
    if(debug) {
      Rcpp::Rcout << "Copying items from data and idata into answer..." << std::endl;
      Rcpp::Rcout << "Carrying " << n_idata_carry << " items from idata." << std::endl;
      Rcpp::Rcout << "Carrying " << n_data_carry << " items from data set." << std::endl;
    }
    crow = 0;
    
    int lastpos = -1;
    
    unsigned int idatarow=0;
    
    bool carry_from_data = n_data_carry > 0;
    
    for(recstack::iterator it=a.begin(); it!=a.end(); ++it) {
      
      j = it-a.begin();
      
      if((n_idata_carry > 0) && (nidata > 0)) {
        idatarow = idat->get_idata_row(dat->get_uid(j));
      }
      
      std::vector<rec_ptr> thisi = *it;
      
      for(size_t i = 0; i < thisi.size(); ++i) {
        
        if(carry_from_data) {
          // Need to reset this for each ID; indicates that
          // We haven't hit a dataset record yet
          if(i==0) lastpos = -1;
          // Need to log lastpos here regardless
          if(thisi.at(i)->from_data()) lastpos = thisi.at(i)->pos();
        }
        
        if(!thisi.at(i)->output()) continue;
        
        // Copy from idata:
        for(k=0; k < n_idata_carry; ++k) {
          ans(crow, idata_carry_start+k) = idata(idatarow,idata_carry[k]);
        }
        
        if(carry_from_data) {
          if(lastpos >=0) {
            for(k=0; k < n_data_carry; ++k) ans(crow, data_carry_start+k)  = data(lastpos,data_carry[k]);
          } else {
            for(k=0; k < n_data_carry; ++k) ans(crow, data_carry_start+k)  = data(dat->start(j),data_carry[k]);
          }
        }
        // Increment current row:
        ++crow;
      }
    }
  }
  
  if((verbose||debug)) {
    //Rcpp::Rcout << std::endl <<  "========================" << std::endl;
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << "THIS IS MRGSOLVE (DEVTRAN) " << std::endl;
    Rcpp::Rcout << "TOT. NO. OF INDIVIDUALS:   " << NID << std::endl;
    Rcpp::Rcout << "TOT. NO. OF OBS RECS:      " << obscount  << std::endl;
    Rcpp::Rcout << "TOT. NO. OF EV  RECS:      " << evcount   << std::endl;
    Rcpp::Rcout << "TOT. NO. OF ETA:           " << neta << std::endl;
    Rcpp::Rcout << "TOT. NO. OF EPS:           " << neps << std::endl;
    Rcpp::Rcout << "Parameters:                " << prob->npar()  << std::endl;
    Rcpp::Rcout << "Equations:                 " << neq  << std::endl;
    Rcpp::Rcout << "Requested compartments:    ";
    for(i=0; i < nreq; ++i) Rcpp::Rcout << (1+request[i]) << " ";
    Rcpp::Rcout << std::endl;
    Rcpp::Rcout << "OUTPUT MATRIX:             " << NN << " rows, " << n_out_col << " columns" << std::endl;
    
  }
  if(debug) {
    Rcpp::Rcout << "========================" << std::endl;
    Rcpp::Rcout << "id   in data column " << (dat->idcol()+1) << std::endl;
    Rcpp::Rcout << "time in data column " << (dat->col_n("time") + 1) << std::endl;
    Rcpp::Rcout << "evid in data column " << (dat->col_n("evid") + 1) << std::endl ;
    Rcpp::Rcout << "amt  in data column " << (dat->col_n("amt")+1) << std::endl;
    Rcpp::Rcout << "cmt  in data column " << (dat->col_n("cmt")+1) << std::endl;
    Rcpp::Rcout << "rate in data column " << (dat->col_n("rate")+1) << std::endl ;
    Rcpp::Rcout << "========================" << std::endl;
  }
  
  crow = 0;
  if(verbose||debug)  Rcpp::Rcout << "Solving ... ";
  
  int this_cmt = 0;
  //prob->reset_newid(this_id);
  // Do one last reset on parameters:
  dat->reload_parameters(inpar,prob);
  // First, get idata parameters from the first ID in data
  idat->copy_parameters(idat->get_idata_row(dat->get_uid(0)),prob);
  // Then, copy parameters from the first record in data
  dat->copy_parameters(0,prob);
  
  // The current difference between tto and tfrom
  double dt = 0;
  double denom = 1;
  bool last_record = false;
  // LOOP ACROSS IDS:
  for(size_t i=0; i < a.size(); ++i) {
    
    std::vector<rec_ptr> thisi = a.at(i);
    
    tfrom = thisi.at(0)->time();
    double id = thisi.at(0)->id();
    double maxtime = thisi.back()->time();
    
    prob->reset_newid(id);
    
    if(i==0) prob->newind(0);
    
    // Copy eta values for this ID
    for(j=0; j < neta; ++j) prob->eta(j,eta(i,j));
    
    // Copy eps values:
    for(j=0; j < neps; ++j) prob->eps(j,eps(crow,j));
    
    // Refresh parameters in data:
    dat->reload_parameters(inpar,prob);
    //Copy parameters from idata
    idat->copy_parameters(idat->get_idata_row(id),prob);
    
    // Copy parameters from data
    rec_ptr this_rec  = thisi.at(0);
    if(this_rec->from_data()) {
      // If this record is from the data set, copy parameters from data
      dat->copy_parameters(this_rec->pos(), prob);
    } else {
      if(filbak) {
        dat->copy_parameters(dat->start(i),prob);
      }
    }
    
    // Calculate initial conditions:
    for(k=0; k < neq; ++k) prob->y_init(k,init[k]);
    // Copy initials from idata
    idat -> copy_inits(idat->get_idata_row(id),prob);
    // Call $MAIN
    prob->init_call(tfrom);
    
    add_mtime(thisi, mtimes, prob->mtime(),(debug||verbose));
    
    prob->table_call();
    
    // LOOP ACROSS EACH RECORD for THIS ID:
    for(size_t j=0; j < thisi.size(); ++j) {
      
      if(j==0) {
        prob->solving(true);
      } else {
        prob->newind(2);
      }
      
      last_record = j==thisi.size()-1;
      
      rec_ptr this_rec = thisi.at(j);
      
      // Fill in the remaining records once system is turned off
      if(prob->systemoff()) {
        if(this_rec->output()) {
          if(prob->CFONSTOP()) {
            ans(crow,0) = this_rec->id();
            ans(crow,1) = this_rec->time();
            for(int i=0; i < ntable; ++i)    ans(crow,(i+table_start  )) = prob->table(tablenames.at(i));
            for(int i=0; i < n_capture; ++i) ans(crow,(i+capture_start)) = prob->capture(capture[i+1]);
            for(int k=0; k < nreq; ++k)      ans(crow,(k+req_start    )) = prob->y(request[k]);
          } else {
            ans(crow,0) = NA_REAL;
          }
          ++crow;
        }
        continue;
      }
      
      // For this ID, we already have parameters from the first row; only update when
      // we come across a record from the data set
      if(this_rec->from_data()) {
        if(last_record) {
          dat->copy_parameters(this_rec -> pos(), prob);
        } else {
          dat->copy_parameters(this_rec->pos() + t2advance, prob);
        }
      }
      
      tto = this_rec->time();
      denom = tfrom == 0 ? 1 : tfrom;
      dt  = (tto-tfrom)/denom;
      
      
      // If tto is too close to tfrom, set tto to tfrom
      // dt is never negative; dt will never be < mindt when mindt==0
      if((dt > 0.0) && (dt < mindt)) { // don't bother if dt==0
        if(debug) {
          Rcpp::Rcout << "" << std::endl;
          Rcpp::Rcout << "Two records are too close to each other:" <<  std::endl;
          Rcpp::Rcout << "  evid: " << this_rec->evid() << std::endl;
          Rcpp::Rcout << "  tfrom: " << tfrom << std::endl;
          Rcpp::Rcout << "  tto: " << tto << std::endl;
          Rcpp::Rcout << "  dt:  " << tto - tfrom << std::endl;
          Rcpp::Rcout << "  pos: " << this_rec->pos() << std::endl;
          Rcpp::Rcout << "  id: "  << this_rec->id() << std::endl;
        }
        tto = tfrom;
      }
      
      
      // Only copy in a new eps value if we are actually advancing in time:
      if((tto > tfrom) && (crow < NN)) {
        for(int k = 0; k < neps; ++k) {
          prob->eps(k,eps(crow,k));
        }
      }
      
      prob->evid(this_rec->evid());
      prob->init_call_record(tto);
      prob->INITSOLV();
      
      // Schedule ADDL and infusion end times
      if((this_rec->is_event()) && (this_rec->from_data())) {
        
        ev_ptr ev = boost::dynamic_pointer_cast<pkevent>(this_rec);
        
        //if(ev->evid()==11) {
        // ev->amt(prob->xdose());
        // ev->rate();
        //}
        
        // Grab Bioavailability
        double biofrac = prob->fbio(abs(ev->cmt())-1);
        
        if(biofrac < 0) {
          CRUMP("mrgsolve: Bioavailability fraction is less than zero.");
        }
        
        ev->fn(biofrac);
        
        // We already found an negative rate or duration in the data set.
        if(ev->rate() < 0) {
          if(ev->rate() == -1) {
            this_cmt = ev->cmt()-1;
            if(prob->rate(this_cmt) <= 0) {
              Rcpp::Rcout << "R(" << this_cmt + 1 << ") must be set to a positive value in $MAIN." << std::endl;
              Rcpp::stop("Invalid infusion settings.");
            }
            ev->rate(prob->rate(this_cmt));
          }
          
          if(ev->rate() == -2) {
            this_cmt = ev->cmt()-1;
            if(prob->dur(this_cmt) <= 0) {
              Rcpp::Rcout << "D(" << this_cmt + 1 << ") must be set to a positive value in $MAIN." << std::endl;
              Rcpp::stop("Invalid infusion settings.");
            }
            ev->rate(ev->amt() * biofrac / prob->dur(this_cmt));
          }
        }
        
        // If alag set for this compartment
        // spawn a new event with no output and time modified by alag
        // disarm this event
        if((prob->alag(ev->cmt()) > mindt)) {
          
          ev->unarm();
          
          ev_ptr newev(new pkevent(ev->cmt(),
                                   ev->evid(),
                                   ev->amt(),
                                   ev->time() + prob->alag(ev->cmt()),
                                   ev->rate()));
          newev->addl(ev->addl());
          newev->ii(ev->ii());
          newev->ss(ev->ss());
          newev->id(ev->id());
          newev->pos(-1200);
          newev->fn(biofrac);
          newev->output(false);
          
          reclist::iterator it = thisi.begin()+j;
          advance(it,1);
          thisi.insert(it,newev);
          newev->schedule(thisi, maxtime,addl_ev_first);
          std::sort(thisi.begin()+j,thisi.end(),CompByTimePosRec);
          
        } else {
          ev->schedule(thisi, maxtime,addl_ev_first); //pkevent.cpp
          if(ev->needs_sorting()) {
            std::sort(thisi.begin()+j+1,thisi.end(),CompByTimePosRec);
          }
        }
      }
      
      prob -> advance(tfrom,tto);
      
      if(ev_before_table) {
        if(this_rec->evid() != 2) {
          this_rec->implement(prob);
          if(this_rec->evid() != 0) prob->lsoda_init();
        }
      }
      
      // Write save values to output matrix:
      prob->table_call();
      
      if(this_rec->output()) {
        ans(crow,0) = this_rec->id();
        ans(crow,1) = this_rec->time();
        
        if(ntable>0) {
          k=0;
          for(int i=0; i < ntable; ++i) {
            ans(crow,k+table_start) = prob->table(tablenames.at(i));
            ++k;
          }
        }
        if(n_capture > 0) {
          k=0;
          for(int i=0; i < n_capture; ++i) {
            ans(crow,k+capture_start) = prob->capture(capture[i+1]);
            ++k;
          }
        }
        for(int k=0; k < nreq; ++k) ans(crow,(k+req_start)) = prob->y(request[k]);
        ++crow;
      } // end if ouput()
      
      
      // Reset or other events:
      if(this_rec->evid()==2) {
        this_rec->implement(prob);
        prob->lsoda_init();
      }
      tfrom = tto;
    }
  }
  
  if((verbose||debug)) Rcpp::Rcout << "done. " << std::endl;
  
  
  // Significant digits in simulated variables and outputs too
  if(digits > 0) for(int i=req_start; i < ans.ncol(); ++i) ans(Rcpp::_, i) = signif(ans(Rcpp::_,i), digits);
  if((tscale != 1) && (tscale >= 0)) ans(Rcpp::_,1) = ans(Rcpp::_,1) * tscale;
  
  // // Assemble return List
  ret["data"] = ans;
  ret["outnames"] = tablenames;
  ret["trannames"] = tran_names;
  ret["issues"] = Rcpp::CharacterVector(0);
  
  
  // // Clean up
  delete prob;
  delete dat;
  delete idat;
  return ret;
}


