// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


#include <boost/shared_ptr.hpp>
#include <boost/pointer_cast.hpp>
#include <iostream>
#include <string>
#include "mrgsolve.h"
#include "odeproblem.h"
#include "pkevent.h"
#include "dataobject.h"

#include "RcppInclude.h"

#define CRUMP(a) Rcpp::stop(a)
#define REP(a)   Rcpp::Rcout << #a << std::endl;
#define nREP(a)  Rcpp::Rcout << a << std::endl;
#define say(a)   Rcpp::Rcout << a << std::endl;


/* Main simulation function.
 * 
 * @param parin list of data and options for the simulation
 * @param inpar numeric parameter values
 * @param parnames parameter names
 * @param init numeric initial values
 * @param cmtnames compartment names
 * @param capture indices in capture vector to actually get
 * @param funs list of pointer addresses to model functions
 * @param data the main data set
 * @param idata the idata data aset
 * @param OMEGA between-ID normal random effects
 * @param SIGMA within-ID normal random effects
 *
 */

// [[Rcpp::export]]
Rcpp::List DEVTRAN(const Rcpp::List parin,
                   const Rcpp::NumericVector& inpar,
                   const Rcpp::CharacterVector& parnames,
                   const Rcpp::NumericVector& init,
                   Rcpp::CharacterVector& cmtnames,
                   const Rcpp::IntegerVector& capture,
                   const Rcpp::List& funs,
                   const Rcpp::NumericMatrix& data,
                   const Rcpp::NumericMatrix& idata,
                   Rcpp::NumericMatrix& OMEGA,
                   Rcpp::NumericMatrix& SIGMA) {
  
  
  
  const unsigned int verbose  = Rcpp::as<int>    (parin["verbose"]);
  const bool debug            = Rcpp::as<bool>   (parin["debug"]  );
  const int digits            = Rcpp::as<int>    (parin["digits"] );
  const double tscale         = Rcpp::as<double> (parin["tscale"] );
  const bool obsonly          = Rcpp::as<bool>   (parin["obsonly"]);
  bool obsaug                 = Rcpp::as<bool>   (parin["obsaug"] );
  const int  recsort          = Rcpp::as<int>    (parin["recsort"]);
  const bool filbak           = Rcpp::as<bool>   (parin["filbak"]);
  const double mindt          = Rcpp::as<double> (parin["mindt"]);
  
  if(mindt > 1E-4) Rcpp::Rcout << "Warning: mindt may be too large (" << mindt << ")" << std::endl;
  
  // Create data objects from data and idata
  dataobject *dat = new dataobject(data,parnames);
  dat->map_uid();
  dat->locate_tran();
  
  // Really only need cmtnames to get initials from idata
  for(size_t i=0; i < cmtnames.size(); ++i) cmtnames[i] += "_0";
  dataobject *idat = new dataobject(idata, parnames, cmtnames);
  idat->map_uid();
  idat->idata_row();
  
  // Number of individuals in the data set
  const unsigned int NID = dat->nid();
  const int nidata = idat->nrow();
  
  int i=0,j=0,k=0;
  const double time0 = 0.0;
  int crow =0; 
  size_t h=0;
  
  obsaug  = obsaug & (data.nrow() > 0);
  
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
  
  // Requested compartments  
  Rcpp::IntegerVector request = parin["request"];
  const int nreq = request.size();
  
  // Columns from the data set to carry:
  Rcpp::CharacterVector data_carry_ = Rcpp::as<Rcpp::CharacterVector >(parin["carry_data"]);
  Rcpp::IntegerVector data_carry =  dat->get_col_n(data_carry_);
  const int n_data_carry = data_carry.size();
  
  // Columns from the idata set to carry:
  int n_idata_carry=0;
  Rcpp::IntegerVector idata_carry;  
  if(idata.nrow()>0) {
    Rcpp::CharacterVector idata_carry_ = Rcpp::as<Rcpp::CharacterVector >(parin["carry_idata"]);
    idata_carry =  idat->get_col_n(idata_carry_);
    n_idata_carry = idata_carry.size();
  }
  
  // Tran Items to carry:
  Rcpp::CharacterVector tran_carry = Rcpp::as<Rcpp::CharacterVector >(parin["carry_tran"]);
  const int n_tran_carry = tran_carry.size();
  
  const svec tablenames = Rcpp::as<svec> (parin["table_names"]);
  const int ntable = tablenames.size();
  const int n_capture  = capture.size()-1;
  
  if(debug) say("Creating odeproblem object");
  
  odeproblem *prob  = new odeproblem(inpar, init, funs, capture.at(0));
  arma::mat OMEGA_(OMEGA.begin(), OMEGA.nrow(), OMEGA.ncol(),false);
  prob->pass_omega(&OMEGA_);
  prob->copy_parin(parin);
  const int neq = prob->neq();
  
  // Every ID in the data set needs to be found in idata if supplied:
  // dataobject.cpp
  if(nidata > 0) dat->check_idcol(idat);
  
  // Allocate the record list:
  recstack a(NID);
  
  // dataobject.cpp
  // Extract data records from the data set
  int obscount = 0;
  int evcount  = 0;
  
  dat->get_records(a, NID, neq, obscount, evcount, obsonly, debug);
  
  // Observations from stime will always come after events;
  // unsigned int nextpos = 0; warnings
  // Vector of simulation times
  // only active if no evid=0 records in data (cleared out in that case).
  dvec mtimes = Rcpp::as<dvec>(parin["mtime"]);
  
  // Need this for later
  int nextpos = put_ev_first ?  (data.nrow() + 10) : -100;
  
  // Only if we need to insert observations into the stack
  if((obscount == 0) || (obsaug)) {
    
    // Padded times
    dvec ptimes = Rcpp::as<dvec>(parin["ptimes"]);
    
    // Matrix of designs
    Rcpp::NumericMatrix tgrid = Rcpp::as<Rcpp::NumericMatrix>(parin["tgridmatrix"]);
    
    // Vector of length idata.nrow() that maps each ID to a design
    // Already has C indexing
    Rcpp::IntegerVector tgridi = Rcpp::as<Rcpp::IntegerVector>(parin["whichtg"]);
    
    if(tgridi.size() == 0) tgridi = Rcpp::rep(0,NID);
    
    if(tgridi.size() < NID) CRUMP("Length of design indicator less than NID.");
    
    if(max(tgridi) >= tgrid.ncol()) Rcpp::stop("Insufficient number of designs specified for this problem.");
    
    // Number of non-na times in each design
    std::vector<int> tgridn;
    if(tgrid.ncol() > 1) {
      for(i = 0; i < tgrid.ncol(); ++i) {
        tgridn.push_back(Rcpp::sum(!Rcpp::is_na(tgrid(Rcpp::_,i))));
      }
    } else {
      tgridn.push_back(tgrid.nrow());
    }
    
    // Create a common dictionary of observation events
    // Vector of vectors
    // Outer vector: length = number of designs
    // Inner vector: length = number of times in that design
    
    std::vector<std::vector<rec_ptr> > designs;
    designs.reserve(tgridn.size());
    for(int i = 0; i < tgridn.size(); ++i) {
      std::vector<rec_ptr> z;
      z.reserve(tgridn[i]);
      for(int j = 0; j < tgridn[i]; ++j) { 
        rec_ptr obs(new datarecord(0,tgrid(j,i),0,nextpos,0));
        z.push_back(obs); 
      }
      designs.push_back(z);
    }
    
    double id;
    size_t n;
    size_t m = ptimes.size();
    
    for(recstack::iterator it = a.begin(); it != a.end(); ++it) {
      
      id = dat->get_uid(it-a.begin());
      
      j = idat->get_idata_row(id);
      
      n = tgridn[tgridi[j]];
      
      (*it).reserve(((*it).size() + n + m + 10));
      
      for(h=0; h < n; h++) {
        //rec_ptr obs(new datarecord(0,tgrid(h,tgridi[j]),0,nextpos,id));
        (*it).push_back(designs.at(tgridi[j]).at(h));
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
  } // End tgrid
  
  // Create results matrix:
  //  rows: ntime*nset
  //  cols: rep, time, eq[0], eq[1], ..., yout[0], yout[1],...
  
  int NN = obscount;
  if(!obsonly) NN = NN + evcount;
  
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
  
  // Carry along TRAN data items (evid, amt, ii, ss, rate)
  Rcpp::CharacterVector tran_names;
  
  if(n_tran_carry > 0) {
    
    Rcpp::CharacterVector::iterator tcbeg  = tran_carry.begin();
    Rcpp::CharacterVector::iterator tcend  = tran_carry.end();
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
    int n = 0;
    for(recstack::const_iterator it = a.begin(); it !=a.end(); ++it) {
      for(reclist::const_iterator itt = (*it).begin(); itt != (*it).end(); ++itt) {
        if(!(*itt)->output()) continue;
        n = 0;
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
    
    crow = 0;
    
    int lastpos = -1;
    
    unsigned int idatarow=0;
    
    const bool carry_from_data = n_data_carry > 0;
    
    for(recstack::iterator it=a.begin(); it!=a.end(); ++it) {
      
      j = it-a.begin();
      
      if((n_idata_carry > 0) && (nidata > 0)) {
        idatarow = idat->get_idata_row(dat->get_uid(j));
      }
      
      size_t this_n = (*it).size();
      
      for(size_t i = 0; i < this_n; ++i) {
        
        if(carry_from_data) {
          // Need to reset this for each ID; indicates that
          // We haven't hit a dataset record yet
          if(i==0) lastpos = -1;
          // Need to log lastpos here regardless
          if((*it)[i]->from_data()) lastpos = (*it)[i]->pos();
        }
        
        if(!(*it)[i]->output()) continue;
        
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
  
  double tto, tfrom;
  
  crow = 0;
  
  if(verbose||debug)  Rcpp::Rcout << "Solving ... ";
  
  int this_cmt = 0;
  // Do one last reset on parameters:
  dat->reload_parameters(inpar,prob);
  // First, get idata parameters from the first ID in data
  idat->copy_parameters(idat->get_idata_row(dat->get_uid(0)),prob);
  // Then, copy parameters from the first record in data
  dat->copy_parameters(0,prob);
  
  // The current difference between tto and tfrom
  double dt = 0;
  double id = 0;
  double maxtime = 0;
  double biofrac = 1.0;
  int this_idata_row = 0;
  size_t a_size = a.size();
  
  // LOOP ACROSS IDS:
  for(size_t i=0; i < a_size; ++i) {
    
    tfrom = a[i][0]->time();
    id = dat->get_uid(i);//a[i][0]->id();
    this_idata_row  = idat->get_idata_row(id);
    maxtime = a[i].back()->time();
    
    prob->reset_newid(id);
    
    if(i==0) prob->newind(0);
    
    // Copy eta values for this ID
    for(j=0; j < neta; ++j) prob->eta(j,eta(i,j));
    
    // Copy eps values:
    for(j=0; j < neps; ++j) prob->eps(j,eps(crow,j));
    
    // Refresh parameters in data:
    dat->reload_parameters(inpar,prob);
    
    //Copy parameters from idata
    idat->copy_parameters(this_idata_row,prob);
    
    if(a[i][0]->from_data()) {
      // If this record is from the data set, copy parameters from data
      dat->copy_parameters(a[i][0]->pos(), prob);
    } else {
      if(filbak) {
        dat->copy_parameters(dat->start(i),prob);
      }
    }
    
    // Calculate initial conditions:
    for(k=0; k < neq; ++k) prob->y_init(k,init[k]);
    
    // Copy initials from idata
    idat->copy_inits(this_idata_row,prob);
    
    // Call $MAIN
    prob->init_call(tfrom);
    
    // mtime
    add_mtime(a[i], mtimes, prob->mtime(),(debug||verbose));
    
    prob->table_call();
    
    // LOOP ACROSS EACH RECORD for THIS ID:
    size_t ai_size = a[i].size();
    for(size_t j=0; j < ai_size; ++j) {
      
      if(j==0) {
        prob->solving(true);
      } else {
        prob->newind(2);
      }
      
      rec_ptr this_rec = a[i][j];
      this_rec->id(id);
      
      // Fill in the remaining records once system is turned off
      if(prob->systemoff()) {
        if(this_rec->output()) {
          if(prob->CFONSTOP()) {
            ans(crow,0) = this_rec->id();
            ans(crow,1) = this_rec->time();
            for(int i=0; i < ntable; ++i)    ans(crow,(i+table_start  )) = prob->table(tablenames[i]);
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
        dat->copy_parameters(this_rec -> pos(), prob);
        // if t2advance
        //dat->copy_parameters(this_rec->pos() + t2advance, prob);
      }
      
      tto = this_rec->time();
      dt  = (tto-tfrom)/(tfrom == 0.0 ? 1.0 : tfrom);
      
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
        
        // Grab Bioavailability
        biofrac = prob->fbio(abs(ev->cmt())-1);
        
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
        } // End ev->rate() < 0
        
        // If alag set for this compartment
        // spawn a new event with no output and time modified by alag
        // disarm this event
        if((prob->alag(ev->cmt()) > mindt)) {
          
          ev->unarm();
          
          ev_ptr newev(new pkevent(ev->cmt(),
                                   ev->evid(),
                                   ev->amt(),
                                   ev->time() + prob->alag(ev->cmt()),
                                   ev->rate(), 
                                   -1200, 
                                   ev->id()));
          newev->addl(ev->addl());
          newev->ii(ev->ii());
          newev->ss(ev->ss());
          //newev->id(ev->id());
          //newev->pos(-1200);
          newev->fn(biofrac);
          newev->output(false);
          
          reclist::iterator it = a[i].begin()+j;
          advance(it,1);
          a[i].insert(it,newev);
          newev->schedule(a[i], maxtime, addl_ev_first);
          std::sort(a[i].begin()+j,a[i].end(),CompByTimePosRec);
          
        } else {
          ev->schedule(a[i], maxtime, addl_ev_first); //pkevent.cpp
          if(ev->needs_sorting()) {
            std::sort(a[i].begin()+j+1,a[i].end(),CompByTimePosRec);
          }
        }
        // Remember to update the value of size
        ai_size = a[i].size();
      }
      
      prob -> advance(tfrom,tto);
      
      if(this_rec->evid() != 2) {
        this_rec->implement(prob);
      }
      
      // Write save values to output matrix:
      prob->table_call();
      
      if(this_rec->output()) {
        
        // Write out ID and time
        ans(crow,0) = this_rec->id();
        ans(crow,1) = this_rec->time();
        
        // Write out tabled items
        k=0;
        for(int i=0; i < ntable; ++i) {
          ans(crow,k+table_start) = prob->table(tablenames[i]);
          ++k;
        }
        
        // Write out captured items
        k=0;
        for(int i=0; i < n_capture; ++i) {
          ans(crow,k+capture_start) = prob->capture(capture[i+1]);
          ++k;
        }
        
        // Write out requested compartments
        for(int k=0; k < nreq; ++k) {
          ans(crow,(k+req_start)) = prob->y(request[k]);
        }
        
        ++crow;
      } // end if ouput()
      
      
      // Reset or other events:
      if(this_rec->evid()==2) {
        this_rec->implement(prob);
      }
      
      // Move tto to tfrom
      tfrom = tto;
    }
  }
  
  if((verbose||debug)) Rcpp::Rcout << "done. " << std::endl;
  
  // Significant digits in simulated variables and outputs too
  if(digits > 0) for(int i=req_start; i < ans.ncol(); ++i) ans(Rcpp::_, i) = signif(ans(Rcpp::_,i), digits);
  if((tscale != 1) && (tscale >= 0)) ans(Rcpp::_,1) = ans(Rcpp::_,1) * tscale;
  
  // Clean up
  delete prob;
  delete dat;
  delete idat;
  
  return Rcpp::List::create(Rcpp::Named("data") = ans,
                            Rcpp::Named("trannames") = tran_names);
  
  
}


