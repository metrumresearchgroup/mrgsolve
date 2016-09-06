
// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include "RcppInclude.h"
#include "dataobject.h"
#include "mrgsolve.h"
#include "pkevent.h"
#include <numeric>
#include "mrgsolv.h"


#define _COL_amt_   0u
#define _COL_ii_    1u
#define _COL_addl_  2u
#define _COL_ss_    3u
#define _COL_rate_  4u
#define _COL_evid_  5u
#define _COL_cmt_   6u
#define _COL_time_  7u


dataobject::dataobject(Rcpp::NumericMatrix _data, 
                       Rcpp::CharacterVector _parnames) {
  Data = _data;
  parnames = _parnames;
  
  Rcpp::List dimnames = Data.attr("dimnames");
  Data_names = Rcpp::as<Rcpp::CharacterVector>(dimnames[1]);
  
  Idcol = find_position("ID", Data_names);
  if(Idcol < 0) Rcpp::stop("Could not find ID column in data set.");
  
  // Connect Names in the data set with positions in the parameter list
  from_to(Data_names,parnames, par_from, par_to);
  
  col.resize(8,0);
}

dataobject::dataobject(Rcpp::NumericMatrix _data,
                       Rcpp::CharacterVector _parnames,
                       Rcpp::CharacterVector _cmtnames) {
  Data = _data;
  parnames = _parnames;
  cmtnames = _cmtnames;
  
  Rcpp::List dimnames = Data.attr("dimnames");
  Data_names = Rcpp::as<Rcpp::CharacterVector>(dimnames[1]);
  
  Idcol = find_position("ID", Data_names);
  if(Idcol < 0) Rcpp::stop("Could not find ID column in data set.");
  
  // Connect Names in the data set with positions in the parameter list
  from_to(Data_names, parnames, par_from, par_to);
  from_to(Data_names, cmtnames, cmt_from, cmt_to);
  
  col.resize(8,0);
}


dataobject::~dataobject(){}

void dataobject::map_uid() {
  
  int i=0;
  Uid.push_back(Data(0,Idcol));
  Startrow.push_back(0);
  for(i=1; i < Data.nrow(); ++i) {
    if(Data(i,Idcol) != Data(i-1, Idcol)) {
      Uid.push_back(Data(i,Idcol));
      Startrow.push_back(i);
      Endrow.push_back(i-1);
    }
  }
  Endrow.push_back(Data.nrow()-1);
}


Rcpp::IntegerVector dataobject::get_col_n(Rcpp::CharacterVector what) {
  Rcpp::IntegerVector ret = Rcpp::match(what, Data_names);
  ret = Rcpp::na_omit(ret);
  return(ret-1);
}


void dataobject::locate_tran() {
  
  int zeros = Data.ncol()-1;
  
  if(zeros==0) {
    col[_COL_amt_]  = 0;
    col[_COL_ii_]   = 0;
    col[_COL_addl_] = 0;
    col[_COL_ss_]   = 0;
    col[_COL_rate_] = 0;
    col[_COL_evid_] = 0;
    col[_COL_cmt_]  = 0;
    col[_COL_time_] = 0;
    return;
  }
  
  Rcpp::CharacterVector::iterator bg = Data_names.begin();
  Rcpp::CharacterVector::iterator ed = Data_names.end();
  
  int tcol = std::find(bg,ed,"time") - bg;
  
  bool lc = true;
  
  if(tcol > zeros) {
    tcol = std::find(bg,ed,"TIME") - bg;
    if(tcol > zeros) Rcpp::stop("Could not find time or TIME column in the data set.");
    lc = false;
  }
  
  col[_COL_time_] = tcol;
  
  
  
  if(lc) {
    col[_COL_amt_]  = std::find(bg,ed,"amt")  - bg;
    col[_COL_ii_]   = std::find(bg,ed,"ii")   - bg;
    col[_COL_addl_] = std::find(bg,ed,"addl") - bg;
    col[_COL_ss_]   = std::find(bg,ed,"ss")   - bg;
    col[_COL_rate_] = std::find(bg,ed,"rate") - bg;
    col[_COL_evid_] = std::find(bg,ed,"evid") - bg;
    col[_COL_cmt_]  = std::find(bg,ed,"cmt")  - bg;
    
  } else {
    col[_COL_amt_]  = std::find(bg,ed,"AMT")  - bg;
    col[_COL_ii_]   = std::find(bg,ed,"II")   - bg;
    col[_COL_addl_] = std::find(bg,ed,"ADDL") - bg;
    col[_COL_ss_]   = std::find(bg,ed,"SS")   - bg;
    col[_COL_rate_] = std::find(bg,ed,"RATE") - bg;
    col[_COL_evid_] = std::find(bg,ed,"EVID") - bg;
    col[_COL_cmt_]  = std::find(bg,ed,"CMT")  - bg;
  }
  
  if(col[_COL_amt_]  > zeros) col[_COL_amt_]  = zeros;
  if(col[_COL_ii_]   > zeros) col[_COL_ii_]   = zeros;
  if(col[_COL_addl_] > zeros) col[_COL_addl_] = zeros;
  if(col[_COL_ss_]   > zeros) col[_COL_ss_]   = zeros;
  if(col[_COL_rate_] > zeros) col[_COL_rate_] = zeros;
  if(col[_COL_evid_] > zeros) col[_COL_evid_] = zeros;
  
  if(col[_COL_cmt_] > zeros  && zeros > 0) {
    Rcpp::stop("Couldn't locate cmt or CMT in data set.");
  }
}



void dataobject::idata_row() {
  int i=0;
  for(i=0; i < Data.nrow(); ++i) {
    idmap[Data(i,Idcol)] = i;
  }
}


void dataobject::copy_parameters(int this_row, odeproblem* prob) {
  size_t i;
  for(i=0; i < par_from.size(); ++i) {
    prob->param(par_to[i],Data(this_row,par_from[i]));
  }
}



void dataobject::copy_inits(int this_row, odeproblem* prob) {
  // this should only be done from idata sets
  size_t i;
  for(i=0; i < cmt_from.size(); ++i) {
    prob->y_init(cmt_to[i],Data(this_row,cmt_from[i]));
  }
}


void dataobject::reload_parameters(Rcpp::NumericVector PARAM, odeproblem* prob) {
  for(size_t i = 0; i < par_to.size(); ++i) {
    prob->param(par_to[i],PARAM[par_to[i]]);
  }
}



void dataobject::get_records(recstack& a, int NID, int neq,
                             int& obscount, int& evcount,
                             bool obsonly, bool debug) {
  
  // only look here for events or observations if there is more than one column:
  // size_t h=0; warnings
  int j=0, k=0;
  int evid, this_cmt;
  double lastime = 0;
  if(debug) Rcpp::Rcout << "Generating record set ... " << std::endl;
  
  if(Data.ncol() <= 1) return;
  
  for(int h=0; h < NID; ++h) {
    
    lastime = 0;
    
    for(j = this -> start(h); j <= this -> end(h); ++j) {
      
      if(Data(j,col[_COL_time_]) < lastime) Rcpp::stop("Problem with time: data set is not sorted by time or time is negative.");
      
      lastime = Data(j,col[_COL_time_]);
      
      this_cmt = Data(j,col[_COL_cmt_]);
      evid = Data(j,col[_COL_evid_]);
      k = j - this -> start(h);
      
      // If this is an observation record
      if(evid==0) {
        if((this_cmt < 0) || (this_cmt > neq)) {
          Rcpp::stop("cmt number in observation record out of range.");
        }
        rec_ptr obs(new datarecord(0,Data(j,col[_COL_time_]),Data(j,col[_COL_cmt_]),j,Data(j,Idcol)));
        obs->from_data(true);
        //a.at(h).at(k) = obs;
        a.at(h).push_back(obs);
        ++obscount;
        continue;
      }
      
      // If this is not an obsrevation record:
      
      // Check that cmt is valid:
      if((this_cmt==0) || (abs(this_cmt) > neq)) {
        Rcpp::stop("cmt number in dosing record out of range.");
      }
      
      ++evcount;
      
      ev_ptr ev(new pkevent(Data(j,col[_COL_cmt_]),
                            Data(j,col[_COL_evid_]),
                            Data(j,col[_COL_amt_]),
                            Data(j,col[_COL_time_]),
                            Data(j,col[_COL_rate_])));
      
      if((ev->rate() < 0) && (ev ->rate() != -1) && (ev->rate() !=-2)) {
        Rcpp::stop("Non-zero rate must be positive or equal to -1 or -2");
      }
      if((ev->rate() !=0 ) && (ev->amt() <= 0) && (ev->evid()==1)) {
        Rcpp::stop("Non-zero rate requires positive amt.");
      }
      
      if(obsonly) ev->output(false);
      
      ev->from_data(true);
      ev->evid(evid);
      ev->pos(j);
      ev->report(0);
      ev->ss(Data(j,col[_COL_ss_]));
      ev->addl(Data(j,col[_COL_addl_]));
      ev->ii(Data(j,col[_COL_ii_]));
      ev->id(Data(j,Idcol));
      
      if((ev->addl() > 0) && (ev->ii() <=0)) {
        Rcpp::stop("Found dosing record with addl > 0 and ii <= 0.");
      }
      if((ev->ss()) && (ev->ii() <=0)) {
        Rcpp::stop("Found dosing record with ss==1 and ii <= 0.");
      }
      //a.at(h).at(k) = ev;
      a.at(h).push_back(ev);
    }
  }
}



void dataobject::check_idcol(dataobject* data) {
  
  if(data->ncol() == 0) {return;}
  
  dvec udata;
  int i;
  int ndata  = data->nrow();
  int idcol = data->idcol();
  
  udata.resize(ndata);
  
  for(i=0; i < ndata; ++i) udata[i] = data->get_value(i,idcol);
  
  dvec uthis  = this->return_uid();
  
  sort_unique(uthis);
  sort_unique(udata);
  
  dvec inter;
  std::set_intersection(uthis.begin(), uthis.end(),
                        udata.begin(), udata.end(),
                        std::back_inserter(inter));
  
  if(inter!=uthis) Rcpp::stop("ID found in the data set, but not in idata.");
  
}



