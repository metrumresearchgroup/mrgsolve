// Copyright (C) 2013 - 2019  Metrum Research Group
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
 * @file dataobject.cpp
 * 
 */

#include "RcppInclude.h"
#include "dataobject.h"
#include "mrgsolve.h"
#include "mrgsolv.h"

#define _COL_amt_   0u
#define _COL_ii_    1u
#define _COL_addl_  2u
#define _COL_ss_    3u
#define _COL_rate_  4u
#define _COL_evid_  5u
#define _COL_cmt_   6u
#define _COL_time_  7u
#define say(a) Rcpp::Rcout << a << std::endl;

dataobject::dataobject(Rcpp::NumericMatrix _data, 
                       Rcpp::CharacterVector _parnames) {
  Data = _data;
  parnames = _parnames;
  
  Rcpp::List dimnames = _data.attr("dimnames");
  Data_names = Rcpp::as<Rcpp::CharacterVector>(dimnames[1]);
  
  Idcol = find_position("ID", Data_names);
  if(Idcol < 0) {
    throw Rcpp::exception("could not find ID column in data set.",false);
  }
  
  // Connect Names in the data set with positions in the parameter list
  from_to(Data_names, parnames, par_from, par_to);
  
  col.resize(8,0);
  
}

dataobject::dataobject(Rcpp::NumericMatrix _data,
                       Rcpp::CharacterVector _parnames,
                       Rcpp::CharacterVector _cmtnames) {
  Data = _data;
  parnames = _parnames;
  cmtnames = Rcpp::clone(_cmtnames);
  
  Rcpp::List dimnames = _data.attr("dimnames");
  Data_names = Rcpp::as<Rcpp::CharacterVector>(dimnames[1]);
  
  Idcol = find_position("ID", Data_names);
  
  if(Idcol < 0) {
    throw Rcpp::exception(
        "could not find ID column in data set.",false
    );
  }
  
  for(Rcpp::CharacterVector::iterator it = cmtnames.begin(); it != cmtnames.end(); ++it) {
    *it += "_0";
  }
  
  // Connect Names in the data set with positions in the parameter list
  from_to(Data_names, parnames, par_from, par_to);
  from_to(Data_names, cmtnames, cmt_from, cmt_to);
  
  col.resize(8,0);
  
}


dataobject::~dataobject(){}

void dataobject::map_uid() {
  int n = Data.nrow();
  Uid.push_back(Data(0,Idcol));
  Startrow.push_back(0);
  for(int i=1; i < n; ++i) {
    if(Data(i-1,Idcol) != Data(i, Idcol)) {
      Uid.push_back(Data(i,Idcol));
      Startrow.push_back(i);
      Endrow.push_back(i-1);
    }
  }
  Endrow.push_back(n-1);
}


Rcpp::IntegerVector dataobject::get_col_n(const Rcpp::CharacterVector& what) {
  Rcpp::IntegerVector ret = Rcpp::match(what, Data_names);
  ret = Rcpp::na_omit(ret);
  return(ret-1);
}


void dataobject::locate_tran() {
  
  unsigned int zeros = Data.ncol()-1;
  
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
  
  unsigned int tcol = std::find(bg,ed,"time") - bg;
  
  bool lc = tcol <= zeros;
  col[_COL_time_] = tcol;
  
  if(!lc) {
    tcol = std::find(bg,ed,"TIME") - bg;
    if(tcol > zeros) {
      col[_COL_time_] = zeros;
    } else {
      col[_COL_time_] = tcol;
    }
  }
  
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
  if(col[_COL_cmt_] > zeros) col[_COL_cmt_] = zeros;
}

void dataobject::idata_row() {
  Uid.resize(Data.nrow());
  for(int i=0; i < Data.nrow(); ++i) {
    idmap[Data(i,Idcol)] = i;
    Uid[i] = Data(i,Idcol);
  }
}

void dataobject::copy_parameters(int this_row, odeproblem* prob) {
  size_t n = par_from.size();
  for(size_t i=0; i < n; ++i) {
    prob->param(par_to[i],Data(this_row,par_from[i]));
  }
}

void dataobject::copy_inits(int this_row, odeproblem *prob) {
  // this should only be done from idata sets
  size_t n = cmt_from.size();
  for(size_t i=0; i < n; ++i) {
    prob->y_init(cmt_to[i],Data(this_row,cmt_from[i]));
  }
}

void dataobject::reload_parameters(const Rcpp::NumericVector& PARAM, 
                                   odeproblem *prob) {
  size_t n = par_to.size();
  for(size_t i = 0; i < n; ++i) {
    prob->param(par_to[i],PARAM[par_to[i]]);
  }
}

void dataobject:: get_records_pred(recstack& a, int NID, int neq,
                                   unsigned int& obscount, unsigned int& evcount,
                                   bool obsonly, bool debug) {
  
  int j=0;
  double lastime = 0;
  if(Data.ncol() <=1) {
    return;  
  }
  for(int h=0; h < NID; ++h) {
    lastime = Data(this->start(h),col[_COL_time_]);
    a[h].reserve(this->end(h) - this->start(h) + 5);
    
    for(j=this->start(h); j <= this->end(h); ++j) {
      if(Data(j,col[_COL_time_]) < lastime) {
        Rcpp::Rcout << lastime << std::endl;
        throw Rcpp::exception(
            "the data set is not sorted by time.",
            false
        );
      }
      lastime = Data(j,col[_COL_time_]);        
      rec_ptr obs = std::make_shared<datarecord>(
        Data(j,col[_COL_time_]),
        Data(j,col[_COL_cmt_]),
        j,
        Data(j,Idcol)
      );
      
      if(Data(j,col[_COL_cmt_]) != 0.0) {
        throw Rcpp::exception(
            "all records must have cmt set to zero.",
            false
        ); 
      }
      if(Data(j,col[_COL_rate_]) != 0.0) {
        throw Rcpp::exception(
            "all records must have rate set to zero.",
            false
        ); 
      }
      if(Data(j,col[_COL_ss_]) != 0.0) {
        throw Rcpp::exception(
            "all records must have ss set to zero.",
            false
        ); 
      }
      
      obs->evid(Data(j,col[_COL_evid_]));
      obs->addl(Data(j,col[_COL_addl_]));
      obs->ii(Data(j,col[_COL_ii_]));
      obs->unarm();
      a[h].push_back(obs);
      if(obs->evid() ==0) {
        ++obscount;
      } else {
        ++evcount;  
      }
    }
  }
}


void dataobject::get_records(recstack& a, int NID, int neq,
                             unsigned int& obscount, unsigned int& evcount,
                             bool obsonly, bool debug) {
  
  if(neq==0) {
    get_records_pred(a, NID, neq, obscount, evcount, obsonly, debug);
    return;  
  }
  
  // only look here for events or observation if there is more than one column:
  // size_t h=0; warnings
  int j=0;
  int this_cmt;
  double lastime = 0;
  
  if(Data.ncol() <= 1) {
    return;
  }
  
  for(int h=0; h < NID; ++h) {
    
    lastime = 0;
    
    a[h].reserve(this->end(h) - this->start(h) + 5);
    
    for(j = this->start(h); j <= this->end(h); ++j) {
      
      if(Data(j,col[_COL_time_]) < lastime) {
        throw Rcpp::exception(
            tfm::format(
              "the data set is not sorted by time or time is negative \n ID: %d, row: %i, time: %d", 
              Data(j,Idcol), j+1, Data(j,col[_COL_time_])
            ).c_str(),
            false
        );
      }
      
      lastime = Data(j,col[_COL_time_]);
      
      this_cmt = Data(j,col[_COL_cmt_]);
      
      // If this is an observation record
      if(Data(j,col[_COL_evid_])==0) {
        
        if((this_cmt < 0) || (this_cmt > neq)) {
          throw Rcpp::exception(
              tfm::format(
                "cmt number in observation record out of range \n ID: %d, row: %i, cmt: %i, neq: %i", 
                Data(j,Idcol), j+1, this_cmt, neq
              ).c_str(),
              false
          );
        }
        
        rec_ptr obs = std::make_shared<datarecord>(
          Data(j,col[_COL_time_]),
          Data(j,col[_COL_cmt_]),
          j,
          Data(j,Idcol)
        );
        
        a[h].push_back(obs);
        ++obscount;
        continue;
      }
      
      // Check that cmt is valid:
      if((this_cmt==0) || (abs(this_cmt) > neq)) {
        throw Rcpp::exception(
            tfm::format(
              "event record cmt must be between 1 and %i: \n ID: %d, row: %i, cmt: %i, evid: %i", 
              neq, Data(j,Idcol), j+1, this_cmt, Data(j,col[_COL_evid_])
            ).c_str(),
            false
        );
      }
      
      ++evcount;
      
      rec_ptr ev = std::make_shared<datarecord>(
        Data(j,col[_COL_cmt_]),
        Data(j,col[_COL_evid_]),
        Data(j,col[_COL_amt_]),
        Data(j,col[_COL_time_]),
        Data(j,col[_COL_rate_]),
        j, 
        Data(j,Idcol)
      );
      ev->ss(Data(j,col[_COL_ss_]));
      ev->addl(Data(j,col[_COL_addl_]));
      ev->ii(Data(j,col[_COL_ii_]));
      ev->from_data(true);
      if(!obsonly) ev->output(true);
      
      bool zero_inf = ev->ss_infusion();
      
      if(zero_inf) {
        if(ev->addl() !=0) {
          throw Rcpp::exception(
              tfm::format(
                "addl must be zero for ss infusion \n ID: %d, row: %i, ii: %d", 
                ev->id(), j+1,  ev->addl()
              ).c_str(),
              false
          );
        }
      }
      
      if((ev->rate() < 0) && (ev->rate() != -2) && (ev->rate() != -1)) {
        throw Rcpp::exception(
            tfm::format(
              "non-zero rate must be positive or -1 or -2 \n ID: %d, row: %i, rate: %d", 
              ev->id(), j+1,  ev->rate()
            ).c_str(),
            false
        );
      }
      
      if((ev->rate() != 0) && (ev->amt() <= 0) && (ev->evid()==1) && !zero_inf) {
        throw Rcpp::exception(
            tfm::format(
              "non-zero rate requires positive amt \n ID: %d, row: %i, rate: %d, amt: %d", 
              ev->id(), j+1,  ev->rate(), ev->amt()
            ).c_str(),
            false
        );
      }
      
      if(ev->ii() <= 0) {
        if(ev->addl() > 0) {
          throw Rcpp::exception(
              tfm::format(
                "dosing record with addl > 0 and ii <= 0 \n ID: %d, row: %i, addl: %i", 
                ev->id(), j+1, ev->addl()
              ).c_str(),
              false
          );
        }
        if(ev->ss() && (!zero_inf)) {
          throw Rcpp::exception(
              tfm::format(
                "dosing record with ss > 0 and ii <= 0 \n ID: %d, row: %i", 
                ev->id(), j+1
              ).c_str(),
              false
          );
        }
      } // ii <=0
      
      a[h].push_back(ev);
    }
  }
}

unsigned int dataobject::get_idata_row(const double ID) {
  if(idmap.size()==0) return 0;
  return idmap[ID];
}

void dataobject::check_idcol(dataobject& idat) {
  
  if(idat.ncol() == 0) {return;}
  
  uidtype uidata = idat.Uid;
  uidtype udata = Uid;
  sort_unique(uidata);
  sort_unique(udata);
  
  if(!std::includes(uidata.begin(),uidata.end(),udata.begin(),udata.end())) {
    throw Rcpp::exception(
        "ID found in the data set, but not in idata.",
        false);
  }
}


void dataobject::carry_out(const recstack& a, 
                           Rcpp::NumericMatrix& ans,
                           dataobject& idat,
                           const Rcpp::IntegerVector& data_carry,
                           const unsigned int data_carry_start,
                           const Rcpp::IntegerVector& idata_carry,
                           const unsigned int idata_carry_start) {
  
  int crow = 0;
  int j = 0;
  int lastpos = -1;
  unsigned int idatarow=0;
  int nidata = idat.nrow();
  int n_data_carry = data_carry.size();
  int n_idata_carry = idata_carry.size();
  int k = 0;
  
  const bool carry_from_data = n_data_carry > 0;
  const bool carry_from_idata = (n_idata_carry > 0) & (nidata > 0); 
  
  for(recstack::const_iterator it=a.begin(); it!=a.end(); ++it) {
    
    j = it-a.begin();
    
    if(carry_from_idata) {
      idatarow = idat.get_idata_row(this->get_uid(j));
    }
    
    lastpos = -1;
    
    for(reclist::const_iterator itt = it->begin(); itt != it->end(); ++itt) {
      
      // Get the last valid data set position to carry from
      if(carry_from_data) {
        if((*itt)->from_data()) lastpos = (*itt)->pos();
      }
      
      if(!(*itt)->output()) continue;
      
      // Copy from idata:
      for(k=0; k < n_idata_carry; ++k) {
        ans(crow, idata_carry_start+k) = idat.Data(idatarow,idata_carry[k]);
      }
      
      if(carry_from_data) {
        if(lastpos >=0) {
          for(k=0; k < n_data_carry; ++k) {
            ans(crow, data_carry_start+k)  = Data(lastpos,data_carry[k]);
          }
        } else {
          for(k=0; k < n_data_carry; ++k) {
            ans(crow, data_carry_start+k)  = Data(this->start(j),data_carry[k]);
          }
        }
      } // end carry_from_data
      ++crow;
    }
  }
}

// SAVE FOR LATER
// void dataobject::expand_records(recstack& a, 
//                                 dataobject& idat,
//                                 int& NID, 
//                                 unsigned int& obscount, 
//                                 unsigned int& evcount,
//                                 bool debug) {
//   
//   if(idat.nrow()==0) {
//     //Rcpp::Rcout << "Returning ... no idata rows" << std::endl;  
//     return;
//   }
//   
//   if(a.size() > 1) {
//     return;
//     throw Rcpp::exception("data size isn't 1",false);
//   }
//   
//   if(!(Uid.size()==1)) {
//     throw Rcpp::exception("more than 1 found in the data set",false);  
//   }
//   
//   obscount = 0;
//   evcount = 0;
//   int idcol = idat.Idcol;
//   int nid = idat.nrow();
//   int nrec = a[0].size();
//   int start = Startrow[0];
//   int end = Endrow[0];
//   Startrow.resize(nid);
//   Endrow.resize(nid);
//   Uid.resize(nid);
//   a.resize(nid);
//   
//   NID = nid;
//   
//   for(int i = 0; i < nid; ++i) {
//     Startrow[i] = start;
//     Endrow[i]= end;
//     Uid[i] = idat.Data(i,idcol);
//     a[i].resize(nrec);
//     for(int j = 0; j < nrec; ++j) {
//       rec_ptr rec = std::make_shared<datarecord>(
//         a[0][j]->Cmt,
//         a[0][j]->Evid, 
//         a[0][j]->Amt,
//         a[0][j]->Time,
//         a[0][j]->Rate,
//         a[0][j]->Pos,
//         Uid[i]
//       );
//       rec->Ss = a[0][j]->Ss;
//       rec->Ii = a[0][j]->Ii;
//       rec->Addl = a[0][j]->Addl;
//       rec->Output = true;
//       rec->Fromdata = true;
//       rec->Armed = a[0][j]->Armed;
//       if(rec->Evid==0) {
//         ++obscount;
//       } else {
//         ++evcount;
//       }
//       a[i][j] = rec;
//     }
//   }
//   return;
// }
