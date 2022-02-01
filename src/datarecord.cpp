// Copyright (C) 2013 - 2020  Metrum Research Group
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
 * @file datarecord.cpp
 * 
 */

#include "RcppInclude.h"
#include "datarecord.h"
#include "odeproblem.h"
//#include <boost/make_shared.hpp>
#include <functional>
#include <algorithm>

// Tgrid Observations that need to get output
// And Ptime observations
// Need to say if it is output (stime) or not (ptime)

datarecord::datarecord(double time_, int pos_, bool output_) {
  Time = time_;
  Pos = pos_;
  Output = output_;
  Cmt = 0;
  Evid = 0;
  Amt = 0;
  Rate = 0;
  Ii = 0;
  Ss = 0;
  Addl = 0;
  Id = 1;
  Fromdata=false;
  Armed = false;
}


// Data set observations
datarecord::datarecord(double time_, short int cmt_, int pos_, double id_) {
  Time = time_;
  Cmt  = cmt_;
  Pos = pos_;
  Id = id_;
  Evid = 0;
  Amt = 0;
  Rate = 0;
  Ii = 0;
  Ss = 0;
  Addl = 0;
  Output = true;
  Armed = false;
  Fromdata = true;
}

// Data set event
// Schedule events
//   Need to make sure that scheduled events are output false, from data false
datarecord::datarecord(short int cmt_, int evid_, double amt_, double time_, 
                       double rate_, int pos_, double id_) {
  Time = time_;
  Cmt  = cmt_;
  Pos = pos_;
  Id = id_;
  Evid = evid_;
  Amt = amt_;
  Rate = rate_;
  Addl = 0;
  Ii = 0;
  Ss = 0;
  Output = false;
  Armed = true;
  Fromdata = false;
}



// Short event
// cmt evid amt time rate
datarecord::datarecord(short int cmt_, int evid_, double amt_, 
                       double time_, double rate_) {
  
  Cmt  = cmt_;
  Evid = evid_;
  Amt = amt_;
  Time = time_;
  Rate = rate_;
  Pos = 1;
  Id = 1;
  Addl = 0;
  Ii = 0;
  Ss = 0;
  Output = false;
  Armed = true;
  Fromdata = false;
}



datarecord::~datarecord() {}

bool CompByTimePosRec(const rec_ptr& a, const rec_ptr& b) {
  bool res = a->time() < b->time(); 
  if(!res) res = a->pos() < b->pos(); 
  return res;
}

bool CompEqual(const reclist& a, double time, unsigned int evid, int cmt) {
  for(size_t i = 0; i < a.size(); ++i) {
    if(a[i]->time() != time) continue;
    if((a[i]->evid()==evid) && (a[i]->cmt()==cmt)) {
      return true;    
    }
  }
  return false;
}

double datarecord::dur(double b) {
  return(b*Amt/Rate);
}

bool datarecord::ss_infusion() {
  return (Evid==1) && (Amt==0) && (Ss==1) && ((Rate > 0) || (Rate == -1));  
}

void datarecord::implement(odeproblem* prob) {
  
  if(Evid==0 || (!Armed && Evid ==1) || (prob->neq()==0)) {
    return;
  }
  
  unsigned int evid = Evid;
  
  if(this->infusion() && Evid != 4) evid = 5;
  
  int eq_n = this->cmtn();
  
  double Fn = prob->fbio(eq_n);
  
  switch (evid) {
  case 1: // Dosing event record
    if(!prob->is_on(eq_n)) prob->on(eq_n);
    prob->y_add(eq_n, Amt * Fn);
    break;
  case 5:  // Turn infusion on event record
    if(!prob->is_on(eq_n)) prob->on(eq_n);
    if(Fn == 0) break;
    prob->rate_add(eq_n,Rate);
    break;
  case 9: // Turn infusion off event record
    if(!prob->is_on(eq_n)) break;
    prob->rate_rm(eq_n, Rate);
    break;
  case 2: // Other type event record:
    if(Cmt > 0) { 
      prob->on(eq_n);
    }
    if(Cmt < 0) {
      prob->off(eq_n);
      prob->y(eq_n,0.0);
    }
    break;
  case 3: // reset event record
    for(int i=0; i < prob->neq(); ++i) {
      prob->y(i,0.0);
      prob->on(i);
      prob->rate0(i,0.0);
    }
    prob->init_call(Time);
    break;
  case 8: // replace
    prob->y(eq_n, Amt);
    break;
  case 4:
    for(int i=0; i < prob->neq(); ++i) {
      prob->y(i,0.0);
      prob->on(i);
      prob->rate0(i,0.0);
    } {
      prob->init_call(Time);
      if(!Armed) break;
      if(Rate > 0) {
        this->evid(5);
      } else {
        this->evid(1);
      }
      this-> implement(prob);
      return;
    }
  }
  prob->lsoda_init();
}

/* 
 * Brings system to steady state if appropriate.
 */
void datarecord::steady(odeproblem* prob, reclist& thisi, double Fn, LSODA& solver) {
  if(Ss > 0) {
    if(Rate == 0) this->steady_bolus(prob,solver);
    if(Rate >  0) this->steady_infusion(prob,thisi,solver);
  }
}

void datarecord::steady_bolus(odeproblem* prob, LSODA& solver) {
  
  prob->ss_flag = true;
  
  dvec state_incoming;
  
  if(Ss == 2) {
    state_incoming.resize(prob->neq());
    for(size_t i = 0; i < state_incoming.size(); i++) {
      state_incoming[i] = prob->y(i);
    }
  }
  
  prob->rate_reset();
  bool warn = !prob->ss_fixed;
  int N_SS = prob->ss_n;  
  double tfrom = 0.0;
  double tto = 0.0;
  
  std::vector<double> last(prob->neq(), -1E9);
  double diff = 0, err = 0;
  bool made_it = false;
  size_t n_cmt = prob->Ss_cmt.size();

  prob->lsoda_init();
  
  rec_ptr evon = NEWREC(Cmt, 1, Amt, Time, Rate);
  
  for(int i=1; i < N_SS; ++i) {
    
    tfrom = double(i-1)*Ii;
    tto = double(i)*Ii;
    
    evon->implement(prob);
    prob->lsoda_init();
    prob->advance(tfrom,tto,solver);
    
    size_t ngood = 0, j = 0;
    for(size_t jj=0; jj < n_cmt; ++jj) {
      j = prob->Ss_cmt[jj];
      diff = fabs(prob->y(j) - last[j]);
      err = prob->ssRtol * fabs(prob->y(j)) + prob->ssAtol;
      if(diff < err ) ++ngood;
      last[j] = prob->y(j);
    } 
    if(ngood == n_cmt) {
      made_it = true;
      break;
    }
  }
  
  if((!made_it) && warn) {
    Rcpp::warning(
      tfm::format(
        "[steady_bolus] ID %d failed to reach steady state\n  ss_n: %d, "
        "ss_rtol: %d, ss_atol: %d", 
        this->id(),N_SS, prob->ssRtol, prob->ssAtol
      ).c_str()
    );
  }
  
  // If we need a lagtime, give one more dose
  // and advance to tto - lagtime.
  double lagt = prob->alag(this->cmtn());
  if(lagt > 0) {
    if(Ss==2) {
      throw Rcpp::exception("Ss == 2 with lag time is not currently supported.",false);
    }
    prob->lsoda_init();
    evon->implement(prob); 
    if(lagt <= Ii) {
      tfrom = tto;
      tto = tfrom + Ii - lagt;
      if(tto <= tfrom) {
        throw Rcpp::exception("tto <= tfrom in seady_bolus with lag time.",false);  
      }
      prob->advance(tfrom, tto, solver);
    }
  }
  
  if(Ss == 2) {
    for(size_t i=0; i < state_incoming.size(); i++) {
      prob->y(i,prob->y(i) + state_incoming[i]); 
    }
  } 
  prob->lsoda_init();
  prob->ss_flag = false;
} 


void datarecord::steady_infusion(odeproblem* prob, reclist& thisi, LSODA& solver) {
  
  if(this->unarmed()) {
    this->steady_bolus(prob,solver);
    return;
  }
  
  if(this->ss_infusion()) {
    this->steady_zero(prob,solver);
    return;
  }
  
  prob->ss_flag = true;
  
  std::vector<double> state_incoming;
  
  if(Ss == 2) {
    state_incoming.resize(prob->neq());
    for(size_t i = 0; i < state_incoming.size(); i++) {
      state_incoming[i] = prob->y(i);
    }
  }
  double lagt = prob->alag(this->cmtn());
  
  double Fn = prob->fbio(this->cmtn());
  
  double duration = this->dur(Fn);
  
  double tfrom = 0.0;
  
  int i;

  bool warn = !prob->ss_fixed;
  int N_SS = prob->ss_n;  
  size_t n_cmt = prob->Ss_cmt.size();
  std::vector<double> last(prob->neq(),-1e9);
  
  reclist offs;
  
  int start = 0;
  int end = 0;
  bool made_it = false;  
  double diff = 0, err = 0;
  double nexti = 0.0, toff = 0.0;
  prob->rate_reset();
  
  // We only need one of these; it gets updated and re-used immediately
  rec_ptr evon = NEWREC(Cmt, 1, Amt, tfrom, Rate);
  
  for(i=1; i < N_SS ; ++i) {
    evon->time(tfrom);
    ++start;
    evon->implement(prob);
    prob->lsoda_init();
    toff = tfrom + duration;
    
    // Create an event to turn the infusion off and push onto offs vector
    // Keep on creating these
    rec_ptr evoff = NEWREC(Cmt, 9, Amt, toff, Rate);
    offs.push_back(evoff);
    
    // The next time an infusion will start
    nexti = double(i)*Ii;
    // As long as there are infusions to turn off and the
    // first one is before or at the next infusion start time
    while((!offs.empty()) && (offs[0]->time()  <= nexti)) {
      toff = offs[0]->time();
      prob->advance(tfrom,toff,solver);
      ++end;
      offs[0]->implement(prob);
      prob->lsoda_init();
      tfrom = toff;
      offs.erase(offs.begin());
    }
    
    prob->lsoda_init();
    prob->advance(tfrom,nexti,solver);
    
    tfrom = nexti;
    
    size_t ngood = 0, j = 0;
    for(size_t jj=0; jj < n_cmt; ++jj) {
      j = prob->Ss_cmt[jj];
      diff = fabs(prob->y(j) - last[j]);
      err = prob->ssRtol * fabs(prob->y(j)) + prob->ssAtol;
      if(diff < err ) ++ngood;
      last[j] = prob->y(j);
    } 
    if(ngood == n_cmt) {
      tfrom = nexti;
      nexti  = double(i+1)*Ii;
      made_it = true;
      break;
    }
  }
  if((!made_it) && warn) {
    Rcpp::warning(
      tfm::format(
        "[steady_infusion] ID %d failed to reach steady state\n  ss_n: %d, "
        "ss_rtol: %d, ss_atol: %d", 
        this->id(),N_SS, prob->ssRtol, prob->ssAtol
      ).c_str()
    );
  }
  // If we need a lagtime, give one more dose
  // and advance to tto - lagtime.
  if(lagt > 0) {
    if(lagt >= Ii) {
      throw Rcpp::exception(
          "ALAG_CMT greater than ii on ss record.",
          false
      );
    }
    if((duration + lagt) >= Ii) {
      throw Rcpp::exception(
          "infusion duration + ALAG_CMT greater than ii on ss record.",
          false
      );
    }
    if(Ss==2) {
      throw Rcpp::exception(
          "Ss == 2 with lag time is not currently supported.",
          false
      );
    }
    if(lagt <= Ii) {
      evon->time(tfrom);
      evon->implement(prob);
      toff  = tfrom + duration;
      prob->advance(tfrom,toff,solver);
      rec_ptr evoff = NEWREC(Cmt, 9, Amt, toff, Rate);
      evoff->implement(prob);
      prob->lsoda_init();
      prob->advance(toff, (nexti - lagt),solver);
    }
  }
  if(Ss == 2) {
    for(size_t i=0; i < state_incoming.size(); i++) {
      prob->y(i,prob->y(i) + state_incoming[i]); 
    }
  }
  
  // Add on infusion off events
  int ninf_ss = floor(duration/this->ii());
  
  double first_off = Time + duration - double(ninf_ss)*Ii - lagt;
  if(first_off == Time) {
    first_off = duration - Ii + Time + lagt;
    --ninf_ss;
  }
  for(size_t k = 0; k < offs.size(); ++k) {
    offs.at(k)->time(first_off + double(k)*double(Ii));
    thisi.push_back(offs.at(k)); 
  }
  std::sort(thisi.begin(),thisi.end(),CompRec());
  prob->lsoda_init();
  prob->ss_flag = false;
}

void datarecord::steady_zero(odeproblem* prob, LSODA& solver) {
  
  if(this->unarmed()) {
    this->steady_bolus(prob,solver);
    return;
  }
  
  prob->ss_flag = true;
  
  double tfrom = 0.0;
  double tto = 0.0;
  double a1 = 0, a2 = 0, t1 = 0, t2 = 0;
  bool warn = !prob->ss_fixed;
  int N_SS = prob->ss_n;  
  size_t n_cmt = prob->Ss_cmt.size();
  std::vector<double> last(prob->neq(),-1e9);
  bool made_it = false;
  
  double diff = 0.0, err = 0.0;
  prob->rate_reset();
  rec_ptr evon = NEWREC(Cmt, 5, Amt, tfrom, Rate);
  evon->implement(prob);
  prob->lsoda_init();
  double duration = 10;
  for(int i=1; i < N_SS ; ++i) {
    prob->lsoda_init();
    tto = tfrom + duration;
    prob->advance(tfrom,tto,solver);
    tfrom = tto;
    size_t ngood = 0, j = 0;
    for(size_t jj=0; jj < n_cmt; ++jj) {
      j = prob->Ss_cmt[jj];
      diff = fabs(prob->y(j) - last[j]);
      err = prob->ssRtol*fabs(prob->y(j)) + prob->ssAtol;
      if(diff < err) ++ngood;
      last[j] = prob->y(j);
    }
    if(ngood == n_cmt) {
      made_it = true;
      break;
    }
    if(i==10) duration = 15;
    if(i==15) {
      a1 = prob->y(Cmt);
      t1 = tto;
      duration = 20;
    }
    if(i==25) {
      a2 = prob->y(Cmt);
      t2 = tto;
      double k_ = Rate/(a1+a2) + (a1-a2)/((a1+a2)*(t2-t1));
      duration = std::max(duration,0.693/k_); // 2*thalf Chiou
    }
  }
  if((!made_it) && warn) {
    Rcpp::warning(
      tfm::format(
        "[steady_zero] ID %d failed to reach steady state\n  ss_n: %d, "
        "ss_rtol: %d, ss_atol: %d", 
        this->id(),N_SS, prob->ssRtol, prob->ssAtol
      ).c_str()
    );
  }
  prob->rate_reset();
  prob->lsoda_init();
  this->unarm();
  prob->ss_flag = false;
}

void datarecord::schedule(std::vector<rec_ptr>& thisi, double maxtime, 
                          bool addl_ev_first, const unsigned int maxpos, double Fn) {
  
  if(Addl ==0) return;
  
  unsigned int this_evid = Evid;
  
  if(this_evid == 4) {
    this_evid = Rate > 0 ? 5 : 1;
  }
  
  if(this->int_infusion()) {
    thisi.reserve(thisi.size() + Addl);  
  } else {
    thisi.reserve(thisi.size() + Addl); 
  }
  
  double ontime = 0;
  
  int mp = 1000000000;
  
  int nextpos = addl_ev_first ?  -1000000000 : mp;
  
  for(unsigned int k=1; k<=Addl; ++k) {
    
    ontime = Time + Ii*double(k);
    
    if(ontime > maxtime) break;
    
    rec_ptr evon = NEWREC(Cmt, this_evid, Amt, ontime, Rate, nextpos, Id);
    
    thisi.push_back(evon);
    
  }
}  
