#ifndef DEVTRAN_H
#define DEVTRAN_H

#include <algorithm>
#include <vector>
#include "RcppInclude.h"
#include "odeproblem.h"
#include "LSODA.h"

#ifndef CRUMP
#define CRUMP(a) throw Rcpp::exception(a,false)
#endif

#ifndef __ALAG_POS
#define __ALAG_POS -1200
#endif

// Finish out a record for an individual once SYSTEMOFF has been set; no
// other calls to any model functions will be made.
//
//   SYSTEMOFF == 1          --> copy model results to the line
//   SYSTEMOFF != 0, != 1    --> fill NA
//
// Advances the row counters when the record is an output record.
inline void systemoff_impl(odeproblem& prob,
                           const rec_ptr& this_rec,
                           Rcpp::NumericMatrix& ans,
                           unsigned int& crow,
                           int& icrow,
                           const double id,
                           const double tto,
                           const Rcpp::IntegerVector& request,
                           const unsigned int nreq,
                           const unsigned int req_start,
                           const Rcpp::IntegerVector& capture,
                           const unsigned int n_capture,
                           const unsigned int capture_start) {

  unsigned short int status = prob.systemoff();

  if(status==9)   CRUMP("the problem was stopped at user request.");
  if(status==999) CRUMP("999 sent from the model.");

  if(!this_rec->output()) return;

  if(status==1) {
    ans(crow,0) = id;
    ans(crow,1) = tto;
    for(unsigned int k=0; k < nreq; ++k) {
      ans(crow,(k+req_start)) = prob.y(request[k]);
    }
    for(unsigned int k=0; k < n_capture; ++k) {
      ans(crow,(k+capture_start)) = prob.capture(capture[k]);
    }
  } else {
    for(int k=0; k < ans.ncol(); ++k) {
      ans(crow,k) = NA_REAL;
    }
  }

  ++crow;
  ++icrow;
}

// Handle a non-observation event record (dose, reset, other event) prior to
// advancing the system.
//
// Note that F needs to get updated at every dose including addl but rate is
// fixed to the parent rate and we only verify infusions on actual dose records
// in the data set.
//
// This may append to and re-sort `thisi` (the record list for the current
// individual): additional doses (addl), the phantom record generated when
// there is a lag time, and infusion-off records. It also advances `tfrom`
// when a steady-state record with lag time is implemented, sets `this_cmtn`
// for the caller, and updates `told` when `tad` is requested.
//
// `j` is the index of `this_rec` within `thisi`.
inline void event_record_impl(odeproblem& prob,
                              rec_ptr& this_rec,
                              reclist& thisi,
                              const size_t j,
                              LSODA& solver,
                              int& this_cmtn,
                              double& tfrom,
                              const double tto,
                              double& told,
                              const double id,
                              const double mindt,
                              const double maxtime,
                              const unsigned int NN,
                              const bool put_ev_first,
                              const bool addl_ev_first,
                              const bool tad) {

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

  // Check data set rate against modeled rate and dur
  // This is only a check; rate_main will actually set the rate
  if(this_rec->rate() < 0) {
    prob.check_data_rate(this_rec, this_cmtn);
  }

  // Only check data set records
  if(this_rec->from_data()) {
    // Validate modeled rates
    if(prob.dur(this_cmtn) > 0) {
      prob.check_modeled_dur(this_rec);
    }
    // Validate modeled infusion rate
    if(prob.rate(this_cmtn) > 0) {
      prob.check_modeled_rate(this_rec);
    }
  }

  if(this_rec->rate() < 0) {
    prob.rate_main(this_rec, this_cmtn);
  }

  bool sort_recs = false;

  // Checking
  if(!this_rec->is_lagged()) {

    // there is a valid lag time
    if(prob.alag(this_cmtn) > mindt && this_rec->is_dose()) {
      if(this_rec->ss() > 0) {
        this_rec->steady(&prob, thisi, solver);
        tfrom = tto;
      }
      // We already advanced to ss; prob has the system_at_ss flag set
      // Lagged dose and all subsequent should be vanilla EVID=1 doses
      this_rec->ss(0);
      rec_ptr newev = NEWREC(*this_rec);
      if(newev->evid()==4) {
        newev->evid(1);
      }
      newev->pos(__ALAG_POS);
      newev->phantom_rec();
      newev->lagged();
      newev->time(this_rec->time() + prob.alag(this_cmtn));
      insert_record(thisi, j, newev, put_ev_first);
      // Save index (not iterator) before schedule appends; deque
      // push_back can invalidate iterators
      size_t merge_idx = thisi.size();
      newev->schedule(thisi, maxtime, put_ev_first, NN, prob.alag(this_cmtn));
      this_rec->unarm();
      sort_recs = newev->needs_sorting();
      if(sort_recs) {
        std::inplace_merge(
          thisi.begin()+j+1,
          thisi.begin()+merge_idx,
          thisi.end(),
          CompRec()
        );
        sort_recs = false;
      }
    } else { // no valid lagtime
      size_t merge_idx = thisi.size();
      this_rec->schedule(thisi, maxtime, addl_ev_first, NN, 0.0);
      sort_recs = this_rec->needs_sorting();
      if(sort_recs) {
        std::inplace_merge(
          thisi.begin()+j+1,
          thisi.begin()+merge_idx,
          thisi.end(),
          CompRec()
        );
        sort_recs = false;
      }
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
    insert_record(thisi, j, evoff, true);
  }

  if(tad) { // Adjusts told for lagtime
    if(!this_rec->is_lagged() && this_rec->is_dose()) {
      told = tto;
    }
  }
}

// Process the modeled events (mtime) that the model requested on this record;
// call only when prob.any_mtime() is true.
//
// Events requested for "now" are implemented immediately against `prob`
// (unless a lag time pushes them into the future); events in the future are
// inserted into `thisi` (the record list for the current individual), subject
// to the optional uniqueness check against `mtimehx`. Modeled observations
// are inserted directly.
//
// Advances `tfrom` when a steady-state modeled dose with lag time is
// implemented, updates `told` for any dose implemented now, records inserted
// events in `mtimehx` / `used_mtimehx`, and clears the mtime queue on `prob`.
//
// `j` is the index of the record being processed within `thisi`.
inline void mtime_impl(odeproblem& prob,
                       reclist& thisi,
                       const size_t j,
                       LSODA& solver,
                       reclist& mtimehx,
                       bool& used_mtimehx,
                       double& tfrom,
                       const double tto,
                       double& told,
                       const double id,
                       const double mindt,
                       const double maxtime,
                       const unsigned int neq,
                       const unsigned int NN,
                       const bool put_ev_first,
                       const bool addl_ev_first) {

  // Will set used_mtimehx only if we push back
  std::vector<mrgsolve::evdata> mt = prob.mtimes();
  for(size_t mti = 0; mti < mt.size(); ++mti) {
    // Unpack and check
    double this_time = (mt[mti]).time;
    unsigned int this_evid = (mt[mti]).evid;
    if(this_time < tto && !mt[mti].now) {
      throw Rcpp::exception(
          tfm::format(
            "modeled events or observations cannot start in the past\n"
            "ID: %d, time: %d, modeled record time: %d, evid: %i",
            id, tto, this_time, this_evid
          ).c_str(),
          false
      );
    };
    if(this_evid==0 || this_evid > 99) {
      insert_observations(thisi, mt[mti], j, addl_ev_first);
      continue;
    };
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
    //   get handled later as well
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
        prob.rate_main(new_ev, new_ev->cmtn());
      }
      if(prob.alag(new_ev->cmtn()) > mindt && new_ev->is_dose()) {
        if(new_ev->ss() > 0) {
          new_ev->steady(&prob, thisi, solver);
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
      new_ev->steady(&prob,thisi,solver);
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
        insert_record(thisi, j, evoff, true);
      }
    } else {
      bool do_mt_ev = true;
      if((mt[mti].check_unique)) {
        bool found = CompEqual(mtimehx,this_time,this_evid,this_cmt,
                               this_amt);
        do_mt_ev = do_mt_ev && !found;
      }
      if(do_mt_ev) {
        insert_record(thisi, j, new_ev, put_ev_first);
        mtimehx.push_back(new_ev);
      }
    } // Done processing "this" event
    if(schedule_addl) {
      // For parent doses happening "now", with or without lag time,
      //   but with positive addl
      // There is *no* unique check for additional doses
      size_t addl_merge_idx = thisi.size();
      new_ev->schedule(thisi, maxtime, addl_ev_first, NN, 0.0);
      std::inplace_merge(
        thisi.begin()+j+1,
        thisi.begin()+addl_merge_idx,
        thisi.end(),
        CompRec()
      );
    }
  } // Closes iteration across vector of events
  used_mtimehx = mtimehx.size() > 0;
  prob.clear_mtime();
}

#endif
