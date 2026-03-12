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
 * @file datarecord.h
 */

#ifndef DATARECORD_H
#define DATARECORD_H
#include "mrgsolv.h"
#include "LSODA.h"
#include <deque>

class odeproblem;
class datarecord;
typedef std::deque<datarecord> reclist;

class datarecord {

public:
  //! constructor
  datarecord(double time_, int pos_, bool output_);

  //! constructor
  datarecord(double time_, short int cmt_, int pos_, double id_);

  //! constructor
  datarecord(short int cmt_, int evid_, double amt_, double time_, double rate_,
             int pos_, double id_);

  //! short event constructor
  datarecord(short int cmt_, int evid_, double amt_, double time_, double rate_, double fn_);

  ~datarecord();

  double time() const {return Time;}
  void time(double time_){Time = time_;}

  double id() const {return Id;}
  void id(double id_) {Id = id_;}

  unsigned int evid() const {return Evid;}
  void evid(unsigned short int evid_){Evid=evid_;}

  int pos() const {return Pos;}
  void pos(int pos_) {Pos=pos_;}

  short int cmt() const {return Cmt;}
  short int cmtn() const {return std::abs(Cmt)-1;}

  void output(bool in){Output=in;}
  bool output() const {return Output;}

  bool from_data() const {return Fromdata;}
  void from_data(bool val){Fromdata = val;}

  double amt() const {return Amt;}

  double rate() const {return Rate;}
  void rate(double value) {Rate = value;}

  double dur();

  void addl(int addl_){Addl = std::max(0,addl_);}
  unsigned int addl() const {return Addl;}

  void ss(int ss_){Ss = std::max(0,ss_);}
  unsigned short ss() const {return Ss;}

  void ii(double ii_){Ii = ii_;}
  double ii() const {return Ii;}

  double fn() const {return Fn;}
  void fn(double fn_){Fn = fn_;}

  void schedule(reclist& thisi, double maxtime, bool put_ev_first,
                const unsigned int maxpos, double lagt);
  void implement(odeproblem* prob);
  void steady_zero(odeproblem* prob, LSODA& solver);
  void steady_infusion(odeproblem* prob,reclist& thisi,LSODA& solver);
  void steady_bolus(odeproblem* prob,LSODA& solver);
  void steady(odeproblem* prob, reclist& thisi,LSODA& solver);

  bool infusion() const {return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0);}
  bool int_infusion() const {return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0) && (Amt > 0);}
  bool ss_int_infusion() const {return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0) && (Amt > 0) && (Ss > 0);}
  bool const_infusion() const {return (Evid==1) && (Rate > 0) && (Amt == 0);}
  bool ss_infusion();
  bool is_event() const {return (Evid > 0);}
  bool is_dose() const {return (Evid==1) || (Evid==4);}
  bool is_event_data() const {return (Evid != 0) && (Evid != 2) && Fromdata;}
  bool needs_sorting() const {return ((Addl > 0) || (Ss == 1));}

  bool unarmed() const {return !Armed;}
  void arm() {Armed=true;}
  bool armed() const {return Armed;}
  void unarm() {Armed=false;}

  void phantom_rec() {Output=false; Fromdata=false;}
  bool is_phantom() const {return !Output && !Fromdata;}
  bool is_lagged() const {return Lagged;}
  void lagged() {Lagged = true;}

  int Pos; ///< record position number
  unsigned short int Evid; ///< record event ID
  unsigned short int Ss; ///< record steady-state indicator
  short int Cmt; ///< record compartment number
  unsigned int Addl; ///< number of additional doses

  double Time; ///< record time
  double Id; ///< record ID value
  double Amt; ///< record dosing amount value
  double Rate; ///< record infusion rate value
  double Ii; ///< record inter-dose interval value
  double Fn; ///< record bioavailability

  bool Output; ///< should this record be included in output?
  bool Fromdata; ///< is this record from the original data set?
  bool Lagged; ///< this record was added as result of ALAG
  bool Armed; ///< only armed records are actually executed
};


bool CompByTimePosRec(const datarecord& a, const datarecord& b);
bool CompEqual(const reclist& a, double time, unsigned int evid, int cmt,
               double amt);

/**
 * @brief Functor for sorting data records in <code>reclist</code>.
 *
 * Records are first sorted by time, then by position.
 *
 * @param a first record
 * @param b second record
 * @return boolean
 */
struct CompRec {
  inline bool operator()(const datarecord& a, const datarecord& b) const {
    if(a.Time == b.Time) {
      return a.Pos < b.Pos;
    }
    return a.Time < b.Time;
  }
};

void insert_record(reclist& thisi, const size_t start, datarecord& rec,
                   const bool put_ev_first);

void insert_observations(reclist& thisi, mrgsolve::evdata& ev, const size_t start,
                         const bool put_ev_first);
#endif
