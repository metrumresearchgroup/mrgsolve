// Copyright (C) 2013 - 2017  Metrum Research Group, LLC
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
#include <boost/shared_ptr.hpp>
#include "mrgsolv.h"

class odeproblem;
class datarecord;
typedef boost::shared_ptr<datarecord> rec_ptr;
typedef std::vector<rec_ptr> reclist;

void add_mtime(reclist& thisi, dvec& b, dvec& c, bool debug);

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
  datarecord(short int cmt_, int evid_, double amt_, double time_, double rate_);
  
  ~datarecord();
  
  double time() {return Time;}
  void time(double time_){Time = time_;}
  
  double id() {return Id;}
  void id(double id_) {Id = id_;}
  
  unsigned int evid() {return Evid;}
  void evid(unsigned short int evid_){Evid=evid_;}
  
  int pos(){return Pos;}
  void pos(int pos_) {Pos=pos_;}
  
  short int cmt(){return Cmt;}
  
  void output(bool in){Output=in;}
  bool output(){return Output;}
  
  bool from_data(){return Fromdata;}
  void from_data(bool val){Fromdata = val;}
  
  double amt(){return Amt;}
  
  double rate(){return Rate;}
  void rate(double value) {Rate = value;}
  
  double dur(double b);
  
  void addl(unsigned int addl_){Addl = addl_;}
  unsigned int addl(){return Addl;}
  
  void ss(unsigned short int ss_){Ss = ss_;}
  unsigned short ss(){return Ss;}
  
  void ii(double ii_){Ii = ii_;}
  double ii(){return Ii;}
  
  void fn(double value){Fn = value;}
  double fn(){return Fn;}
  
  void schedule(std::vector<rec_ptr>& thisi, double maxtime, bool put_ev_first);
  void implement(odeproblem* prob);
  void steady_infusion(odeproblem* prob);
  void steady_bolus(odeproblem* prob);
  
  bool infusion(){return (Evid==1) && (Rate > 0);}
  bool is_event() {return Evid != 0 ;}
  bool needs_sorting(){return ((Addl > 0) || (Rate > 0) || (Ss == 1));}
  
  bool unarmed() {return !Armed;}
  void arm() {Armed=true;}
  void unarm() {Armed=false;}
  
  void phantom_rec() {Output=false; Fromdata=false;}
  
protected:
  
  double Time; ///< record time
  double Id; ///< record ID value
  int Pos; ///< record position number
  unsigned short int Evid; ///< record event ID
  bool Output; ///< should this record be included in output?
  bool Fromdata; ///< is this record from the original data set?
  short int Cmt; ///< record compartment number
  unsigned int Addl; ///< number of additional doses
  unsigned short int Ss; ///< record steady-state indicator
  double Amt; ///< record dosing amount value
  double Rate; ///< record infusion rate value
  double Ii; ///< record inter-dose interval value
  double Fn; ///< record bioavailability value
  bool Armed; ///< only armed records are actually executed
  
};


bool CompByTimePosRec(const rec_ptr& a, const rec_ptr& b);

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
  inline bool operator()(const rec_ptr& a, const rec_ptr& b) {
    if(a->time() == b->time()) {
      return a->pos() < b->pos();
    }
    return a->time() < b->time();
  }
};

#endif
