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
 * @file datarecord.h
 */

#ifndef DATARECORD_H
#define DATARECORD_H
//#include <boost/shared_ptr.hpp>
#include "mrgsolv.h"
#include "LSODA.h"

class odeproblem;
class datarecord;
class eventrecord;

typedef std::shared_ptr<datarecord> rec_ptr;
typedef std::shared_ptr<eventrecord> evt_ptr;
typedef std::vector<rec_ptr> reclist;
typedef std::vector<evt_ptr> evtlist;

#define NEWOBS std::make_shared<datarecord>
#define NEWEVT std::make_shared<eventrecord>

class datarecord {
  
public:
  //! constructor
  datarecord(double time_, int pos_, bool output_);   

  //! constructor
  datarecord(double time_, short int cmt_, int pos_, double id_);

  //! constructor to handle eventrecord initialization
  datarecord(double time_, short int cmt_, unsigned short int evid_, int pos_, double id_);

  //! constructor to handle eventrecord initialization 
  datarecord(double time_, short int cmt_, unsigned short int evid_);

  double Time; ///< record time
  double Id; ///< record ID value
  int Pos; ///< record position number
  unsigned short int Evid; ///< record event ID
  bool Output; ///< should this record be included in output?
  bool Fromdata; ///< is this record from the original data set?
  bool Lagged; ///< this record was added as result of ALAG
  short int Cmt; ///< record compartment number
  
  virtual ~datarecord() = default;
  
  double time() {return Time;}
  void time(double time_){Time = time_;}
  
  double id() {return Id;}
  void id(double id_) {Id = id_;}
  
  unsigned int evid() {return Evid;}
  void evid(unsigned short int evid_){Evid=evid_;}
  
  int pos(){return Pos;}
  void pos(int pos_) {Pos=pos_;}
  
  short int cmt(){return Cmt;}
  short int cmtn(){return std::abs(Cmt)-1;}
  
  void output(bool in){Output=in;}
  bool output(){return Output;}
  
  bool from_data(){return Fromdata;}
  void from_data(bool val){Fromdata = val;}

  bool is_event() {return (Evid > 0);}
  bool is_dose(){return (Evid==1) || (Evid==4);}
  bool is_event_data() {return (Evid != 0) && (Evid != 2) && Fromdata;}
  
  void phantom_rec() {Output=false; Fromdata=false;}
  bool is_phantom() {return !Output && !Fromdata;}
  bool is_lagged() {return Lagged;}
  void lagged() {Lagged = true;}
  

  // Virtual 
  virtual bool unarmed() {return true;}
  virtual void arm() {}
  virtual bool armed() {return false;}
  virtual void unarm() {}
  
  virtual double rate(){return 0.0;}
  virtual void rate(double value){}
  
  virtual double dur(double b);
  
  virtual void addl(int addl_){}
  virtual unsigned int addl(){return 1;}
  
  virtual void ss(int ss_){}
  virtual unsigned short ss(){return 0;}
  
  virtual void ii(double ii_){}
  virtual double ii(){return 0.0;}
  
  virtual double amt(){return 0.0;}
  
  virtual bool needs_sorting(){return false;}
  
  virtual void schedule(std::vector<rec_ptr>& thisi, double maxtime, bool put_ev_first, 
                const unsigned int maxpos, double lagt){};
  virtual void implement(odeproblem* prob){};
  virtual void steady_zero(odeproblem* prob, LSODA& solver){};
  virtual void steady_infusion(odeproblem* prob,reclist& thisi,LSODA& solver){};
  virtual void steady_bolus(odeproblem* prob,LSODA& solver){};
  virtual void steady(odeproblem* prob, reclist& thisi,LSODA& solver){};
  
  virtual bool infusion(){return false;}
  virtual bool int_infusion(){return false;}
  virtual bool ss_int_infusion(){return false;}
  virtual bool const_infusion(){return false;}
  virtual bool ss_infusion(){return false;}
};

class eventrecord : public virtual datarecord {
  
public:
  
  //! constructor
  eventrecord(short int cmt_, int evid_, double amt_, double time_, 
               double rate_, int pos_, double id_);
  
  //! short event constructor
  eventrecord(short int cmt_, int evid_, double amt_, double time_, 
               double rate_);
  
  virtual ~eventrecord() = default;

  double Fn;
  unsigned int Addl; ///< number of additional doses
  unsigned short int Ss; ///< record steady-state indicator
  double Amt; ///< record dosing amount value
  double Rate; ///< record infusion rate value
  double Ii; ///< record inter-dose interval value
  bool Armed; ///< only armed records are actually executed
  
  bool unarmed() {return !Armed;}
   void arm() {Armed=true;}
   bool armed() {return Armed;}
   void unarm() {Armed=false;}
  
   double rate(){return Rate;}
   void rate(double value) {Rate = value;}
  
   double dur(double b);
  
   void addl(int addl_){Addl = std::max(0,addl_);}
   unsigned int addl(){return Addl;}
  
   void ss(int ss_){Ss = std::max(0,ss_);}
   unsigned short ss(){return Ss;}
  
   void ii(double ii_){Ii = ii_;}
   double ii(){return Ii;}
  
   double amt(){return Amt;}
  
   bool needs_sorting(){return ((Addl > 0) || (Ss == 1));}
  
   void schedule(std::vector<rec_ptr>& thisi, double maxtime, bool put_ev_first, 
                const unsigned int maxpos, double lagt);
   void implement(odeproblem* prob);
   void steady_zero(odeproblem* prob, LSODA& solver);
   void steady_infusion(odeproblem* prob,reclist& thisi,LSODA& solver);
   void steady_bolus(odeproblem* prob,LSODA& solver);
   void steady(odeproblem* prob, reclist& thisi,LSODA& solver);
  
   bool infusion(){return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0);}
   bool int_infusion(){return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0) && (Amt > 0);}
   bool ss_int_infusion(){return (Evid==1 || Evid==4 || Evid==5) && (Rate > 0) && (Amt > 0) && (Ss > 0);}
   bool const_infusion(){return (Evid==1) && (Rate > 0) && (Amt == 0);}
   bool ss_infusion();
};


bool CompByTimePosRec(const rec_ptr& a, const rec_ptr& b);
bool CompEqual(const reclist& a, double time, unsigned int evid, int cmt);

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
