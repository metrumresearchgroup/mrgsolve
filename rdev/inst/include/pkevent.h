// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#ifndef PKEVENT_H
#define PKEVENT_H

#include <boost/shared_ptr.hpp>
#include <math.h>
#include <iostream>
#include <vector>
#include "datarecord.h"
#include "mrgsolv.h"

class pkevent;
class odeproblem;

typedef boost::shared_ptr<pkevent> ev_ptr;
typedef std::vector<ev_ptr> evlist;
typedef std::vector<evlist> evstack;
typedef std::vector<rec_ptr> reclist;
typedef std::vector<reclist> recstack;

void add_mtime(reclist& thisi, dvec& b, dvec& c, bool debug);


/**
   @file pkevent.h
   @brief Header file for pkevent class.
*/


class pkevent : public datarecord {

 public:
  pkevent(short int cmt_,
	  unsigned int evid_,
	  double amt_,
	  double time_,
	  double rate_);

  pkevent(short int cmt_,
	  unsigned int evid_,
	  double amt_,
	  double time_);

  pkevent(short int cmt_,
	  unsigned int evid_,
	  double amt_,
	  double time_,
	  int opos_,
	  double id_);

  pkevent(short int cmt_,
	  unsigned int evid_,
	  double amt_,
	  double time_,
	  double rate_,
	  int opos_,
	  double id_);


  //int evid(){return Evid;}
  //void evid(int evid_){Evid=evid_;}
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

  void schedule_addl(std::vector<rec_ptr>& thisi, double& maxtime, double& biofrac);
  void schedule_infusion(std::vector<rec_ptr>& thisi, double& maxtime, double& biofrac);
  void schedule(std::vector<rec_ptr>& thisi, double& maxtime, bool put_ev_first);
  virtual void implement(odeproblem* prob);
  virtual void steady_infusion(odeproblem* prob);
  virtual void steady_bolus(odeproblem* prob);

  bool infusion(){return (Evid==1) && (Rate > 0);}
  bool is_event() {return true;}
  bool needs_sorting(){return ((Addl > 0) || (Rate > 0) || (Ss == 1));}

  bool unarmed() {return !Armed;}
  void arm() {Armed=true;}
  void unarm() {Armed=false;}


 private:
  unsigned int Addl;
  unsigned short int Ss;
  double Amt;
  double Rate;
  double Ii;
  double Fn;
  bool Armed;

};




bool CompByTime(ev_ptr a, ev_ptr b);
bool CompByoPos(ev_ptr a, ev_ptr b);
bool CompByPos(ev_ptr a, ev_ptr b);




#endif
