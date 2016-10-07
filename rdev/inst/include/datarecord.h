// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
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
  //datarecord(int evid_, double time_, short int cmt_, int pos_, double id_);
  //datarecord(int evid_, double time_, short int cmt_);
  
  
  datarecord(double time_, int pos_, bool output_);   
  
  // Data set observations
  datarecord(double time_, short int cmt_, int pos_, double id_);
  
  // Data set event
  // Schedule events
  //   Need to make sure that scheduled events are output false, from data false
  datarecord(short int cmt_, int evid_, double amt_, double time_, double rate_,
                         int pos_, double id_);   
  
  // Short event
  // cmt evid amt time rate
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
  
private:
  double Time;
  double Id;
  int Pos;
  unsigned short int Evid;
  bool Output;
  bool Fromdata;
  short int Cmt;
  unsigned int Addl;
  unsigned short int Ss;
  double Amt;
  double Rate;
  double Ii;
  double Fn;
  bool Armed;
};


bool CompByTimePosRec(const rec_ptr& a, const rec_ptr& b);

struct CompRec {
  inline bool operator()(const rec_ptr& a, const rec_ptr& b) {
    if(a->time() == b->time())
      return a->pos() < b->pos();
    return a->time() < b->time();
  }
};


#endif




