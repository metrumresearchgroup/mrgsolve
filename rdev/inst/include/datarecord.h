// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#ifndef DATARECORD_H
#define DATARECORD_H
#include <boost/shared_ptr.hpp>


class odeproblem;
class datarecord;
typedef boost::shared_ptr<datarecord> rec_ptr;


class datarecord {
  friend class pkevent;
 public:
  datarecord(int evid_, double time_, short int cmt_, int pos_, double id_);
  datarecord(int evid_, double time_, short int cmt_);
  datarecord(const datarecord& other);
  virtual ~datarecord();
  datarecord &operator=(const datarecord& other);

  double time() {return Time;}
  void time(double time_){Time = time_;}
  double id() {return Id;}
  void id(double id_) {Id = id_;}
  unsigned int evid() {return Evid;}
  void evid(unsigned short int evid_){Evid=evid_;}
  int pos(){return Pos;}
  void pos(int pos_) {Pos=pos_;}

  virtual void implement(odeproblem*){}
  virtual void steady_bolus(odeproblem*){}
  virtual void steady_infusion(odeproblem*){};
  
  virtual bool infusion(){return false;}
  virtual double amt(){return 0.0;};
  virtual unsigned short int ss(){return 0;}
  virtual unsigned  int addl(){return 0;}
  virtual double ii(){return 0.0;}
  virtual double rate(){return 0.0;}

  short int cmt(){return Cmt;}

  virtual bool is_event() {return false;}
  virtual void fn(double){};

  void output(bool in){Output=in;}
  bool output(){return Output;}

  bool from_data(){return Fromdata;}
  void from_data(bool val){Fromdata = val;}

 private:
  double Time;
  double Id;
  int Pos;
  unsigned short int Evid;
  bool Output;
  bool Fromdata;
  short int Cmt;
};


bool CompByTimePosRec(rec_ptr& a, rec_ptr& b);

// struct Comparator {
//   bool operator()(rec_ptr& a, rec_ptr& b) {
//     // time-a != time-b
//     if (a->time() < b->time()) return true;
//     if (b->time() < a->time()) return false;
//     // time-a == time-b
//     if (a->pos() < b->pos()) return true;
//     if (b->pos() < b->pos()) return false;
//     return false;
//   }
// };

#endif




