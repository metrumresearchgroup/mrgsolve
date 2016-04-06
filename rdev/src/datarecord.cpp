// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include "RcppInclude.h"
#include "datarecord.h"

datarecord::datarecord(int evid_, double time_, short int cmt_, int pos_, double id_) {
  Id = id_;
  Time = time_;
  Pos = pos_;
  Evid = evid_;
  Report=0;
  Output=true;
  Fromdata=false;
  Cmt = cmt_;
}
datarecord::datarecord(int evid_, double time_, short int cmt_) {
  Id =  0.0;
  Pos = -1;
  Time = time_;
  Evid = evid_;
  Report=0;
  Output=true;
  Fromdata=false;
  Cmt = cmt_;
}
datarecord::datarecord(int evid_, double time_, short int cmt_, int pos_) {
  Id =  0.0;
  Pos = pos_;
  Time = time_;
  Evid = evid_;
  Report=0;
  Output=true;
  Fromdata = false;
  Cmt = cmt_;
}


datarecord::~datarecord() {}

bool CompByTimeRec(rec_ptr a, rec_ptr b) {return a->time() < b->time();}

bool CompByTimePosRec(rec_ptr a, rec_ptr b)
{
  // time-a != time-b
  if (a->time() < b->time()) return true;
  if (b->time() < a->time()) return false;
  // time-a == time-b
  if (a->pos() < b->pos()) return true;
  if (b->pos() < b->pos()) return false;
  return false;
}


