
namespace evt {

typedef mrgsolve::evdata ev;

mrgsolve::evdata bolus(const double amt, const int cmt) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.check_unique = false;
  ev.now = true;
  return ev;
}

void bolus(databox& self, const double amt, const int cmt) {
  mrgsolve::evdata ev = bolus(amt, cmt);
  self.mevector.push_back(ev);
  return;
}

mrgsolve::evdata infuse(const double amt, const int cmt, const double rate) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.rate = rate;
  ev.now = true;
  ev.check_unique = false;
  return ev;
}

void infuse(databox& self, const double amt, const int cmt, const double rate) {
  mrgsolve::evdata ev = infuse(amt, cmt, rate);
  self.mevector.push_back(ev);
  return;
}

void retime(mrgsolve::evdata& ev, const double time) {
  ev.time = time;
  ev.now = false;
  return;
}

void now(mrgsolve::evdata& ev) {
  ev.now = true;
  return;
}

void push(databox& self, mrgsolve::evdata ev) {
  self.push(ev);
  return;
}

bool near(double a, double b, double eps = 1e-7) {
  bool ans = std::fabs(a-b) < eps;
  return ans;
}

}

namespace mrgsolve {
namespace evt {
void push(databox& self, mrg::evdata ev) {
  self.mevector.push_back(ev);  
}
}
}


namespace evt {

class regimen {
public:
  regimen();
  void init(databox& self);
  void reset();
  void dose();
  void cmt(int cmt_);
  int cmt() {return Cmt;}
  void amt(double amt_);
  double amt() {return Amt;}
  void ii(double ii_);
  double ii() {return Ii;}
  void rate(double rate_);
  double rate() {return Rate;}
  void dc(double dose_stop_);
  double dose_time;
  double prev_dose_time;
  double dose_stop;
  
private: 
  databox* Self;
  int Cmt;
  double Ii;
  double Amt; 
  double Rate;
};

void regimen::reset() {
  Cmt = 1;
  Amt = 0.0;
  Ii = 24.0;
  Rate = 0.0;
  dose_time = 0.0;
  dose_stop = 1.0e9;
  prev_dose_time = -1e9;
}

regimen::regimen() {
  reset();
}

void regimen::init(databox& self_) {
  reset();
  Self = &self_;
}

void regimen::amt(double amt_) {
  Amt = amt_;
  return;
}

void regimen::ii(double ii_) {
  Ii = ii_;
  if(prev_dose_time > -1e9) {
    dose_time = prev_dose_time + Ii;  
  }
  return;
}

void regimen::dose() {
  if(!evt::near(Self->time, dose_time)) return;
  if(Self->time > dose_stop) return;
  mrg::evdata ev = evt::bolus(Amt, Cmt);
  ev.rate = Rate;
  Self->push(ev);
  prev_dose_time = dose_time;
  dose_time = dose_time + Ii;
  return;
}

void regimen::dc(double dose_stop_) {
  dose_stop = dose_stop_;
  return;
}

}
