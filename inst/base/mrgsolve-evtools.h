
namespace evt {

typedef mrgsolve::evdata ev;

struct regimen {
  double ii;
  int addl;
  regimen(double ii_, int addl_);
};

regimen::regimen(double ii_, int addl_) {
  ii = ii_; 
  addl = addl_;
  return;
}

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

}
