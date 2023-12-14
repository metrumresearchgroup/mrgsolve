
namespace evt {

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

void push(databox& self, mrg::evdata x) {
  self.mevector.push_back(x);  
}

void amt(mrgsolve::evdata& x, double value) {
  x.amt = value;
  return;
}

void now(mrgsolve::evdata& x) {
  x.now = true;
  return;
}

mrgsolve::evdata bolus(const double amt, const int cmt) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  return ev;
}

void bolus(databox& self, const double amt, const int cmt) {
  mrgsolve::evdata ev = bolus(amt, cmt);
  self.mevector.push_back(ev);
  return;
}

mrgsolve::evdata infuse(const double amt, const double rate, const int cmt) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.rate = rate;
  return ev;
}

void infuse(databox& self, const double amt, const double rate, const int cmt) {
  mrgsolve::evdata ev = infuse(amt, rate, cmt);
  self.mevector.push_back(ev);
  return;
}

}
