
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

mrgsolve::evdata new_event() {
  mrgsolve::evdata x(0.0, 1);
  return x;
}; 

void amt(mrgsolve::evdata& x, double value) {
  x.amt = value;
  return;
}

void now(mrgsolve::evdata& x) {
  x.now = true;
  return;
}

void bolus(databox& self, double amt, int cmt) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  self.mevector.push_back(ev);
  return;
}

void bolus(databox& self, double amt, int cmt, regimen& reg) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  self.mevector.push_back(ev);
  return;
}


void infuse(databox& self, double amt, double rate, int cmt) {
  mrgsolve::evdata ev(0, 1); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.rate = rate;
  self.mevector.push_back(ev);
  return;
}

}
