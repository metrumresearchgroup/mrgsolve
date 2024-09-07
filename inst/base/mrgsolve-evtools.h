
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
  mrgsolve::evdata ev = evt::bolus(amt, cmt);
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
  mrgsolve::evdata ev = evt::infuse(amt, cmt, rate);
  self.mevector.push_back(ev);
  return;
}

mrgsolve::evdata replace(const double amt, const int cmt) {
  mrgsolve::evdata ev(0, 8); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.now = true;
  ev.check_unique = false;
  return ev;
}

void replace(databox& self, const double amt, const int cmt) {
  mrgsolve::evdata ev = evt::replace(amt, cmt);
  self.mevector.push_back(ev);
  return;
}

mrgsolve::evdata reset() {
  mrgsolve::evdata ev(0, 3); 
  ev.now = true;
  ev.check_unique = false;
  return ev;
}

mrgsolve::evdata reset(const double amt, const int cmt, 
                       const double rate = 0) {
  mrgsolve::evdata ev(0, 4);
  ev.amt = amt;
  ev.cmt = cmt;
  ev.rate = rate;
  ev.now = true;
  ev.check_unique = false;
  return ev;
}

void reset(databox& self) {
  mrgsolve::evdata ev = evt::reset(); 
  self.mevector.push_back(ev);
  return;
}

void reset(databox& self, const double amt, const int cmt, 
           const double rate = 0) {
  mrgsolve::evdata ev = evt::reset(amt, cmt, rate);
  self.mevector.push_back(ev);
  return;
}

void retime(mrgsolve::evdata& ev, const double time) {
  ev.time = time;
  ev.now = false;
  return;
}

void addl(mrgsolve::evdata& ev, const int addl) {
  ev.addl = addl;
  return;
}

void ii(mrgsolve::evdata& ev, const double ii) {
  ev.ii = ii;
  return;
}

void ss(mrgsolve::evdata& ev, const int ss) {
  ev.ss = ss;
  return;
}

void amt(mrgsolve::evdata& ev, const double amt) {
  ev.amt = amt; 
  return;
}

void rate(mrgsolve::evdata& ev, const double rate) {
  ev.rate = rate; 
  return;
}

void cmt(mrgsolve::evdata& ev, const int cmt) {
  ev.cmt = cmt;
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

bool near(double a, double b, double eps = 1e-8) {
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
