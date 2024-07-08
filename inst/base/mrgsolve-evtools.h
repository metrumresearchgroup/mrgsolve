
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

mrgsolve::evdata replace(const double amt, const int cmt) {
  mrgsolve::evdata ev(0, 8); 
  ev.amt = amt; 
  ev.cmt = cmt;
  ev.now = true;
  ev.check_unique = false;
  return ev;
}

void replace(databox& self, const double amt, const int cmt) {
  mrgsolve::evdata ev = replace(amt, cmt);
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
