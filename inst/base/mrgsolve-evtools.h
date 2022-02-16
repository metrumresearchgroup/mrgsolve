
namespace mrgsolve {
  namespace evt {
    void push(databox& self, mrg::evdata ev) {
      self.mevector.push_back(ev);  
    }
    mrg::evdata ev(double time, int evid) {
      mrg::evdata obj(time, evid);
      return obj;
    }
    void bolus(databox& self, const double time, const double amt, 
               const int cmt, bool now = false) {
      mrg::evdata ev(time, 1); 
      ev.now = now;
      ev.cmt = cmt;
      ev.amt = amt;
      mrgsolve::evt::push(self, ev);
    }
    void infuse(databox& self, const double time, const double amt, 
                const double rate, const int cmt, bool now = false) {
      mrg::evdata ev(time, 1); 
      ev.now = now;
      ev.cmt = cmt;
      ev.amt = amt;
      ev.rate = rate;
      mrgsolve::evt::push(self, ev);
    }
    void replace(databox& self, const double time, const double amt, 
                 const int cmt, bool now = false) {
      mrg::evdata ev(time, 8); 
      ev.now = now; 
      ev.cmt = cmt; 
      ev.amt = amt;
      mrgsolve::evt::push(self, ev);
    }
  }
}
