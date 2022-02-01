
namespace mrgsolve {
  namespace evtools {
    void dose(databox& self, const double amt, const int cmt, 
              bool now = false) {
      mrg::evdata ev(self.time, 1); 
      ev.now = now;
      ev.cmt = cmt;
      ev.amt = amt;
      self.mevector.push_back(ev);
    }
    void replace(databox& self, const double amt, const int cmt, 
                 bool now = false) {
      mrg::evdata ev(self.time, 8); 
      ev.now = now; 
      ev.cmt = cmt; 
      ev.amt = amt;
      self.mevector.push_back(ev);
    }
  }
}
