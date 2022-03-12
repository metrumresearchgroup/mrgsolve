
namespace mrgsolve {
  namespace evt {
    void push(databox& self, mrg::evdata ev) {
      self.mevector.push_back(ev);  
    }
}
