#ifdef __MRGSOLVE_USE_PLUGIN_TAD__
namespace plugin {
double tad(const databox& self) {
  static double time;
  if(self.newind <=1) time = -1;
  if(self.evid==1) time = self.time;
  return time < 0 ? -1 : self.time - time;
} 
#define PLUGIN_TAD() (plugin::tad(self))
}
#endif
