
void databox::mevent(double time, int evid) {
  mrgsolve::evdata ev(time,evid);
  mevector.push_back(ev);
}

double databox::tad() {
  static double told = time;
  if(newind <=1) told = 0;
  if(evid==1) told = time;
  return told <= 0 ? 0 : time - told;
}

