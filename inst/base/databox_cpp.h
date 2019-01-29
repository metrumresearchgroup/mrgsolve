
void databox::mevent(double time, int evid) {
  mrgsolve::evdata ev(time,evid);
  mevector.push_back(ev);
}

double databox::tad() {
  static double told = -1.;
  if(newind <=1) told = -1.0;
  if(evid==1|evid==4) told = time;
  return told < 0 ? -1.0 : time - told;
}

