
class tadose {
public: 
  int cmt;
  double last_time;
  bool had_dose;
  tadose(int cmt_);
  tadose();
  double tad(int newind, double time,int evid, int cmt);
  double tad(databox self);
  void reset();
};

tadose::tadose(int cmt_) {
  cmt = cmt_;  
  last_time = -1e9;
}

tadose::tadose() {
  last_time = -1e9;  
}

double tadose::tad(int newind, double time, int evid, int cmt_) {
  if(newind <=1) this->reset();
  if((cmt_==cmt) && (evid == 1 || evid == 4)) {
    last_time = time;
    had_dose = true;
    return 0.0;
  }
  if(!had_dose) return -1.0;
  return time - last_time;
}

double tadose::tad(databox self) {
  return tad(self.newind, self.time, self.evid, self.cmt);  
}

void tadose::reset() {
  had_dose = false;
  last_time = -1e9;
}
