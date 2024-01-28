namespace evt {

class regimen {
public:
  // Constructor
  regimen();
  // Methods
  void init(databox& self);
  void reset();
  void execute();
  // Set and get
  void cmt(int cmt_);
  int cmt() {return Cmt;}
  void amt(double amt_);
  double amt() {return Amt;}
  void ii(double ii_);
  double ii() {return Ii;}
  void rate(double rate_);
  double rate() {return Rate;}
  void until(double until_);
  double until() {return Until;}
  void flag_next();
  // Public members
  double dose_time;
  double prev_dose_time;
  bool flagnext;
  
private:
  int Cmt;
  double Ii;
  double Amt; 
  double Rate;
  double Until;
  databox* Self;
};

void regimen::reset() {
  Cmt = 1;
  Amt = 0.0;
  Rate = 0.0;
  Ii = 1e9;
  Until = 1.0e9;
  dose_time = 0.0;
  prev_dose_time = -1e9;
  flagnext = false;
}

regimen::regimen() {
  reset();
}

void regimen::init(databox& self_) {
  Self = &self_;
  reset();
}

void regimen::amt(double amt_) {
  Amt = amt_;
  return;
}

void regimen::ii(double ii_) {
  Ii = ii_;
  if(prev_dose_time > -1e9) {
    dose_time = prev_dose_time + Ii;  
  }
  return;
}

void regimen::until(double until_) {
  Until = until_;
  return;
}

void regimen::execute() {
  if(!evt::near(Self->time, dose_time)) return;
  if(Self->time >= Until) return;
  evt::infuse(*Self, Amt, Cmt, Rate);
  prev_dose_time = dose_time;
  dose_time = dose_time + Ii;
  if(flagnext) {
    mrg::evdata ev(dose_time, 3333);
    ev.check_unique = true;
    Self->push(ev);
  }
  return;
}

void regimen::flag_next() {
  flagnext = true;
  return;
}

} // Closes regimen class
