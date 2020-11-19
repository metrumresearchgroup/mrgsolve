

namespace mrgsolve {
/** 
 * Caculate time after dose for a specific dosing compartment. 
 * 
 * NOTE: this is in the `mrgsolve` namespace, so use `mrg::tadose` to construct
 * these objects. You can include the header file or use the `tad` plugin (see
 * examples below).
 * 
 * First, create a `tadose` object and assign a compartment number. The 
 * compartment number can be assigned in the constructor or by assigning to the 
 * `cmt` member. Calling the `tad()` method will calculate time after dose 
 * for the designated compartment. The value `-1` is returned for records that 
 * occur prior to the first administered dose.
 * 
 * There are a couple of approaches.  
 * 
 * First, you could create static objects in `[main]`. This code would give 
 * you a global variable called `time_after_dose` and also includes the result
 * in the simulated output.
 * 
 * @code
 * [ plugin ] tad
 * 
 * [ main ] 
 * static mrg::tadose obj(1); 
 * 
 * capture time_after_dose = obj.tad(self);
 * @endcode 
 * 
 * The other way would be to declare the object globally in `[global]`.  For 
 * example:
 * 
 * @code
 * [ plugin ] tad
 * 
 * [ global ] 
 * mrg::tadose obj;
 * 
 * [ preamble ] 
 * obj.cmt = 2;
 * 
 * [ main ] 
 * capture time_after_dose = obj.tad(self);
 * @endcode
 * 
 */
class tadose {
public: 
  int cmt = -1e9; ///< dosing compartment number to follow 
  double told = -1e9; ///< time of last dose
  bool had_dose = false; ///< has the current individual received a dose yet?
  tadose(int cmt_);
  tadose(){};
  double tad(databox& self);
  void reset();
};

/**
 * Constructor.
 * 
 * @param cmt_ compartment number
 * 
 */
tadose::tadose(int cmt_) {
  cmt = cmt_;
}

/**
 * Calculate time after dose.
 * 
 * When `self.newind <= 1`, the `reset()` method is also called.  
 * 
 * @param self the model `databox` object
 * 
 * @return the time after dose or `-1` if no dose has been given
 * 
 */
double tadose::tad(databox& self) {
  if(self.newind <= 1) this->reset();
  if(self.evid==0) {
    return had_dose ? self.time - told : -1.0;
  }
  if((self.cmt == cmt) && (self.evid == 1 || self.evid == 4)) {
    told = self.time;
    had_dose = true;
    return 0.0;
  }
  return had_dose ? self.time - told : -1.0;
}
/**
 * Reset the `tadose` object.
 * 
 * This sets `had_dose` to `false` and resets the time of last dose `told`.
 * 
 */
void tadose::reset() {
  had_dose = false;
  told = -1e9;
}
}
