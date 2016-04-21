// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include "pkevent.h"
#include "RcppInclude.h"
#include "odeproblem.h"
#include "mrgsolve.h"

#define N_SS 2000
#define CRIT_DIFF_SS 1E-10



pkevent::pkevent(short int cmt_,
		 unsigned int evid_,
		 double amt_,
		 double time_): datarecord(evid_, time_, cmt_) {
  Amt = amt_;
  Rate = 0.0;
  Ss = 0;
  Addl = 0;
  Ii = 0.0;
  Fn = 1.0;
  Armed = true;
}

pkevent::pkevent(short int cmt_,
		 unsigned int evid_,
		 double amt_,
		 double time_,
		 int opos_,
		 double id_): datarecord(evid_, time_, cmt_, opos_, id_) {

  Amt = amt_;
  Rate = 0.0;
  Ss = 0;
  Addl = 0;
  Ii = 0.0;
  Fn = 1.0;
  Armed = true;
}

pkevent::pkevent(short int cmt_,
		 unsigned int evid_,
		 double amt_,
		 double time_,
		 double rate_): datarecord(evid_, time_, cmt_) {

  Amt = amt_;
  Rate = rate_;
  Ss = 0;
  Addl = 0;
  Ii = 0.0;
  Fn = 1.0;
  Armed = true;
}

pkevent::pkevent(short int cmt_,
		 unsigned int evid_,
		 double amt_,
		 double time_,
		 double rate_,
		 int opos_,
		 double id_):datarecord(evid_, time_, cmt_, opos_, id_){
  Amt = amt_;
  Rate = rate_;
  Ss=0;
  Addl = 0;
  Ii = 0.0;
  Fn = 1.0;
  Armed = true;
}


double pkevent::dur(double b) {
  return digits(b*this->amt()/this->rate(),1000000.0);
}

void pkevent::implement(odeproblem * prob) {

  if(this->unarmed()) return;

  unsigned int evid = this->evid();

  if(this->infusion()) evid = 5;

  int eq_n = std::abs(this->cmt())-1;

  // Check for steady state records:
  if(this->ss()==1) {
    if(this->fn()==0) Rcpp::stop("Cannot use ss flag when F(n) is zero.");
    if((this->rate() == 0)) this->steady_bolus(prob);
    if((this->rate() >  0)) this->steady_infusion(prob);
  }

  switch (evid) {
  case 1: // Dosing event record
    if(!prob->is_on(eq_n)) Rcpp::stop("Attemped bolus dose into a compartment that is off");
    prob->fbio(eq_n, this->fn());
    prob->y(eq_n,(prob->y(eq_n)  + this->amt() * this->fn()));
    break;
  case 5:  // Turn infusion on event record
    if(!prob->is_on(eq_n)) Rcpp::stop("Attemped infusion start for a compartment that is off");
    if(this->fn()==0) break;
    prob->fbio(eq_n, this->fn());
    prob->rate_add(eq_n,this->rate());
    break;
  case 9: // Turn infusion off event record
    if(!prob->is_on(eq_n)) break;
    prob->rate_rm(eq_n, this->rate());
    break;
  case 2: // Other type event record:
    if(this->cmt() > 0) { // trn the compartment on
      prob->on(eq_n);
    }
    if(this->cmt() < 0) {
      prob->off(eq_n);
      prob->y(eq_n,0.0);
    }
    break;
  case 3: // reset event record
    for(int i=0; i < prob->neq(); i++) {
      prob->y(i,0.0);
      prob->on(i);
      prob->rate0(i,0.0);

    }
    {
      double tm = this->time();
      prob->newind(1);
      prob->init_call(tm);
    }
    break;
  case 8: // replace
    prob->y(eq_n, this->amt());
    break;
  case 4:
    for(int i=0; i < prob->neq(); i++) {
      prob->y(i,0.0);
      prob->on(i);
      prob->rate0(i,0.0);
    } {
      double tm = this->time();
      //prob->newind(2);
      prob->init_call(tm);
      if(this->rate() > 0) {
	this->evid(5);
      } else {
	this->evid(1);
      }
      this-> implement(prob);
      return;
    }

  case 11:
    prob->y(eq_n,(prob->y(eq_n)  + prob->xdose()));
    break;
  }
  prob->lsoda_init();
}

void pkevent::steady_bolus(odeproblem* prob) {

  prob->rate_reset();

  double ii = this->ii();
  double tfrom = 0.0;
  double tto = 0.0;
  int i;
  size_t j;
  std::vector<double> res(prob->neq(), 1E-9);
  std::vector<double> last(prob->neq(),1E-9);

  double this_sum = 0.0;
  double last_sum = 1E-6;
  double diff = 1E6;

  prob->lsoda_init();

  ev_ptr evon(new pkevent(this->cmt(), 1, this->amt(), this->time(), this->rate()));
  evon->fn(this->fn());

  for(i=1; i < N_SS; i++) {
    tfrom = double(i-1)*ii;
    tto = double(i)*ii;
    evon->implement(prob);
    prob->lsoda_init();
    prob->advance(tfrom,tto);

    for(j=0; j < prob->neq(); j++) {
      res[j]  = pow(prob->y(j) - last[j],2);
      last[j] = prob->y(j);
    }

    this_sum = std::accumulate(res.begin(), res.end(), 0.0);

    if(i>10) {
      diff = std::abs(this_sum - last_sum);

      if((diff < CRIT_DIFF_SS)){
    	break;
      }
    }

    tfrom = tto;
    last_sum = this_sum;
  }
  prob->itask(1);
}


void pkevent::steady_infusion(odeproblem * prob) {


  double ii = this->ii();

  double duration = this->dur(this->fn());//(this->fn()*this->amt())/(this->rate());

  double tfrom = 0.0;

  int i;
  size_t j;
  std::vector<double> res(prob->neq(), 0.0);
  std::vector<double> last(prob->neq(),1E-10);
  evlist offs;
  double this_sum = 0.0;
  double last_sum = 1E-6;

  double diff = 1E6;
  double nexti, toff;
  prob->rate_reset();
  // We only need one of these; it gets updated and re-used immediately
  ev_ptr evon(new pkevent(this->cmt(), 1, this->amt(), this->time(), this->rate()));

  for(i=1; i < N_SS ; i++) {
    evon->time(tfrom);
    evon->implement(prob);
    prob->lsoda_init();
    toff = tfrom + duration;

    // Create an event to turn the infusion off and push onto offs vector
    // Keep on creating these
    ev_ptr evoff(new pkevent(this->cmt(),9,this->amt(),toff, this->rate()));
    offs.push_back(evoff);

    // The next time an infusion will start
    nexti = double(i)*ii;
    // As long as there are infusions to turn off and the
    // first one is before or at the next infusion start time
    while((!offs.empty()) && (offs[0]->time()  <= nexti)) {

      toff = offs[0]->time();
      prob->advance(tfrom,toff);
      offs[0]->implement(prob);
      prob->lsoda_init();
      tfrom = toff;
      offs.erase(offs.begin());
    }

    prob->lsoda_init();
    prob->advance(tfrom,nexti);

    tfrom = nexti;

    for(j=0; j < prob->neq(); j++) {
      res[j]  = pow((prob->y(j)  - last[j]),2);
      last[j] = prob->y(j);
    }

    this_sum = std::accumulate(res.begin(), res.end(), 0.0);

    if(i>10) {
      diff = std::abs(this_sum - last_sum);
      if(diff < CRIT_DIFF_SS) {
	break;
      }
    }
    last_sum = this_sum;
  }
  prob->lsoda_init();
}



bool CompByTime(ev_ptr a, ev_ptr b) {return a->time() < b->time();}
bool CompByPos(ev_ptr a, ev_ptr b)  {return a->pos() < b->pos();  }


void pkevent::schedule(std::vector<rec_ptr>& thisi, double& maxtime, bool put_ev_first) {

  int nextpos = put_ev_first ? -600 : (thisi.size() + 10);

  double biofrac = this->fn();

  if(biofrac==0) return;

  if(this->addl() > 0) {

    unsigned int this_evid = this -> evid();

    if(this_evid == 4) {
      this_evid = this->rate() > 0 ? 5 : 1;
    }

    for(unsigned int k=1; k <= this->addl(); k++) {

      double ontime = this->time() + this->ii()*double(k);
      if(ontime > maxtime) break;
      ev_ptr evon(new pkevent(this->cmt(),
			      this_evid,
			      this->amt(),
			      ontime,
			      this->rate()));

      evon->id(this->id());
      evon->pos(nextpos);
      evon->fn(biofrac);
      evon->output(false);

      thisi.push_back(evon);
      if(this->infusion()) {
	ev_ptr evoff(new pkevent(this->cmt(),
				 9, // EVID 9 means infusion off
				 this->amt(),
				 ontime + this->dur(biofrac),//biofrac*this->amt()/this->rate(),
				 this->rate()));

	evoff->id(this->id());
	evoff->pos(-300);
	evoff->output(false);

	thisi.push_back(evoff);
      }
    }
  }

  if(this->rate() > 0) {

    ev_ptr evoff(new pkevent(this->cmt(),
			     9, // EVID 9 means infusion off
			     this->amt(),
			     this->time() + this->dur(biofrac),//biofrac*this->amt()/this->rate(),
			     this->rate()));
    evoff->id(this->id());
    evoff->pos(-300);
    evoff->output(false);
    thisi.push_back(evoff);

    if(this->ss()) {

      double duration = this->dur(biofrac);//biofrac*this->amt()/this->rate();

      int ninf_ss = floor(duration/this->ii());

      double first_off = duration - double(ninf_ss)*this->ii() + this->time();

      if(first_off == this->time()) {
	first_off = duration - this->ii() +  this->time();
	--ninf_ss;
      }

      for(int k=0; k < ninf_ss; k++) {

	double offtime =  (first_off+double(k)*double(this->ii()));

	ev_ptr evoff(new pkevent(this->cmt(),
				 9, // EVID 9 means infusion off
				 this->amt(),
				 offtime,
				 this->rate()));

	evoff->id(this->id());
	evoff->pos(-300);
	evoff->output(false);
	thisi.push_back(evoff);

      } // Done creating off infusions for end of steady_infusion
    } // end if(ss)
  } // end rate>0
}


