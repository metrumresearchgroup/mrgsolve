
// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#include "RcppInclude.h"
#include "dataobject.h"
#include "mrgsolve.h"
#include "pkevent.h"
#include <numeric>
#include "mrgsolv.h"


dataobject::dataobject(Rcpp::NumericMatrix _data, svec _parnames) {
  Data = _data;
  parnames = _parnames;

  Rcpp::List dimnames = Data.attr("dimnames");
  Data_names = Rcpp::as<svec>(dimnames[1]);

  Idcol = find_position("ID", Data_names);
  if(Idcol < 0) Rcpp::stop("Could not find ID column in data set.");

  // Connect Names in the data set with positions in the parameter list
  match_both(Data_names, parnames, par_from, par_to);

}

dataobject::dataobject(Rcpp::NumericMatrix _data, svec _parnames,svec _cmtnames) {
  Data = _data;
  parnames = _parnames;
  cmtnames = _cmtnames;

  Rcpp::List dimnames = Data.attr("dimnames");
  Data_names = Rcpp::as<svec>(dimnames[1]);

  Idcol = find_position("ID", Data_names);
  if(Idcol < 0) Rcpp::stop("Could not find ID column in data set.");

  // Connect Names in the data set with positions in the parameter list
  match_both(Data_names, parnames, par_from, par_to);
  match_both(Data_names, cmtnames, cmt_from,cmt_to);
}




dataobject::~dataobject(){}


void dataobject::map_uid() {

  int i=0;
  Uid.push_back(Data(0,Idcol));
  Startrow.push_back(0);
  for(i=1; i < Data.nrow(); i++) {
    if(Data(i,Idcol) != Data(i-1, Idcol)) {
      Uid.push_back(Data(i,Idcol));
      Startrow.push_back(i);
      Endrow.push_back(i-1);
    }
  }
  Endrow.push_back(Data.nrow()-1);
}

void dataobject::locate_tran(bool lc) {

  int zeros = Data.ncol()-1;


  svec::const_iterator bg = Data_names.begin();
  svec::const_iterator ed = Data_names.end();

  if(lc) {
    col["amt"]  = std::find(bg,ed,"amt")  - bg;
    col["ii"]   = std::find(bg,ed,"ii")   - bg;
    col["cmt"]  = std::find(bg,ed,"cmt")  - bg;
    col["addl"] = std::find(bg,ed,"addl") - bg;
    col["ss"]   = std::find(bg,ed,"ss")   - bg;
    col["rate"] = std::find(bg,ed,"rate") - bg;
    col["evid"] = std::find(bg,ed,"evid") - bg;
    col["time"] = std::find(bg,ed,"time") - bg;
    if(col["cmt"] > zeros  && zeros > 0)
      Rcpp::stop("Couldn't locate cmt in data set.");
    if(col["time"] > zeros && zeros > 0)
      Rcpp::stop("Couldn't locate time in data set.");
  } else {
    col["amt"]  = std::find(bg,ed, "AMT") - bg;
    col["ii"]   = std::find(bg,ed,"II")   - bg;
    col["cmt"]  = std::find(bg,ed, "CMT") - bg;
    col["addl"] = std::find(bg,ed,"ADDL") - bg;
    col["ss"]   = std::find(bg,ed,"SS")   - bg;
    col["rate"] = std::find(bg,ed,"RATE") - bg;
    col["evid"] = std::find(bg,ed,"EVID") - bg;
    col["time"] = std::find(bg,ed,"TIME") - bg;
    if(col["cmt"]  > zeros && zeros > 0)
      Rcpp::stop("Couldn't locate CMT in data set.");
    if(col["time"] > zeros && zeros > 0)
      Rcpp::stop("Couldn't locate TIME in data set.");
  }

  if(col["amt"]  > zeros) col["amt"]  = zeros;
  if(col["ii"]   > zeros) col["ii"]   = zeros;
  if(col["addl"] > zeros) col["addl"] = zeros;
  if(col["ss"]   > zeros) col["ss"]   = zeros;
  if(col["rate"] > zeros) col["rate"] = zeros;
  if(col["evid"] > zeros) col["evid"] = zeros;


}



void dataobject::idata_row() {
  int i=0;
  for(i=0; i < Data.nrow(); i++) {
    idmap[Data(i,Idcol)] =i;
  }
}


void dataobject::copy_parameters(int this_row, odeproblem* prob) {
  size_t i;
  for(i=0; i < par_from.size(); i++) {
    prob->param(par_to.at(i),Data(this_row,par_from.at(i)));
  }
}

void dataobject::copy_inits(int this_row, odeproblem* prob) {
  // this should only be done from idata sets
  size_t i;
  for(i=0; i < cmt_from.size(); i++) {
    prob->y_init(cmt_to.at(i),Data(this_row,cmt_from.at(i)));
  }
}


void dataobject::reload_parameters(Rcpp::NumericVector PARAM, odeproblem* prob) {
  for(size_t i = 0; i < par_to.size(); i++) {
    prob->param(par_to[i],PARAM[par_to[i]]);
  }
}



void dataobject::get_records(recstack& a, int NID, int neq, int& obscount, int& evcount, bool obsonly, bool debug) {

  // only look here for events or observations if there is more than one column:
  size_t h=0;
  int j=0, k=0;
  int evid, this_cmt;
  double lastime = 0;
  if(debug) Rcpp::Rcout << "Generating record set ... " << std::endl;

  if(Data.ncol() <= 1) return;

  for(h=0; h < NID; h++) {

    lastime = 0;

    for(j=this->start(h); j <= this->end(h); j++) {

      if(Data(j,col["time"]) < lastime) Rcpp::stop("Problem with time: data set is not sorted by time or time is negative.");
      lastime = Data(j,col["time"]);

      this_cmt = Data(j,col["cmt"]);
      evid = Data(j,col["evid"]);
      k = j - this->start(h);

      // If this is an observation record
      if(evid==0) {
	if((this_cmt < 0) || (this_cmt > neq)) {
	  Rcpp::stop("cmt number in observation record out of range.");
	}
	rec_ptr obs(new datarecord(0,Data(j,col["time"]),Data(j,col["cmt"]),j,Data(j,Idcol)));
	obs->from_data(true);
	a.at(h).at(k) = obs;
	++obscount;
	continue;
      }

      // If this is not an obsrevation record:

      // Check that cmt is valid:
      if((this_cmt==0) || (abs(this_cmt) > neq)) {
	Rcpp::stop("cmt number in dosing record out of range.");
      }

      ++evcount;

      ev_ptr ev(new pkevent(Data(j,col["cmt"]),
			    Data(j,col["evid"]),
			    Data(j,col["amt"]),
			    Data(j,col["time"]),
			    Data(j,col["rate"])));

      if((ev->rate() < 0) && (ev ->rate() != -1) && (ev->rate() !=-2)) Rcpp::stop("rate must be positive or equal to -1 or -2");

      if(obsonly) ev->output(false);

      ev->from_data(true);
      ev->evid(evid);
      ev->pos(j);
      ev->report(0);
      ev->ss(Data(j,col["ss"]));
      ev->addl(Data(j,col["addl"]));
      ev->ii(Data(j,col["ii"]));
      ev->id(Data(j,Idcol));

      if((ev->addl() > 0) && (ev->ii() <=0)) {
	Rcpp::stop("found dosing record with addl > 0 and ii <= 0.");
      }
      if((ev->ss()) && (ev->ii() <=0)) {
	Rcpp::stop("found dosing record with ss==1 and ii <= 0.");
      }
      a.at(h).at(k) = ev;
    }
  }
}



void dataobject::check_idcol(dataobject* data) {

  if(data->ncol() == 0) {return;}

  dvec udata;
  int i;
  int ndata  = data->nrow();
  int idcol = data->idcol();

  udata.resize(ndata);

  for(i=0; i < ndata; i++) udata[i] = data->get_value(i,idcol);

  dvec uthis  = this->return_uid();

  sort_unique(uthis);
  sort_unique(udata);

  dvec inter;
  std::set_intersection(uthis.begin(), uthis.end(),
			udata.begin(), udata.end(),
			std::back_inserter(inter));

  if(inter!=uthis) Rcpp::stop("ID found in the data set, but not in idata.");

}


Rcpp::List dataobject::ex_port() {

  Rcpp::List ret;

  ret["Startrow"]  = Startrow;
  ret["Endrow"] =   Endrow;


  ret["Uid"] = Uid;
  ret["idmap"] = idmap;
  ret["par_from"] = par_from;
  ret["par_to"]  = par_to;
  ret["Idcol"] = Idcol;
  ret["parnames"] = parnames;
  ret["tran_cols"] = col;
  ret["Data_names"]  = Data_names;
  return(ret);

}


