// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.
#ifndef DATAOBJECT_H
#define DATAOBJECT_H
#include <math.h>
#include <memory>
#include <iostream>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <string>
#include "odeproblem.h"
#include "RcppInclude.h"


typedef std::map<std::string, ivec> sivec_map;
typedef std::map<double, int> di_map;
typedef std::map<std::string, int> si_map;

class dataobject {

public:

  dataobject(Rcpp::NumericMatrix _data, svec _parnames);
  dataobject(Rcpp::NumericMatrix _data, svec _parnames, svec _initnames);

  virtual ~dataobject();

  unsigned int nrow() {return Data.nrow();}
  unsigned int ncol() {return Data.ncol();}
  unsigned int nid() {return Uid.size();}
  unsigned int idcol(){return Idcol;}
  int start(int i){return Startrow.at(i);}
  int end(int i){return Endrow.at(i);}
  void map_uid();
  double get_uid(int i) {return Uid.at(i);}
  dvec return_uid() {return Uid;}
  void copy_parameters(int this_row,odeproblem* prob);
  void copy_inits(int this_row,odeproblem* prob);
  void reload_parameters(Rcpp::NumericVector param, odeproblem* prob);
  void idata_row();
  unsigned int get_idata_row(double ID){return idmap[ID];}
  void locate_tran(bool lc_);
  void get_records(recstack& a, int NID, int neq, int& obscount, int& evcount, bool obsonly,bool debug);
  int col_n(std::string name) {return col.at(name);}
  void check_idcol(dataobject* data);
  double get_value(int row, int col) {return Data(row,col);}
  Rcpp::List ex_port();

 protected:

  dvec Uid;
  ivec Startrow;
  ivec Endrow;
  int Idcol;
  Rcpp::NumericMatrix Data;
  svec Data_names;

  si_map col;
  ivec par_from;  // data set index
  ivec par_to;    // parameter list index
  svec parnames;
  di_map idmap;

  ivec cmt_from; // data set index
  ivec cmt_to;  // cmt index
  svec cmtnames;


};


#endif


