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
#include "pkevent.h"

#define _COL_amt_   0u
#define _COL_ii_    1u
#define _COL_addl_  2u
#define _COL_ss_    3u
#define _COL_rate_  4u
#define _COL_evid_  5u
#define _COL_cmt_   6u
#define _COL_time_  7u


typedef std::map<std::string, ivec> sivec_map;
typedef std::map<double, int> di_map;
typedef std::map<std::string, int> si_map;

class dataobject {

public:

  dataobject(Rcpp::NumericMatrix _data, 
             Rcpp::CharacterVector _parnames);
  
  dataobject(Rcpp::NumericMatrix _data, 
             Rcpp::CharacterVector _parnames, 
             Rcpp::CharacterVector _initnames);

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
  void copy_parameters(int this_row,odeproblem *prob);
  void copy_inits(int this_row,odeproblem *prob);
  void reload_parameters(Rcpp::NumericVector param, odeproblem *prob);
  void idata_row();
  unsigned int get_idata_row(double ID){return idmap[ID];}
  void locate_tran();
  void get_records(recstack& a, int NID, int neq, int& obscount, int& evcount, bool obsonly,bool debug);
  //int col_n(std::string name) {return col.at(name);}
  void check_idcol(dataobject *data);
  double get_value(int row, int col) {return Data(row,col);}
  Rcpp::IntegerVector get_col_n(Rcpp::CharacterVector what);


 protected:

  dvec Uid;
  ivec Startrow;
  ivec Endrow;
  int Idcol;
  Rcpp::NumericMatrix Data;
  Rcpp::CharacterVector Data_names;

  std::vector<unsigned int> col;
  Rcpp::IntegerVector par_from;  // data set index
  Rcpp::IntegerVector par_to;    // parameter list index
  Rcpp::CharacterVector parnames;
  di_map idmap;

  Rcpp::IntegerVector cmt_from; // data set index
  Rcpp::IntegerVector cmt_to;  // cmt index
  Rcpp::CharacterVector cmtnames;
};


#endif


