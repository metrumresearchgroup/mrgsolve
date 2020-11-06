// Copyright (C) 2013 - 2019  Metrum Research Group
//
// This file is part of mrgsolve.
//
// mrgsolve is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// mrgsolve is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with mrgsolve.  If not, see <http://www.gnu.org/licenses/>.


/**
 * @file dataobject.h
 */

#ifndef DATAOBJECT_H
#define DATAOBJECT_H

#include <vector>
#include "odeproblem.h"
#include "RcppInclude.h"

typedef std::deque<double> uidtype;

class dataobject {
  
public:
  //! constructor
  dataobject(Rcpp::NumericMatrix _data, 
             Rcpp::CharacterVector _parnames);
  
  //! constructor
  dataobject(Rcpp::NumericMatrix _data, 
             Rcpp::CharacterVector _parnames, 
             Rcpp::CharacterVector _initnames);
  
  Rcpp::NumericMatrix Data;
  
  ~dataobject();
  
  unsigned int nrow() const  {return Data.nrow();}
  unsigned int ncol() const {return Data.ncol();}
  unsigned int nid() const {return Uid.size();}
  unsigned int idcol() const {return Idcol;}
  int start(int i) const {return Startrow.at(i);}
  int end(int i) const {return Endrow.at(i);}
  void map_uid();
  double get_uid(int i) const {return Uid.at(i);}
  uidtype return_uid()  {return Uid;}
  void copy_parameters(int this_row,odeproblem *prob);
  void copy_next_parameters(int id_n, bool from_data, int this_row, odeproblem *prob);
  void next_id(int id_n);
  void copy_inits(int this_row,odeproblem *prob);
  void reload_parameters(const Rcpp::NumericVector& param, odeproblem *prob);
  void idata_row();
  unsigned int get_idata_row(const double ID);  
  void locate_tran();
  void get_records(recstack& a, int NID, int neq, 
                   unsigned int& obscount, unsigned int& evcount, bool obsonly,
                   bool debug);
  void get_records_pred(recstack& a, int NID, int neq, unsigned int& obscount, 
                        unsigned int& evcount, bool obsonly,bool debug);
  // Function commented in dataobject.cpp
  // void expand_records(recstack& a, dataobject& idat, int& NID, 
  //                     unsigned int& obscount, unsigned int& evcount, bool debug);
  void check_idcol(dataobject& data);
  double get_value(const int row, const int col) const {return Data(row,col);}
  double get_id_value(const int row) const {return Data(row,Idcol);}
  Rcpp::IntegerVector get_col_n(const Rcpp::CharacterVector& what);
  void carry_out(const recstack& a, 
                 Rcpp::NumericMatrix& ans,
                 dataobject& idat,
                 const Rcpp::IntegerVector& data_carry,
                 const unsigned int data_carry_start,
                 const Rcpp::IntegerVector& idata_carry,
                 const unsigned int idata_carry_start, 
                 const bool nocb);
  std::vector<unsigned int> col;
  Rcpp::CharacterVector Data_names;
  
  std::deque<double> Uid;  ///< unique IDs in the data set
  std::deque<int> Startrow;  ///< start row for each ID
  std::deque<int> Endrow; ///< data set end row for each ID
  int Idcol; ///< which column holds ID
  std::map<double,int> idmap; ///< map to get 
  
  std::vector<int> par_from;  ///< index for parameters in data set
  std::vector<int>  par_to;    ///< index for parameters in param list
  Rcpp::CharacterVector parnames; ///< names of model parameters
  
  std::vector<int> cmt_from; ///< index for compartments in data set
  std::vector<int> cmt_to;  ///< index for compartments in init list
  Rcpp::CharacterVector cmtnames; ///< names of model compartments
  
  bool any_copy; ///< are there any parameter columns for copy / update?
  bool done_copying; ///< have we reached the last data record for this ID?
  int next_copy_row; ///< tracking current actual data row
  int last_copy_row; ///< tracking previous actual data row
  
};


#endif


