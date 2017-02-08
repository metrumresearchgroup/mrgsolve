// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.


/**
 * @file dataobject.h
 */

#ifndef DATAOBJECT_H
#define DATAOBJECT_H

#include <vector>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include "odeproblem.h"
#include "RcppInclude.h"

typedef std::map<double,int> idat_map;
typedef std::deque<double> uidtype;
typedef std::deque<int> datarowtype;


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
  
  virtual ~dataobject();
  
  unsigned int nrow() const {return Data.nrow();}
  unsigned int ncol() const {return Data.ncol();}
  unsigned int nid() const {return Uid.size();}
  unsigned int idcol() const {return Idcol;}
  int start(int i) const {return Startrow.at(i);}
  int end(int i) const {return Endrow.at(i);}
  void map_uid();
  double get_uid(int i) const {return Uid.at(i);}
  uidtype return_uid() const {return Uid;}
  void copy_parameters(int this_row,odeproblem *prob);
  void copy_inits(int this_row,odeproblem *prob);
  void reload_parameters(const Rcpp::NumericVector& param, odeproblem *prob);
  void idata_row();
  unsigned int get_idata_row(const double ID)  {return idmap[ID];}
  void locate_tran();
  void get_records(recstack& a, int NID, int neq, unsigned int& obscount, unsigned int& evcount, bool obsonly,bool debug);
  void check_idcol(dataobject& data);
  double get_value(const int row, const int col) const {return Data(row,col);}
  double get_id_value(const int row) const {return Data(row,Idcol);}
  void get_ids(uidtype* ids);
  Rcpp::IntegerVector get_col_n(const Rcpp::CharacterVector& what);
  void carry_out(const recstack& a, 
                 Rcpp::NumericMatrix& ans,
                 dataobject& idat,
                 const Rcpp::IntegerVector& data_carry,
                 const unsigned int data_carry_start,
                 const Rcpp::IntegerVector& idata_carry,
                 const unsigned int idata_carry_start);
    
protected:
  
  uidtype Uid;  ///< unique IDs in the data set
  datarowtype Startrow;  ///< start row for each ID
  datarowtype Endrow; ///< data set end row for each ID
  int Idcol; ///< which column holds ID value
  
  Rcpp::CharacterVector Data_names;
  
  std::vector<unsigned int> col;
  
  Rcpp::IntegerVector par_from;  ///< index for parameters in data set
  Rcpp::IntegerVector par_to;    ///< index for parameters in param list
  Rcpp::CharacterVector parnames; ///< names of model parameters
  idat_map idmap; ///< map to get 
  
  Rcpp::IntegerVector cmt_from; ///< index for compartments in data set
  Rcpp::IntegerVector cmt_to;  ///< index for compartments in init list
  Rcpp::CharacterVector cmtnames; ///< names of model compartments
};


#endif


