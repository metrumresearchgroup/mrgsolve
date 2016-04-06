// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifndef MRGSOLV_H
#define MRGSOLV_H
typedef std::vector<bool> bvec;
typedef std::vector<double> dvec;
typedef std::map<std::string,double> sd_map;
typedef std::vector<double> vec_double;

typedef std::map<std::string,double> tablemap;
typedef sd_map::iterator sdmit;
typedef std::vector<std::string > svec;
typedef std::vector<int > ivec;
typedef std::map<std::string,bool> sbmap;
typedef std::map<double,int> di_map;

typedef  const std::vector<double> cdvec;
typedef  const std::map<std::string,double> csd_map;



#define MRGSOLVE_INIT_SIGNATURE  dvec& _A_0_,const double* _A_, const dvec& _THETA_, dvec& _F_, dvec& _ALAG_, dvec& _R_, dvec& _D_,  databox& _databox_, dvec& _pred_

#define MRGSOLVE_TABLE_SIGNATURE const double* _A_, cdvec& _A_0_,  cdvec& _THETA_,  cdvec& _F_, cdvec& _R_, sd_map& _tabledata_, databox& _databox_, const dvec& _pred_

#define MRGSOLVE_ODE_SIGNATURE const  double* _ODETIME_, const double* _A_, double* _DADT_,  const dvec& _A_0_, cdvec& _THETA_

#endif
