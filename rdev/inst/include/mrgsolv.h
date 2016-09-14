// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

#ifndef MRGSOLV_H
#define MRGSOLV_H

#include <vector>
#include <map>

typedef std::vector<double> dvec;
typedef std::map<std::string,double> sd_map;
typedef std::map<std::string,double> tablemap;
typedef std::vector<std::string > svec;
typedef std::vector<int > ivec;
//typedef  const std::vector<double> cdvec;


#define MRGSOLVE_INIT_SIGNATURE  double* _A_0_,const double* _A_, const double* _THETA_, dvec& _F_, dvec& _ALAG_, dvec& _R_, dvec& _D_,  databox& _databox_, dvec& _pred_

#define MRGSOLVE_TABLE_SIGNATURE const double* _A_, const double* _A_0_,  const double* _THETA_,  const dvec& _F_, const dvec& _R_, sd_map& _tabledata_, databox& _databox_, const dvec& _pred_, dvec& _capture_

#define MRGSOLVE_ODE_SIGNATURE const double* _ODETIME_, const double* _A_, double* _DADT_,  const double* _A_0_, const double* _THETA_

#define MRGSOLVE_CONFIG_SIGNATURE svec& capture_names, dvec& capture_values

#endif
