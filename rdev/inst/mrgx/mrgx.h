#ifndef MRGX_H
#define MRGX_H

#include "modelheader.h"
#define mrgx_get_envir() (*(reinterpret_cast<Rcpp::Environment*>(_databox_.envir)))

namespace mrgx {
  
  Rcpp::Environment get_envir(databox& _databox_) {
    return mrgx_get_envir(); 
  }
  
}

#endif
