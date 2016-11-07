#ifndef MRGX_H
#define MRGX_H

#include "modelheader.h"

namespace mrgx {
  Rcpp::Environment get_envir(databox& self) {
    return (*(reinterpret_cast<Rcpp::Environment*>(self.envir)));
  }
}
#endif
