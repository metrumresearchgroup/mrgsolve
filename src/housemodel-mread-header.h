// Source MD5: 8b31b620adebe9145ea11a12f5d6800b

// PLUGINS:

// FIXED:
// No fixed parameters.

// NAMESPACES:

// MODEL HEADER FILES:
#include "mrgsolv.h"
#include "modelheader.h"

// INCLUDE databox functions:
#include "databox_cpp.h"

// USING plugins:

// INCLUDES:


// GLOBAL CODE BLOCK:
// GLOBAL VARS FROM BLOCKS & TYPEDEFS:
// DECLARED BY USER
typedef double capture;
namespace {
  double CLi;
  double VCi;
  double KAi;
  double KOUTi;
  double DV;
}
// DECLARED VIA AUTODEC

// GLOBAL START USER CODE:
#define CP (CENT/VCi)
#define INH (CP/(IC50+CP))
typedef double localdouble;

// DEFS:
#define __INITFUN___ _model_housemodel_main__
#define __ODEFUN___ _model_housemodel_ode__
#define __TABLECODE___ _model_housemodel_table__
#define __EVENTFUN___ _model_housemodel_event__
#define __CONFIGFUN___ _model_housemodel_config__
#define __REGISTERFUN___ R_init_housemodel
#define _nEQ 3
#define _nPAR 14
#define ECL _xETA(1)
#define EVC _xETA(2)
#define EKA _xETA(3)
#define EKOUT _xETA(4)
#define EXPO _xEPS(1)

