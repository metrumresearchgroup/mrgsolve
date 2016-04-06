// This work is licensed under the Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.
// To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-nd/4.0/ or send a letter to
// Creative Commons, PO Box 1866, Mountain View, CA 94042, USA.

//* MRGSOLVE file
#define INSERTQUOTES 1
#include "modelheader.h"
#ifndef MODELINCLUDEGUARD
#define MODEL___
#define INITFUN___ _model_housemodel_CODEBLOCKmain__
#define ODEFUN___ _model_housemodel_CODEBLOCKode__
#define TABLECODE___ _model_housemodel_CODEBLOCKtable__
#define GUT _A_[0]
#define CENT _A_[1]
#define RESP _A_[2]
#define GUT_0 _A_0_[0]
#define CENT_0 _A_0_[1]
#define RESP_0 _A_0_[2]
#define dxdt_GUT _DADT_[0]
#define dxdt_CENT _DADT_[1]
#define dxdt_RESP _DADT_[2]
#define CL _THETA_.at(0)
#define VC _THETA_.at(1)
#define KA _THETA_.at(2)
#define F1 _THETA_.at(3)
#define WT _THETA_.at(4)
#define SEX _THETA_.at(5)
#define WTCL _THETA_.at(6)
#define WTVC _THETA_.at(7)
#define SEXCL _THETA_.at(8)
#define SEXVC _THETA_.at(9)
#define KIN _THETA_.at(10)
#define KOUT _THETA_.at(11)
#define IC50 _THETA_.at(12)
#define MODELINCLUDEGUARD
#endif
